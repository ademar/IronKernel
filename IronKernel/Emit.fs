namespace IronKernel

/// Emits Kernel programs to CLR packages and runs compiled forms.
module Emit =

    open System
    open System.IO
    open System.Reflection
    open System.Reflection.Emit
    open Ast
    open Errors
    open Runtime
    open Compiler
    open Choice
    open Eval

    let runCompiledForms (env: LispVal) (forms: KernelFunc list) : ThrowsError<LispVal> =
        let cont = newContinuation env
        let rec loop (fs: KernelFunc list) (last: LispVal) =
            match fs with
            | [] -> returnM last
            | f :: rest ->
                match f.Invoke(env, cont) with
                | Choice1Of2 e -> throwError e
                | Choice2Of2 v -> loop rest v
        loop forms Inert

    let compileSource (source: string) : ThrowsError<KernelFunc list> =
        analyzeAndCompile source

    let compileFile (path: string) : ThrowsError<KernelFunc list> =
        try
            compileSource (File.ReadAllText path)
        with ex -> throwError (Default ("Failed to read '" + path + "': " + ex.Message))

    let private writeIkcPackage (outputPath: string) (source: string) =
        use fs = File.Create outputPath
        let magic = Text.Encoding.ASCII.GetBytes("IKC1")
        fs.Write(magic, 0, magic.Length)
        let bytes = Text.Encoding.UTF8.GetBytes source
        let len = BitConverter.GetBytes(bytes.Length)
        fs.Write(len, 0, len.Length)
        fs.Write(bytes, 0, bytes.Length)

    let bootstrapEnv () =
        let env = primitiveBindings
        let loadFile name =
            let path =
                if File.Exists name then name
                else Path.Combine(AppContext.BaseDirectory, name)
            eval env (newContinuation env) (List [Atom "load"; Obj (path :> obj)])
        match loadFile "kernel.scm" with
        | Choice1Of2 e -> throwError e
        | Choice2Of2 _ ->
            match loadFile "promises.scm" with
            | Choice1Of2 e -> throwError e
            | Choice2Of2 _ -> returnM env

    type EmitHelpers =
        static member RunSource(source: string) : string =
            match bootstrapEnv () with
            | Choice1Of2 e -> showError e
            | Choice2Of2 env ->
                match analyzeAndCompile source with
                | Choice1Of2 e -> showError e
                | Choice2Of2 forms ->
                    match runCompiledForms env forms with
                    | Choice1Of2 e -> showError e
                    | Choice2Of2 v -> showVal v

        static member RunSourceFile(path: string) : string =
            EmitHelpers.RunSource(File.ReadAllText path)

    /// Compile a Kernel source file to an IKC1 package (.dll extension by convention).
    let compileFileToAssembly (inputPath: string) (outputPath: string) : ThrowsError<string> =
        match bootstrapEnv () with
        | Choice1Of2 e -> throwError e
        | Choice2Of2 env ->
            match compileFile inputPath with
            | Choice1Of2 e -> throwError e
            | Choice2Of2 forms ->
                match runCompiledForms env forms with
                | Choice1Of2 e -> throwError e
                | Choice2Of2 _ ->
                    let source = File.ReadAllText inputPath

                    let asmName = AssemblyName(Path.GetFileNameWithoutExtension outputPath)
                    let asmBuilder = AssemblyBuilder.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.Run)
                    let modBuilder = asmBuilder.DefineDynamicModule(asmName.Name)
                    let typeBuilder = modBuilder.DefineType("IronKernelProgram", TypeAttributes.Public ||| TypeAttributes.Class)
                    let methodBuilder =
                        typeBuilder.DefineMethod(
                            "Run",
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<string>,
                            [||])
                    let il = methodBuilder.GetILGenerator()
                    let runSource = typeof<EmitHelpers>.GetMethod("RunSource")
                    il.Emit(OpCodes.Ldstr, source)
                    il.Emit(OpCodes.Call, runSource)
                    il.Emit(OpCodes.Ret)
                    typeBuilder.CreateType() |> ignore

                    writeIkcPackage outputPath source
                    File.WriteAllText(Path.ChangeExtension(outputPath, ".ikc"), source)
                    returnM outputPath

    let loadIkc (path: string) : ThrowsError<LispVal> =
        try
            use fs = File.OpenRead path
            let magic = Array.zeroCreate 4
            if fs.Read(magic, 0, 4) <> 4 then throwError (Default "Truncated IKC package")
            elif Text.Encoding.ASCII.GetString magic <> "IKC1" then
                throwError (Default "Not an IronKernel compiled package (missing IKC1 header)")
            else
                let lenBytes = Array.zeroCreate 4
                fs.Read(lenBytes, 0, 4) |> ignore
                let len = BitConverter.ToInt32(lenBytes, 0)
                let bytes = Array.zeroCreate len
                fs.Read(bytes, 0, len) |> ignore
                let source = Text.Encoding.UTF8.GetString bytes
                match bootstrapEnv () with
                | Choice1Of2 e -> throwError e
                | Choice2Of2 env ->
                    match compileSource source with
                    | Choice1Of2 e -> throwError e
                    | Choice2Of2 forms -> runCompiledForms env forms
        with
        | :? IOException as ex -> throwError (Default ex.Message)
