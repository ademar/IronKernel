namespace IronKernel

/// Emits Kernel programs to CLR packages and runs compiled forms.
module Emit =

    open System
    open System.IO
    open Ast
    open Errors
    open Runtime
    open SymbolTable
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

    let runLocatedForms (env: LispVal) (forms: LocatedKernelFunc list) : ThrowsError<LispVal> =
        let cont = newContinuation env
        let rec loop remaining last =
            match remaining with
            | [] -> returnM last
            | form :: rest ->
                match form.func.Invoke(env, cont) with
                | Choice1Of2 (LocatedError _ as error) -> throwError error
                | Choice1Of2 error ->
                    throwError (LocatedError(form.span, form.sourceLine, error))
                | Choice2Of2 value -> loop rest value
        loop forms Inert

    let compileSource (source: string) : ThrowsError<KernelFunc list> =
        analyzeAndCompile source

    let compileSourceLocated sourceName source =
        analyzeAndCompileLocated sourceName source

    let private readSource (path: string) : ThrowsError<string> =
        try
            returnM (File.ReadAllText path)
        with ex -> throwError (Default ("Failed to read '" + path + "': " + ex.Message))

    let compileFile (path: string) : ThrowsError<KernelFunc list> =
        match readSource path with
        | Choice1Of2 e -> throwError e
        | Choice2Of2 source ->
            match compileSourceLocated path source with
            | Choice1Of2 e -> throwError e
            | Choice2Of2 forms -> forms |> List.map (fun form -> form.func) |> returnM

    let runSource (env: LispVal) sourceName source =
        match compileSourceLocated sourceName source with
        | Choice1Of2 e -> throwError e
        | Choice2Of2 forms -> runLocatedForms env forms

    let runSourceFile (env: LispVal) path =
        match readSource path with
        | Choice1Of2 e -> throwError e
        | Choice2Of2 source -> runSource env path source

    let private writeIkcPackage (outputPath: string) (source: string) : ThrowsError<string> =
        try
            use fs = File.Create outputPath
            let magic = Text.Encoding.ASCII.GetBytes("IKC1")
            fs.Write(magic, 0, magic.Length)
            let bytes = Text.Encoding.UTF8.GetBytes source
            let len = BitConverter.GetBytes(bytes.Length)
            fs.Write(len, 0, len.Length)
            fs.Write(bytes, 0, bytes.Length)
            returnM outputPath
        with ex ->
            throwError (Default ("Failed to write '" + outputPath + "': " + ex.Message))

    let bootstrapEnv () =
        let env = makePrimitiveBindings ()
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

    let defaultPackagePath inputPath = Path.ChangeExtension(inputPath, ".ikc")

    /// Validate and package Kernel source without evaluating it.
    let compileFileToPackage (inputPath: string) (outputPath: string) : ThrowsError<string> =
        if not (String.Equals(Path.GetExtension(outputPath), ".ikc", StringComparison.OrdinalIgnoreCase)) then
            throwError (Default "IronKernel packages must use the .ikc extension")
        else
            match readSource inputPath with
            | Choice1Of2 e -> throwError e
            | Choice2Of2 source ->
                match compileSourceLocated inputPath source with
                | Choice1Of2 e -> throwError e
                | Choice2Of2 _ -> writeIkcPackage outputPath source

    [<Obsolete("Use compileFileToPackage; IKC files are packages, not CLR assemblies.")>]
    let compileFileToAssembly inputPath outputPath =
        compileFileToPackage inputPath outputPath

    let private readExactly (stream: Stream) count =
        let bytes = Array.zeroCreate count
        let mutable offset = 0
        while offset < count do
            let read = stream.Read(bytes, offset, count - offset)
            if read = 0 then
                raise (EndOfStreamException("Truncated IKC package"))
            offset <- offset + read
        bytes

    let loadIkcWithArgs (path: string) (args: string list) : ThrowsError<LispVal> =
        try
            use fs = File.OpenRead path
            let magic = readExactly fs 4
            if Text.Encoding.ASCII.GetString magic <> "IKC1" then
                throwError (Default "Not an IronKernel compiled package (missing IKC1 header)")
            else
                let lenBytes = readExactly fs 4
                let len = BitConverter.ToInt32(lenBytes, 0)
                if len < 0 || int64 len > fs.Length - fs.Position then
                    throwError (Default "Truncated IKC package")
                else
                    let source = readExactly fs len |> Text.Encoding.UTF8.GetString
                    match bootstrapEnv () with
                    | Choice1Of2 e -> throwError e
                    | Choice2Of2 standardEnv ->
                        let env =
                            bindVars standardEnv
                                [ "args", List (List.map (fun arg -> Obj(arg :> obj)) args) ]
                        runSource env path source
        with
        | :? IOException as ex -> throwError (Default ("Failed to load '" + path + "': " + ex.Message))

    let loadIkc path = loadIkcWithArgs path []
