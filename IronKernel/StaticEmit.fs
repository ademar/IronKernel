namespace IronKernel

/// Builds persisted managed artifacts from statically generated F# source.
module StaticEmit =

    open System
    open System.Diagnostics
    open System.IO
    open System.Security
    open Ast
    open Errors
    open Choice

    type private ArtifactMode =
        | Managed
        | Native of rid: string

    let private projectSource (assemblyName: string) (references: string list) mode =
        let referenceItems =
            references
            |> List.map (fun path ->
                let name = Path.GetFileNameWithoutExtension path
                sprintf
                    "    <Reference Include=\"%s\"><HintPath>%s</HintPath><Private>true</Private></Reference>"
                    (SecurityElement.Escape name)
                    (SecurityElement.Escape path))
            |> String.concat Environment.NewLine
        let nativeProperties =
            match mode with
            | Managed -> ""
            | Native rid ->
                $"""    <TreatWarningsAsErrors>false</TreatWarningsAsErrors>
    <PublishAot>true</PublishAot>
        <RuntimeIdentifier>{SecurityElement.Escape rid}</RuntimeIdentifier>
        <SelfContained>true</SelfContained>
        <TrimmerSingleWarn>false</TrimmerSingleWarn>
    """
        $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>{SecurityElement.Escape assemblyName}</AssemblyName>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    {nativeProperties}  </PropertyGroup>
  <ItemGroup>
        <Compile Include="Program.fs" />
    </ItemGroup>
    <ItemGroup>
{referenceItems}
  </ItemGroup>
</Project>
"""

    let private artifactName (inputPath: string) =
        let raw = Path.GetFileNameWithoutExtension inputPath
        let valid =
            raw
            |> Seq.map (fun character ->
                if Char.IsLetterOrDigit character || character = '_' || character = '-' then character
                else '_')
            |> Seq.toArray
            |> String
        if String.IsNullOrWhiteSpace valid then "IronKernelProgram" else valid

    let private resolveLibraryPath name =
        [ name; Path.Combine(AppContext.BaseDirectory, name) ]
        |> List.tryFind File.Exists
        |> Option.defaultValue name

    let private analyzeSource env path =
        let source = File.ReadAllText path
        match Parser.readLocatedExprList path source with
        | Choice1Of2 error -> Choice1Of2 error
        | Choice2Of2 forms ->
            forms
            |> List.map (Analyze.analyzeLocatedGuarded env source)
            |> Choice2Of2

    let private configureNativeEnvironment mode temporaryDirectory (startInfo: ProcessStartInfo) =
        match mode with
        | Native rid when rid.StartsWith("osx-", StringComparison.OrdinalIgnoreCase) ->
            let libraryRoots =
                [ [ "/opt/homebrew/opt/openssl@3/lib"; "/usr/local/opt/openssl@3/lib" ],
                    [ "libssl.a"; "libcrypto.a" ]
                  [ "/opt/homebrew/opt/brotli/lib"; "/usr/local/opt/brotli/lib" ],
                    [ "libbrotlienc.a"; "libbrotlidec.a"; "libbrotlicommon.a" ] ]
            let staticLibraryDirectory = Path.Combine(temporaryDirectory, "native-libs")
            let missing = System.Collections.Generic.List<string>()
            for candidates, archives in libraryRoots do
                match candidates |> List.tryFind Directory.Exists with
                | Some root ->
                    for archive in archives do
                        let source = Path.Combine(root, archive)
                        if File.Exists source then
                            Directory.CreateDirectory staticLibraryDirectory |> ignore
                            File.Copy(source, Path.Combine(staticLibraryDirectory, archive), true)
                        else
                            missing.Add archive
                | None -> archives |> List.iter missing.Add
            if missing.Count > 0 then
                invalidOp(
                    "macOS NativeAOT requires Homebrew openssl@3 and brotli static libraries; missing: "
                    + String.concat ", " missing)
            else
                let existing =
                    match startInfo.Environment.TryGetValue "LIBRARY_PATH" with
                    | true, value when not (String.IsNullOrWhiteSpace value) -> value + string Path.PathSeparator
                    | _ -> ""
                startInfo.Environment["LIBRARY_PATH"] <- existing + staticLibraryDirectory
        | _ -> ()

    let private compileFileToArtifact mode profile inputPath outputDirectory : ThrowsError<string> =
        try
            let analysisEnvironment = Runtime.makePrimitiveBindingsForProfile profile
            let sources =
                [ resolveLibraryPath "kernel.ikr"
                  resolveLibraryPath "promises.ikr"
                  inputPath ]
            let expressions =
                sources
                |> List.fold (fun state path ->
                    match state with
                    | Choice1Of2 error -> Choice1Of2 error
                    | Choice2Of2 accumulated ->
                        match analyzeSource analysisEnvironment path with
                        | Choice1Of2 error -> Choice1Of2 error
                        | Choice2Of2 analyzed -> Choice2Of2(accumulated @ analyzed)) (Choice2Of2 [])
            match expressions with
            | Choice1Of2 error -> throwError error
            | Choice2Of2 expressions ->
                match StaticCompiler.generateProgram profile expressions with
                | Error message -> throwError (Default message)
                | Ok program ->
                    let name = artifactName inputPath
                    let temporaryDirectory =
                        Path.Combine(Path.GetTempPath(), "ironkernel-managed-" + Guid.NewGuid().ToString("N"))
                    try
                        Directory.CreateDirectory temporaryDirectory |> ignore
                        Directory.CreateDirectory outputDirectory |> ignore
                        let runtimeAssembly = typeof<LispVal>.Assembly.Location
                        let projectPath = Path.Combine(temporaryDirectory, name + ".fsproj")
                        File.WriteAllText(projectPath, projectSource name [runtimeAssembly] mode)
                        File.WriteAllText(Path.Combine(temporaryDirectory, "Program.fs"), program)

                        let startInfo = ProcessStartInfo("dotnet")
                        startInfo.WorkingDirectory <- temporaryDirectory
                        startInfo.UseShellExecute <- false
                        startInfo.RedirectStandardError <- true
                        startInfo.RedirectStandardOutput <- true
                        configureNativeEnvironment mode temporaryDirectory startInfo
                        for argument in
                            [ "publish"
                              projectPath
                              "-c"
                              "Release"
                              "-o"
                              Path.GetFullPath outputDirectory
                              "--nologo" ] do
                            startInfo.ArgumentList.Add argument
                        use child = Process.Start startInfo
                        let stdout = child.StandardOutput.ReadToEnd()
                        let stderr = child.StandardError.ReadToEnd()
                        child.WaitForExit()
                        if child.ExitCode <> 0 then
                            let detail = if String.IsNullOrWhiteSpace stderr then stdout else stderr
                            let kind = match mode with Managed -> "Managed" | Native _ -> "Native"
                            throwError (Default(kind + " artifact build failed: " + detail.Trim()))
                        else
                            let fileName =
                                match mode with
                                | Managed -> name + ".dll"
                                | Native rid when rid.StartsWith("win-", StringComparison.OrdinalIgnoreCase) ->
                                    name + ".exe"
                                | Native _ -> name
                            returnM (Path.Combine(Path.GetFullPath outputDirectory, fileName))
                    finally
                        if Directory.Exists temporaryDirectory then
                            Directory.Delete(temporaryDirectory, true)
        with ex ->
            let kind = match mode with Managed -> "Managed" | Native _ -> "Native"
            throwError (Default(kind + " artifact build failed: " + ex.Message))

    let compileFileToManagedArtifact profile inputPath outputDirectory =
        compileFileToArtifact Managed profile inputPath outputDirectory

    let compileFileToNativeArtifact profile rid inputPath outputDirectory =
        if profile = Unrestricted then
            throwError (Default "Native artifacts support only the minimal and safe profiles")
        elif String.IsNullOrWhiteSpace rid then
            throwError (Default "Native artifact publishing requires a runtime identifier")
        else
            compileFileToArtifact (Native rid) profile inputPath outputDirectory