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

    let private projectSource (assemblyName: string) (references: string list) =
        let referenceItems =
            references
            |> List.map (fun path ->
                let name = Path.GetFileNameWithoutExtension path
                sprintf
                    "    <Reference Include=\"%s\"><HintPath>%s</HintPath><Private>true</Private></Reference>"
                    (SecurityElement.Escape name)
                    (SecurityElement.Escape path))
            |> String.concat Environment.NewLine
        $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>{SecurityElement.Escape assemblyName}</AssemblyName>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
  </PropertyGroup>
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

    let compileFileToManagedArtifact profile inputPath outputDirectory : ThrowsError<string> =
        try
            let source = File.ReadAllText inputPath
            match Parser.readExprListFromSource inputPath source with
            | Choice1Of2 error -> throwError error
            | Choice2Of2 forms ->
                match StaticCompiler.generateProgram profile (Analyze.analyzeForms forms) with
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
                        File.WriteAllText(projectPath, projectSource name [runtimeAssembly])
                        File.WriteAllText(Path.Combine(temporaryDirectory, "Program.fs"), program)

                        let startInfo = ProcessStartInfo("dotnet")
                        startInfo.WorkingDirectory <- temporaryDirectory
                        startInfo.UseShellExecute <- false
                        startInfo.RedirectStandardError <- true
                        startInfo.RedirectStandardOutput <- true
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
                            throwError (Default("Managed artifact build failed: " + detail.Trim()))
                        else
                            returnM (Path.Combine(Path.GetFullPath outputDirectory, name + ".dll"))
                    finally
                        if Directory.Exists temporaryDirectory then
                            Directory.Delete(temporaryDirectory, true)
        with ex ->
            throwError (Default("Managed artifact build failed: " + ex.Message))