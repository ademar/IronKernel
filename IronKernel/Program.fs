open System
open System.IO
open IronKernel.Repl
open IronKernel.Emit
open IronKernel.Ast
open IronKernel.Errors
open IronKernel.Project

let private usage =
    """Usage:
  ironkernel [--profile <profile>]                         Start the REPL
  ironkernel [--profile <profile>] <file.ikr> [args...]    Run a source script
  ironkernel [--profile <profile>] run <file> [args...]    Run a .ikr script or .ikc package
  ironkernel [--profile <profile>] compile <file.ikr> [-o <file.ikc>]
  ironkernel --help
  ironkernel --version

Profiles: minimal, safe, unrestricted (default)

Project commands:
  ik new <app|lib> <name>
  ik restore [project.ikproj] [--locked]
  ik run [project.ikproj] [args...]
  ik build|test|tree|pack [project.ikproj]
  ik add <package> <version> [--clr]
  ik remove <package>
  ik publish <source> [api-key]
  ik doctor"""

let private usageError message =
    eprintfn "%s\n\n%s" message usage
    2

let private runPackage profile (path: string) (args: string list) =
    match loadIkcWithArgsForProfile profile path args with
    | Choice1Of2 error ->
        eprintfn "Package error: %s" (showError error)
        1
    | Choice2Of2 Inert -> 0
    | Choice2Of2 value ->
        printfn "%s" (showVal value)
        0

let private runPath profile (path: string) (args: string list) =
    let extension = Path.GetExtension(path)
    if String.Equals(extension, ".scm", StringComparison.OrdinalIgnoreCase) then
        eprintfn "Warning: .scm source is deprecated; rename this file to .ikr."
    if String.Equals(extension, ".ikc", StringComparison.OrdinalIgnoreCase) then
        runPackage profile path args
    else
        runOneWithProfile profile path args

let private parseProfile = function
    | "minimal" -> Choice2Of2 Minimal
    | "safe" -> Choice2Of2 Safe
    | "unrestricted" -> Choice2Of2 Unrestricted
    | value -> Choice1Of2 ("Unknown capability profile: " + value)

let private parseGlobalOptions args =
    match args with
    | "--profile" :: value :: rest ->
        match parseProfile value with
        | Choice1Of2 error -> Choice1Of2 error
        | Choice2Of2 profile -> Choice2Of2(profile, rest)
    | "--profile" :: [] -> Choice1Of2 "Missing value for --profile."
    | rest -> Choice2Of2(Unrestricted, rest)

let private withProject explicitPath action =
    let path =
        match explicitPath with
        | Some value -> Some value
        | None -> discover (Directory.GetCurrentDirectory())
    match path with
    | None ->
        eprintfn "No .ikproj file found."
        2
    | Some path ->
        match Project.load path with
        | Choice1Of2 error ->
            eprintfn "Project error: %s" (showError error)
            2
        | Choice2Of2 project -> action project

let private optionalProject = function
    | path :: rest when Path.GetExtension(path).Equals(".ikproj", StringComparison.OrdinalIgnoreCase) ->
        Some path, rest
    | rest -> None, rest

let private dispatch profile args =
    match args with
    | [] -> runReplWithProfile profile ()
    | ["--help"] | ["-h"] | ["help"] ->
        printfn "%s" usage
        0
    | ["--version"] | ["-V"] ->
        printfn "IronKernel %s" version
        0
    | ["new"; kind; name] when kind = "app" || kind = "lib" ->
        Project.create kind name (Directory.GetCurrentDirectory())
    | "restore" :: rest ->
        let projectPath, options = optionalProject rest
        let locked = List.contains "--locked" options
        withProject projectPath (fun project -> Project.restore project locked)
    | ["build"] -> withProject None Project.build
    | ["test"] -> withProject None Project.test
    | ["tree"] -> withProject None Project.tree
    | ["pack"] -> withProject None Project.pack
    | "build" :: [projectPath]
    | "test" :: [projectPath]
    | "tree" :: [projectPath]
    | "pack" :: [projectPath] ->
        let action =
            match args.Head with
            | "build" -> Project.build
            | "test" -> Project.test
            | "tree" -> Project.tree
            | _ -> Project.pack
        withProject (Some projectPath) action
    | ["add"; id; version] ->
        withProject None (fun project -> Project.addPackage project.path id version "IronKernel")
    | ["add"; id; version; "--clr"] ->
        withProject None (fun project -> Project.addPackage project.path id version "Clr")
    | ["remove"; id] ->
        withProject None (fun project -> Project.removePackage project.path id)
    | ["publish"; source] ->
        let apiKey = Environment.GetEnvironmentVariable("NUGET_API_KEY")
        if String.IsNullOrWhiteSpace apiKey then usageError "Set NUGET_API_KEY before publishing."
        else withProject None (fun project -> Project.publish project source apiKey)
    | ["publish"; source; apiKey] ->
        withProject None (fun project -> Project.publish project source apiKey)
    | ["doctor"] -> Project.doctor ()
    | ["run"] -> withProject None (fun project -> Project.run project [])
    | "run" :: path :: scriptArgs
        when Path.GetExtension(path).Equals(".ikproj", StringComparison.OrdinalIgnoreCase) ->
        withProject (Some path) (fun project -> Project.run project scriptArgs)
    | "run" :: path :: scriptArgs -> runPath profile path scriptArgs
    | ["compile"] -> usageError "Missing source file for 'compile'."
    | "compile" :: input :: rest ->
        let outputResult =
            match rest with
            | [] -> Choice2Of2 (defaultPackagePath input)
            | ["-o"; path] | ["--output"; path] -> Choice2Of2 path
            | _ -> Choice1Of2 "Expected only '-o <file.ikc>' after the source file."
        match outputResult with
        | Choice1Of2 message -> usageError message
        | Choice2Of2 output ->
            match compileFileToPackageForProfile profile input output with
            | Choice2Of2 path ->
                printfn "Wrote %s" path
                0
            | Choice1Of2 err ->
                eprintfn "Compile error: %s" (showError err)
                1
    | option :: _ when option.StartsWith("-") ->
        usageError ("Unknown option: " + option)
    | filename :: scriptArgs -> runPath profile filename scriptArgs

[<EntryPoint>]
let main argv =
    match parseGlobalOptions (Array.toList argv) with
    | Choice1Of2 error -> usageError error
    | Choice2Of2 (profile, args) -> dispatch profile args
