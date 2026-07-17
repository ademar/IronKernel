open System
open System.IO
open IronKernel.Repl
open IronKernel.Emit
open IronKernel.Ast
open IronKernel.Errors

let private usage =
    """Usage:
  ironkernel [--profile <profile>]                         Start the REPL
  ironkernel [--profile <profile>] <file.scm> [args...]    Run a source script
  ironkernel [--profile <profile>] run <file> [args...]    Run a .scm script or .ikc package
  ironkernel [--profile <profile>] compile <file.scm> [-o <file.ikc>]
  ironkernel --help
  ironkernel --version

Profiles: minimal, safe, unrestricted (default)"""

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
    if String.Equals(Path.GetExtension(path), ".ikc", StringComparison.OrdinalIgnoreCase) then
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

let private dispatch profile args =
    match args with
    | [] -> runReplWithProfile profile ()
    | ["--help"] | ["-h"] | ["help"] ->
        printfn "%s" usage
        0
    | ["--version"] | ["-V"] ->
        printfn "IronKernel %s" version
        0
    | ["run"] -> usageError "Missing file for 'run'."
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
