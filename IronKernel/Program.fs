open System
open System.IO
open IronKernel.Repl
open IronKernel.Emit
open IronKernel.Ast
open IronKernel.Errors

let private usage =
    """Usage:
  ironkernel                         Start the REPL
  ironkernel <file.scm> [args...]    Run a source script
  ironkernel run <file> [args...]    Run a .scm script or .ikc package
  ironkernel compile <file.scm> [-o <file.ikc>]
  ironkernel --help
  ironkernel --version"""

let private usageError message =
    eprintfn "%s\n\n%s" message usage
    2

let private runPackage (path: string) (args: string list) =
    match loadIkcWithArgs path args with
    | Choice1Of2 error ->
        eprintfn "Package error: %s" (showError error)
        1
    | Choice2Of2 Inert -> 0
    | Choice2Of2 value ->
        printfn "%s" (showVal value)
        0

let private runPath (path: string) (args: string list) =
    if String.Equals(Path.GetExtension(path), ".ikc", StringComparison.OrdinalIgnoreCase) then
        runPackage path args
    else
        runOne path args

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | [] -> runRepl ()
    | ["--help"] | ["-h"] | ["help"] ->
        printfn "%s" usage
        0
    | ["--version"] | ["-V"] ->
        printfn "IronKernel %s" version
        0
    | ["run"] -> usageError "Missing file for 'run'."
    | "run" :: path :: args -> runPath path args
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
            match compileFileToPackage input output with
            | Choice2Of2 path ->
                printfn "Wrote %s" path
                0
            | Choice1Of2 err ->
                eprintfn "Compile error: %s" (showError err)
                1
    | option :: _ when option.StartsWith("-") ->
        usageError ("Unknown option: " + option)
    | filename :: args -> runPath filename args
