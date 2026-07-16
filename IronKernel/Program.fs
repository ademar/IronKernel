open System
open IronKernel.Repl
open IronKernel.Emit

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | [] ->
        runRepl ()
        0
    | "compile" :: input :: rest ->
        let output =
            match rest with
            | ["-o"; path] | ["--output"; path] -> path
            | [path] when not (path.StartsWith("-")) -> path
            | _ -> System.IO.Path.ChangeExtension(input, ".dll")
        match compileFileToAssembly input output with
        | Choice2Of2 path ->
            printfn "Wrote %s" path
            0
        | Choice1Of2 err ->
            eprintfn "Compile error: %s" (IronKernel.Errors.showError err)
            1
    | filename :: args ->
        runOne filename args
        0
