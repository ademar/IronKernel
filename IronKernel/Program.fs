open System
open IronKernel.Repl

let main _ =
    let args = Environment.GetCommandLineArgs()
    match Array.toList args with 
    | [programName] -> runRepl ()
    | programName::filename::args -> runOne filename args
    | _ -> putStrLn "Program only takes one or two arguments."

main ()
