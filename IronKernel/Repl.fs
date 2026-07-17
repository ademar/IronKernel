namespace IronKernel
    
module Repl =

    open Ast
    open System
    open Errors
    open Eval
    open Parser
    open SymbolTable
    open Choice
    open Runtime
    open Compiler
    open Emit
    open Mono.Terminal
    open System.Text

    let lineEditor = LineEditor("ironkernel")

    let flushStr (str:String) =
      Console.Write(str)

    let putStrLn (str:String) =
      Console.WriteLine(str)

    let readPrompt prompt = 
      fun _ -> lineEditor.Edit(prompt, "")

    let evalString env cont expr = 
        let evaled = 
            match (readExpr expr) with
            |Choice1Of2(error) -> throwError error
            |Choice2Of2(result) -> try 
                                    // Prefer hybrid compiled evaluation; residual path covers full Kernel.
                                    evalCompiled env cont result
                                   with ex -> throwError (ClrException ex)
        extractValue (trapError evaled) 

    let evalAndPrint env cont expr : unit = (evalString env cont expr |> showVal |> putStrLn)

    let until cond (prompt:unit -> string) action =
      let rec loop _ =
        let result = prompt ()
        if result <> null && not(cond result) then  
            if result <> "" then action result
            loop ()
      loop ()

    let runOne filename (args:string list) =
        match bootstrapEnv () with
        | Choice1Of2 error ->
            eprintfn "Startup error: %s" (showError error)
            1
        | Choice2Of2 standardEnv ->
            let env =
                bindVars standardEnv
                    [ "args", List (List.map (fun x -> Ast.Obj(x :> obj)) args) ]
            match eval env (newContinuation env) (List [Atom "load"; Ast.Obj(filename :> obj)]) with
            | Choice1Of2 error ->
                eprintfn "Script error: %s" (showError error)
                1
            | Choice2Of2 _ -> 0

    let version = "0.2.0-net10"

    let showBanner _ = 
        putStrLn ""
        putStrLn (sprintf " IronKernel v%s" version)
        putStrLn " Full-Kernel hybrid CLR compiler"
        putStrLn " https://github.com/ironkernel-lang/IronKernel"
        putStrLn ""

    let runRepl _ =
        Console.OutputEncoding <- Encoding.UTF8
        showBanner ()
        match bootstrapEnv () with
        | Choice1Of2 error ->
            eprintfn "Startup error: %s" (showError error)
            1
        | Choice2Of2 env ->
            let run = evalAndPrint env (newContinuation env)
            until (fun x -> x.ToLowerInvariant().Equals("quit"))
                (readPrompt "IronKernel> ") run
            0
