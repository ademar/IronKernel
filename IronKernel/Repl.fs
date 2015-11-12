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
    open Mono.Terminal

    open System.Text

    let lineEditor = LineEditor("ironkernel")

    let getLine () =
      Console.InputEncoding  <- Encoding.Unicode
      Console.ReadLine ()

    let flushStr (str:String) () =
      Console.OutputEncoding <- Encoding.Unicode
      Console.Write(str)

    let putStrLn (str:String) () =
      Console.OutputEncoding <- Encoding.Unicode
      Console.WriteLine(str)

    let readPrompt prompt = 
      fun _ -> lineEditor.Edit(prompt, "")

    let evalString env cont expr = 
        let evaled = 
            match (readExpr expr) with
            |Choice1Of2(error) -> throwError error
            |Choice2Of2(result) -> try 
                                    eval env cont result
                                   with ex -> throwError (ClrException ex)
        extractValue (trapError evaled) 

    let evalAndPrint env cont expr : unit = (evalString env cont expr |> showVal |> putStrLn) ()

    let rec until cond (prompt:unit -> string) action =
        let result = prompt ()
        if not(cond result) then  
            if result <> "" then action result
            until cond prompt action

    let runOne filename (args:string list) = 
        let env = bindVars primitiveBindings [ "args", List (List.map (fun x -> Ast.Obj( x :> obj)) args) ]
        either { 
            let! p = eval env (newContinuation env) (List [Atom "load"; Ast.Obj filename]) 
            return (p |> showVal |> flushStr) ()
            } |> ignore

    let version = "0.1"

    let showBanner _ = 
        putStrLn "                                                                      " ()
        putStrLn (sprintf " IronKernel v%s                                                       " version) ()
        putStrLn " http://ironkernel.net                                                " ()
        putStrLn " (c) 2012 Code Maker Inc.                                             " ()
        putStrLn "                                                                      " ()
        
    let runRepl _ = 
        showBanner ()
        until (fun x -> x.ToLower().Equals("quit")) 
            (readPrompt "IronKernel> ") (evalAndPrint primitiveBindings (newContinuation primitiveBindings))


