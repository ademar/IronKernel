namespace IronKernel

module Errors =
    
    open Ast
    open System

    let throwError (error:LispError) = Choice1Of2(error)
    let succeed p = Choice2Of2(p)

    let catchError action (f: LispError -> ThrowsError<LispVal>) = 
        match action with
        |Choice1Of2(error) -> f error
        |Choice2Of2(result) -> action

    let showError = function
        | UnboundVar(msg,varname) -> msg + ": '" + varname + "' "
        | BadSpecialForm(msg,form) -> msg + ": " + showVal form
        | NotFunction(msg,func) -> msg + ": " + func
        | NumArgs(expected,found) -> "Expected " + expected.ToString()  + " args, found values " + unwordsList found
        | TypeMismatch(expected,found) -> "Invalid type: expected " + expected  + ", found " + showVal found
        | ClrTypeMismatch(expected,found) -> "Invalid type: expected " + expected  + ", found " + found
        | Parser(parseError) -> "Parse error at " + parseError
        | Default(msg) -> msg

    let trapError (action:ThrowsError<LispVal>) : ThrowsError<LispVal> = 
        catchError action (fun x -> succeed (Ast.Status(showError x)))
    
    let extractValue (Choice2Of2 x) = x

    let rec sequence lst result =
        match lst with
        |[] -> succeed (result |> List.rev)
        |Choice2Of2(p)::tail -> sequence tail (p::result)
        |Choice1Of2(p)::_    -> throwError p 

    let mapM f a = sequence (List.map f a)

    let bind p rest =
        match p with
            | Choice1Of2 r  -> rest r
            | Choice2Of2 e -> p

    let (>>=) a b = fun x -> bind (a x) b


