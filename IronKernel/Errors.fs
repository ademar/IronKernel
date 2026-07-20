namespace IronKernel

module Errors =
    
    open Ast
    open System

    let throwError error = Choice1Of2(error)
    let succeed p = Choice2Of2(p)

    let catchError action (f: LispError -> ThrowsError<LispVal>) = 
        match action with
        |Choice1Of2(error) -> f error
        |Choice2Of2(result) -> action

    let rec showError = function
        | UnboundVar(msg,varname) -> msg + ": '" + varname + "' "
        | BadSpecialForm(msg,form) -> msg + ": " + showVal form
        | NotFunction(msg,func) -> msg + ": " + func
        | NumArgs(expected,found) -> "Expected " + expected.ToString()  + " args, found values " + unwordsList found
        | TypeMismatch(expected,found) -> "Invalid type: expected " + expected  + ", found " + showVal found
        | ClrTypeMismatch(expected,found) -> "Invalid type: expected " + expected  + ", found " + found
        | Parser(parseError) -> "Parse error: " + parseError
        | Default(msg) -> msg
        | ClrException ex -> ex.Message
        | CapabilityDenied message -> "Capability denied: " + message
        | ContractViolation message -> "Contract violation: " + message
        | LocatedError(span, sourceLine, error) ->
            let source =
                if String.IsNullOrWhiteSpace span.sourceName then "<input>"
                else span.sourceName
            let position = span.startPosition
            let header =
                sprintf "%s:%d:%d: %s" source position.line position.column (showError error)
            match sourceLine with
            | None -> header
            | Some line ->
                let indent = String(' ', max 0 (int position.column - 1))
                let requestedWidth =
                    if span.endPosition.line = position.line then
                        int (span.endPosition.column - position.column)
                    else 1
                let availableWidth = max 1 (line.Length - indent.Length)
                let width = min availableWidth (max 1 requestedWidth)
                header + Environment.NewLine + line + Environment.NewLine
                + indent + String('^', width)

    let trapError (action:ThrowsError<LispVal>) : ThrowsError<LispVal> = 
        catchError action (fun x -> succeed (Ast.Status(showError x)))
    
    let extractValue : ThrowsError<LispVal> -> LispVal = function
        | Choice2Of2 value -> value
        | Choice1Of2 error -> Status(showError error)

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


