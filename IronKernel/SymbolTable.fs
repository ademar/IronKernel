namespace IronKernel

module SymbolTable =

    open Ast
    open Errors
    
    let keyEq name (k,_) = k = name

    let rec getVar' (Environment(env,st)) var = 
        let result = !env |> List.tryFind (keyEq var)
        match result with
        |Some(_,x) -> Some !x
        |None      -> let r = List.choose (fun x -> getVar' x var) st 
                      match r with
                      |s::_ -> Some s
                      |[] -> None

    let rec setVar' (Environment(env,st)) var value = 
        let result = !env |> List.tryFind (keyEq var)
        match result with
        |Some(_,x) -> x := value; Some value
        |None      -> let r = List.choose (fun x -> setVar' x var value) st 
                      match r with
                      |s::_ -> Some s
                      |[] -> None

    let getVar env var = 
        match getVar' env var with
        |Some(x) -> succeed x
        |None      -> throwError (UnboundVar("Getting an unbound variable",var))

    let setVar env var value = 
        match setVar' env var value with
        |Some(x) -> succeed x
        |None      -> throwError (UnboundVar("Getting an unbound variable",var))

    let defineVar (Environment(env,_)) var value = 
        let result = !env |> List.tryFind (keyEq var)
        match result with
        |Some(_,x) -> x := value ; succeed value
        |None      -> env := (var,ref value) :: !env; succeed value

    /// Import bindings into the environment
    let bindVars (Environment(env,fr)) bindings = 
        Environment(ref ((bindings |> List.map ( fun (x,y) -> x, ref y)) @ !env),fr)
