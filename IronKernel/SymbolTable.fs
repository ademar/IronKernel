namespace IronKernel

module SymbolTable =

    open System
    open System.Threading
    open Ast
    open Errors

    type BindingGuard = {
        name : string
        cellId : int64
        version : int64
        expectedIdentity : PrimitiveIdentity
    }

    let mutable private nextBindingId = 0L

    let private newBindingCell value =
        { id = Interlocked.Increment(&nextBindingId)
          state = { value = value; version = 0L } }

    let private updateBindingCell cell value =
        let state = cell.state
        cell.state <- { value = value; version = state.version + 1L }

    let keyEq name (k,_) = k = name

    let rec resolveBindingCell (Environment(env, parents)) var =
        match !env |> List.tryFind (keyEq var) with
        | Some (_, cell) -> Some cell
        | None ->
            let rec search = function
                | [] -> None
                | (Environment _ as parent) :: rest ->
                    match resolveBindingCell parent var with
                    | Some cell -> Some cell
                    | None -> search rest
                | _ :: rest -> search rest
            search parents

    let getVar' env var =
        resolveBindingCell env var |> Option.map (fun cell -> cell.state.value)

    let setVar' env var value =
        match resolveBindingCell env var with
        | Some cell ->
            updateBindingCell cell value
            Some value
        | None -> None

    let rec private primitiveIdentity = function
        | PrimitiveOperative primitive -> primitive.identity
        | Applicative combiner -> primitiveIdentity combiner
        | _ -> None

    let tryCreateBindingGuard env name expectedIdentity =
        match resolveBindingCell env name with
        | Some cell when primitiveIdentity cell.state.value = Some expectedIdentity ->
            Some
                { name = name
                  cellId = cell.id
                  version = cell.state.version
                  expectedIdentity = expectedIdentity }
        | _ -> None

    let bindingGuardMatches env guard =
        match resolveBindingCell env guard.name with
        | Some cell ->
            cell.id = guard.cellId
            && cell.state.version = guard.version
            && primitiveIdentity cell.state.value = Some guard.expectedIdentity
        | None -> false

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
        | Some (_, cell) ->
            updateBindingCell cell value
            succeed value
        | None ->
            env := (var, newBindingCell value) :: !env
            succeed value

    /// Import bindings into the environment
    let bindVars (Environment(env,fr)) bindings =
        Environment(ref ((bindings |> List.map (fun (name, value) -> name, newBindingCell value)) @ !env),fr)
