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

    let rec private tryFindBinding name = function
        | [] -> ValueNone
        | (bindingName, cell) :: _ when bindingName = name -> ValueSome cell
        | _ :: rest -> tryFindBinding name rest

    let private resolveBindingCellValue env var =
        let visited = Collections.Generic.HashSet<obj>(Collections.Generic.ReferenceEqualityComparer.Instance)
        let mutable pending = [env]
        let mutable result = ValueNone

        while result.IsNone && not pending.IsEmpty do
            let current = pending.Head
            pending <- pending.Tail
            match current with
            | Environment record when visited.Add(record :> obj) ->
                match tryFindBinding var !record.bindings with
                | ValueSome cell -> result <- ValueSome cell
                | ValueNone ->
                    for parent in List.rev record.parents do
                        match parent with
                        | Environment _ -> pending <- parent :: pending
                        | _ -> ()
            | _ -> ()

        result

    let resolveBindingCell env var =
        resolveBindingCellValue env var |> ValueOption.toOption

    let getVar' env var =
        resolveBindingCell env var |> Option.map (fun cell -> cell.state.value)

    let setVar' env var value =
        match resolveBindingCell env var with
        | Some cell ->
            updateBindingCell cell value
            Some value
        | None -> None

    /// Only bare primitive operatives carry a guarded identity. An Applicative
    /// wrapping a primitive (e.g. after `(define if (wrap if))`) must not match,
    /// because the compiler fast path invokes operative semantics while the
    /// binding evaluates all operands first.
    let private primitiveIdentity = function
        | PrimitiveOperative primitive -> primitive.identity
        | _ -> None

    let tryCreateBindingGuard env name expectedIdentity =
        match resolveBindingCell env name with
        | Some cell ->
            let state = cell.state
            if primitiveIdentity state.value = Some expectedIdentity then
                Some
                    { name = name
                      cellId = cell.id
                      version = state.version
                      expectedIdentity = expectedIdentity }
            else None
        | _ -> None

    let bindingGuardMatches env guard =
        match resolveBindingCell env guard.name with
        | Some cell ->
            let state = cell.state
            cell.id = guard.cellId
            && state.version = guard.version
            && primitiveIdentity state.value = Some guard.expectedIdentity
        | None -> false

    let bindingHasPrimitiveIdentity env name expectedIdentity =
        resolveBindingCell env name
        |> Option.exists (fun cell -> primitiveIdentity cell.state.value = Some expectedIdentity)

    let getVar env var = 
        match getVar' env var with
        |Some(x) -> succeed x
        |None      -> throwError (UnboundVar("Getting an unbound variable",var))

    let setVar env var value = 
        match setVar' env var value with
        |Some(x) -> succeed x
        |None      -> throwError (UnboundVar("Getting an unbound variable",var))

    let defineVar env var value =
        match env with
        | Environment record ->
            let result = !record.bindings |> List.tryFind (keyEq var)
            match result with
            | Some (_, cell) ->
                updateBindingCell cell value
                succeed value
            | None ->
                record.bindings := (var, newBindingCell value) :: !record.bindings
                succeed value
        | found -> throwError(TypeMismatch("environment", found))

    /// Import bindings into the environment
    let bindVars env bindings =
        match env with
        | Environment record ->
            Environment
                { record with
                    bindings =
                        ref
                            ((bindings
                              |> List.map (fun (name, value) -> name, newBindingCell value))
                             @ !record.bindings) }
        | _ -> invalidArg (nameof env) "Expected an environment"
