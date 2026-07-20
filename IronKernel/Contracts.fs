namespace IronKernel

module Contracts =

    open Ast
    open SymbolTable

    type ContractGuard = {
        name : string
        cellId : int64
        version : int64
        contractSnapshot : OperativeContract
    }

    let shapeName = function
        | AnyShape -> "any"
        | NumberShape -> "number"
        | IntegerShape -> "integer"
        | StringShape -> "string"
        | BooleanShape -> "boolean"
        | AtomShape -> "atom"
        | ListShape -> "list"
        | PromptTagShape -> "prompt-tag"
        | ResumptionShape -> "resumption"

    let shapeMatches shape value =
        match shape, value with
        | AnyShape, _ -> true
        | NumberShape, Obj (:? byte)
        | NumberShape, Obj (:? int)
        | NumberShape, Obj (:? int64)
        | NumberShape, Obj (:? float32)
        | NumberShape, Obj (:? double) -> true
        | IntegerShape, Obj (:? byte)
        | IntegerShape, Obj (:? int)
        | IntegerShape, Obj (:? int64) -> true
        | StringShape, Obj (:? string) -> true
        | BooleanShape, Bool _ -> true
        | AtomShape, Atom _ -> true
        | ListShape, List _
        | ListShape, DottedList _ -> true
        | PromptTagShape, PromptTag _ -> true
        | ResumptionShape, Resumption _ -> true
        | _ -> false

    let validateArguments contract args =
        if List.length contract.operands <> List.length args then
            Some(
                ContractViolation(
                    sprintf
                        "%s expected %d operands, found %d"
                        contract.name
                        (List.length contract.operands)
                        (List.length args)))
        else
            List.zip contract.operands args
            |> List.tryFindIndex (fun (shape, value) -> not (shapeMatches shape value))
            |> Option.map (fun index ->
                let expected = contract.operands.[index] |> shapeName
                ContractViolation(
                    sprintf "%s operand %d expected %s" contract.name (index + 1) expected))

    let validateResult contract value =
        if shapeMatches contract.result value then None
        else
            Some(
                ContractViolation(
                    sprintf "%s result expected %s" contract.name (shapeName contract.result)))

    let rec stripContract = function
        | ContractedCombiner contracted -> stripContract contracted.combiner
        | value -> value

    let tryGetContract = function
        | ContractedCombiner contracted -> Some contracted.contract
        | Applicative (ContractedCombiner contracted) -> Some contracted.contract
        | _ -> None

    let attach contract value =
        let normalized =
            match value with
            | Applicative underlying -> Applicative(stripContract underlying)
            | other -> stripContract other
        match contract.mode, normalized with
        | RawOperands, Applicative _ -> None
        | EvaluatedArguments, Applicative underlying ->
            Some(
                Applicative(
                    ContractedCombiner
                        { combiner = underlying
                          contract = contract }))
        | RawOperands, (Operative _ | PrimitiveOperative _ | CompiledCombiner _) ->
            Some(
                ContractedCombiner
                    { combiner = normalized
                      contract = contract })
        | _ -> None

    let tryCreateContractGuard env name =
        match resolveBindingCell env name with
        | Some cell ->
            let state = cell.state
            match tryGetContract state.value with
            | Some contract
                when contract.mode = EvaluatedArguments
                     && contract.effect = Pure
                     && contract.inlineable
                     && contract.trust = Certified ->
                 let guard = { name = name; cellId = cell.id; version = state.version; contractSnapshot = contract }
                 Some(guard, contract)
            | _ -> None
        | None -> None

    let contractGuardMatches env guard =
        match resolveBindingCell env guard.name with
        | Some cell ->
            let state = cell.state
            cell.id = guard.cellId
            && state.version = guard.version
            && (tryGetContract state.value
                |> Option.exists (fun contract -> contract = guard.contractSnapshot))
        | None -> false

    let certifiedApplicative name operands result =
        { name = name
          mode = EvaluatedArguments
          operands = operands
          result = result
          effect = Pure
          inlineable = true
          trust = Certified }
