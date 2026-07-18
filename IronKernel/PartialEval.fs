namespace IronKernel

module PartialEval =

    open Ast
    open Contracts
    open Eval
    open Ir

    let private isStaticLiteral = function
        | Bool _
        | Inert
        | Nil
        | Keyword _
        | List [] -> true
        | Obj value ->
            isNull value
            || value :? string
            || value :? byte
            || value :? int
            || value :? int64
            || value :? float32
            || value :? double
        | _ -> false

    let private isFoldedValue = isStaticLiteral

    let rec partialEvaluate env expression =
        match expression with
        | COperate(CVar name, operands) as fallback
            when List.forall isStaticLiteral operands ->
            match tryCreateContractGuard env name with
            | Some (guard, _) ->
                match eval env (newContinuation env) (List(Atom name :: operands)) with
                | Choice2Of2 value when isFoldedValue value ->
                    CContractFold(guard, value, fallback)
                | _ -> fallback
            | None -> fallback
        | CIf(condition, consequent, alternative) ->
            CIf(
                partialEvaluate env condition,
                partialEvaluate env consequent,
                partialEvaluate env alternative)
        | CSeq expressions ->
            CSeq(List.map (partialEvaluate env) expressions)
        | CDefine(lhs, rhs) ->
            CDefine(lhs, partialEvaluate env rhs)
        | CGuarded(guard, specialized, fallback) ->
            CGuarded(guard, partialEvaluate env specialized, fallback)
        | other -> other
