namespace IronKernel

/// Lowers homoiconic LispVal trees to CoreExpr for compilation / IR evaluation.
module Analyze =

    open Ast
    open Ir
    open SymbolTable
    open ClrSugar

    let rec analyze (value: LispVal) : CoreExpr =
        match value with
        | Atom id -> CVar id
        | Bool _
        | Keyword _
        | Inert
        | Nil
        | Obj _
        | Vector _
        | Encapsulation _
        | Environment _
        | PrimitiveOperative _
        | ContractedCombiner _
        | Operative _
        | Applicative _
        | Continuation _
        | PromptTag _
        | Resumption _
        | IOFunc _
        | Port _
        | Status _ -> CLit value
        | List [] -> CLit (List [])
        | DottedList _ as form -> CResidual form
        | List (Atom name :: args) ->
            // Desugar Clojure-style CLR calls before binding analysis so the
            // hybrid compiler sees ordinary `.` / `new` / `.get` combinations.
            match tryRewrite name args with
            | Some rewritten -> analyze rewritten
            | None -> COperate (analyze (Atom name), args)
        | List (op :: args) ->
            // Kernel has no syntactically privileged operator names. Preserve operand
            // trees exactly and let runtime binding lookup select operative semantics.
            // Specialized forms require binding-identity guards, which this analyzer
            // intentionally does not yet have.
            COperate (analyze op, args)
        | other -> CResidual other

    let analyzeForms (forms: LispVal list) : CoreExpr list =
        List.map analyze forms

    let rec analyzeGuarded env (value: LispVal) : CoreExpr =
        match value with
        | List (Atom "if" :: [condition; consequent; alternative] as form) ->
            let fallback = COperate(CVar "if", List.tail form)
            match tryCreateBindingGuard env "if" PrimitiveIf with
            | Some guard ->
                CGuarded(
                    guard,
                    CIntrinsicOperate(PrimitiveIf, [condition; consequent; alternative]),
                    fallback)
            | None -> fallback
        | List (Atom "define" :: [Atom name; rhs] as form) ->
            let fallback = COperate(CVar "define", List.tail form)
            match tryCreateBindingGuard env "define" PrimitiveDefine with
            | Some guard ->
                CGuarded(
                    guard,
                    CIntrinsicOperate(PrimitiveDefine, [Atom name; rhs]),
                    fallback)
            | None -> fallback
        | List (Atom name :: operands) ->
            // Prefer a real binding over CLR call sugar (same rule as Eval).
            match getVar' env name with
            | Some _ -> COperate(analyzeGuarded env (Atom name), operands)
            | None ->
                match tryRewrite name operands with
                | Some rewritten -> analyzeGuarded env rewritten
                | None -> COperate(analyzeGuarded env (Atom name), operands)
        | List (op :: operands) ->
            COperate(analyzeGuarded env op, operands)
        | other -> analyze other

    let analyzeFormsGuarded env forms =
        List.map (analyzeGuarded env) forms

    let rec analyzeLocatedGuarded env source (value: Source.LocatedValue) =
        let expression = analyzeGuarded env (Source.toLispVal value)
        let expressionWithLocatedOperator =
            match value.kind, expression with
            | Source.LList [_; condition; consequent; alternative],
              CGuarded (guard, CIntrinsicOperate (PrimitiveIf, _), fallback) ->
                CGuarded(
                    guard,
                    CIf(
                        analyzeLocatedGuarded env source condition,
                        analyzeLocatedGuarded env source consequent,
                        analyzeLocatedGuarded env source alternative),
                    fallback)
            | Source.LList [_; { kind = Source.LAtom name }; rhs],
              CGuarded (guard, CIntrinsicOperate (PrimitiveDefine, _), fallback) ->
                CGuarded(
                    guard,
                    CDefine(CVar name, analyzeLocatedGuarded env source rhs),
                    fallback)
            | Source.LList (operator :: operands), COperate (_, rawOperands) ->
                let isClrSugar =
                    match operator.kind with
                    | Source.LAtom name when getVar' env name |> Option.isNone ->
                        tryRewrite name (List.map Source.toLispVal operands)
                        |> Option.isSome
                    | _ -> false
                if isClrSugar then expression
                else COperate(analyzeLocatedGuarded env source operator, rawOperands)
            | _ -> expression
        CLocated(
            value.span,
            Source.sourceLineAt source value.span.startPosition.line,
            expressionWithLocatedOperator)

    /// Reify CoreExpr back to a LispVal tree for residual evaluation.
    let rec toLispVal = function
        | CLit v -> v
        | CVar s -> Atom s
        | CQuote v -> List [Atom "quote"; v]
        | CIf (a, b, c) -> List [Atom "if"; toLispVal a; toLispVal b; toLispVal c]
        | CSeq xs -> List (Atom "sequence" :: List.map toLispVal xs)
        | CDefine (l, r) -> List [Atom "define"; toLispVal l; toLispVal r]
        | CVau (f, e, body) -> List (Atom "vau" :: f :: Atom e :: List.map toLispVal body)
        | CApp (op, args) -> List (toLispVal op :: List.map toLispVal args)
        | COperate (op, operands) -> List (toLispVal op :: operands)
        | CIntrinsicOperate (PrimitiveIf, operands) -> List (Atom "if" :: operands)
        | CIntrinsicOperate (PrimitiveDefine, operands) -> List (Atom "define" :: operands)
        | CGuarded (_, _, fallback) -> toLispVal fallback
        | CContractFold (_, _, fallback) -> toLispVal fallback
        | CEval (e, x) -> List [Atom "eval"; toLispVal e; toLispVal x]
        | CReset x -> List [Atom "reset"; toLispVal x]
        | CResidual v -> v
        | CLocated (_, _, expression) -> toLispVal expression
