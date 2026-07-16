namespace IronKernel

/// Lowers homoiconic LispVal trees to CoreExpr for compilation / IR evaluation.
module Analyze =

    open Ast
    open Ir

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
        | Operative _
        | Applicative _
        | Continuation _
        | IOFunc _
        | Port _
        | Status _ -> CLit value
        | List [] -> CLit (List [])
        | List (Atom "quote" :: [x]) -> CQuote x
        | List (Atom "if" :: [cond; a; b]) -> CIf (analyze cond, analyze a, analyze b)
        | List (Atom "if" :: _) as form -> CResidual form
        | List (Atom "define" :: [lhs; rhs]) -> CDefine (analyze lhs, analyze rhs)
        | List (Atom "define" :: _) as form -> CResidual form
        | List (Atom "vau" :: prms :: Atom envarg :: body) ->
            CVau (prms, envarg, List.map analyze body)
        | List (Atom "vau" :: _) as form -> CResidual form
        | List (Atom "reset" :: [exp]) -> CReset (analyze exp)
        | List (Atom "reset" :: _) as form -> CResidual form
        | List (Atom "eval" :: [envE; exprE]) -> CEval (analyze envE, analyze exprE)
        | List (Atom "eval" :: _) as form -> CResidual form
        | List (Atom "sequence" :: body)
        | List (Atom "begin" :: body) -> CSeq (List.map analyze body)
        | DottedList _ as form -> CResidual form
        | List (op :: args) ->
            // Conservative: mark as applicative-style app; runtime still dispatches operatives.
            CApp (analyze op, List.map analyze args)
        | other -> CResidual other

    let analyzeForms (forms: LispVal list) : CoreExpr list =
        List.map analyze forms

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
        | CEval (e, x) -> List [Atom "eval"; toLispVal e; toLispVal x]
        | CReset x -> List [Atom "reset"; toLispVal x]
        | CResidual v -> v
