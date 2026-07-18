namespace IronKernel

/// Core and CPS intermediate representations for the Kernel compiler.
/// Surface programs remain homoiconic LispVal trees; analysis produces CoreExpr.
module Ir =

    open Ast
    open SymbolTable
    open Contracts

    /// Explicit core forms after analysis. Residual wraps trees that stay interpreted.
    type CoreExpr =
        | CLit of LispVal
        | CVar of string
        | CQuote of LispVal
        | CIf of CoreExpr * CoreExpr * CoreExpr
        | CSeq of CoreExpr list
        | CDefine of CoreExpr * CoreExpr
        | CVau of formals: LispVal * envarg: string * body: CoreExpr list
        | CApp of op: CoreExpr * args: CoreExpr list
        | COperate of op: CoreExpr * operands: LispVal list
        | CIntrinsicOperate of identity: PrimitiveIdentity * operands: LispVal list
        | CGuarded of guard: BindingGuard * specialized: CoreExpr * fallback: CoreExpr
        | CContractFold of guard: ContractGuard * folded: LispVal * fallback: CoreExpr
        | CEval of envExpr: CoreExpr * expr: CoreExpr
        | CReset of CoreExpr
        | CResidual of LispVal

    /// CPS IR — mirrors the interpreter's ContinuationRecord / DeferredCode model.
    type CpsCont =
        | CpsHalt
        | CpsMeta of CpsCont
        | CpsNative of name: string * CpsCont
        | CpsKernel of body: CoreExpr list * CpsCont

    type CpsExpr =
        | CpsValue of CoreExpr * CpsCont
        | CpsOperate of op: CoreExpr * args: CoreExpr list * CpsCont
        | CpsBind of formals: LispVal * value: CoreExpr * body: CpsExpr

    /// Operative vs applicative calling convention (compiler ABI).
    /// Operatives receive unevaluated operand trees + the dynamic environment.
    /// Applicatives evaluate arguments, then operate the underlying combiner.
    type CallingConvention =
        | OperativeCall
        | ApplicativeCall

    let rec showCore = function
        | CLit v -> "lit:" + showVal v
        | CVar s -> "var:" + s
        | CQuote v -> "quote:" + showVal v
        | CIf (a, b, c) -> sprintf "(if %s %s %s)" (showCore a) (showCore b) (showCore c)
        | CSeq xs -> "(seq " + String.concat " " (List.map showCore xs) + ")"
        | CDefine (l, r) -> sprintf "(define %s %s)" (showCore l) (showCore r)
        | CVau (f, e, body) -> sprintf "(vau %s %s ...)" (showVal f) e
        | CApp (op, args) -> sprintf "(%s %s)" (showCore op) (String.concat " " (List.map showCore args))
        | COperate (op, _) -> sprintf "(operate %s ...)" (showCore op)
        | CIntrinsicOperate (identity, _) -> sprintf "(intrinsic %A ...)" identity
        | CGuarded (guard, specialized, _) ->
            sprintf "(guard %s@%d %s)" guard.name guard.version (showCore specialized)
        | CContractFold (guard, folded, _) ->
            sprintf "(contract-fold %s@%d %s)" guard.name guard.version (showVal folded)
        | CEval (e, x) -> sprintf "(eval %s %s)" (showCore e) (showCore x)
        | CReset x -> sprintf "(reset %s)" (showCore x)
        | CResidual v -> "residual:" + showVal v
