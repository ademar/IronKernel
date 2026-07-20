namespace IronKernel

/// Expression-tree / hybrid compiler for Core IR.
/// Compiles statically visible forms to CLR delegates; residual trees fall back
/// to the trampolined interpreter so full Kernel semantics (vau, eval, conts) remain.
module Compiler =

    open System
    open System.Linq.Expressions
    open Ast
    open Errors
    open Ir
    open Analyze
    open Eval
    open SymbolTable
    open Contracts
    open PartialEval
    open Choice

    type KernelFunc = Func<LispVal, LispVal, ThrowsError<LispVal>>

    type Helpers =
        static member Continue(env: LispVal, cont: LispVal, v: LispVal) : ThrowsError<LispVal> =
            continueEval env cont v
        static member Lookup(env: LispVal, cont: LispVal, name: string) : ThrowsError<LispVal> =
            match SymbolTable.getVar env name with
            | Choice2Of2 r -> continueEval env cont r
            | Choice1Of2 e -> throwError e
        static member IfThenElse(env: LispVal, cont: LispVal, fc: KernelFunc, fa: KernelFunc, fb: KernelFunc) : ThrowsError<LispVal> =
            match fc.Invoke(env, newContinuation env) with
            | Choice2Of2 (Bool true) -> fa.Invoke(env, cont)
            | Choice2Of2 (Bool false) -> fb.Invoke(env, cont)
            | Choice2Of2 found -> throwError (TypeMismatch("bool", found))
            | Choice1Of2 e -> throwError e
        static member Seq(env: LispVal, cont: LispVal, forms: KernelFunc[]) : ThrowsError<LispVal> =
            let rec loop i =
                if i >= forms.Length then continueEval env cont Inert
                elif i = forms.Length - 1 then forms.[i].Invoke(env, cont)
                else
                    match forms.[i].Invoke(env, newContinuation env) with
                    | Choice1Of2 e -> throwError e
                    | Choice2Of2 _ -> loop (i + 1)
            loop 0
        static member Define(env: LispVal, cont: LispVal, name: string, fr: KernelFunc) : ThrowsError<LispVal> =
            match fr.Invoke(env, newContinuation env) with
            | Choice1Of2 e -> throwError e
            | Choice2Of2 v ->
                match SymbolTable.defineVar env name v with
                | Choice1Of2 e -> throwError e
                | Choice2Of2 _ -> continueEval env cont Inert
        static member EvalForms(env: LispVal, cont: LispVal, fe: KernelFunc, fx: KernelFunc) : ThrowsError<LispVal> =
            match fe.Invoke(env, newContinuation env) with
            | Choice1Of2 e -> throwError e
            | Choice2Of2 envVal ->
                match fx.Invoke(env, newContinuation env) with
                | Choice1Of2 e -> throwError e
                | Choice2Of2 code -> eval envVal cont code
        /// Kernel combination: evaluate operator, then operate with *unevaluated* operands.
        /// Applicatives evaluate their arguments inside `operate`; operatives see raw trees.
        static member App(env: LispVal, cont: LispVal, fop: KernelFunc, operands: LispVal[]) : ThrowsError<LispVal> =
            match fop.Invoke(env, newContinuation env) with
            | Choice1Of2 e -> throwError e
            | Choice2Of2 combiner -> operate env cont combiner (Array.toList operands)

    let private resolveHelper name parameterTypes =
        let methodInfo =
            typeof<Helpers>.GetMethod(
                name,
                Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Static,
                null,
                parameterTypes,
                null)
        if isNull methodInfo || methodInfo.ReturnType <> typeof<ThrowsError<LispVal>> then
            invalidOp (sprintf "Compiler helper '%s' has an incompatible signature" name)
        methodInfo

    let private continueMethod =
        resolveHelper "Continue" [| typeof<LispVal>; typeof<LispVal>; typeof<LispVal> |]

    let private lookupMethod =
        resolveHelper "Lookup" [| typeof<LispVal>; typeof<LispVal>; typeof<string> |]

    let private ifThenElseMethod =
        resolveHelper
            "IfThenElse"
            [| typeof<LispVal>; typeof<LispVal>; typeof<KernelFunc>; typeof<KernelFunc>; typeof<KernelFunc> |]

    let private appMethod =
        resolveHelper
            "App"
            [| typeof<LispVal>; typeof<LispVal>; typeof<KernelFunc>; typeof<LispVal[]> |]

    let rec compileToFunc (expr: CoreExpr) : KernelFunc =
        match expr with
        | CLit v ->
            let envP = Expression.Parameter(typeof<LispVal>, "env")
            let contP = Expression.Parameter(typeof<LispVal>, "cont")
            let call =
                Expression.Call(
                    continueMethod,
                    envP, contP, Expression.Constant(v, typeof<LispVal>))
            Expression.Lambda<KernelFunc>(call, envP, contP).Compile()
        | CVar name ->
            let envP = Expression.Parameter(typeof<LispVal>, "env")
            let contP = Expression.Parameter(typeof<LispVal>, "cont")
            let call =
                Expression.Call(
                    lookupMethod,
                    envP, contP, Expression.Constant(name))
            Expression.Lambda<KernelFunc>(call, envP, contP).Compile()
        | CQuote v ->
            compileToFunc (CLit v)
        | CIf (cond, a, b) ->
            let fc = compileToFunc cond
            let fa = compileToFunc a
            let fb = compileToFunc b
            let envP = Expression.Parameter(typeof<LispVal>, "env")
            let contP = Expression.Parameter(typeof<LispVal>, "cont")
            let call =
                Expression.Call(
                    ifThenElseMethod,
                    envP, contP,
                    Expression.Constant(fc),
                    Expression.Constant(fa),
                    Expression.Constant(fb))
            Expression.Lambda<KernelFunc>(call, envP, contP).Compile()
        | CSeq xs ->
            let compiled = List.map compileToFunc xs |> List.toArray
            KernelFunc(fun env cont -> Helpers.Seq(env, cont, compiled))
        | CDefine (CVar name, rhs) ->
            let fr = compileToFunc rhs
            KernelFunc(fun env cont -> Helpers.Define(env, cont, name, fr))
        | CVau (formals, envarg, body) ->
            let bodyLv = List.map toLispVal body
            KernelFunc(fun env cont ->
                let op = Operative { prms = formals; envarg = envarg; body = bodyLv; closure = env }
                continueEval env cont op)
        | CEval (envE, exprE) ->
            let fe = compileToFunc envE
            let fx = compileToFunc exprE
            KernelFunc(fun env cont -> Helpers.EvalForms(env, cont, fe, fx))
        | CReset body ->
            // Delimited continuations must go through the trampoline interpreter so
            // shift sees the proper meta-continuation chain (including under begin/applicatives).
            let form = List [Atom "reset"; toLispVal body]
            KernelFunc(fun env cont -> eval env cont form)
        | CApp (op, args) ->
            let fop = compileToFunc op
            let operands = List.map toLispVal args |> List.toArray
            let envP = Expression.Parameter(typeof<LispVal>, "env")
            let contP = Expression.Parameter(typeof<LispVal>, "cont")
            let call =
                Expression.Call(
                    appMethod,
                    envP, contP,
                    Expression.Constant(fop),
                    Expression.Constant(operands))
            Expression.Lambda<KernelFunc>(call, envP, contP).Compile()
        | COperate (op, operands) ->
            let fop = compileToFunc op
            let ops = List.toArray operands
            KernelFunc(fun env cont -> Helpers.App(env, cont, fop, ops))
        | CIntrinsicOperate (identity, operands) ->
            KernelFunc(fun env cont ->
                match identity with
                | PrimitiveIf -> run (Runtime.if_then_else env cont operands)
                | PrimitiveDefine -> run (Runtime.define env cont operands))
        | CGuarded (guard, specialized, fallback) ->
            let fast = compileToFunc specialized
            let generic = compileToFunc fallback
            KernelFunc(fun env cont ->
                if bindingGuardMatches env guard then fast.Invoke(env, cont)
                else generic.Invoke(env, cont))
        | CContractFold (guard, folded, fallback) ->
            let generic = compileToFunc fallback
            KernelFunc(fun env cont ->
                if contractGuardMatches env guard then
                    continueEval env cont folded
                else generic.Invoke(env, cont))
        | CResidual v ->
            KernelFunc(fun env cont -> eval env cont v)
        | CLocated (span, sourceLine, expression) ->
            let compiled = compileToFunc expression
            KernelFunc(fun env cont ->
                match compiled.Invoke(env, cont) with
                | Choice1Of2 (LocatedError _ as error) -> throwError error
                | Choice1Of2 error -> throwError (LocatedError(span, sourceLine, error))
                | Choice2Of2 value -> returnM value)
        | other ->
            let v = toLispVal other
            KernelFunc(fun env cont -> eval env cont v)

    let compileLispVal (v: LispVal) = compileToFunc (analyze v)
    let compileLispValGuarded env (v: LispVal) =
        analyzeGuarded env v
        |> partialEvaluate env
        |> compileToFunc

    let compileForms (forms: LispVal list) = List.map compileLispVal forms
    let compileFormsGuarded env forms = List.map (compileLispValGuarded env) forms

    let evalCompiled env cont (v: LispVal) = compileLispValGuarded env v |> fun f -> f.Invoke(env, cont)

    let analyzeAndCompile (source: string) : ThrowsError<KernelFunc list> =
        match Parser.readExprList source with
        | Choice1Of2 e -> throwError e
        | Choice2Of2 forms -> returnM (compileForms forms)

    type LocatedKernelFunc = {
        func : KernelFunc
        span : SourceSpan
        sourceLine : string option
    }

    let analyzeAndCompileLocated env sourceName (source: string) : ThrowsError<LocatedKernelFunc list> =
        match Parser.readLocatedExprList sourceName source with
        | Choice1Of2 e -> throwError e
        | Choice2Of2 forms ->
            forms
            |> List.map (fun form ->
                let compiled =
                    analyzeLocatedGuarded env source form
                    |> partialEvaluate env
                    |> compileToFunc
                { func = compiled
                  span = Source.spanOf form
                  sourceLine = Source.sourceLineAt source form.span.startPosition.line })
            |> returnM
