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

    type private CompilationWork =
        | CompileExpression of CoreExpr
        | BuildIf
        | BuildSequence of int
        | BuildDefine of string
        | BuildEval
        | BuildApp of LispVal[]
        | BuildOperation of LispVal[]
        | BuildGuarded of BindingGuard
        | BuildContractFold of ContractGuard * LispVal
        | BuildLocated of SourceSpan * string option

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
        static member AppNamed(env: LispVal, cont: LispVal, name: string, operands: LispVal[]) : ThrowsError<LispVal> =
            match SymbolTable.getVar env name with
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

    let private compileLiteral v =
        let envP = Expression.Parameter(typeof<LispVal>, "env")
        let contP = Expression.Parameter(typeof<LispVal>, "cont")
        let call =
            Expression.Call(
                continueMethod,
                envP, contP, Expression.Constant(v, typeof<LispVal>))
        Expression.Lambda<KernelFunc>(call, envP, contP).Compile()

    let private compileVariable name =
        let envP = Expression.Parameter(typeof<LispVal>, "env")
        let contP = Expression.Parameter(typeof<LispVal>, "cont")
        let call =
            Expression.Call(
                lookupMethod,
                envP, contP, Expression.Constant(name))
        Expression.Lambda<KernelFunc>(call, envP, contP).Compile()

    let compileToFunc (expr: CoreExpr) : KernelFunc =
        let mutable pending = [CompileExpression expr]
        let mutable completed : KernelFunc list = []

        let takeCompleted count =
            let mutable functions = []
            for _ in 1..count do
                match completed with
                | func :: rest ->
                    functions <- func :: functions
                    completed <- rest
                | [] -> invalidOp "Compiler work stack is incomplete"
            functions

        while not pending.IsEmpty do
            let work = pending.Head
            pending <- pending.Tail
            match work with
            | CompileExpression expression ->
                match expression with
                | CLit v -> completed <- compileLiteral v :: completed
                | CVar name -> completed <- compileVariable name :: completed
                | CQuote v -> completed <- compileLiteral v :: completed
                | CIf (condition, consequent, alternative) ->
                    pending <-
                        CompileExpression condition
                        :: CompileExpression consequent
                        :: CompileExpression alternative
                        :: BuildIf
                        :: pending
                | CSeq expressions ->
                    pending <- BuildSequence expressions.Length :: pending
                    for child in List.rev expressions do
                        pending <- CompileExpression child :: pending
                | CDefine (CVar name, rhs) ->
                    pending <- CompileExpression rhs :: BuildDefine name :: pending
                | CVau (formals, envarg, body) ->
                    let bodyLv = List.map toLispVal body
                    completed <-
                        KernelFunc(fun env cont ->
                            let op = Operative { prms = formals; envarg = envarg; body = bodyLv; closure = env }
                            continueEval env cont op)
                        :: completed
                | CEval (environmentExpression, valueExpression) ->
                    pending <-
                        CompileExpression environmentExpression
                        :: CompileExpression valueExpression
                        :: BuildEval
                        :: pending
                | CReset body ->
                    // Delimited continuations must go through the trampoline interpreter so
                    // shift sees the proper meta-continuation chain (including under begin/applicatives).
                    let form = List [Atom "reset"; toLispVal body]
                    completed <- KernelFunc(fun env cont -> eval env cont form) :: completed
                | CApp (operator, args) ->
                    let operands = List.map toLispVal args |> List.toArray
                    pending <- CompileExpression operator :: BuildApp operands :: pending
                | COperate (CVar name, operands) ->
                    let ops = List.toArray operands
                    completed <-
                        KernelFunc(fun env cont -> Helpers.AppNamed(env, cont, name, ops))
                        :: completed
                | COperate (operator, operands) ->
                    pending <-
                        CompileExpression operator
                        :: BuildOperation(List.toArray operands)
                        :: pending
                | CIntrinsicOperate (identity, operands) ->
                    completed <-
                        KernelFunc(fun env cont ->
                            match identity with
                            | PrimitiveIf -> run (Runtime.if_then_else env cont operands)
                            | PrimitiveDefine -> run (Runtime.define env cont operands))
                        :: completed
                | CGuarded (guard, specialized, fallback) ->
                    pending <-
                        CompileExpression specialized
                        :: CompileExpression fallback
                        :: BuildGuarded guard
                        :: pending
                | CContractFold (guard, folded, fallback) ->
                    pending <-
                        CompileExpression fallback
                        :: BuildContractFold(guard, folded)
                        :: pending
                | CResidual v ->
                    completed <- KernelFunc(fun env cont -> eval env cont v) :: completed
                | CLocated (span, sourceLine, inner) ->
                    pending <- CompileExpression inner :: BuildLocated(span, sourceLine) :: pending
                | other ->
                    let value = toLispVal other
                    completed <- KernelFunc(fun env cont -> eval env cont value) :: completed
            | BuildIf ->
                match takeCompleted 3 with
                | [condition; consequent; alternative] ->
                    let envP = Expression.Parameter(typeof<LispVal>, "env")
                    let contP = Expression.Parameter(typeof<LispVal>, "cont")
                    let call =
                        Expression.Call(
                            ifThenElseMethod,
                            envP, contP,
                            Expression.Constant(condition),
                            Expression.Constant(consequent),
                            Expression.Constant(alternative))
                    completed <- Expression.Lambda<KernelFunc>(call, envP, contP).Compile() :: completed
                | _ -> invalidOp "Conditional compilation is incomplete"
            | BuildSequence count ->
                let functions = takeCompleted count |> List.toArray
                completed <- KernelFunc(fun env cont -> Helpers.Seq(env, cont, functions)) :: completed
            | BuildDefine name ->
                match takeCompleted 1 with
                | [rhs] ->
                    completed <- KernelFunc(fun env cont -> Helpers.Define(env, cont, name, rhs)) :: completed
                | _ -> invalidOp "Definition compilation is incomplete"
            | BuildEval ->
                match takeCompleted 2 with
                | [environmentExpression; valueExpression] ->
                    completed <-
                        KernelFunc(fun env cont ->
                            Helpers.EvalForms(env, cont, environmentExpression, valueExpression))
                        :: completed
                | _ -> invalidOp "Eval compilation is incomplete"
            | BuildApp operands ->
                match takeCompleted 1 with
                | [operator] ->
                    let envP = Expression.Parameter(typeof<LispVal>, "env")
                    let contP = Expression.Parameter(typeof<LispVal>, "cont")
                    let call =
                        Expression.Call(
                            appMethod,
                            envP, contP,
                            Expression.Constant(operator),
                            Expression.Constant(operands))
                    completed <- Expression.Lambda<KernelFunc>(call, envP, contP).Compile() :: completed
                | _ -> invalidOp "Application compilation is incomplete"
            | BuildOperation operands ->
                match takeCompleted 1 with
                | [operator] ->
                    completed <- KernelFunc(fun env cont -> Helpers.App(env, cont, operator, operands)) :: completed
                | _ -> invalidOp "Operation compilation is incomplete"
            | BuildGuarded guard ->
                match takeCompleted 2 with
                | [specialized; fallback] ->
                    completed <-
                        KernelFunc(fun env cont ->
                            if bindingGuardMatches env guard then specialized.Invoke(env, cont)
                            else fallback.Invoke(env, cont))
                        :: completed
                | _ -> invalidOp "Guarded compilation is incomplete"
            | BuildContractFold (guard, folded) ->
                match takeCompleted 1 with
                | [fallback] ->
                    completed <-
                        KernelFunc(fun env cont ->
                            if contractGuardMatches env guard then continueEval env cont folded
                            else fallback.Invoke(env, cont))
                        :: completed
                | _ -> invalidOp "Contract fold compilation is incomplete"
            | BuildLocated (span, sourceLine) ->
                match takeCompleted 1 with
                | [inner] ->
                    completed <-
                        KernelFunc(fun env cont ->
                            match inner.Invoke(env, cont) with
                            | Choice1Of2 (LocatedError _ as error) -> throwError error
                            | Choice1Of2 error -> throwError (LocatedError(span, sourceLine, error))
                            | Choice2Of2 value -> returnM value)
                        :: completed
                | _ -> invalidOp "Located compilation is incomplete"

        match completed with
        | [result] -> result
        | _ -> invalidOp "Compilation did not produce one function"

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
