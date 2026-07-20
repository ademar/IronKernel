namespace IronKernel

/// Runtime operations shared by dynamically and statically generated code.
module RuntimeDispatch =

    open System
    open Ast
    open Errors
    open Eval
    open SymbolTable

    let appNamed env cont name (operands: LispVal[]) : ThrowsError<LispVal> =
        match getVar env name with
        | Choice1Of2 error -> throwError error
        | Choice2Of2 combiner -> operate env cont combiner (Array.toList operands)

    type GeneratedFunc = Func<LispVal, LispVal, ThrowsError<LispVal>>

    let runIf env cont (condition: GeneratedFunc) (consequent: GeneratedFunc) (alternative: GeneratedFunc) =
        match condition.Invoke(env, newContinuation env) with
        | Choice2Of2 (Bool true) -> consequent.Invoke(env, cont)
        | Choice2Of2 (Bool false) -> alternative.Invoke(env, cont)
        | Choice2Of2 found -> throwError (TypeMismatch("bool", found))
        | Choice1Of2 error -> throwError error

    let runDefine env cont name (rhs: GeneratedFunc) =
        match rhs.Invoke(env, newContinuation env) with
        | Choice1Of2 error -> throwError error
        | Choice2Of2 value ->
            match defineVar env name value with
            | Choice1Of2 error -> throwError error
            | Choice2Of2 _ -> continueEval env cont Inert

    let runSequence env cont (forms: GeneratedFunc[]) =
        let mutable index = 0
        let mutable result = Choice2Of2 Inert
        let mutable running = true
        while index < forms.Length && running do
            let nextCont = if index = forms.Length - 1 then cont else newContinuation env
            result <- forms.[index].Invoke(env, nextCont)
            running <- match result with Choice2Of2 _ -> true | Choice1Of2 _ -> false
            index <- index + 1
        if forms.Length = 0 then continueEval env cont Inert else result

    let runGuard env cont name expectedIdentity (specialized: GeneratedFunc) (fallback: GeneratedFunc) =
        if bindingHasPrimitiveIdentity env name expectedIdentity then specialized.Invoke(env, cont)
        else fallback.Invoke(env, cont)

    let runLocated env cont span sourceLine (body: GeneratedFunc) =
        match body.Invoke(env, cont) with
        | Choice1Of2 (LocatedError _ as error) -> throwError error
        | Choice1Of2 error -> throwError (LocatedError(span, sourceLine, error))
        | Choice2Of2 value -> Choice2Of2 value