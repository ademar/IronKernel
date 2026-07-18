namespace IronKernel

module Eval =
    
    open System.Threading
    open System.Threading.Tasks
    open Choice
    open Ast
    open Errors
    open SymbolTable
    open Capabilities
    open Contracts
    open ClrSugar

    let inline ok (x: LispVal) : Step = Done (returnM x)
    let inline fail (e: LispError) : Step = Done (throwError e)
    let inline ofResult (r: ThrowsError<LispVal>) : Step = Done r

    let rec runAsync (step: Step) : Task<ThrowsError<LispVal>> =
        let mutable current = step
        let mutable running = true
        while running do
            match current with
            | More next -> current <- next ()
            | Done _
            | Await _ -> running <- false
        match current with
        | Done result -> Task.FromResult result
        | Await request ->
            let completion =
                TaskCompletionSource<ThrowsError<LispVal>>(
                    TaskCreationOptions.RunContinuationsAsynchronously)
            try
                request.register (fun outcome -> completion.TrySetResult(outcome) |> ignore)
            with ex ->
                completion.TrySetResult(throwError (ClrException ex)) |> ignore
            task {
                let! outcome = completion.Task.ConfigureAwait(false)
                return! runAsync (request.resume outcome)
            }
        | More _ -> failwith "unreachable trampoline state"

    let run (step: Step) : ThrowsError<LispVal> =
        runAsync step |> fun pending -> pending.GetAwaiter().GetResult()

    let rec private appendContinuationRecord left right =
        match left.nextCont with
        | None ->
            { left with nextCont = Some (Continuation(right, None, Full)) }
        | Some (Continuation(next, None, continuationType)) ->
            { left with
                nextCont =
                    Some
                        (Continuation(
                            appendContinuationRecord next right,
                            None,
                            continuationType)) }
        | Some _ -> left

    let findPrompt tag continuation =
        let rec search accumulated = function
            | Continuation(current, Some frame, continuationType) ->
                let combined =
                    match accumulated with
                    | None -> current
                    | Some previous -> appendContinuationRecord previous current
                if frame.tag = tag then Some(combined, frame)
                else search (Some combined) frame.parentCont
            | _ -> None
        search None continuation

    let promptContinuation env parent tag handler =
        Continuation(
            { closure = env
              currentCont = None
              nextCont = None
              args = None },
            Some
                { parentCont = parent
                  tag = tag
                  handler = handler },
            Full)

    let rec continueEvalStep (Environment _ as env) (Continuation _ as cont) value : Step =
        match cont with
        | Continuation ({ currentCont = None; nextCont = None }, None, _) ->
            ok value
        | Continuation ({ closure = e; currentCont = None; nextCont = None }, Some frame, _) ->
            More (fun () -> continueEvalStep e frame.parentCont value)
        | Continuation ({ closure = e; currentCont = None; nextCont = Some (Continuation (cr, None, _)) }, metaCont, ct) ->
            More (fun () -> continueEvalStep e (Continuation (cr, metaCont, ct)) value)
        | Continuation ({ closure = e; currentCont = Some (NativeCode { cont = f; args = args }); nextCont = Some (Continuation (ncr, None, _)) }, metaCont, ct) ->
            More (fun () -> f e (Continuation (ncr, metaCont, ct)) value args)
        | Continuation ({ closure = e; currentCont = Some (KernelCode cBody); nextCont = nextCont }, metaCont, ct) ->
            match cBody with
            | [] ->
                match nextCont with
                | Some (Continuation (cr, None, _)) ->
                    More (fun () -> continueEvalStep e (Continuation (cr, metaCont, ct)) value)
                | None ->
                    match metaCont with
                    | Some frame -> More (fun () -> continueEvalStep e frame.parentCont value)
                    | None -> ok value
                | Some _ ->
                    fail (Default "Internal Error: metacontinuation in wrong position")
            | p :: tail ->
                More (fun () ->
                    evalStep e
                        (Continuation ({ closure = e; currentCont = Some (KernelCode tail); nextCont = nextCont; args = None }, metaCont, ct))
                        p)
        | _ -> fail (TypeMismatch ("continuation", cont))

    and evalStep (Environment _ as env) (Continuation _ as cont) value : Step =
        match value with
        | Atom id ->
            match getVar env id with
            | Choice2Of2 r -> More (fun () -> continueEvalStep env cont r)
            | Choice1Of2 e -> fail e
        | List (Atom name :: args) ->
            match getVar' env name with
            | Some r -> More (fun () -> operateStep env cont r args)
            | None ->
                match tryRewrite name args with
                | Some rewritten -> More (fun () -> evalStep env cont rewritten)
                | None -> fail (UnboundVar("Getting an unbound variable", name))
        | List (op :: args) ->
            let cps e v a _ =
                operateStep e v a args
            More (fun () -> evalStep env (makeCPS env cont cps) op)
        | z ->
            More (fun () -> continueEvalStep env cont z)

    and evalArgsExStep _env cont args f : Step =
        let rec cpsEvalArgs e c evaledArg aa =
            match aa with
            | Some [func; List argsEvaled; List argsRemaining] ->
                match argsRemaining with
                | [] -> operateStep e c func (argsEvaled @ [evaledArg])
                | [a] ->
                    More (fun () ->
                        evalStep e (makeCPSWArgs e c cpsEvalArgs [func; List (argsEvaled @ [evaledArg]); List []]) a)
                | a :: tail ->
                    More (fun () ->
                        evalStep e (makeCPSWArgs e c cpsEvalArgs [func; List (argsEvaled @ [evaledArg]); List tail]) a)
            | _ -> fail (Default "Internal error at evalArgsEx")

        let cpsPrepArgs e c func =
            function
            | Some args' ->
                match args' with
                | [] -> operateStep _env cont f []
                | [a] -> More (fun () -> evalStep _env (makeCPSWArgs e c cpsEvalArgs [f; List []; List []]) a)
                | a :: tail -> More (fun () -> evalStep _env (makeCPSWArgs e c cpsEvalArgs [f; List []; List tail]) a)
            | _ -> fail (Default "Internal error at evalArgsEx")

        More (fun () -> evalStep _env (makeCPSWArgs _env cont cpsPrepArgs args) f)

    and operateStep (Environment _ as _env) (Continuation (cpr, metaCont, ct) as cont) (func: LispVal) (args: LispVal list) : Step =
        match func with
        | PrimitiveOperative primitive -> primitive.invoke _env cont args
        | CompiledCombiner f -> f _env cont args
        | ContractedCombiner contracted ->
            match validateArguments contracted.contract args with
            | Some error -> fail error
            | None ->
                let validate e c value _ =
                    match validateResult contracted.contract value with
                    | Some error -> fail error
                    | None -> More (fun () -> continueEvalStep e c value)
                More (fun () ->
                    operateStep
                        _env
                        (makeCPS _env cont validate)
                        contracted.combiner
                        args)
        | IOFunc (requiredCapability, f) ->
            if not (has requiredCapability _env) then
                fail (CapabilityDenied(sprintf "I/O requires %A" requiredCapability))
            else
                match evalArgs _env (newContinuation _env) args with
                | Choice1Of2 e -> fail e
                | Choice2Of2 q -> ofResult (f q)
        | Applicative f -> evalArgsExStep _env cont args f
        | Continuation (cr, capturedPrompt, ct') ->
            match args with
            | [] -> fail (NumArgs (1, []))
            | [a] ->
                match ct' with
                | Full -> More (fun () -> evalStep _env func a)
                | Delimited ->
                    let prompt =
                        match capturedPrompt with
                        | Some frame -> Some { frame with parentCont = cont }
                        | None ->
                            Some
                                { parentCont = cont
                                  tag = None
                                  handler = None }
                    More (fun () -> evalStep _env (Continuation (cr, prompt, Full)) a)
            | _ -> fail (NumArgs (1, args))
        | Resumption resumption ->
            match args with
            | [argument] when Interlocked.Exchange(&resumption.consumed, 1) = 0 ->
                // Resume is a non-local exit from the handler: reinstall the
                // delimited body under its captured prompt, then continue to
                // that prompt's parent. Returning through the handler cont
                // would let trailing handler forms replace the prompt result.
                let resumeCont =
                    match resumption.continuation with
                    | Continuation(_, Some frame, _) -> frame.parentCont
                    | _ -> cont
                More (fun () -> operateStep _env resumeCont resumption.continuation [argument])
            | [_] -> fail (Default "resumption has already been consumed")
            | _ -> fail (NumArgs(1, args))
        | Operative { prms = prms; envarg = envarg; body = body; closure = closure } ->
            let evalBody env =
                match cpr with
                | { currentCont = Some (KernelCode cBody); nextCont = nextCont } ->
                    match cBody with
                    | [] ->
                        More (fun () ->
                            continueEvalStep env
                                (Continuation ({ closure = env; currentCont = Some (KernelCode body); nextCont = nextCont; args = None }, metaCont, ct))
                                Nil)
                    | _ ->
                        More (fun () ->
                            continueEvalStep env
                                (Continuation ({ closure = env; currentCont = Some (KernelCode body); nextCont = Some (Continuation (cpr, None, Full)); args = None }, metaCont, ct))
                                Nil)
                | _ ->
                    More (fun () ->
                        continueEvalStep env
                            (Continuation ({ closure = env; currentCont = Some (KernelCode body); nextCont = Some (Continuation (cpr, None, Full)); args = None }, metaCont, ct))
                            Nil)

            let newEnv = newEnv [closure]
            bind newEnv (newContinuation _env) prms (List args) |> ignore
            defineVar newEnv envarg _env |> ignore
            evalBody newEnv
        | Inert -> More (fun () -> continueEvalStep _env cont Nil)
        | _ -> fail (BadSpecialForm ("Expecting a combiner, got ", func))

    and bindStep env cont lf rf : Step =
        match lf with
        | Atom var ->
            match defineVar env var rf with
            | Choice1Of2 e -> fail e
            | Choice2Of2 _ -> More (fun () -> continueEvalStep env cont Inert)
        | List [] ->
            match rf with
            | List [] -> More (fun () -> continueEvalStep env cont Inert)
            | badForm -> fail (BadSpecialForm ("invalid arguments", badForm))
        | List (a :: aa) ->
            match rf with
            | List (b :: bb) ->
                let cps e c _result _ =
                    bindStep e c (List aa) (List bb)
                bindStep env (makeCPS env cont cps) a b
            | badForm -> fail (BadSpecialForm ("invalid arguments", badForm))
        | DottedList ([], rest) ->
            match rf with
            | DottedList ([], rest') -> bindStep env cont rest rest'
            | _ -> bindStep env cont rest rf
        | DottedList (x :: xx, rest) ->
            match rf with
            | List (y :: yy) ->
                let cps e c _result _ =
                    bindStep e c (DottedList (xx, rest)) (List yy)
                bindStep env (makeCPS env cont cps) x y
            | DottedList (y :: yy, rest') ->
                let cps e c _result _ =
                    bindStep e c (DottedList (xx, rest)) (DottedList (yy, rest'))
                bindStep env (makeCPS env cont cps) x y
            | badForm -> fail (BadSpecialForm ("invalid arguments", badForm))
        | badForm -> fail (BadSpecialForm ("invalid arguments", badForm))

    and evalArgs _env cont args =
        sequence (List.map (fun a -> run (evalStep _env cont a)) args) []

    /// Public API — single trampoline entry.
    and continueEval env cont value = run (continueEvalStep env cont value)
    and eval env cont value = run (evalStep env cont value)
    and operate env cont func args = run (operateStep env cont func args)
    and bind env cont lf rf = run (bindStep env cont lf rf)

    /// Schedule continue/eval without nesting trampolines (for primitives).
    let bounceContinue env cont value = More (fun () -> continueEvalStep env cont value)
    let bounceEval env cont value = More (fun () -> evalStep env cont value)
    let bounceOperate env cont func args = More (fun () -> operateStep env cont func args)
    let bounceBind env cont lf rf = More (fun () -> bindStep env cont lf rf)

    let evalAsync env cont value = runAsync (evalStep env cont value)
