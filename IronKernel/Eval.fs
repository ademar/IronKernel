namespace IronKernel

module Eval =
    
    open Choice
    open Ast
    open Errors
    open System
    open SymbolTable

    let rec continueEval  (Environment _) (Continuation _ as cont) value : ThrowsError<LispVal> = 
        match cont with
        |Continuation ({             currentCont = None  ; nextCont = None   }, None, _) -> returnM value
        |Continuation ({closure = e; currentCont = None  ; nextCont = None   }, Some pCont, _) -> continueEval e pCont value 
        |Continuation ({closure = e; currentCont = None  ; nextCont = Some (Continuation (cr,None, _)) },  metaCont, ct) -> continueEval e (Continuation(cr, metaCont,ct)) value
        |Continuation ({closure = e; currentCont = Some (NativeCode{ cont = f ; args = args} ); nextCont = Some (Continuation(ncr, None,_))}, metaCont, ct) -> f e (Continuation(ncr, metaCont,ct)) value args
        |Continuation ({closure = e; currentCont = Some (KernelCode (cBody)); nextCont = nextCont }, metaCont, ct) -> 
            match cBody with
            | [] -> match nextCont with
                    | Some (Continuation (cr,None,_)) ->  continueEval e (Continuation(cr, metaCont, ct)) value
                    | None ->   match metaCont with 
                                    | Some pCont -> continueEval e  pCont value
                                    | None -> returnM value
                    | Some _ ->  throwError (Default("Internal Error: metacontinuation in wrong position"))
            | p::tail -> eval e (Continuation ({closure = e; currentCont = Some (KernelCode (tail)); nextCont = nextCont; args = None}, metaCont, ct)) p

        |_ -> throwError (TypeMismatch("continuation",cont))

    and eval (Environment _ as env) (Continuation _ as cont) value : ThrowsError<LispVal> = 
        match value with 
        | Atom(id)          -> 
                               let v = getVar env id 
                               match v with 
                               |Choice2Of2(r) -> continueEval env cont r
                               |Choice1Of2(e) -> throwError e
        | List (op::args)   -> 
                                let cps e v a _ =
                                    operate e v a args 
                                eval env (makeCPS env cont cps) op
                                
        | z -> continueEval env cont z 
    
    and evalArgsEx _env cont args f =  

        let rec cpsEvalArgs e c evaledArg aa = 
            match aa with 
            | Some ([func; List argsEvaled; List argsRemaining]) ->
                match argsRemaining with
                | [] -> operate e c func (argsEvaled@[evaledArg])
                | [a]  -> eval e (makeCPSWArgs e c cpsEvalArgs [func;List(argsEvaled@[evaledArg]);List[]]) a
                | a :: tail -> eval e (makeCPSWArgs e c cpsEvalArgs [func;List(argsEvaled@[evaledArg]);List(tail)]) a
            |_ -> throwError(Default("Internal error at evalArgsEx"))

        let cpsPrepArgs e c func = function 
            | Some args -> match args with
                            | [] -> operate _env cont f []
                            | [a] -> eval _env (makeCPSWArgs e c cpsEvalArgs [f;List[];List[]]) a
                            | a::tail -> eval _env (makeCPSWArgs e c cpsEvalArgs [f;List[];List(tail)]) a
             |_ -> throwError(Default("Internal error at evalArgsEx"))

        eval _env (makeCPSWArgs _env cont cpsPrepArgs args) f

    and evalArgs _env cont args = 
        sequence (List.map (eval _env cont) args) [] 

    and operate (Environment _ as _env)  (Continuation (cpr,metaCont, ct) as cont) (func:LispVal) (args: LispVal list): ThrowsError<LispVal> = 
        
        match func with 
        | PrimitiveOperative f ->  f _env cont args

        | IOFunc f -> either {
                                let! q = evalArgs _env (newContinuation _env) args
                                return! f q 
                      }

        | Applicative f -> evalArgsEx _env cont args f

        | Continuation({ currentCont = fcc; nextCont = nc} as cr, _metaCont, ct' ) -> 
                                    match args with 
                                    | [] -> throwError (NumArgs(1,[]))
                                    | [a]-> match ct' with
                                            | Full -> eval _env func a
                                            | Delimited -> eval _env (Continuation (cr, Some cont, Full)) a
                                            

        | Operative { prms = prms ; envarg = envarg ; body = body ; closure = closure} -> 
                
                let evalBody env = 
                    match cpr with
                    | { currentCont = Some (KernelCode cBody); nextCont = nextCont }
                        -> match cBody with
                           | [] -> continueEval env (Continuation({ closure = env; currentCont = Some (KernelCode body); nextCont = nextCont ; args = None}, metaCont, ct)) Nil
                           | _  -> continueEval env (Continuation({ closure = env; currentCont = Some (KernelCode body); nextCont = Some (Continuation (cpr, None, Full)) ; args = None}, metaCont, ct)) Nil
                    | _ ->  continueEval env (Continuation ({ closure = env; currentCont = Some (KernelCode body); nextCont = Some (Continuation (cpr,None, Full)); args = None}, metaCont, ct)) Nil
               
            
                let newEnv = newEnv [closure]

                bind newEnv ((newContinuation _env)) prms (List(args)) |> ignore
                defineVar newEnv envarg _env |> ignore 
               
                evalBody newEnv
        | Inert -> continueEval _env cont Nil       
        | _ -> throwError (BadSpecialForm("Expecting a combiner, got ",func))

    and bind env cont lf rf =
        match lf with
        | Atom var -> 
            either {
                let!_ = defineVar env var rf 
                return! continueEval env cont Inert
            }
            
        | List[] -> match rf with 
                    | List[]    -> continueEval env cont Inert 
                    | badForm   -> throwError (BadSpecialForm("invalid arguments",badForm))
        | List(a::aa) -> match rf with 
                            | List(b::bb) -> 
                                            let cps e c result _ = 
                                                bind e c (List(aa)) (List(bb))
                                            bind env (makeCPS env cont cps) a b
                            | badForm -> throwError (BadSpecialForm("invalid arguments",badForm))
       
        | DottedList([],rest) ->   match rf with
                                    | DottedList([],rest') -> bind env cont rest rest'
                                    | _ -> bind env cont rest rf
        | DottedList(x::xx,rest) -> match rf with 
                                    | List(y::yy)  ->   let cps e c result _ =
                                                            bind e c (DottedList(xx,rest)) (List (yy))
                                                        bind env (makeCPS env cont cps) x y
                                    | DottedList(y::yy,rest')  ->   
                                                        let cps e c result _ =
                                                            bind e c (DottedList(xx,rest)) (DottedList (yy,rest'))
                                                        bind env (makeCPS env cont cps) x y
                                    |  badForm -> throwError (BadSpecialForm("invalid arguments",badForm))
        | badForm -> throwError (BadSpecialForm("invalid arguments",badForm))
            
    