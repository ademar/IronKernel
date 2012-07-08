namespace IronKernel

module Eval =
    
    open Choice
    open Ast
    open Errors
    open System
    open SymbolTable

    let rec continueEval  (Environment(_) as env) (Continuation (_) as cont) value : ThrowsError<LispVal> = 
        match cont with
        |Continuation (_,None,None) -> returnM value
        |Continuation (e,None,Some c) -> continueEval e c value
        |Continuation (e,Some f, Some (Continuation(a,b,c))) -> f env (Continuation(a,b,c)) value

    let rec eval (Environment(_) as env) (Continuation (_) as cont) value : ThrowsError<LispVal> = 
        match value with 
       
        | Atom(id)  -> //(getVar env id) >>= (continueEval env cont)
                       either { 
                            let! v = getVar env id 
                            let! r = continueEval env cont v
                            return r
                        }
        | List (op::args) ->  either { 
                                        let! f = eval env cont op

                                        let cps e v a =
                                            operate e v a args 

                                        let! q = eval env (makeCPS env cont cps) f
                                        
                                        return q
                                        //let! r = operate env cont f args 
                                        //let! s = continueEval env cont r
                                        //return s
                                        //return r
                              }
       // | DottedList(
        
        | z -> continueEval env cont z 
        
    and evalArgs _env cont args = sequence (List.map (eval _env cont) args) [] 
    and operate (Environment(_) as _env)  (Continuation (_,cc,_) as cont) (func:LispVal) (args: LispVal list): ThrowsError<LispVal> = 
        
        match func with 
        | PrimitiveOperative f -> either {
                                    let! r = f _env cont args
                                    (*let! u = 
                                        match cont with
                                        | Continuation (ce,_,_) -> continueEval _env cont r
                                        | _  -> returnM r
                                    return u*)
                                    return r
                                  }
        | PrimitiveFunc f -> either {
                                    let! q = evalArgs _env cont args
                                    let! r  = f q 
                                    
                                    let! u = 
                                        match cont with
                                        | Continuation (ce,_,_) -> continueEval _env cont r
                                        | _  ->  returnM r

                                    return u
                                    //return r
                                  }
        | IOFunc f -> either {
                                    let! q = evalArgs _env cont args
                                    let! r  = f q 

                                    return r
                                  }
        | Applicative f -> either {
                                    let! q = evalArgs _env cont args
                                    let! r  = operate _env cont f q 
                                    
                                    (*let! u = 
                                        match cont with
                                        | Continuation (ce,_,_) -> continueEval _env cont r
                                        | _  ->  returnM r

                                    return u*)
                                    return r
                                  }

        | Continuation(closure,cc, nc) -> continueEval _env (Continuation(closure,cc,nc)) (List.head args)
            
        | Operative { prms = prms ; vararg = vararg; envarg = envarg ; body = body ; closure = closure} -> 
            if List.length prms <> List.length args && vararg  = None then 
                throwError (NumArgs(List.length prms, args))
            else 
                let remainingArgs = List.skip (List.length prms) args
                
                let evalBody env =  either { 
                                            let! r = evalArgs env cont body//sequence (List.map (eval env cont) body) [] 
                                            //--
                                            let r = List.last  r
                                            return r
                                             (*let! u =
                                            
                                                match cont with
                                                | Continuation (ce,_,_) -> continueEval _env (Continuation(env,cc,None)) r
                                                | _  -> either { return r }

                                            return u *)
                                            }
                
                let bindVarArgs arg env = 
                    match arg with
                    |None -> env
                    |Some argName -> bindVars env [ argName, List (remainingArgs)]

                let newEnv = bindVars (newEnv [closure]) (Seq.zip prms args |> Seq.toList) |> bindVarArgs vararg
                
                defineVar newEnv envarg _env |> ignore 
                
                evalBody newEnv
               

        | _ -> throwError (BadSpecialForm("Expecting a function, got ",func))
