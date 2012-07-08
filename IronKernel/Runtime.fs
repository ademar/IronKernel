namespace IronKernel

    open System
    open Choice
    open Errors
    open Ast
    open Parser
    open SymbolTable

    module Runtime = 
        

        let cast<'T> (o:obj) = 
            let typ = typeof<'T>
            let found = o.GetType()
            try 
                returnM (o :?> 'T)
            with | :? InvalidCastException  -> throwError(ClrTypeMismatch(typ.Name,found.Name))


        let dic (s:^seq) =  
            Seq.map (|KeyValue|) s |> Map.ofSeq

        let unpackStr = function
            
            | Bool s -> returnM (s.ToString())
            | notString -> throwError (TypeMismatch ("string", notString))

        let unpackBool = function
            | Bool b -> returnM b
            | notBool -> throwError (TypeMismatch ("boolean", notBool))
    
        let numericBinOp (op: LispVal -> LispVal -> ThrowsError<LispVal>) prms : ThrowsError<LispVal> = 
            match prms with 
            | [_] -> throwError (NumArgs(2,prms))
            | _   -> Choice.fold op (Obj 0) prms

        let boolBinop (unpacker: LispVal -> ThrowsError<'a>) (op: 'a -> 'a -> bool) args = 
            if List.length args <> 2 then throwError (NumArgs(2,args))
            else
                either {
                    let! left  = unpacker (args.[0])
                    let! right = unpacker (args.[1])
                    return Bool(op left right)
                }

       
        let strBoolBinop = boolBinop unpackStr
        let boolBoolBinop = boolBinop unpackBool

        let car = function
            | [List (x::_)] -> returnM x
            | [badArg] -> throwError (TypeMismatch("pair",badArg))
            | badArgList -> throwError (NumArgs(1,badArgList))

        let cdr = function 
            | [List(x::xs)] -> returnM (List xs)
            | [badArg] -> throwError (TypeMismatch("pair",badArg))
            | badArgList -> throwError (NumArgs(1,badArgList))

        let cons = function
            | [x; List(xs)] -> returnM (List(x::xs))
            | [x1;x2] -> returnM (List([x1;x2]))
            | badArgList -> throwError (NumArgs(2,badArgList))

        let rec eqv = function
            | [(Bool arg1); (Bool arg2)] -> returnM (Bool(arg1 = arg2))
            | [(Atom arg1); (Atom arg2)] -> returnM (Bool(arg1 = arg2))
            | [(List arg1); (List arg2)] -> 
                let eqvPair (x1,x2) = 
                    match eqv [x1;x2] with
                    |Choice1Of2(error) -> false
                    |Choice2Of2(Bool value) -> value
                returnM (Bool((List.length arg1) = (List.length arg2) && List.forall eqvPair <| List.zip arg1 arg2)) 
            | [_; _] -> returnM (Bool false)
            | badArgList -> throwError (NumArgs(2,badArgList))

        
        open System.IO

        let tryLoad filename = 
            try
                returnM (File.ReadAllText filename :> obj |> Ast.Obj) //don;t like much this cast downs
            with _ -> throwError (Default("File not found"))

        let makePort mode [Obj filename] = 
            either{
                let! fname = cast filename
                let y = File.Open(fname,FileMode.OpenOrCreate,mode)
                return  Port (y)
            
            }
            
    
        let closePort [Port port] = 
                try port.Close(); Bool true with _ -> Bool false
                |> returnM

        let readContents [Obj filename] = 
            either {
                let! c = cast filename 
                let r = File.ReadAllText c 
                return makeObj r 
            }
        
        open Eval

        let load filename = either{
                let! Obj(q) = tryLoad filename
                let! r = readExprList (string q)
                return r
                }

        let readAll [Obj filename] = 
                either { 
                    let! r = load (filename :?> string)
                    return List r
                }
        
        let applyProc  = function
            | [func; List args] -> let env = newEnv [] in operate env (newContinuation env) func args 
            | func::args -> let env = newEnv [] in operate env (newContinuation env) func args  
            |_ -> failwith "invalid"

        open System.IO

        let rec writeProc = function
                | [ob] -> Console.Out.Write(showVal ob);returnM (Bool true)
                | [ob; Port port] -> use writer = new StreamWriter(port)
                                     writer.Write(showVal ob)
                                     returnM (Bool true)

        let rec readProc port =
               let parseReader (reader:TextReader) = reader.ReadLine() |> readExpr
               match port with
                | [Port p]  -> use s = new StreamReader(p) in parseReader s
                | [] -> parseReader Console.In
          
        let ioPrimitives = 
            Map.ofList [ ("apply", applyProc);//--
                    ("open-input-file", makePort FileAccess.Read);
                    ("open-output-file", makePort FileAccess.Write);
                    ("close-input-port", closePort);
                    ("close-output-port", closePort);
                    ("read", readProc);
                    ("write", writeProc);
                    ("read-contents", readContents);
                    ("read-all", readAll) ]

        let isNull = function |[List[]] -> returnM <| Bool(true) |_ -> returnM <| Bool(false)

        open Arithmetic

        let dd env cont args = 
            either {
                let! q = numericBinOp opAdd args //aparently we dont need to continue here >!>?@?@!?~
                let! r = continueEval env cont q
                return r
            }

        let primitives = 
            Map.ofList [ //("+", numericBinOp opAdd);
                  ("-", numericBinOp opMinus );
                  ("*", numericBinOp opMultiply);
                  ("/", numericBinOp opDivide);
                  //("mod", numericBinOp (%));
                  //("=", numBoolBinop (=));
                  //("<", numBoolBinop (<));
                  //(">", numBoolBinop (>));
                  //("/=", numBoolBinop (<>));
                  //(">=", numBoolBinop (>=));
                  //("<=", numBoolBinop (<=));
                  //("&&", boolBoolBinop (&&));
                  //("||", boolBoolBinop (||));
                  //("string=?", strBoolBinop (=));
                  //("string<?", strBoolBinop (<));
                  //("string>?", strBoolBinop (>));
                  //("string<=?", strBoolBinop (<=));
                  //("string>=?", strBoolBinop (>=));
                  ("car", car);
                  ("cdr", cdr);
                  ("cons", cons);
                  ("eq?", eqv);
                  ("eqv?", eqv);
                  ("null?", isNull) ]

        let define' env c var form =
            either {
                let! f = eval env c form
                let! r = defineVar env var f
                return r
            }

        let rec define env cont = function 
            | [Atom var; form]  -> define' env cont var form
            (*| [List(h::tail);List(f::w)] -> define env [h;f] 
                                            define env [List(tail);List(w)]*)
        //--                            
        let vau _env cont = function 
            | List(prms) :: Atom e :: body   -> (Operative{ prms = List.map showVal prms; vararg = None; envarg = e; body = body; closure = _env} ) |> continueEval _env cont 
            | Atom(varargs) ::Atom e:: body  -> (Operative{ prms = []; vararg = Some varargs;envarg = e; body = body; closure =_env }) |> continueEval _env cont 

        let wrap env cont (a::_)  =  (Applicative a) |> continueEval env cont 

        let unwrap env cont (a::_)  = 
            match a with
            | Applicative c -> continueEval env cont  c
            | _ -> throwError (TypeMismatch("applicative",a))

        let evaluate _ cont = function 
            | (a::b::_) -> eval a cont b 
            | badArgList -> throwError(NumArgs(2, badArgList))

        //.net interop
        let toObjects : LispVal -> obj  = function
            |Atom x -> x :> obj
            |Bool b -> b :> obj
            |Obj o -> o

        let new_object _ _ (Atom t::args) =
            let typ = Type.GetType(t) 
            if typ = null then throwError(Default("Couldn't find type '" + t + "'"))
            else
                try
                    let obj = Activator.CreateInstance(typ,List.map toObjects args)
                    returnM (Obj obj)
                with ex -> throwError(Default("Couldn't create type '" + t+ "', " + ex.Message))

        open System.Reflection

        let get (t:Type) (o:obj) p =
            let f = t.GetField(p)
            if f = null then
                let f = t.GetProperty(p)
                if f = null then 
                    throwError(Default("field or property '" + p + "' does not exist"))
                else
                    returnM (Obj(f.GetValue(o,null)))
            else
                returnM (Obj(f.GetValue(o)))

        let set (t:Type) (o:obj) p (v:obj)=
            let f = t.GetField(p)
            if f = null then
                let f = t.GetProperty(p)
                if f = null then 
                    throwError(Default("field or property '" + p + "' does not exist"))
                else
                    returnM (Obj(f.SetValue(o,v,null)))
            else
                returnM (Obj(f.SetValue(o,v)))

        let dot_get _ _ prms = 
            match prms with
            | (clazz::Atom(p)::_) ->
                match clazz with
                |Obj o -> let typ = o.GetType() in get typ o p
                |Atom c ->let typ = Type.GetType(c)  in get typ null p
            | _ -> throwError (NumArgs(2,prms))

        let dot_set _ _ prms = 
            match prms with
            | (clazz::Atom(p)::Obj(x)::_) ->
                match clazz with
                |Obj o -> let typ = o.GetType() in set typ o p x
                |Atom c ->let typ = Type.GetType(c)  in set typ null p x
            | _ -> throwError (NumArgs(3,prms))

        let invoke (t:Type) (o:obj) (m: string) args= 
            let oargs = List.map toObjects args
            try
                let r = t.InvokeMember(m,BindingFlags.InvokeMethod,Type.DefaultBinder,o, List.toArray oargs)
                //void returning methods r is null 
                //there should be a difference
                returnM (Obj(r))
            with ex -> throwError(Default("member invokation failed: " + ex.Message))

        let dot env cont (clazz::Atom(m)::args) =
            either {
                let! args = sequence (List.map (eval env cont) args) []
                let! result = match clazz with
                    |Obj o  -> let typ = o.GetType() in invoke typ o m args
                               //if it is an atom it should had been evaled
                    |Atom c -> let typ = Type.GetType(c) 
                               if typ = null then 
                                either {
                                    let! Obj(o) = eval env cont clazz
                                    let typ = o.GetType() 
                                    let! r = invoke  typ o m args
                                    return r
                                    }
                               else invoke typ null m args
                   
                return result
            }
    
        let if_then_else env cont args = 
            match args with
            | cond::b::c::_ ->
        
                let cps e cn r =
                     match r with
                        |Bool(true) -> eval e cn b 
                        |Bool(false) -> eval e cn c
                        |found -> throwError <| TypeMismatch("bool",found)

                either {
                    //force condition evaluation
                    let! r = eval env (newContinuation env) cond //? should we provide an empty continuation here
                    let! s = eval env (makeCPS env cont cps) r
                    return s
                }
            |_ -> throwError <| NumArgs(3,args)
        
        

        let loadAndEval env cont [Obj(filename)] = either {
                            let! fname = cast filename
                            let! f = load fname
                            let! r = f |> List.map (eval env cont) |> List.last
                            return r
                        }

        let setbang env cont exp = 
            match exp with
                | [Atom bar; form] ->
                                                    either { 
                                                        let! q = eval env cont form
                                                        let! r = setVar env bar q 
                                                        return r
                                                    } 
                |badForm -> throwError(NumArgs(2,badForm))
   
        let primitiveOperatives = 
            Map.ofList [ 
                  ("vau", vau);
                  ("define", define);
                  ("if", if_then_else);
                  ("."   , dot) ;
                  ("set!", setbang)
                  ]
        
        let callcc env cont  = function // cont = (+ 2 _) so func have to return and integer and pass it to cont
            | [func] -> //asuming func was evaluated eh

                match func with 
                | Continuation _ -> operate env cont func [cont]
                | PrimitiveFunc f -> either {
                                        let! r = f [cont]
                                        let! u = match cont with
                                            | Continuation (ce,_,_) -> continueEval ce cont r
                                            | _ -> returnM r
                                        return u
                                     }
                | Applicative f -> either {
                                        let! r = operate env cont f [cont]
                                        let! u = match cont with
                                            |Continuation (ce,_,_) -> continueEval ce cont r
                                            | _ -> returnM r
                                        return u
                                   }
                | badForm -> throwError(TypeMismatch("continuation",badForm))
            | badForm -> throwError(NumArgs(1,badForm))
            


        let primitiveApplicatives = 
            Map.ofList [ 
                  ("eval", evaluate);
                  ("wrap", wrap);
                  ("new" , new_object);
                  (".get", dot_get);
                  (".set", dot_set);
                  ("load", loadAndEval);
                  ("call/cc", callcc );
                  ("+", dd);
                  ]

        let primitiveBindings = 
            let makeFunc t (var,func) = (var, t func)
            let primi = (Map.toList ioPrimitives |> List.map (makeFunc IOFunc)) 
                      @ (Map.toList primitives   |> List.map (makeFunc PrimitiveFunc))
                      @ (Map.toList primitiveOperatives     |> List.map (makeFunc PrimitiveOperative))
                      @ (Map.toList primitiveApplicatives   |> List.map (makeFunc (Applicative << PrimitiveOperative)))
            bindVars (newEnv []) primi

