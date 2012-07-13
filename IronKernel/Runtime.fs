namespace IronKernel

    open System
    open Choice
    open Errors
    open Ast
    open Parser
    open SymbolTable
    open Interop

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
    
        //interesting
        let numericBinOp (op: LispVal -> LispVal -> ThrowsError<LispVal>) prms : ThrowsError<LispVal> = 
            match prms with 
            | a::tail   -> Choice.fold op a tail
            | _ -> throwError (NumArgs(2,prms))

        let boolBinop (unpacker: LispVal -> ThrowsError<'a>) (op: 'a -> 'a -> bool) args = 
            if List.length args <> 2 then throwError (NumArgs(2,args))
            else
                either {
                    let! left  = unpacker (args.[0])
                    let! right = unpacker (args.[1])
                    return Bool(op left right)
                }

        let numBoolBinop (op: LispVal -> LispVal -> ThrowsError<LispVal>) prms : ThrowsError<LispVal> = 
            match prms with 
            | [a;b]   -> op a b
            | _ -> throwError (NumArgs(2,prms))

        let strBoolBinop = boolBinop unpackStr
        let boolBoolBinop = boolBinop unpackBool

        let car = function
            | [List (x::_)] -> returnM x
            | [DottedList (x::xs,_)] -> returnM x
            | [badArg] -> throwError (TypeMismatch("pair",badArg))
            | badArgList -> throwError (NumArgs(1,badArgList))

        let cdr = function 
            | [List(x::xs)] -> returnM (List xs)
            | [DottedList([xs],x)] -> returnM x
            | [DottedList(_::xs,x)] -> returnM (DottedList(xs, x))
            | [badArg] -> throwError (TypeMismatch("pair",badArg))
            | badArgList -> throwError (NumArgs(1,badArgList))

        let cons = function
            | [x; List(xs)] -> returnM (List(x::xs))
            | [x;DottedList(xs,xlast)] -> returnM (DottedList(x::xs,xlast))
            | [x1;x2] -> returnM (DottedList([x1],x2))
            | badArgList -> throwError (NumArgs(2,badArgList))

        let rec eqv = function
            | [Inert ; Inert] -> returnM (Bool true)
            | [(Obj arg1); (Obj arg2)] -> returnM (Bool(arg1.Equals(arg2)))
            | [(Bool arg1); (Bool arg2)] -> returnM (Bool(arg1 = arg2))
            | [(Atom arg1); (Atom arg2)] -> returnM (Bool(arg1 = arg2))
            | [(DottedList (xs,x)); (DottedList (ys,y))] -> eqv [List (xs@[x]); List(ys@[y])]
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
                returnM (File.ReadAllText filename :> obj |> Ast.Obj) 
            with _ -> throwError (Default("File not found: '" + filename + "'"))

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
        let isPair = function |[DottedList _] -> returnM <| Bool(true) |_ -> returnM <| Bool(false)

        let isZero = function 
            |[Obj x] -> match x with 
                        | :? byte -> returnM <| Bool(byte (0) = (x :?> byte)) 
                        | :? int -> returnM <| Bool(0 = (x :?> int)) 
                        | :? int64 -> returnM <| Bool((x :?> int64) = 0L) 
                        | :? float32 -> returnM <| Bool((x :?> float32) = 0.0f) 
                        | :? float -> returnM <| Bool((x :?> float) = 0.0) 
                        | _ -> returnM <| Bool(false)
            |_ -> returnM <| Bool(false)
        
        open Arithmetic

        let dd env cont args = 
            either {
                let! q = numericBinOp opAdd args 
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
                  ("<=", numBoolBinop (opLessThanOrEqual));
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
                  ("null?", isNull)
                  ("pair?", isPair) 
                  ("zero?", isZero)]

        let define' env c var form =
            let cps e cn r = defineVar e var r
            eval env (makeCPS env c cps) form
            
        let rec define env cont = function 
            | [Atom var; form]  -> define' env cont var form
            | List(Atom var :: prms)::body -> define' env cont var (Operative{ prms = List.map showVal prms; vararg = None; envarg = "_"; body = body; closure = env} |> Applicative)
            | badForm -> throwError (BadSpecialForm("invalid arguments",List(badForm)))
        
        let vau _env cont = function 
            | List(prms) :: Atom e :: body   -> (Operative{ prms = List.map showVal prms; vararg = None; envarg = e; body = body; closure = _env} ) |> continueEval _env cont 
            | Atom(varargs) ::Atom e:: body  -> (Operative{ prms = []; vararg = Some varargs;envarg = e; body = body; closure =_env }) |> continueEval _env cont
            | DottedList(prms,Atom varargs):: Atom e ::body -> (Operative{ prms = List.map showVal prms; vararg = Some varargs;envarg = e; body = body; closure =_env }) |> continueEval _env cont 

        let wrap env cont (a::_)  =  (Applicative a) |> continueEval env cont 

        let unwrap env cont (a::_)  = 
            match a with
            | Applicative c -> continueEval env cont  c
            | _ -> throwError (TypeMismatch("applicative",a))

        let evaluate _ cont = function 
            | (a::b::_) -> eval a cont b 
            | badArgList -> throwError(NumArgs(2, badArgList))
    
        let if_then_else env cont args = 
            match args with
            | cond::b::c::_ ->
        
                let cps e cn r =
                     match r with
                        |Bool(true) -> eval e cn b 
                        |Bool(false) -> eval e cn c
                        |found -> throwError <| TypeMismatch("bool",found)

                either {
                    let! r = eval env (newContinuation env) cond
                    let! s = eval env (makeCPS env cont cps) r
                    return s
                }
            |_ -> throwError <| NumArgs(3,args)

        //the real load
        let loadAndEval env cont [Obj(filename)] = either {
                            let! fname = cast filename
                            let! f = load fname
                            //this should abort if can not eval something right?
                            //let! _ = f |> List.map (eval env cont) |> List.last
                            let! _ = sequence (List.map (eval env cont) f) []
                            //return r
                            return Inert
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

