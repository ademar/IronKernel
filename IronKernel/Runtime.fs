namespace IronKernel

    open System
    open Choice
    open Errors
    open Ast
    open Parser
    open SymbolTable
    open Interop
    open Eval

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
        
        let numericBinOp env cont (op: LispVal -> LispVal -> ThrowsError<LispVal>) prms : ThrowsError<LispVal> = 
            match prms with 
            | [a;b]   -> let cps e c r _ =
                                    let r = op r b
                                    match r with 
                                    | Choice2Of2 q -> continueEval e c q
                                    | Choice1Of2 e -> throwError e
                         continueEval env (makeCPS env cont cps) a
            | _ -> throwError (NumArgs(2,prms))

        let boolBinop (unpacker: LispVal -> ThrowsError<'a>) (op: 'a -> 'a -> bool) args = 
            if List.length args <> 2 then throwError (NumArgs(2,args))
            else
                either {
                    let! left  = unpacker (args.[0])
                    let! right = unpacker (args.[1])
                    return Bool(op left right)
                }

        let numBoolBinop env cont (op: LispVal -> LispVal -> ThrowsError<LispVal>) prms : ThrowsError<LispVal> = 
            match prms with 
            | [a;b]   -> let cps e c r _ = 
                                    let r =  op r b
                                    match r with 
                                    | Choice2Of2 q -> continueEval e c q
                                    | Choice1Of2 e -> throwError e
                                
                         continueEval env (makeCPS env cont cps) a
                        
            | _ -> throwError (NumArgs(2,prms))

        let strBoolBinop = boolBinop unpackStr
        let boolBoolBinop = boolBinop unpackBool

        let car env cont = function
            | [List (x::_)] -> continueEval env cont x
            | [DottedList (x::_,_)] -> continueEval env cont x
            | [badArg] -> throwError (TypeMismatch("pair",badArg))
            | badArgList -> throwError (NumArgs(1,badArgList))

        let cdr env cont = function 
            | [List(_::xs)] -> continueEval env cont (List xs)
            | [DottedList([_],x)] -> continueEval env cont x
            | [DottedList(_::xs,x)] -> continueEval env cont (DottedList(xs, x))
            | [badArg] -> throwError (TypeMismatch("pair",badArg))
            | badArgList -> throwError (NumArgs(1,badArgList))

        let cons env cont = function
            | [x; List []] -> continueEval env cont (List[x])
            | [x; List(xs)] -> continueEval env cont (List(x::xs))
            | [x;DottedList(xs,xlast)] -> continueEval env cont (DottedList(x::xs,xlast))
            | [x1;x2] -> continueEval env cont (DottedList([x1],x2))
            | badArgList -> throwError (NumArgs(2,badArgList))

        let rec eqv' = function
            | [Inert ; Inert] -> returnM  (Bool true)
            | [(Obj arg1); (Obj arg2)] -> returnM (Bool(arg1.Equals(arg2)))
            | [(Bool arg1); (Bool arg2)] -> returnM (Bool(arg1 = arg2))
            | [(Atom arg1); (Atom arg2)] -> returnM (Bool(arg1 = arg2))
            | [(DottedList (xs,x)); (DottedList (ys,y))] -> eqv' [List (xs@[x]); List(ys@[y])]
            | [(List arg1); (List arg2)] -> 
                
                let eqvPair (x1,x2) = 
                    match eqv' [x1;x2] with
                    |Choice1Of2(error) -> false
                    |Choice2Of2(Bool value) -> value

                returnM (Bool((List.length arg1) = (List.length arg2) && List.forall eqvPair <| List.zip arg1 arg2)) 
            | [_; _] -> returnM (Bool false)
            | badArgList -> throwError (NumArgs(2,badArgList))

        let rec eqv env cont parms = 
            either {
                let! q = eqv' parms
                return! continueEval env cont q
            }

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
                return! readExprList (string q)
                }

        let readAll [Obj filename] = 
                either { 
                    let! r = load (filename :?> string)
                    return List r
                }

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
            Map.ofList [ 
                    ("open-input-file", makePort FileAccess.Read);
                    ("open-output-file", makePort FileAccess.Write);
                    ("close-input-port", closePort);
                    ("close-output-port", closePort);
                    ("read", readProc);
                    ("write", writeProc);
                    ("read-contents", readContents);
                    ("read-all", readAll) ]

        let isNull env cont = function 
            | [List[]]   -> continueEval env cont <| Bool(true) 
            | _          -> continueEval env cont <| Bool(false)

        let isPair env cont = function 
            | [DottedList _]    -> continueEval env cont <| Bool(true) 
            | [List (_::_) ]    -> continueEval env cont <| Bool(true) 
            | _                 -> continueEval env cont <| Bool(false)

        let isZero env cont = function 
            |[Obj x] -> match x with 
                        | :? byte -> continueEval env cont <| Bool(byte (0) = (x :?> byte)) 
                        | :? int -> continueEval env cont <| Bool(0 = (x :?> int)) 
                        | :? int64 -> continueEval env cont <| Bool((x :?> int64) = 0L) 
                        | :? float32 -> continueEval env cont <| Bool((x :?> float32) = 0.0f) 
                        | :? float -> continueEval env cont <| Bool((x :?> float) = 0.0) 
                        | _ -> continueEval env cont <| Bool(false)
            |_ -> continueEval env cont <| Bool(false)
        
        open Arithmetic

        let wrap env cont (a::_)  =  continueEval env cont (Applicative a) 

        let unwrap env cont (a::_)  = 
            match a with
            | Applicative c -> continueEval env cont  c
            | _ -> throwError (TypeMismatch("applicative",a))

        let evaluate _ cont = function 
            | (a::b::_) -> eval a cont b 
            | badArgList -> throwError(NumArgs(2, badArgList))

        let makeEnvironment env cont xs = continueEval env cont (newEnv xs)
    
        let if_then_else env cont args = 
            match args with
            | cond::b::c::_ ->
        
                let cps e cn r _ =
                     match r with
                        |Bool(true) -> eval e cn b 
                        |Bool(false) -> eval e cn c
                        |found -> throwError <| TypeMismatch("bool",found)

                eval env (makeCPS env cont cps) cond
            |_ -> throwError <| NumArgs(3,args)

        //the real load
        let loadAndEval env cont [Obj(filename)] = either {
                            let! fname = cast filename
                            let! lisp = load fname
                            let! _ = sequence (List.map (eval env cont) lisp) []
                            return Inert
                        }

        let vau _env cont xs = 
            match xs with
            | prms :: Atom e :: body   -> continueEval _env cont (Operative{ prms = prms; envarg = e; body = body; closure = _env} ) 
            | badForm ->  throwError (Default("invalid arguments"))


        let define env cont xs = 
            match xs with 
            | [ l; r ] ->   let cps e c result _ = 
                                bind e c l result
                            eval env (makeCPS env cont cps) r 
            | badForm -> throwError (BadSpecialForm("invalid arguments",List(badForm)))

        let reset env cont  = function 
            | (exp::_) -> eval env (Continuation({closure = env; currentCont = None ; nextCont = None; args = None}, Some cont, Full)) exp
            | badform -> throwError (NumArgs(1,badform))
         
        let primitiveOperatives = 
            Map.ofList [ 
                  ("vau"    , vau);
                  ("define" , define);
                  ("if"     , if_then_else);
                  ("."      , dot) ;
                  ("new" , new_object);
                  (".get", dot_get);
                  (".set", dot_set);
                  ("reset", reset);
                  ]
        
        let callcc env cont  = function 
            | [func] -> 
                match func with 
                | Continuation _    -> continueEval env func cont 
                | Applicative f     -> operate env cont f [cont]
                | badForm -> throwError(TypeMismatch("continuation",badForm))
            | badForm -> throwError(NumArgs(1,badForm))

        let shift env cont (Applicative f::_)  = 
            match cont with
            |(Continuation(continuationRecord, Some parentCont, _)) 
                -> operate env (Continuation({closure = env; currentCont = None  ; nextCont = None; args = None} , Some parentCont, Full)) f [(Continuation(continuationRecord, None, Delimited ))]
            | _ -> throwError(Default("reset needs to be called before shift"))

        let plus env cont args = 
            numericBinOp env cont opAdd args

        let minus env cont args = 
            numericBinOp env cont opMinus args

        let times env cont args = 
            numericBinOp env cont opMultiply args

        let divide env cont args = 
            numericBinOp env cont opDivide args

        let lessThanOrEqual env cont args =
            numBoolBinop env cont opLessThanOrEqual args

        let greaterThan env cont args =
            numBoolBinop env cont opGreaterThan args

        let vector env cont args =
            Vector(List.toArray args) |> continueEval env cont

        let primitiveApplicatives = 
            Map.ofList [ 
                  ("eval", evaluate);
                  ("wrap", wrap);
                  ("unwrap", unwrap);
                  ("load", loadAndEval);
                  ("call/cc", callcc );
                  ("+", plus);
                  ("-", minus );
                  ("*", times);
                  ("/", divide);
                  ("<=",lessThanOrEqual);
                  (">",greaterThan);
                  ("car", car);
                  ("cdr", cdr);
                  ("cons", cons);
                  ("eq?", eqv);
                  ("eqv?", eqv);
                  ("null?", isNull);
                  ("pair?", isPair) ;
                  ("zero?", isZero);
                  ("make-environment", makeEnvironment);
                  ("print", print);
                  ("printf", printf');
                  ("show", show);
                  ("shift", shift);
                  ("vector", vector);
                  ]

        let primitiveBindings = 
            let makeFunc t (var,func) = (var, t func)
            let primi = (Map.toList ioPrimitives |> List.map (makeFunc IOFunc)) 
                      @ (Map.toList primitiveOperatives     |> List.map (makeFunc PrimitiveOperative))
                      @ (Map.toList primitiveApplicatives   |> List.map (makeFunc (Applicative << PrimitiveOperative)))
            bindVars (newEnv []) primi

