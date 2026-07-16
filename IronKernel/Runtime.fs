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

        let unpackStr = function
            | Bool s -> returnM (s.ToString())
            | notString -> throwError (TypeMismatch ("string", notString))

        let unpackBool = function
            | Bool b -> returnM b
            | notBool -> throwError (TypeMismatch ("boolean", notBool))
        
        let numericBinOp env cont (op: LispVal -> LispVal -> ThrowsError<LispVal>) prms : Step = 
            match prms with 
            | [a;b] ->
                let cps e c r _ =
                    match op r b with 
                    | Choice2Of2 q -> bounceContinue e c q
                    | Choice1Of2 err -> fail err
                bounceContinue env (makeCPS env cont cps) a
            | _ -> fail (NumArgs(2,prms))

        let boolBinop (unpacker: LispVal -> ThrowsError<'a>) (op: 'a -> 'a -> bool) args = 
            if List.length args <> 2 then throwError (NumArgs(2,args))
            else
                either {
                    let! left  = unpacker (args.[0])
                    let! right = unpacker (args.[1])
                    return Bool(op left right)
                }

        let numBoolBinop env cont (op: LispVal -> LispVal -> ThrowsError<LispVal>) prms : Step = 
            match prms with 
            | [a;b] ->
                let cps e c r _ = 
                    match op r b with 
                    | Choice2Of2 q -> bounceContinue e c q
                    | Choice1Of2 err -> fail err
                bounceContinue env (makeCPS env cont cps) a
            | _ -> fail (NumArgs(2,prms))

        let car env cont = function
            | [List (x::_)] -> bounceContinue env cont x
            | [DottedList (x::_,_)] -> bounceContinue env cont x
            | [badArg] -> fail (TypeMismatch("pair",badArg))
            | badArgList -> fail (NumArgs(1,badArgList))

        let cdr env cont = function 
            | [List(_::xs)] -> bounceContinue env cont (List xs)
            | [DottedList([_],x)] -> bounceContinue env cont x
            | [DottedList(_::xs,x)] -> bounceContinue env cont (DottedList(xs, x))
            | [badArg] -> fail (TypeMismatch("pair",badArg))
            | badArgList -> fail (NumArgs(1,badArgList))

        let cons env cont = function
            | [x; List []] -> bounceContinue env cont (List[x])
            | [x; List(xs)] -> bounceContinue env cont (List(x::xs))
            | [x;DottedList(xs,xlast)] -> bounceContinue env cont (DottedList(x::xs,xlast))
            | [x1;x2] -> bounceContinue env cont (DottedList([x1],x2))
            | badArgList -> fail (NumArgs(2,badArgList))

        let rec eqv' = function
            | [Inert ; Inert] -> returnM  (Bool true)
            | [(Obj arg1); (Obj arg2)] -> returnM (Bool(arg1.Equals(arg2)))
            | [(Bool arg1); (Bool arg2)] -> returnM (Bool(arg1 = arg2))
            | [(Atom arg1); (Atom arg2)] -> returnM (Bool(arg1 = arg2))
            | [(DottedList (xs,x)); (DottedList (ys,y))] -> eqv' [List (xs@[x]); List(ys@[y])]
            | [(List arg1); (List arg2)] -> 
                let eqvPair (x1,x2) = 
                    match eqv' [x1;x2] with
                    |Choice1Of2(_) -> false
                    |Choice2Of2(Bool value) -> value
                    | _ -> false
                returnM (Bool((List.length arg1) = (List.length arg2) && List.forall eqvPair <| List.zip arg1 arg2)) 
            | [_; _] -> returnM (Bool false)
            | badArgList -> throwError (NumArgs(2,badArgList))

        let eqv env cont parms =
            match eqv' parms with
            | Choice1Of2 e -> fail e
            | Choice2Of2 q -> bounceContinue env cont q

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

        let load filename = either{
                let! Obj(q) = tryLoad filename
                return! readExprList (string q)
                }

        let readAll [Obj filename] = 
                either { 
                    let! r = load (filename :?> string)
                    return List r
                }

        let writeProc = function
                | [ob] -> Console.Out.Write(showVal ob);returnM (Bool true)
                | [ob; Port port] -> use writer = new StreamWriter(port)
                                     writer.Write(showVal ob)
                                     returnM (Bool true)
                | bad -> throwError (NumArgs(1, bad))

        let readProc port =
               let parseReader (reader:TextReader) = reader.ReadLine() |> readExpr
               match port with
                | [Port p]  -> use s = new StreamReader(p) in parseReader s
                | [] -> parseReader Console.In
                | bad -> throwError (NumArgs(1, bad))
          
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
            | [List[]]   -> bounceContinue env cont <| Bool(true) 
            | _          -> bounceContinue env cont <| Bool(false)

        let isEnvironment env cont = function 
            | [Environment _ ]   -> bounceContinue env cont <| Bool(true) 
            | _          -> bounceContinue env cont <| Bool(false)

        let isVector env cont = function 
            | [Vector _ ]   -> bounceContinue env cont <| Bool(true) 
            | _          -> bounceContinue env cont <| Bool(false)

        let isPair env cont = function 
            | [DottedList _]    -> bounceContinue env cont <| Bool(true) 
            | [List (_::_) ]    -> bounceContinue env cont <| Bool(true) 
            | _                 -> bounceContinue env cont <| Bool(false)

        let isZero env cont = function 
            |[Obj x] -> match x with 
                        | :? byte -> bounceContinue env cont <| Bool(byte (0) = (x :?> byte)) 
                        | :? int -> bounceContinue env cont <| Bool(0 = (x :?> int)) 
                        | :? int64 -> bounceContinue env cont <| Bool((x :?> int64) = 0L) 
                        | :? float32 -> bounceContinue env cont <| Bool((x :?> float32) = 0.0f) 
                        | :? float -> bounceContinue env cont <| Bool((x :?> float) = 0.0) 
                        | _ -> bounceContinue env cont <| Bool(false)
            |_ -> bounceContinue env cont <| Bool(false)
        
        open Arithmetic

        let wrap env cont (a::_)  =  bounceContinue env cont (Applicative a) 

        let unwrap env cont = function
            | Applicative c :: _ -> bounceContinue env cont c
            | a :: _ -> fail (TypeMismatch("applicative",a))
            | [] -> fail (NumArgs(1, []))

        let evaluate _ cont = function 
            | (a::b::_) -> bounceEval a cont b 
            | badArgList -> fail (NumArgs(2, badArgList))

        let makeEnvironment env cont xs = bounceContinue env cont (newEnv xs)
    
        let if_then_else env cont args = 
            match args with
            | cond::b::c::_ ->
                let cps e cn r _ =
                     match r with
                        |Bool(true) -> bounceEval e cn b 
                        |Bool(false) -> bounceEval e cn c
                        |found -> fail (TypeMismatch("bool",found))
                bounceEval env (makeCPS env cont cps) cond
            |_ -> fail (NumArgs(3,args))

        let loadAndEval env cont = function
            | [Obj(filename)] ->
                match cast filename with
                | Choice1Of2 e -> fail e
                | Choice2Of2 fname ->
                    match load fname with
                    | Choice1Of2 e -> fail e
                    | Choice2Of2 lisp ->
                        match sequence (List.map (eval env cont) lisp) [] with
                        | Choice1Of2 e -> fail e
                        | Choice2Of2 _ -> bounceContinue env cont Inert
            | badform -> fail (NumArgs(1, badform))

        let vau _env cont xs = 
            match xs with
            | prms :: Atom e :: body   -> bounceContinue _env cont (Operative{ prms = prms; envarg = e; body = body; closure = _env} ) 
            | _ -> fail (Default("invalid arguments"))

        let define env cont xs = 
            match xs with 
            | [ l; r ] ->
                let cps e c result _ = bounceBind e c l result
                bounceEval env (makeCPS env cont cps) r 
            | badForm -> fail (BadSpecialForm("invalid arguments",List(badForm)))

        let reset env cont  = function 
            | (exp::_) -> bounceEval env (Continuation({closure = env; currentCont = None ; nextCont = None; args = None}, Some cont, Full)) exp
            | badform -> fail (NumArgs(1,badform))
         
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
                | Continuation _    -> bounceContinue env func cont 
                | Applicative f     -> bounceOperate env cont f [cont]
                | badForm -> fail (TypeMismatch("continuation",badForm))
            | badForm -> fail (NumArgs(1,badForm))

        let shift env cont = function
            | Applicative f::_ ->
                match cont with
                | Continuation(continuationRecord, Some parentCont, _) ->
                    bounceOperate env (Continuation({closure = env; currentCont = None  ; nextCont = None; args = None} , Some parentCont, Full)) f [(Continuation(continuationRecord, None, Delimited ))]
                | _ -> fail (Default("reset needs to be called before shift"))
            | bad -> fail (NumArgs(1, bad))

        let plus env cont args = numericBinOp env cont opAdd args
        let minus env cont args = numericBinOp env cont opMinus args
        let times env cont args = numericBinOp env cont opMultiply args
        let divide env cont args = numericBinOp env cont opDivide args
        let lessThan env cont args = numBoolBinop env cont opLessThan args
        let lessThanOrEqual env cont args = numBoolBinop env cont opLessThanOrEqual args
        let greaterThan env cont args = numBoolBinop env cont opGreaterThan args
        let greaterThanOrEqual env cont args = numBoolBinop env cont opGreaterThanOrEqual args

        let vector env cont args =
            Vector(List.toArray args) |> bounceContinue env cont

        let vector_set env cont args =
            match args with 
            | [Vector arr; Obj pos'; value] when typeof<int> = pos'.GetType() ->
                arr.[pos' :?> int] <- value
                bounceContinue env cont Inert
            | [_; pos; _] -> fail (TypeMismatch("vector/int", pos))
            | _ -> fail (NumArgs(3, args))

        let vector_ref env cont args =
            match args with 
            | [Vector arr; Obj pos'] when typeof<int> = pos'.GetType() ->
                arr.[pos' :?> int] |> bounceContinue env cont
            | [_; pos] -> fail (TypeMismatch("vector/int", pos))
            | _ -> fail (NumArgs(2, args))

        let make_vector env cont args =
            match args with
            | [Obj size'; v] when typeof<int> = size'.GetType() ->
                Vector(Array.create (size' :?> int) v) |> bounceContinue env cont
            | [size; _] -> fail (TypeMismatch("int", size))
            | _ -> fail (NumArgs(2, args))

        let make_encapsulation_type env cont _ =
            let counter =  Guid.NewGuid()
            let encapsulator =
                Applicative (PrimitiveOperative ( fun e c (arg::_) -> Encapsulation { tag = counter; value = arg } |> bounceContinue e c ))
            let predicate =
                Applicative (PrimitiveOperative ( fun e c (arg::_) -> match arg with Encapsulation { tag = tag ; value = _ } -> Bool (counter.Equals(tag))  |> bounceContinue e c | _ -> Bool(false)  |> bounceContinue e c))
            let decapsulator = 
                Applicative (PrimitiveOperative ( fun e c (arg::_) -> match arg with Encapsulation { tag = tag ; value = value} when counter.Equals(tag) -> bounceContinue e c value | _ -> fail (Default "encapsulation type mismatch") ))
                
            List [encapsulator; predicate; decapsulator] |> bounceContinue env cont

        let primitiveApplicatives = 
            Map.ofList [ 
                  ("eval", evaluate);
                  ("wrap", wrap);
                  ("unwrap", unwrap);
                  ("load", loadAndEval);
                  ("call/cc", callcc);
                  ("+", plus);
                  ("-", minus);
                  ("*", times);
                  ("/", divide);
                  ("<", lessThan);
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
                  ("environment?", isEnvironment)
                  ("make-environment", makeEnvironment);
                  ("print", print);
                  ("printf", printf');
                  ("show", show);
                  ("shift", shift);
                  ("vector", vector);
                  ("vector?", isVector);
                  ("make-vector", make_vector);
                  ("vector-ref", vector_ref);
                  ("vector-set!", vector_set);
                  ("make-encapsulation-type", make_encapsulation_type)
                  ]

        /// Environment containing primitive operators
        let primitiveBindings = 
            let makeFunc t (var,func) = (var, t func)
            let primi = (Map.toList ioPrimitives |> List.map (makeFunc IOFunc)) 
                      @ (Map.toList primitiveOperatives     |> List.map (makeFunc PrimitiveOperative))
                      @ (Map.toList primitiveApplicatives   |> List.map (makeFunc (Applicative << PrimitiveOperative)))
            bindVars (newEnv []) primi
