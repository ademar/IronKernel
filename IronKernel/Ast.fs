namespace IronKernel

module Ast =

    type OperativeRecord = { 
        prms    : string list; 
        vararg  : string option; 
        envarg  : string;
        body    : LispVal list; 
        closure : LispVal
    }
    and NativeFuncRecord = { 
        cont : LispVal -> LispVal -> LispVal -> (LispVal list) option -> ThrowsError<LispVal>; 
        args : (LispVal list) option
        }
    and DeferredCode =
        | KernelCode of (LispVal list)
        | NativeCode of NativeFuncRecord
    and ContinuationRecord = {
        closure     : LispVal
        currentCont : DeferredCode option
        nextCont    : LispVal option
        args        : (LispVal list) option
    }
    and Env = (string * LispVal ref) list ref
    and LispVal = 
        | Atom of string 
        | List of LispVal list
        | DottedList of (LispVal list * LispVal)
        | Bool of bool 
        | Environment of Env * (LispVal list)
        | PrimitiveOperative of (LispVal -> LispVal -> LispVal list -> ThrowsError<LispVal>)
        | Operative of OperativeRecord
        | Applicative of LispVal
        | IOFunc of (LispVal list -> ThrowsError<LispVal>)
        | Port of System.IO.FileStream
        | Inert
        | Nil
        | Obj of obj
        | Continuation of ContinuationRecord
        | Status of string

    and LispError = 
       | NumArgs of int * LispVal list
       | TypeMismatch of string * LispVal
       | ClrTypeMismatch of string * string
       | Parser of string
       | BadSpecialForm of string*LispVal
       | NotFunction of string*string
       | UnboundVar of string*string
       | Default of string

    and ThrowsError<'a> = Choice<LispError,'a>

    let makeObj = (fun x -> x :> obj  |> Obj)
    let newEnv frames = Environment(ref List.Empty,frames)
    let newContinuation env = Continuation {closure = env; currentCont = None; nextCont = None; args = None}
    let makeCPS env cont f  = Continuation {closure = env; currentCont = Some (NativeCode { cont = f ; args = None} ); nextCont = Some cont; args = None}
    let makeCPSWArgs env cont f args = 
        Continuation { closure = env; currentCont = Some (NativeCode { cont = f ; args = Some args} ); nextCont = Some cont; args = None}

    let unwords (lst: string list) = System.String.Join(" ",lst)

    let rec unwordsList = (List.map showVal) >> unwords

    and printEnvironment (Environment(env,st)) =
        "(" + (List.fold (fun (acc:string) (a,b) -> acc + "(" + a + ": " + showVal (!b) + " )\n" ) "" !env)
        + " (" + unwordsList st  + "))"

    and showVal = function
        
        | Atom (name) -> name
        
        | Bool(true) -> "#t"
        | Bool(false) -> "#f"
        | List(contents) -> "(" + unwordsList contents + ")"
        | DottedList(head,tail) -> "(" + unwordsList head + " & "  + showVal tail + ")"
        | Applicative(a) -> "<applicative " + showVal a + " >"
        | PrimitiveOperative _ -> "<primitive operative>"
        | Operative({prms = args; vararg = varargs; body = body; closure = env}) 
                -> "(vau (" + unwords args + ( match varargs with |None -> "" | Some arg -> " & " + arg) + ") ...)"
        | Port _ -> "<IO port>"
        | IOFunc _ -> "<IO primitive>"
        | Environment _  as e -> printEnvironment e //"<environment>"
        | Nil -> "()"
        | Obj o -> "<obj " + (if o = null then "null" else (o.ToString() + " : " + o.GetType().Name)) + ">"
        | Continuation _ -> "<continuation>"
        | Status s -> "error : " + s
        | Inert -> "#inert"

   

    
