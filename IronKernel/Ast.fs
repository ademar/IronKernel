namespace IronKernel

module Ast =

    type OperativeRecord = { 
        prms    : string list; 
        vararg  : string option; 
        envarg  : string;
        body    : LispVal list; 
        closure : LispVal
    }
    and Env = (string * LispVal ref) list ref
    and LispVal = 
        | Atom of string 
        | List of LispVal list
        | DottedList of (LispVal list * LispVal)
        | Bool of bool 
        | Environment of Env * (LispVal list)
        | PrimitiveFunc of (LispVal list -> ThrowsError<LispVal>)
        | PrimitiveOperative of (LispVal -> LispVal -> LispVal list -> ThrowsError<LispVal>)
        | Operative of OperativeRecord
        | Applicative of LispVal
        | IOFunc of (LispVal list -> ThrowsError<LispVal>)
        | Port of System.IO.FileStream
        | Inert
        | Nil
        | Obj of obj
        | Continuation of (LispVal*((LispVal -> LispVal -> LispVal -> ThrowsError<LispVal>) option)*(LispVal option))
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
    
    let newContinuation env = Continuation(env, None,None)
    let makeCPS env cont f  = Continuation(env,Some f, Some cont)

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
        | PrimitiveFunc _ -> "<primitive>"
        | PrimitiveOperative _ -> "<primitive operative>"
        | Operative({prms = args; vararg = varargs; body = body; closure = env}) 
                -> "($vau (" + unwords args + ( match varargs with |None -> "" | Some arg -> " & " + arg) + ") ...)"
        | Port _ -> "<IO port>"
        | IOFunc _ -> "<IO primitive>"
        | Environment _  as e -> printEnvironment e //"<environment>"
        | Nil -> "()"
        | Obj o -> "<obj " + (if o = null then "null" else (o.ToString() + " : " + o.GetType().Name)) + ">"
        | Continuation _ -> "<continuation>"
        | Status s -> "error : " + s
        | Inert -> "#inert"

   

    
