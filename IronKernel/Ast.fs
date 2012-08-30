namespace IronKernel

module Ast =

    type ContinuationType = Full | Delimited

    type OperativeRecord = { 
        prms    : LispVal ;
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
        | Continuation of  ContinuationRecord * (LispVal option) * ContinuationType
        | Status of string
        | Keyword of string
        | Vector of LispVal array

    and LispError = 
       | NumArgs of int * LispVal list
       | TypeMismatch of string * LispVal
       | ClrTypeMismatch of string * string
       | Parser of string
       | BadSpecialForm of string*LispVal
       | NotFunction of string*string
       | UnboundVar of string*string
       | Default of string
       | ClrException of System.Exception

    and ThrowsError<'a> = Choice<LispError,'a>

    let makeObj = (fun x -> x :> obj  |> Obj)

    let newEnv frames = Environment(ref List.Empty,frames)

    let newContinuation env = Continuation ({closure = env; currentCont = None; nextCont = None; args = None}, None, Full)

    let makeCPS env (Continuation(cr, mc, ct)) f  = 
        Continuation ({closure = env; currentCont = Some (NativeCode { cont = f ; args = None} ); nextCont = Some (Continuation(cr,None, Full)) ; args = None},mc, ct)

    let makeCPSWArgs env (Continuation(cr,mc,ct)) f args = 
        Continuation ({ closure = env; currentCont = Some (NativeCode { cont = f ; args = Some args} ); nextCont = Some (Continuation(cr,None,Full)); args = None},mc, ct)

    let unwords (lst: string list) = System.String.Join(" ",List.toArray(*mono needs this call toArray*) lst)
    let unwordsa (lst: string array) = System.String.Join(" ",lst)

    let rec unwordsList = (List.map showVal) >> unwords
    and unwordsArray = (Array.map showVal) >> unwordsa
    and printBindings bnds = List.fold (fun (acc:string) (a,b) -> acc + "(" + a + ": " + showVal (!b) + " )\n" ) "" bnds
    and printEnvironment (Environment(env,st)) = "(" + (printBindings !env) + " (" + unwordsList st  + "))"
    and showVal = function
        
        | Atom (name) -> name
        
        | Bool(true) -> "#t"
        | Bool(false) -> "#f"
        | List(contents) -> "(" + unwordsList contents + ")"
        | DottedList(head,tail) -> "(" + unwordsList head + " & "  + showVal tail + ")"
        | Applicative(a) -> "<applicative " + showVal a + " >"
        | PrimitiveOperative _ -> "<primitive operative>"
        | Operative({prms = args; body = body; closure = env}) 
                 -> "(vau (" + (showVal args) + "))"
        | Port _ -> "<IO port>"
        | IOFunc _ -> "<IO primitive>"
        | Environment _  as e -> "<environment>" //printEnvironment e 
        | Nil -> "()"
        | Obj o -> "<obj " + (if o = null then "null" else (o.ToString() + " : " + o.GetType().Name)) + ">"
        | Continuation _ -> "<continuation>"
        | Status s -> "error : " + s
        | Inert -> "#inert"
        | Keyword s -> ":" + s
        | Vector contents ->  "[" + unwordsArray contents + "]"

   

    
