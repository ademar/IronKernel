namespace IronKernel

module Ast =

    type SourcePosition = {
        offset : int64
        line : int64
        column : int64
    }

    type SourceSpan = {
        sourceName : string
        startPosition : SourcePosition
        endPosition : SourcePosition
    }

    type HostCapability =
        | RawClrInterop
        | HostIO
        | SourceLoading
        | HostAsync
        | GeneratedClr of string

    type CapabilityProfile =
        | Minimal
        | Safe
        | Unrestricted

    type CapabilitySet = Set<HostCapability>

    type ContractMode =
        | RawOperands
        | EvaluatedArguments

    type ContractShape =
        | AnyShape
        | NumberShape
        | IntegerShape
        | StringShape
        | BooleanShape
        | AtomShape
        | ListShape
        | PromptTagShape
        | ResumptionShape

    type ContractEffect =
        | Pure
        | Effectful

    type ContractTrust =
        | Certified
        | Asserted

    type OperativeContract = {
        name : string
        mode : ContractMode
        operands : ContractShape list
        result : ContractShape
        effect : ContractEffect
        inlineable : bool
        trust : ContractTrust
    }

    type ContinuationType = Full | Delimited

    /// Trampoline step used by the CPS evaluator / compiler runtime.
    type Step =
        | Done of ThrowsError<LispVal>
        | More of (unit -> Step)
        | Await of AwaitRequest

    and AwaitRequest = {
        register : (ThrowsError<LispVal> -> unit) -> unit
        resume : ThrowsError<LispVal> -> Step
    }

    and OperativeRecord = { 
        prms    : LispVal ;
        envarg  : string;
        body    : LispVal list; 
        closure : LispVal
    }
    and NativeFuncRecord = { 
        cont : LispVal -> LispVal -> LispVal -> (LispVal list) option -> Step
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
    and PromptFrame = {
        parentCont : LispVal
        tag : System.Guid option
        handler : LispVal option
    }
    and ResumptionRecord = {
        continuation : LispVal
        mutable consumed : int
    }
    and EncapsulationRecord = {
        tag     : System.Guid
        value   : LispVal
    }
    and PrimitiveIdentity =
        | PrimitiveIf
        | PrimitiveDefine
    and PrimitiveOperativeRecord = {
        identity : PrimitiveIdentity option
        invoke : LispVal -> LispVal -> LispVal list -> Step
    }
    and ContractedCombinerRecord = {
        combiner : LispVal
        contract : OperativeContract
    }
    and BindingState = {
        value : LispVal
        version : int64
    }
    and BindingCell = {
        id : int64
        mutable state : BindingState
    }
    and Env = (string * BindingCell) list ref
    and EnvironmentRecord = {
        bindings : Env
        parents : LispVal list
        capabilities : CapabilitySet
        /// Prefixes tried when resolving short CLR type names (env-local).
        clrNamespaces : string list ref
        /// Short name → full CLR type name (env-local).
        clrAliases : Map<string, string> ref
    }
    and LispVal = 
        | Atom of string 
        | List of LispVal list
        | DottedList of (LispVal list * LispVal)
        | Bool of bool
        | Environment of EnvironmentRecord
        | PrimitiveOperative of PrimitiveOperativeRecord
        | ContractedCombiner of ContractedCombinerRecord
        | Operative of OperativeRecord
        | Applicative of LispVal
        | IOFunc of HostCapability * (LispVal list -> ThrowsError<LispVal>)
        | Port of System.IO.FileStream
        | Inert
        | Nil
        | Obj of obj
        | Continuation of ContinuationRecord * PromptFrame option * ContinuationType
        | PromptTag of System.Guid
        | Resumption of ResumptionRecord
        | Status of string
        | Keyword of string
        | Vector of LispVal array
        | Encapsulation of EncapsulationRecord
        /// CLR-compiled combiner (Expression / IL).
        | CompiledCombiner of (LispVal -> LispVal -> LispVal list -> Step)

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
       | LocatedError of SourceSpan * string option * LispError
       | CapabilityDenied of string
       | ContractViolation of string

    and ThrowsError<'a> = Choice<LispError,'a>

    let makeObj = (fun x -> x :> obj  |> Obj)

    let allHostCapabilities : CapabilitySet =
        Set.ofList
            [ RawClrInterop
              HostIO
              SourceLoading
              HostAsync
              GeneratedClr "safe" ]

    let private inheritClrState frames =
        let namespaces =
            frames
            |> List.choose (function Environment record -> Some !record.clrNamespaces | _ -> None)
            |> List.concat
            |> List.distinct
        let aliases =
            frames
            |> List.choose (function Environment record -> Some !record.clrAliases | _ -> None)
            |> List.fold
                (fun acc map ->
                    Map.fold
                        (fun merged key value ->
                            if Map.containsKey key merged then merged
                            else Map.add key value merged)
                        acc
                        map)
                Map.empty
        ref namespaces, ref aliases

    let newEnvWithClr capabilities frames clrSources =
        let clrNamespaces, clrAliases = inheritClrState clrSources
        Environment
            { bindings = ref List.Empty
              parents = frames
              capabilities = capabilities
              clrNamespaces = clrNamespaces
              clrAliases = clrAliases }

    let newEnvWithCapabilities capabilities frames =
        newEnvWithClr capabilities frames frames

    let newEnv = function
        | [Environment parent] as frames ->
            Environment
                { bindings = ref List.Empty
                  parents = frames
                  capabilities = parent.capabilities
                  clrNamespaces = ref !parent.clrNamespaces
                  clrAliases = ref !parent.clrAliases }
        | frames ->
            let inherited =
                frames
                |> List.choose (function Environment record -> Some record.capabilities | _ -> None)
            let capabilities =
                match inherited with
                | [] -> allHostCapabilities
                | first :: rest -> List.fold Set.intersect first rest
            newEnvWithCapabilities capabilities frames

    let newContinuation env = Continuation ({closure = env; currentCont = None; nextCont = None; args = None}, None, Full)

    let makeCPS env (Continuation(cr, mc, ct)) f  = 
        Continuation ({closure = env; currentCont = Some (NativeCode { cont = f ; args = None} ); nextCont = Some (Continuation(cr,None, Full)) ; args = None},mc, ct)

    let makeCPSWArgs env (Continuation(cr,mc,ct)) f args = 
        Continuation ({ closure = env; currentCont = Some (NativeCode { cont = f ; args = Some args} ); nextCont = Some (Continuation(cr,None,Full)); args = None},mc, ct)

    let unwords (lst: string list) = System.String.Join(" ",List.toArray(*mono needs this call toArray*) lst)
    let unwordsa (lst: string array) = System.String.Join(" ",lst)

    let rec unwordsList = (List.map showVal) >> unwords
    and unwordsArray = (Array.map showVal) >> unwordsa
    and printBindings bnds =
        List.fold
            (fun (acc:string) (name, cell) ->
                acc + "(" + name + ": " + showVal cell.state.value + " )\n")
            ""
            bnds
    and printEnvironment (Environment record) =
        "(" + (printBindings !record.bindings) + " (" + unwordsList record.parents + "))"
    and showVal = function
        
        | Atom (name) -> name
        
        | Bool(true) -> "#t"
        | Bool(false) -> "#f"
        | List(contents) -> "(" + unwordsList contents + ")"
        | DottedList(head,tail) -> "(" + unwordsList head + " & "  + showVal tail + ")"
        | Applicative(a) -> "<applicative " + showVal a + " >"
        | PrimitiveOperative _ -> "<primitive operative>"
        | ContractedCombiner contracted ->
            "<contracted " + contracted.contract.name + ">"
        | Operative({prms = args; body = body; closure = env}) 
                 -> "(vau (" + (showVal args) + "))"
        | Port _ -> "<IO port>"
        | IOFunc _ -> "<IO primitive>"
        | Environment _  as e -> "<environment>" //printEnvironment e 
        | Nil -> "()"
        | Obj (:? System.Type as t) -> "<type " + t.FullName + ">"
        | Obj o -> "<obj " + (if o = null then "null" else (o.ToString() + " : " + o.GetType().Name)) + ">"
        | Continuation _ -> "<continuation>"
        | PromptTag _ -> "<prompt-tag>"
        | Resumption _ -> "<resumption>"
        | Status s -> "error : " + s
        | Inert -> "#inert"
        | Keyword s -> ":" + s
        | Vector contents ->  "[" + unwordsArray contents + "]"
        | Encapsulation { tag = tag } -> "encapsulation: " + tag.ToString()
        | CompiledCombiner _ -> "<compiled combiner>"

