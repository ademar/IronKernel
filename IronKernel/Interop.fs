namespace IronKernel

    open System
    open Ast
    open Errors
    open Choice
    open Eval

    module Interop =

        let toObjects : LispVal -> ThrowsError<obj>  = function
            |Bool b -> returnM (b :> obj)
            |Obj o  -> returnM o
            |badForm -> throwError (TypeMismatch("Obj",badForm))

        /// Resolve CLR types by name across loaded assemblies (netcore Type.GetType is picky).
        let resolveType (name: string) =
            let direct = Type.GetType(name)
            if direct <> null then direct
            else
                AppDomain.CurrentDomain.GetAssemblies()
                |> Array.tryPick (fun asm ->
                    let t = asm.GetType(name, false)
                    if t = null then None else Some t)
                |> Option.toObj

        let new_object env cont = function
            | Atom t :: args ->
                let typ = resolveType t 
                if typ = null then fail (Default("Couldn't find type '" + t + "'"))
                else
                    try
                        match sequence (List.map toObjects args) [] with
                        | Choice1Of2 e -> fail e
                        | Choice2Of2 mapargs ->
                            let obj = Activator.CreateInstance(typ,List.toArray mapargs)
                            bounceContinue env cont (Obj obj)
                    with ex -> fail (Default("Couldn't create type '" + t+ "', " + ex.Message))
            | bad -> fail (NumArgs(1, bad))

        open System.Reflection

        let get env cont (t:Type) (o:obj) p =
            let f = t.GetField(p)
            if f = null then
                let f = t.GetProperty(p)
                if f = null then 
                    fail (Default("field or property '" + p + "' does not exist"))
                else
                    bounceContinue env cont (Obj(f.GetValue(o,null)))
            else
                bounceContinue env cont (Obj(f.GetValue(o)))

        let set env cont (t:Type) (o:obj) p (v:obj)=
            let f = t.GetField(p)
            if f = null then
                let f = t.GetProperty(p)
                if f = null then 
                    fail (Default("field or property '" + p + "' does not exist"))
                else
                    f.SetValue(o, v, null)
                    bounceContinue env cont Inert
            else
                f.SetValue(o, v)
                bounceContinue env cont Inert

        let rec dot_get env cont prms = 
            match prms with
            | (clazz::Atom(p)::tail) ->
                match clazz with
                |Obj o -> let typ = o.GetType() in get env cont typ o p
                |Atom c ->let typ = resolveType c  in get env cont typ null p
                |exp -> let cps e c result _ =
                            dot_get e c (result::Atom(p)::tail)
                        bounceEval env (makeCPS env cont cps) exp
            | _ -> fail (NumArgs(2,prms))

        let dot_set env cont prms = 
            match prms with
            | (clazz::Atom(p)::Obj(x)::_) ->
                match clazz with
                |Obj o -> let typ = o.GetType() in set env cont typ o p x
                |Atom c ->let typ = resolveType c  in set env cont typ null p x
                | _ -> fail (TypeMismatch("object", clazz))
            | _ -> fail (NumArgs(3,prms))

        let invoke (t:Type) (o:obj) (m: string) args= 
            try
                either {
                    let! mapargs = sequence (List.map toObjects args) []
                    let r = t.InvokeMember(m,BindingFlags.InvokeMethod,Type.DefaultBinder,o, List.toArray mapargs)
                    if r = null then return Inert
                    else return (Obj(r))
                }
            with ex -> throwError(Default("member invokation failed: " + ex.Message))

        let dot env cont = function
            | (clazz::Atom(m)::args) ->
                match sequence (List.map (eval env (newContinuation env)) args) [] with
                | Choice1Of2 e -> fail e
                | Choice2Of2 args' ->
                    let result =
                        match clazz with
                        |Obj o  -> let typ = o.GetType() in invoke typ o m args'
                        |Atom c ->
                            let typ = resolveType c 
                            if typ = null then
                                match eval env cont clazz with
                                | Choice2Of2 (Obj o) ->
                                    let typ = o.GetType() 
                                    invoke typ o m args'
                                | Choice2Of2 bad -> throwError (TypeMismatch("Obj", bad))
                                | Choice1Of2 e -> throwError e
                            else invoke typ null m args'
                        | bad -> throwError (TypeMismatch("object", bad))
                    match result with
                    | Choice1Of2 e -> fail e
                    | Choice2Of2 r -> bounceContinue env cont r
            | prms -> fail (NumArgs(2, prms))

        let print env cont (prms : LispVal list) =
          match prms with
          | [Obj(sf)] ->
                System.Console.Write(sf.ToString())
                bounceContinue env cont Inert
          | _ -> fail (NumArgs(1,prms))

        let printf' env cont (prms : LispVal list) =
          match prms with
          | Obj(sf)::tail when typeof<string> = sf.GetType() -> 
                match sequence (List.map toObjects tail) [] with
                | Choice1Of2 e -> fail e
                | Choice2Of2 mapargs ->
                    let str = String.Format(sf :?> string,List.toArray mapargs) 
                    System.Console.Write(str)
                    bounceContinue env cont Inert
          | _ -> fail (NumArgs(2,prms))

        let show env cont (h::_ : LispVal list) =
          System.Console.Write(showVal h)
          bounceContinue env cont Inert
