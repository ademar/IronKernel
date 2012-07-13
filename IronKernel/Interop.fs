namespace IronKernel

    open System
    open Ast
    open Errors
    open Choice
    open Eval

    module Interop =

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

