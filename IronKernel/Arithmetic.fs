namespace IronKernel

    open Ast
    open Choice
    open Errors

    module Arithmetic =

        let opAdd a' b' = 
            match a',b' with
            |(Obj a), (Obj b) ->
                match a,b with
                | :? byte, :? byte          -> returnM <| Obj ((a :?> byte)         + (b :?> byte))
                | :? byte, :? int32         -> returnM <| Obj (int(a :?> byte)          + (b :?> int))
                | :? byte, :? int64         -> returnM <| Obj (int64(a :?> byte)        + (b :?> int64))
                | :? byte, :? float32       -> returnM <| Obj (float32(a :?> byte)      + (b :?> float32))
                | :? byte, :? float         -> returnM <| Obj (float(a :?> byte)        + (b :?> float))
                | :? byte, _               -> throwError(ClrTypeMismatch("byte",b.GetType().Name))

                | :? int32, :? byte         -> returnM <| Obj ((a :?> int32)        + int32(b :?> byte))
                | :? int32, :? int32        -> returnM <| Obj ((a :?> int32)          + (b :?> int32))
                | :? int32, :? int64        -> returnM <| Obj (int64(a :?> int32)        + (b :?> int64))
                | :? int32, :? float32      -> returnM <| Obj (float32(a:?>int32)      + (b :?> float32))
                | :? int32, :? float        -> returnM <| Obj (float(a :?> int32)        + (b :?> float))
                | :? int32, _               -> throwError(ClrTypeMismatch("int32",b.GetType().Name))

                | :? int64, :? byte         -> returnM <| Obj ((a :?> int64)        + int64(b :?> byte))
                | :? int64, :? int32        -> returnM <| Obj ((a :?> int64)        + int64(b :?> int32))
                | :? int64, :? int64        -> returnM <| Obj ((a :?> int64)        + (b :?> int64))
                | :? int64, :? float32      -> returnM <| Obj (float(a :?> int64)        + float(b :?> float32))
                | :? int64, :? float        -> returnM <| Obj (float(a :?> int64)        + (b :?> float))
                | :? int64, _               -> throwError(ClrTypeMismatch("int64",b.GetType().Name))

                | :? float32, :? byte       -> returnM <| Obj ((a :?> float32)      + float32(b :?> byte))
                | :? float32, :? int32      -> returnM <| Obj ((a :?> float32)      + float32(b :?> int32))
                | :? float32, :? int64      -> returnM <| Obj (double(a :?> float32)        + double(b :?> int64))
                | :? float32, :? float32    -> returnM <| Obj ((a :?> float32)      + (b :?> float32))
                | :? float32, :? float      -> returnM <| Obj (float(a :?> float32)        + (b :?> float))
                | :? float32, _               -> throwError(ClrTypeMismatch("float32",b.GetType().Name))

                | :? float, :? byte         -> returnM <| Obj ((a :?> float)        + float(b :?> byte))
                | :? float, :? int32        -> returnM <| Obj ((a :?> float)        + float(b :?> int32))
                | :? float, :? int64        -> returnM <| Obj ((a :?> float)        + float(b :?> int64))
                | :? float, :? float32      -> returnM <| Obj ((a :?> float)        + float(b :?> float32))
                | :? float, :? float        -> returnM <| Obj ((a :?> float)        + (b :?> float))
                | :? float, _               -> throwError(ClrTypeMismatch("float",b.GetType().Name))
                | _, _                      -> throwError(ClrTypeMismatch("number",a.GetType().Name))

             |(Obj _), _    -> throwError(TypeMismatch("object",b'))
             | _, (Obj b)   -> throwError(TypeMismatch("object",a'))
             | _,_          -> throwError(TypeMismatch("object",a'))

        let opMinus (Obj a) (Obj b) = 
            match a,b with
            | :? byte, :? byte          -> returnM <| Obj ((a :?> byte)         - (b :?> byte))
            | :? byte, :? int32         -> returnM <| Obj ((a :?> int)          - (b :?> int))
            | :? byte, :? int64         -> returnM <| Obj ((a :?> int64)        - (b :?> int64))
            | :? byte, :? float32       -> returnM <| Obj ((a :?> float32)      - (b :?> float32))
            | :? byte, :? float         -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? byte, _               -> throwError(ClrTypeMismatch("byte",b.GetType().Name))

            | :? int32, :? byte         -> returnM <| Obj ((a :?> int32)        - (b :?> int32))
            | :? int32, :? int32        -> returnM <| Obj ((a :?> int)          - (b :?> int))
            | :? int32, :? int64        -> returnM <| Obj ((a :?> int64)        - (b :?> int64))
            | :? int32, :? float32      -> returnM <| Obj ((a :?> float32)      - (b :?> float32))
            | :? int32, :? float        -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? int32, _               -> throwError(ClrTypeMismatch("int32",b.GetType().Name))

            | :? int64, :? byte         -> returnM <| Obj ((a :?> int64)        - (b :?> int64))
            | :? int64, :? int32        -> returnM <| Obj ((a :?> int64)        - (b :?> int64))
            | :? int64, :? int64        -> returnM <| Obj ((a :?> int64)        - (b :?> int64))
            | :? int64, :? float32      -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? int64, :? float        -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? int64, _               -> throwError(ClrTypeMismatch("int64",b.GetType().Name))

            | :? float32, :? byte       -> returnM <| Obj ((a :?> float32)      - (b :?> float32))
            | :? float32, :? int32      -> returnM <| Obj ((a :?> float32)      - (b :?> float32))
            | :? float32, :? int64      -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? float32, :? float32    -> returnM <| Obj ((a :?> float32)      - (b :?> float32))
            | :? float32, :? float      -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? float32, _               -> throwError(ClrTypeMismatch("float32",b.GetType().Name))

            | :? float, :? byte         -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? float, :? int32        -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? float, :? int64        -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? float, :? float32      -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? float, :? float        -> returnM <| Obj ((a :?> float)        - (b :?> float))
            | :? float, _               -> throwError(ClrTypeMismatch("float",b.GetType().Name))
            | _, _                      -> throwError(ClrTypeMismatch("number",a.GetType().Name))


        let opMultiply (Obj a) (Obj b) = 
            match a,b with
            | :? byte, :? byte          -> returnM <| Obj ((a :?> byte)         * (b :?> byte))
            | :? byte, :? int32         -> returnM <| Obj ((a :?> int)          * (b :?> int))
            | :? byte, :? int64         -> returnM <| Obj ((a :?> int64)        * (b :?> int64))
            | :? byte, :? float32       -> returnM <| Obj ((a :?> float32)      * (b :?> float32))
            | :? byte, :? float         -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? byte, _               -> throwError(ClrTypeMismatch("byte",b.GetType().Name))

            | :? int32, :? byte         -> returnM <| Obj ((a :?> int32)        * (b :?> int32))
            | :? int32, :? int32        -> returnM <| Obj ((a :?> int)          * (b :?> int))
            | :? int32, :? int64        -> returnM <| Obj ((a :?> int64)        * (b :?> int64))
            | :? int32, :? float32      -> returnM <| Obj ((a :?> float32)      * (b :?> float32))
            | :? int32, :? float        -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? int32, _               -> throwError(ClrTypeMismatch("int32",b.GetType().Name))

            | :? int64, :? byte         -> returnM <| Obj ((a :?> int64)        * (b :?> int64))
            | :? int64, :? int32        -> returnM <| Obj ((a :?> int64)        * (b :?> int64))
            | :? int64, :? int64        -> returnM <| Obj ((a :?> int64)        * (b :?> int64))
            | :? int64, :? float32      -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? int64, :? float        -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? int64, _               -> throwError(ClrTypeMismatch("int64",b.GetType().Name))

            | :? float32, :? byte       -> returnM <| Obj ((a :?> float32)      * (b :?> float32))
            | :? float32, :? int32      -> returnM <| Obj ((a :?> float32)      * (b :?> float32))
            | :? float32, :? int64      -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? float32, :? float32    -> returnM <| Obj ((a :?> float32)      * (b :?> float32))
            | :? float32, :? float      -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? float32, _             -> throwError(ClrTypeMismatch("float32",b.GetType().Name))

            | :? float, :? byte         -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? float, :? int32        -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? float, :? int64        -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? float, :? float32      -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? float, :? float        -> returnM <| Obj ((a :?> float)        * (b :?> float))
            | :? float, _               -> throwError(ClrTypeMismatch("float",b.GetType().Name))
            | _, _                      -> throwError(ClrTypeMismatch("number",a.GetType().Name))

    
        let opDivide (Obj a) (Obj b) = 
            match a,b with
            | :? byte, :? byte          -> returnM <| Obj ((a :?> byte)         / (b :?> byte))
            | :? byte, :? int32         -> returnM <| Obj ((a :?> int)          / (b :?> int))
            | :? byte, :? int64         -> returnM <| Obj ((a :?> int64)        / (b :?> int64))
            | :? byte, :? float32       -> returnM <| Obj ((a :?> float32)      / (b :?> float32))
            | :? byte, :? float         -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? byte, _               -> throwError(ClrTypeMismatch("byte",b.GetType().Name))

            | :? int32, :? byte         -> returnM <| Obj ((a :?> int32)        / (b :?> int32))
            | :? int32, :? int32        -> returnM <| Obj ((a :?> int)          / (b :?> int))
            | :? int32, :? int64        -> returnM <| Obj ((a :?> int64)        / (b :?> int64))
            | :? int32, :? float32      -> returnM <| Obj ((a :?> float32)      / (b :?> float32))
            | :? int32, :? float        -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? int32, _               -> throwError(ClrTypeMismatch("int32",b.GetType().Name))

            | :? int64, :? byte         -> returnM <| Obj ((a :?> int64)        / (b :?> int64))
            | :? int64, :? int32        -> returnM <| Obj ((a :?> int64)        / (b :?> int64))
            | :? int64, :? int64        -> returnM <| Obj ((a :?> int64)        / (b :?> int64))
            | :? int64, :? float32      -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? int64, :? float        -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? int64, _               -> throwError(ClrTypeMismatch("int64",b.GetType().Name))

            | :? float32, :? byte       -> returnM <| Obj ((a :?> float32)      / (b :?> float32))
            | :? float32, :? int32      -> returnM <| Obj ((a :?> float32)      / (b :?> float32))
            | :? float32, :? int64      -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? float32, :? float32    -> returnM <| Obj ((a :?> float32)      / (b :?> float32))
            | :? float32, :? float      -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? float32, _             -> throwError(ClrTypeMismatch("float32",b.GetType().Name))

            | :? float, :? byte         -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? float, :? int32        -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? float, :? int64        -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? float, :? float32      -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? float, :? float        -> returnM <| Obj ((a :?> float)        / (b :?> float))
            | :? float, _               -> throwError(ClrTypeMismatch("float",b.GetType().Name))
            | _, _                      -> throwError(ClrTypeMismatch("number",a.GetType().Name))


    


