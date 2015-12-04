namespace IronKernel

    open Ast
    open Choice
    open Errors
    open System

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
                | :? byte, _                -> throwError(ClrTypeMismatch("byte",b.GetType().Name))

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

                | :? DateTime, :? TimeSpan -> returnM <| Obj ((a :?> DateTime)        + (b :?> TimeSpan))
                | :? DateTime, _           -> throwError(ClrTypeMismatch("TimeSpan",b.GetType().Name))

                | _, _                      -> throwError(ClrTypeMismatch("number",a.GetType().Name))

             |(Obj _), _    -> throwError(TypeMismatch("object",b'))
             | _, (Obj b)   -> throwError(TypeMismatch("object",a'))
             | _,_          -> throwError(TypeMismatch("object",a'))

        let opMinus a' b' = 
            match a',b' with
            |(Obj a), (Obj b) ->
                match a,b with
                | :? byte, :? byte          -> returnM <| Obj ((a :?> byte)         - (b :?> byte))
                | :? byte, :? int32         -> returnM <| Obj (int(a :?> byte)          - (b :?> int))
                | :? byte, :? int64         -> returnM <| Obj (int64(a :?> byte)        - (b :?> int64))
                | :? byte, :? float32       -> returnM <| Obj (float32(a :?> byte)      - (b :?> float32))
                | :? byte, :? float         -> returnM <| Obj (float(a :?> byte)        - (b :?> float))
                | :? byte, _                -> throwError(ClrTypeMismatch("byte",b.GetType().Name))

                | :? int32, :? byte         -> returnM <| Obj ((a :?> int32)        - int32(b :?> byte))
                | :? int32, :? int32        -> returnM <| Obj ((a :?> int32)          - (b :?> int32))
                | :? int32, :? int64        -> returnM <| Obj (int64(a :?> int32)        - (b :?> int64))
                | :? int32, :? float32      -> returnM <| Obj (float32(a:?>int32)      - (b :?> float32))
                | :? int32, :? float        -> returnM <| Obj (float(a :?> int32)        - (b :?> float))
                | :? int32, _               -> throwError(ClrTypeMismatch("int32",b.GetType().Name))

                | :? int64, :? byte         -> returnM <| Obj ((a :?> int64)        - int64(b :?> byte))
                | :? int64, :? int32        -> returnM <| Obj ((a :?> int64)        - int64(b :?> int32))
                | :? int64, :? int64        -> returnM <| Obj ((a :?> int64)        - (b :?> int64))
                | :? int64, :? float32      -> returnM <| Obj (float(a :?> int64)        - float(b :?> float32))
                | :? int64, :? float        -> returnM <| Obj (float(a :?> int64)        - (b :?> float))
                | :? int64, _               -> throwError(ClrTypeMismatch("int64",b.GetType().Name))

                | :? float32, :? byte       -> returnM <| Obj ((a :?> float32)      - float32(b :?> byte))
                | :? float32, :? int32      -> returnM <| Obj ((a :?> float32)      - float32(b :?> int32))
                | :? float32, :? int64      -> returnM <| Obj (double(a :?> float32)        - double(b :?> int64))
                | :? float32, :? float32    -> returnM <| Obj ((a :?> float32)      - (b :?> float32))
                | :? float32, :? float      -> returnM <| Obj (float(a :?> float32)        - (b :?> float))
                | :? float32, _               -> throwError(ClrTypeMismatch("float32",b.GetType().Name))

                | :? float, :? byte         -> returnM <| Obj ((a :?> float)        - float(b :?> byte))
                | :? float, :? int32        -> returnM <| Obj ((a :?> float)        - float(b :?> int32))
                | :? float, :? int64        -> returnM <| Obj ((a :?> float)        - float(b :?> int64))
                | :? float, :? float32      -> returnM <| Obj ((a :?> float)        - float(b :?> float32))
                | :? float, :? float        -> returnM <| Obj ((a :?> float)        - (b :?> float))
                | :? float, _               -> throwError(ClrTypeMismatch("float",b.GetType().Name))

                | :? DateTime, :? DateTime -> returnM <| Obj ((a :?> DateTime)        - (b :?> DateTime))
                | :? DateTime, _           -> throwError(ClrTypeMismatch("DateTime",b.GetType().Name))

                | _, _                     -> throwError(ClrTypeMismatch("number",a.GetType().Name))

             |(Obj _), _    -> throwError(TypeMismatch("object",b'))
             | _, (Obj b)   -> throwError(TypeMismatch("object",a'))
             | _,_          -> throwError(TypeMismatch("object",a'))


        let opMultiply a' b' = 
            match a',b' with
            |(Obj a), (Obj b) ->
                match a,b with
                | :? byte, :? byte          -> returnM <| Obj ((a :?> byte)         * (b :?> byte))
                | :? byte, :? int32         -> returnM <| Obj (int(a :?> byte)          * (b :?> int))
                | :? byte, :? int64         -> returnM <| Obj (int64(a :?> byte)        * (b :?> int64))
                | :? byte, :? float32       -> returnM <| Obj (float32(a :?> byte)      * (b :?> float32))
                | :? byte, :? float         -> returnM <| Obj (float(a :?> byte)        * (b :?> float))
                | :? byte, _                -> throwError(ClrTypeMismatch("byte",b.GetType().Name))

                | :? int32, :? byte         -> returnM <| Obj ((a :?> int32)        * int32(b :?> byte))
                | :? int32, :? int32        -> returnM <| Obj ((a :?> int32)          * (b :?> int32))
                | :? int32, :? int64        -> returnM <| Obj (int64(a :?> int32)        * (b :?> int64))
                | :? int32, :? float32      -> returnM <| Obj (float32(a:?>int32)      * (b :?> float32))
                | :? int32, :? float        -> returnM <| Obj (float(a :?> int32)        * (b :?> float))
                | :? int32, _               -> throwError(ClrTypeMismatch("int32",b.GetType().Name))

                | :? int64, :? byte         -> returnM <| Obj ((a :?> int64)        * int64(b :?> byte))
                | :? int64, :? int32        -> returnM <| Obj ((a :?> int64)        * int64(b :?> int32))
                | :? int64, :? int64        -> returnM <| Obj ((a :?> int64)        * (b :?> int64))
                | :? int64, :? float32      -> returnM <| Obj (float(a :?> int64)        * float(b :?> float32))
                | :? int64, :? float        -> returnM <| Obj (float(a :?> int64)        * (b :?> float))
                | :? int64, _               -> throwError(ClrTypeMismatch("int64",b.GetType().Name))

                | :? float32, :? byte       -> returnM <| Obj ((a :?> float32)      * float32(b :?> byte))
                | :? float32, :? int32      -> returnM <| Obj ((a :?> float32)      * float32(b :?> int32))
                | :? float32, :? int64      -> returnM <| Obj (double(a :?> float32)        * double(b :?> int64))
                | :? float32, :? float32    -> returnM <| Obj ((a :?> float32)      * (b :?> float32))
                | :? float32, :? float      -> returnM <| Obj (float(a :?> float32)        * (b :?> float))
                | :? float32, _               -> throwError(ClrTypeMismatch("float32",b.GetType().Name))

                | :? float, :? byte         -> returnM <| Obj ((a :?> float)        * float(b :?> byte))
                | :? float, :? int32        -> returnM <| Obj ((a :?> float)        * float(b :?> int32))
                | :? float, :? int64        -> returnM <| Obj ((a :?> float)        * float(b :?> int64))
                | :? float, :? float32      -> returnM <| Obj ((a :?> float)        * float(b :?> float32))
                | :? float, :? float        -> returnM <| Obj ((a :?> float)        * (b :?> float))
                | :? float, _               -> throwError(ClrTypeMismatch("float",b.GetType().Name))
                | _, _                      -> throwError(ClrTypeMismatch("number",a.GetType().Name))

             |(Obj _), _    -> throwError(TypeMismatch("object",b'))
             | _, (Obj b)   -> throwError(TypeMismatch("object",a'))
             | _,_          -> throwError(TypeMismatch("object",a'))

    
        let opDivide a' b' = 
            match a',b' with
            |(Obj a), (Obj b) ->
                match a,b with
                | :? byte, :? byte          -> returnM <| Obj ((a :?> byte)         / (b :?> byte))
                | :? byte, :? int32         -> returnM <| Obj (int(a :?> byte)          / (b :?> int))
                | :? byte, :? int64         -> returnM <| Obj (int64(a :?> byte)        / (b :?> int64))
                | :? byte, :? float32       -> returnM <| Obj (float32(a :?> byte)      / (b :?> float32))
                | :? byte, :? float         -> returnM <| Obj (float(a :?> byte)        / (b :?> float))
                | :? byte, _                -> throwError(ClrTypeMismatch("byte",b.GetType().Name))

                | :? int32, :? byte         -> returnM <| Obj ((a :?> int32)        / int32(b :?> byte))
                | :? int32, :? int32        -> returnM <| Obj ((a :?> int32)          / (b :?> int32))
                | :? int32, :? int64        -> returnM <| Obj (int64(a :?> int32)        / (b :?> int64))
                | :? int32, :? float32      -> returnM <| Obj (float32(a:?>int32)      / (b :?> float32))
                | :? int32, :? float        -> returnM <| Obj (float(a :?> int32)        / (b :?> float))
                | :? int32, _               -> throwError(ClrTypeMismatch("int32",b.GetType().Name))

                | :? int64, :? byte         -> returnM <| Obj ((a :?> int64)        / int64(b :?> byte))
                | :? int64, :? int32        -> returnM <| Obj ((a :?> int64)        / int64(b :?> int32))
                | :? int64, :? int64        -> returnM <| Obj ((a :?> int64)        / (b :?> int64))
                | :? int64, :? float32      -> returnM <| Obj (float(a :?> int64)        / float(b :?> float32))
                | :? int64, :? float        -> returnM <| Obj (float(a :?> int64)        / (b :?> float))
                | :? int64, _               -> throwError(ClrTypeMismatch("int64",b.GetType().Name))

                | :? float32, :? byte       -> returnM <| Obj ((a :?> float32)      / float32(b :?> byte))
                | :? float32, :? int32      -> returnM <| Obj ((a :?> float32)      / float32(b :?> int32))
                | :? float32, :? int64      -> returnM <| Obj (double(a :?> float32)        / double(b :?> int64))
                | :? float32, :? float32    -> returnM <| Obj ((a :?> float32)      / (b :?> float32))
                | :? float32, :? float      -> returnM <| Obj (float(a :?> float32)        / (b :?> float))
                | :? float32, _               -> throwError(ClrTypeMismatch("float32",b.GetType().Name))

                | :? float, :? byte         -> returnM <| Obj ((a :?> float)        / float(b :?> byte))
                | :? float, :? int32        -> returnM <| Obj ((a :?> float)        / float(b :?> int32))
                | :? float, :? int64        -> returnM <| Obj ((a :?> float)        / float(b :?> int64))
                | :? float, :? float32      -> returnM <| Obj ((a :?> float)        / float(b :?> float32))
                | :? float, :? float        -> returnM <| Obj ((a :?> float)        / (b :?> float))
                | :? float, _               -> throwError(ClrTypeMismatch("float",b.GetType().Name))
                | _, _                      -> throwError(ClrTypeMismatch("number",a.GetType().Name))

             |(Obj _), _    -> throwError(TypeMismatch("object",b'))
             | _, (Obj b)   -> throwError(TypeMismatch("object",a'))
             | _,_          -> throwError(TypeMismatch("object",a'))


        let opLessThanOrEqual (Obj a) (Obj b) = 
            match a,b with
            | :? int32 , :? int32 -> returnM <| Bool((a:?>int) <= (b:?>int))
            | :? int32 , :? int64 -> returnM <| Bool(int64(a:?>int) <= (b:?>int64))
            | :? int32 , :? float32 -> returnM <| Bool(float32(a:?>int) <= (b:?>float32))
            | :? int32 , :? float -> returnM <| Bool(float(a:?>int) <= (b:?>float))
            | :? int32,_ -> throwError(TypeMismatch(a.GetType().Name, Obj b))

            | :? int64 , :? int32 -> returnM <| Bool((a:?>int64) <= int64(b:?>int))
            | :? int64 , :? int64 -> returnM <| Bool(int64(a:?>int64) <= (b:?>int64))
            | :? int64 , :? float32 -> returnM <| Bool(float32(a:?>int64) <= (b:?>float32))
            | :? int64 , :? float -> returnM <| Bool(float(a:?>int64) <= (b:?>float))
            | :? int64,_ -> throwError(TypeMismatch(a.GetType().Name, Obj b))

            | :? float32 , :? int32 -> returnM <| Bool((a:?>float32) <= float32(b:?>int))
            | :? float32 , :? int64 -> returnM <| Bool(float(a:?>float32) <= float(b:?>int64))
            | :? float32 , :? float32 -> returnM <| Bool(float32(a:?>float32) <= (b:?>float32))
            | :? float32 , :? float -> returnM <| Bool(float(a:?>float32) <= (b:?>float))
            | :? float32,_ -> throwError(TypeMismatch(a.GetType().Name, Obj b))

            | :? float , :? int32 -> returnM <| Bool((a:?>float) <= float(b:?>int))
            | :? float , :? int64 -> returnM <| Bool(int64(a:?>float) <= (b:?>int64))
            | :? float , :? float32 -> returnM <| Bool((a:?>float) <= float(b:?>float32))
            | :? float , :? float -> returnM <| Bool(float(a:?>float) <= (b:?>float))
            | :? float,_ -> throwError(TypeMismatch(a.GetType().Name, Obj b))
            
            | _,_ -> throwError(TypeMismatch(b.GetType().Name, Obj a))

    
        let opLessThan (Obj a) (Obj b) =
            match a,b with
            | :? int32 , :? int32 -> returnM <| Bool((a:?>int) < (b:?>int))
            | :? int32 , :? int64 -> returnM <| Bool(int64(a:?>int) < (b:?>int64))
            | :? int32 , :? float32 -> returnM <| Bool(float32(a:?>int) < (b:?>float32))
            | :? int32 , :? float -> returnM <| Bool(float(a:?>int) < (b:?>float))
            | :? int32,_ -> throwError(TypeMismatch(a.GetType().Name, Obj b))

            | :? int64 , :? int32 -> returnM <| Bool((a:?>int64) < int64(b:?>int))
            | :? int64 , :? int64 -> returnM <| Bool(int64(a:?>int64) < (b:?>int64))
            | :? int64 , :? float32 -> returnM <| Bool(float32(a:?>int64) < (b:?>float32))
            | :? int64 , :? float -> returnM <| Bool(float(a:?>int64) < (b:?>float))
            | :? int64,_ -> throwError(TypeMismatch(a.GetType().Name, Obj b))

            | :? float32 , :? int32 -> returnM <| Bool((a:?>float32) < float32(b:?>int))
            | :? float32 , :? int64 -> returnM <| Bool(float(a:?>float32) < float(b:?>int64))
            | :? float32 , :? float32 -> returnM <| Bool(float32(a:?>float32) < (b:?>float32))
            | :? float32 , :? float -> returnM <| Bool(float(a:?>float32) < (b:?>float))
            | :? float32,_ -> throwError(TypeMismatch(a.GetType().Name, Obj b))

            | :? float , :? int32 -> returnM <| Bool((a:?>float) < float(b:?>int))
            | :? float , :? int64 -> returnM <| Bool(int64(a:?>float) < (b:?>int64))
            | :? float , :? float32 -> returnM <| Bool((a:?>float) < float(b:?>float32))
            | :? float , :? float -> returnM <| Bool(float(a:?>float) < (b:?>float))
            | :? float,_ -> throwError(TypeMismatch(a.GetType().Name, Obj b))
            
            | _,_ -> throwError(TypeMismatch(b.GetType().Name, Obj a))

        let opGreaterThan a b = opLessThan b a
        let opGreaterThanOrEqual a b = opLessThanOrEqual b a


    


