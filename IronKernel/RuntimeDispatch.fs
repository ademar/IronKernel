namespace IronKernel

/// Runtime operations shared by dynamically and statically generated code.
module RuntimeDispatch =

    open Ast
    open Errors
    open Eval
    open SymbolTable

    let appNamed env cont name (operands: LispVal[]) : ThrowsError<LispVal> =
        match getVar env name with
        | Choice1Of2 error -> throwError error
        | Choice2Of2 combiner -> operate env cont combiner (Array.toList operands)