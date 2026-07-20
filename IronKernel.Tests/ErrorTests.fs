module IronKernel.Tests.ErrorTests

open Xunit
open IronKernel.Ast
open IronKernel.Errors

[<Fact>]
let ``extractValue renders structured errors`` () =
    let result = extractValue (throwError (UnboundVar("Missing binding", "value")))
    match result with
    | Status message -> Assert.Equal("Missing binding: 'value' ", message)
    | value -> failwithf "expected error status, got %A" value