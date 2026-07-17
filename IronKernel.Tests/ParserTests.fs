module IronKernel.Tests.ParserTests

open Xunit
open IronKernel.Ast
open IronKernel.Errors
open IronKernel.Parser
open IronKernel.Source
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``parses integers and floats`` () =
    match parseOk "42" with
    | Obj (:? int as n) -> Assert.Equal(42, n)
    | v -> failwith (showVal v)
    match parseOk "-3" with
    | Obj (:? int as n) -> Assert.Equal(-3, n)
    | v -> failwith (showVal v)
    match parseOk "3.14" with
    | Obj (:? float as n) -> Assert.True(abs (n - 3.14) < 1e-9)
    | v -> failwith (showVal v)

[<Fact>]
let ``parses booleans inert and keywords`` () =
    assertEqv (parseOk "#t") (Bool true)
    assertEqv (parseOk "#f") (Bool false)
    assertEqv (parseOk "#inert") Inert
    match parseOk ":foo" with
    | Keyword "foo" -> ()
    | v -> failwith (showVal v)

[<Fact>]
let ``parses strings atoms and quote`` () =
    match parseOk "\"hi\\n\"" with
    | Obj (:? string as s) -> Assert.Equal("hi\n", s)
    | v -> failwith (showVal v)
    match parseOk "map" with
    | Atom "map" -> ()
    | v -> failwith (showVal v)
    match parseOk "'x" with
    | List [Atom "quote"; Atom "x"] -> ()
    | v -> failwith (showVal v)

[<Fact>]
let ``parses lists dotted lists and vectors`` () =
    assertEqv (parseOk "(a 1)") (List [Atom "a"; Obj 1])
    assertEqv (parseOk "()") (List [])
    match parseOk "(a & b)" with
    | DottedList ([Atom "a"], Atom "b") -> ()
    | v -> failwith (showVal v)
    match parseOk "[1 2 3]" with
    | Vector arr ->
        Assert.Equal(3, arr.Length)
        assertEqv arr.[0] (Obj 1)
        assertEqv arr.[2] (Obj 3)
    | v -> failwith (showVal v)

[<Fact>]
let ``parses nested combinations`` () =
    assertEqv
        (parseOk "(+ 1 (* 2 3))")
        (List [Atom "+"; Obj 1; List [Atom "*"; Obj 2; Obj 3]])

[<Fact>]
let ``rejects unbalanced parentheses`` () =
    parseError "(+ 1 2"

[<Fact>]
let ``located parser records nested source spans without changing datum`` () =
    let source = "(+ 1\n  (* 2 3))"
    match readLocatedExpr "nested.scm" source with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 outer ->
        assertEqv (toLispVal outer) (parseOk source)
        Assert.Equal("nested.scm", outer.span.sourceName)
        Assert.Equal(1L, outer.span.startPosition.line)
        Assert.Equal(1L, outer.span.startPosition.column)
        Assert.Equal(2L, outer.span.endPosition.line)
        match outer.kind with
        | LList [_; _; inner] ->
            Assert.True(contains outer.span inner.span)
            Assert.Equal(2L, inner.span.startPosition.line)
            Assert.Equal(3L, inner.span.startPosition.column)
        | _ -> failwith "expected nested list"

[<Fact>]
let ``named parse diagnostics include position source and caret`` () =
    let source = "(+ 1\n  (* 2 3)"
    match readExprFromSource "broken.scm" source with
    | Choice2Of2 value -> failwith ("unexpectedly parsed " + showVal value)
    | Choice1Of2 error ->
        let diagnostic = showError error
        Assert.Contains("broken.scm:2:", diagnostic)
        Assert.Contains("(* 2 3)", diagnostic)
        Assert.Contains("^", diagnostic)
