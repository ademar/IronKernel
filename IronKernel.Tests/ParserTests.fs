module IronKernel.Tests.ParserTests

open Xunit
open IronKernel.Ast
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
