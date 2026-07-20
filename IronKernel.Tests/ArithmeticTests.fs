module IronKernel.Tests.ArithmeticTests

open Xunit
open IronKernel.Ast
open IronKernel.Arithmetic
open IronKernel.Errors
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``basic arithmetic`` () =
    [
        "(+ 2 3)", Obj 5
        "(- 10 4)", Obj 6
        "(* 6 7)", Obj 42
        "(/ 15 3)", Obj 5
    ] |> evalSession

[<Fact>]
let ``nested arithmetic`` () =
    [
        "(+ 1 (* 2 (+ 3 4)))", Obj 15
        "(- (* 5 5) (+ 2 3))", Obj 20
    ] |> evalSession

[<Fact>]
let ``numeric predicates and comparisons`` () =
    [
        "(zero? 0)", Bool true
        "(zero? 1)", Bool false
        "(< 1 2)", Bool true
        "(< 2 1)", Bool false
        "(<= 2 2)", Bool true
        "(> 5 3)", Bool true
    ] |> evalSession

[<Fact>]
let ``mixed int and float`` () =
    let env = freshEnv ()
    match evalIn env "(+ 1 2.5)" with
    | Obj (:? float as n) -> Assert.True(abs (n - 3.5) < 1e-9)
    | v -> failwith (showVal v)

[<Fact>]
let ``arithmetic preserves structured type errors`` () =
    for mode in [Interpreted; Compiled] do
        let env = freshEnv ()
        match evalRaw mode env "(+ \"wrong\" 1)" with
        | Choice1Of2 (ClrTypeMismatch("number", "String")) -> ()
        | result -> failwithf "%A returned the wrong arithmetic error: %A" mode result

[<Fact>]
let ``comparisons preserve structured type errors`` () =
    for mode in [Interpreted; Compiled] do
        let env = freshEnv ()
        for operator in ["<"; "<="; ">"] do
            match evalRaw mode env $"({operator} #t 1)" with
            | Choice1Of2 (ContractViolation message) ->
                Assert.Contains("operand 1 expected number", message)
            | result -> failwithf "%A %s returned the wrong comparison error: %A" mode operator result

    let invalid = Bool true
    let valid = Obj (1 :> obj)
    for compare in [opLessThan; opLessThanOrEqual; opGreaterThan; opGreaterThanOrEqual] do
        match compare invalid valid with
        | Choice1Of2 (TypeMismatch ("object", Bool true)) -> ()
        | result -> failwithf "direct comparison returned the wrong error: %A" result

[<Fact>]
let ``eqv on numbers`` () =
    [
        "(eqv? 7 7)", Bool true
        "(eqv? 7 8)", Bool false
        "(eq? 1 1)", Bool true
    ] |> evalSession
