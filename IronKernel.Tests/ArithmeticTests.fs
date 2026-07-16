module IronKernel.Tests.ArithmeticTests

open Xunit
open IronKernel.Ast
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
let ``eqv on numbers`` () =
    [
        "(eqv? 7 7)", Bool true
        "(eqv? 7 8)", Bool false
        "(eq? 1 1)", Bool true
    ] |> evalSession
