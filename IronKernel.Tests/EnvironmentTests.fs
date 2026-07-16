module IronKernel.Tests.EnvironmentTests

open Xunit
open IronKernel.Ast
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``define and lookup`` () =
    [
        "(define answer 42)", Inert
        "answer", Obj 42
        "(define answer 7)", Inert
        "answer", Obj 7
    ] |> evalSession

[<Fact>]
let ``make-environment and remote-eval`` () =
    evalSessionKernel [
        "(define e (bindings->environment (x 10) (y 20)))", Inert
        "(remote-eval x e)", Obj 10
        "(remote-eval y e)", Obj 20
        "(environment? e)", Bool true
        "(environment? 1)", Bool false
    ]

[<Fact>]
let ``import! brings bindings into current env`` () =
    evalSessionKernel [
        "(define lib (bindings->environment (a 1) (b 2) (c 3)))", Inert
        "(import! lib a c)", Inert
        "a", Obj 1
        "c", Obj 3
    ]

[<Fact>]
let ``get-current-environment is reified`` () =
    evalSessionKernel [
        "(environment? (get-current-environment))", Bool true
    ]

[<Fact>]
let ``eval with explicit environment`` () =
    evalSessionKernel [
        "(define e (make-environment (get-current-environment)))", Inert
        "(eval e '(define x 99))", Inert
        "(eval e 'x)", Obj 99
    ]
