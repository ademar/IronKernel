module IronKernel.Tests.EnvironmentTests

open Xunit
open IronKernel.Ast
open IronKernel.SymbolTable
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

[<Fact>]
let ``binding guards track identity and version`` () =
    let env = freshEnv ()
    let guard =
        tryCreateBindingGuard env "if" PrimitiveIf
        |> Option.defaultWith (fun () -> failwith "missing primitive if guard")

    Assert.True(bindingGuardMatches env guard)
    ignore (defineVar env "unrelated" (Obj (1 :> obj)))
    Assert.True(bindingGuardMatches env guard)

    ignore (evalIn env "(define if (vau operands caller operands))")
    Assert.False(bindingGuardMatches env guard)

[<Fact>]
let ``set updates only the first binding in depth-first order`` () =
    let first = newEnv []
    let second = newEnv []
    ignore (defineVar first "x" (Obj (1 :> obj)))
    ignore (defineVar second "x" (Obj (2 :> obj)))
    let child = newEnv [first; second]

    ignore (setVar child "x" (Obj (9 :> obj)))

    match getVar first "x", getVar second "x" with
    | Choice2Of2 (Obj (:? int as firstValue)), Choice2Of2 (Obj (:? int as secondValue)) ->
        Assert.Equal(9, firstValue)
        Assert.Equal(2, secondValue)
    | values -> failwithf "unexpected parent values: %A" values
