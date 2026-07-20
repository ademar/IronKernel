module IronKernel.Tests.VauTests

open Xunit
open IronKernel.Ast
open IronKernel.Errors
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``vau receives unevaluated operands`` () =
    [
        "(define quote-one (vau (x) _ x))", Inert
        "(quote-one (+ 1 2))", List [Atom "+"; Obj 1; Obj 2]
    ] |> evalSession

[<Fact>]
let ``vau rejects operands that do not match formals`` () =
    let cases =
        [ "((vau (x y) _ 42) 1)", List []
          "((vau (x y) _ 42) 1 2 3)", List [Obj 3]
          "((vau ((x y)) _ 42) (1))", List [] ]

    for expression, badForm in cases do
        for mode in [Interpreted; Compiled] do
            let env = freshEnv ()
            match evalRaw mode env expression with
            | Choice1Of2 (BadSpecialForm ("invalid arguments", actual)) ->
                assertEqv actual badForm
            | Choice1Of2 error ->
                failwithf "%A returned the wrong error: %s" mode (showError error)
            | Choice2Of2 value ->
                failwithf "%A accepted malformed operands: %s" mode (showVal value)

[<Fact>]
let ``vau dotted formals collect remaining operands`` () =
    assertParityValueSession
        [ "((vau (x & rest) _ rest) 1 2 3)" ]
        (List [Obj 2; Obj 3])

[<Fact>]
let ``vau can eval in caller environment`` () =
    [
        "(define force-it (vau (x) e (eval e x)))", Inert
        "(define n 21)", Inert
        "(force-it (+ n n))", Obj 42
    ] |> evalSession

[<Fact>]
let ``operative unless short-circuits`` () =
    evalSessionKernel [
        """(define unless
             (vau (test & body) env
               (if (eval env test) #inert (eval env (cons sequence body)))))""", Inert
        "(define ran #f)", Inert
        "(unless #t (define ran #t))", Inert
        "ran", Bool false
        "(unless #f (define ran #t))", Inert
        "ran", Bool true
    ]

[<Fact>]
let ``wrap and unwrap`` () =
    [
        "(define op (vau (x) _ x))", Inert
        "(define ap (wrap op))", Inert
        "(ap (+ 1 2))", Obj 3
        "((unwrap ap) (+ 1 2))", List [Atom "+"; Obj 1; Obj 2]
    ] |> evalSession

[<Fact>]
let ``lambda is sugar over vau`` () =
    evalSessionKernel [
        "((lambda (x y) (+ x y)) 3 4)", Obj 7
        "((λ (x) (* x x)) 6)", Obj 36
    ]
