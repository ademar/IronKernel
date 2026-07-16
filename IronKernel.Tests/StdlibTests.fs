module IronKernel.Tests.StdlibTests

open Xunit
open IronKernel.Ast
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``let let-star letrec`` () =
    evalSessionKernel [
        "(let ((x 2) (y 3)) (* x y))", Obj 6
        "(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))", Obj 35
        "(let* ((x 3) (y x)) (+ x y))", Obj 6
        "(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))", Obj 70
        "(letrec ((sum (lambda (x) (if (zero? x) 0 (+ x (sum (- x 1))))))) (sum 5))", Obj 15
    ]

[<Fact>]
let ``cond and logic`` () =
    evalSessionKernel [
        "(cond ((<= 1 0) 'no) ((eqv? 1 1) 'hello))", Atom "hello"
        "(and? #f (/ 1 0))", Bool false
        "(or? #t (/ 1 0))", Bool true
        "(and? #t #t #t)", Bool true
        "(or? #f #f #t)", Bool true
        "(not? #f)", Bool true
        "(not? #t)", Bool false
    ]

[<Fact>]
let ``defn and compose`` () =
    evalSessionKernel [
        "(defn (square x) (* x x))", Inert
        "(square 8)", Obj 64
        "((compose square square) 2)", Obj 16
    ]

[<Fact>]
let ``apply and list-star`` () =
    evalSessionKernel [
        "(apply + (list 1 2))", Obj 3
        "(list* 1 2 (list 3))", List [Obj 1; Obj 2; Obj 3]
    ]

[<Fact>]
let ``promises lazy force memoize`` () =
    withKernelAndPromises (fun env ->
        ignore (evalIn env "(define a (lazy (+ 2 5)))")
        assertEqv (evalIn env "(promise? a)") (Bool true)
        assertEqv (evalIn env "(force a)") (Obj 7)
        ignore (evalIn env "(define b (memoize (* 2 21)))")
        assertEqv (evalIn env "(promise? b)") (Bool true)
        assertEqv (evalIn env "(force b)") (Obj 42))

[<Fact>]
let ``caar cadr helpers`` () =
    evalSessionKernel [
        "(caar '((a b) (c d)))", Atom "a"
        "(cadr '(a b c))", Atom "b"
    ]
