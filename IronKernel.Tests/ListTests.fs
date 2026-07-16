module IronKernel.Tests.ListTests

open Xunit
open IronKernel.Ast
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``cons car cdr`` () =
    [
        "(cons 1 ())", List [Obj 1]
        "(cons 1 (cons 2 ()))", List [Obj 1; Obj 2]
        "(car (cons 1 (cons 2 ())))", Obj 1
        "(cdr (cons 1 (cons 2 ())))", List [Obj 2]
        "(null? ())", Bool true
        "(null? (cons 1 ()))", Bool false
        "(pair? (cons 1 2))", Bool true
        "(pair? ())", Bool false
    ] |> evalSession

[<Fact>]
let ``dotted pairs`` () =
    [
        "(car (cons 1 2))", Obj 1
        "(cdr (cons 1 2))", Obj 2
    ] |> evalSession

[<Fact>]
let ``list library helpers`` () =
    [
        "(list 1 2 3)", List [Obj 1; Obj 2; Obj 3]
        "(length (list 1 2 3 4))", Obj 4
        "(map (lambda (x) (+ x 1)) (list 1 2 3))", List [Obj 2; Obj 3; Obj 4]
    ] |> evalSessionKernel

[<Fact>]
let ``quote preserves structure`` () =
    [
        "'(a b c)", List [Atom "a"; Atom "b"; Atom "c"]
        "(car '(x y))", Atom "x"
    ] |> evalSessionKernel
