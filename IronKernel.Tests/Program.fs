﻿module IronKernel.Tests

open Xunit
open FsUnit.Xunit

open IronKernel.Ast
open IronKernel.Eval
open IronKernel.Runtime
open IronKernel.Repl
open IronKernel.Choice
open IronKernel.Errors

let eval env s =
    evalString env (newContinuation env) s

let evalSession lines =
    let env = primitiveBindings
    let validate x b =
         either { let! (Bool f) = eqv' [x;b] 
                  return f }
    let test a b = 
        let x = eval env a
        match validate x b with 
        | Choice1Of2(p) -> Assert.True(false,"expecting 'Bool' got '" + showError p + "'")
        | Choice2Of2(y) -> Assert.True(y, "expecting '" + (showVal b) + "' got '" + (showVal x) + "'")


    lines |> List.iter (fun (x,y) -> test x y)

[<Fact>] 
let ``arithmetic 101`` () =
    [
         "(+ 2 2)", Obj 4 ;
         "(+ 2 (* 4 3))" , Obj 14;
         "(+ 2 (* 4 3) (- 5 7))" , Obj 12;
         "(define x 3)", Inert;
         "(+ x 2)", Obj 5;
         "(eqv? 1 3)", Bool false;
         "(eqv? 3 3)", Bool true;
    ] |> evalSession 

[<Fact>] 
let ``lambda, define and map`` () =
    [
        "(load \"kernel.scm\")", Inert ;
        "(define (double x) (* 2 x))", Inert ;
        "(map double (list 1 2 3 4))", List [Obj 2; Obj 4; Obj 6; Obj 8]
    ] |> evalSession 

[<Fact>] 
let ``let`` () =
    [
        "(load \"kernel.scm\")", Inert ;
        "(let ((x 2) (y 3)) (* x y))", Obj 6 ;
        "(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))" , Obj 35;
    ] |> evalSession

[<Fact>] 
let ``let*`` () =
    [
        "(load \"kernel.scm\")", Inert ;
        "(let* ((x 3) (y x)) (+ x y))", Obj 6 ;
        "(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))", Obj 70 ;
    ] |> evalSession

[<Fact>] 
let ``letrec`` () =
    [
        "(load \"kernel.scm\")", Inert ;
        "(letrec ((sum (lambda (x) (if (zero? x) 0 (+ x (sum (- x 1))))))) (sum 5))", Obj 6 ;
    ] |> evalSession