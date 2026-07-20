module IronKernel.Tests.ListTests

open Xunit
open IronKernel.Ast
open IronKernel.Runtime
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
let ``structural equality compares lists and dotted tails`` () =
    let assertResult expected left right =
        match eqv' [left; right] with
        | Choice2Of2 (Bool actual) -> Assert.Equal(expected, actual)
        | result -> failwithf "unexpected equality result: %A" result

    assertResult true (List [List [Atom "a"]]) (List [List [Atom "a"]])
    assertResult false (List [Atom "a"]) (List [Atom "a"; Atom "b"])
    assertResult false (DottedList ([Atom "a"], Atom "b")) (DottedList ([Atom "a"], Atom "c"))
    assertResult false (DottedList ([Atom "a"], Atom "b")) (List [Atom "a"; Atom "b"])

    match eqv' [Atom "a"] with
    | Choice1Of2 (NumArgs (2, [_])) -> ()
    | result -> failwithf "unexpected equality arity result: %A" result

[<Fact>]
let ``structural equality handles deeply nested lists`` () =
    let mutable left = Atom "leaf"
    let mutable right = Atom "leaf"
    for _ in 1..100000 do
        left <- List [left]
        right <- List [right]

    match eqv' [left; right] with
    | Choice2Of2 (Bool true) -> ()
    | result -> failwithf "unexpected deep equality result: %A" result

[<Fact>]
let ``showVal handles deeply nested lists`` () =
    let depth = 100_000
    let mutable value = Atom "leaf"
    for _ in 1..depth do
        value <- List [value]

    let rendered = showVal value
    Assert.Equal(depth * 2 + 4, rendered.Length)
    Assert.StartsWith(System.String('(', depth), rendered)
    Assert.EndsWith(System.String(')', depth), rendered)
    Assert.Equal("leaf", rendered.Substring(depth, 4))

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
