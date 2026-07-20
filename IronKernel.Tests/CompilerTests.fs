module IronKernel.Tests.CompilerTests

open Xunit
open IronKernel.Ast
open IronKernel.Analyze
open IronKernel.Compiler
open IronKernel.Emit
open IronKernel.Eval
open IronKernel.Ir
open IronKernel.Errors
open IronKernel.SymbolTable
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``analyze combinations without privileging operator names`` () =
    match analyze (parseOk "(if #t 1 2)") with
    | COperate (CVar "if", [Bool true; Obj _; Obj _]) -> ()
    | other -> failwith (showCore other)

    match analyze (parseOk "(define x 1)") with
    | COperate (CVar "define", [Atom "x"; Obj _]) -> ()
    | other -> failwith (showCore other)

    match analyze (parseOk "(vau (x) e x)") with
    | COperate (CVar "vau", [List [Atom "x"]; Atom "e"; Atom "x"]) -> ()
    | other -> failwith (showCore other)

    match analyze (parseOk "(+ 1 2)") with
    | COperate (CVar "+", [Obj _; Obj _]) -> ()
    | other -> failwith (showCore other)

[<Fact>]
let ``environment-aware analysis guards primitive forms`` () =
    let env = freshEnv ()

    match analyzeGuarded env (parseOk "(if #t 1 2)") with
    | CGuarded (guard, CIntrinsicOperate (PrimitiveIf, _), COperate (CVar "if", _)) ->
        Assert.True(bindingGuardMatches env guard)
    | other -> failwith (showCore other)

    match analyzeGuarded env (parseOk "(define answer 42)") with
    | CGuarded (guard, CIntrinsicOperate (PrimitiveDefine, _), COperate (CVar "define", _)) ->
        Assert.True(bindingGuardMatches env guard)
    | other -> failwith (showCore other)

[<Fact>]
let ``compiled guard deoptimizes after primitive rebinding`` () =
    let env = freshEnv ()
    let compiled = compileLispValGuarded env (parseOk "(if #t 1 2)")

    match compiled.Invoke(env, newContinuation env) with
    | Choice2Of2 (Obj (:? int as value)) -> Assert.Equal(1, value)
    | result -> failwithf "unexpected guarded result: %A" result

    ignore (evalIn env "(define if (vau operands caller operands))")

    match compiled.Invoke(env, newContinuation env) with
    | Choice2Of2 (List [Bool true; Obj (:? int as one); Obj (:? int as two)]) ->
        Assert.Equal(1, one)
        Assert.Equal(2, two)
    | result -> failwithf "guard did not fall back after rebind: %A" result

[<Fact>]
let ``wrapped if is not analyzed as a guarded intrinsic`` () =
    let env = freshEnv ()
    ignore (evalIn env "(define if (wrap if))")

    match analyzeGuarded env (parseOk "(if #t 1 2)") with
    | COperate (CVar "if", [Bool true; Obj _; Obj _]) -> ()
    | CGuarded _ as other ->
        failwithf "wrapped if must not use guarded fast path: %s" (showCore other)
    | other -> failwith (showCore other)

[<Fact>]
let ``compiled wrapped if evaluates unused branch like the interpreter`` () =
    // Operative if skips the unused branch; applicative (wrap if) evaluates both.
    // A buggy guard fast path would return 1 while the interpreter errors.
    assertParitySession
        [ "(define if (wrap if))"
          "(if #t 1 missing-binding)" ]

[<Fact>]
let ``compiled guard detects a new shadowing binding`` () =
    let parent = freshEnv ()
    let child = newEnv [parent]
    let compiled = compileLispValGuarded child (parseOk "(if #t 1 2)")

    ignore (evalIn child "(define if (vau operands caller operands))")

    match compiled.Invoke(child, newContinuation child) with
    | Choice2Of2 (List [Bool true; Obj (:? int as one); Obj (:? int as two)]) ->
        Assert.Equal(1, one)
        Assert.Equal(2, two)
    | result -> failwithf "guard did not detect shadowing: %A" result

[<Fact>]
let ``compiled guard deoptimizes after parent binding mutation`` () =
    let parent = freshEnv ()
    let child = newEnv [parent]
    let compiled = compileLispValGuarded child (parseOk "(if #t 1 2)")

    ignore (evalIn parent "(define if (vau operands caller operands))")

    match compiled.Invoke(child, newContinuation child) with
    | Choice2Of2 (List [Bool true; Obj (:? int as one); Obj (:? int as two)]) ->
        Assert.Equal(1, one)
        Assert.Equal(2, two)
    | result -> failwithf "guard did not detect parent mutation: %A" result

[<Fact>]
let ``toLispVal roundtrips analyzed if`` () =
    let original = parseOk "(if #t 1 2)"
    assertEqv (toLispVal (analyze original)) original

[<Fact>]
let ``compiled arithmetic and define`` () =
    let env = freshEnv ()
    let cont = newContinuation env
    match evalCompiled env cont (parseOk "(+ 10 32)") with
    | Choice2Of2 (Obj n) -> Assert.Equal(42, n :?> int)
    | Choice2Of2 v -> failwith (showVal v)
    | Choice1Of2 e -> failwith (showError e)

    match evalCompiled env cont (parseOk "(define x 5)") with
    | Choice2Of2 Inert -> ()
    | Choice2Of2 v -> failwith (showVal v)
    | Choice1Of2 e -> failwith (showError e)

    match evalCompiled env cont (parseOk "(+ x 1)") with
    | Choice2Of2 (Obj n) -> Assert.Equal(6, n :?> int)
    | Choice2Of2 v -> failwith (showVal v)
    | Choice1Of2 e -> failwith (showError e)

[<Fact>]
let ``compiled operative path keeps operands unevaluated`` () =
    let env = freshEnv ()
    ignore (evalIn env "(define q (vau (x) _ x))")
    match evalCompiled env (newContinuation env) (parseOk "(q (+ 1 2))") with
    | Choice2Of2 (List [Atom "+"; Obj a; Obj b]) ->
        Assert.Equal(1, a :?> int)
        Assert.Equal(2, b :?> int)
    | Choice2Of2 v -> failwith (showVal v)
    | Choice1Of2 e -> failwith (showError e)

[<Fact>]
let ``ikc emit and load`` () =
    let dir = System.IO.Path.GetTempPath()
    let src = System.IO.Path.Combine(dir, "ik-compiler-test.ikr")
    let outp = System.IO.Path.Combine(dir, "ik-compiler-test.ikc")
    System.IO.File.WriteAllText(src, "(+ 20 22)")
    match compileFileToPackage src outp with
    | Choice1Of2 e -> failwith (showError e)
    | Choice2Of2 path ->
        Assert.True(System.IO.File.Exists path)
        match loadIkc path with
        | Choice2Of2 (Obj n) -> Assert.Equal(42, n :?> int)
        | Choice2Of2 v -> failwith (showVal v)
        | Choice1Of2 e -> failwith (showError e)

[<Fact>]
let ``analyzeAndCompile multiple top-level forms`` () =
    match analyzeAndCompile "(define a 1) (+ a 2)" with
    | Choice2Of2 [_; _] -> ()
    | Choice2Of2 xs -> failwithf "expected 2 forms, got %d" (List.length xs)
    | Choice1Of2 e -> failwith (showError e)
