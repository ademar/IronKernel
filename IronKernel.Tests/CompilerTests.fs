module IronKernel.Tests.CompilerTests

open Xunit
open IronKernel.Ast
open IronKernel.Analyze
open IronKernel.Compiler
open IronKernel.Emit
open IronKernel.Eval
open IronKernel.Ir
open IronKernel.Errors
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``analyze if define vau and app`` () =
    match analyze (parseOk "(if #t 1 2)") with
    | CIf (CLit (Bool true), CLit (Obj _), CLit (Obj _)) -> ()
    | other -> failwith (showCore other)

    match analyze (parseOk "(define x 1)") with
    | CDefine (CVar "x", CLit (Obj _)) -> ()
    | other -> failwith (showCore other)

    match analyze (parseOk "(vau (x) e x)") with
    | CVau _ -> ()
    | other -> failwith (showCore other)

    match analyze (parseOk "(+ 1 2)") with
    | CApp (CVar "+", [CLit _; CLit _]) -> ()
    | other -> failwith (showCore other)

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
    let src = System.IO.Path.Combine(dir, "ik-compiler-test.scm")
    let outp = System.IO.Path.Combine(dir, "ik-compiler-test.dll")
    System.IO.File.WriteAllText(src, "(+ 20 22)")
    match compileFileToAssembly src outp with
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
