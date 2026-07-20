module IronKernel.Tests.CompilerTests

open System
open System.IO
open Xunit
open IronKernel.Ast
open IronKernel.Analyze
open IronKernel.Compiler
open IronKernel.Emit
open IronKernel.Eval
open IronKernel.Ir
open IronKernel.Errors
open IronKernel.Parser
open IronKernel.PartialEval
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
let ``analysis handles deeply nested operator positions`` () =
    let depth = 100_000
    let mutable form = Atom "deep-operator"
    for _ in 1..depth do
        form <- List [form]

    let assertShape analyzed =
        let mutable current = analyzed
        for _ in 1..depth do
            match current with
            | COperate(operator, []) -> current <- operator
            | other -> failwithf "expected nested operator, got %s" (showCore other)
        match current with
        | CVar "deep-operator" -> ()
        | other -> failwithf "expected operator leaf, got %s" (showCore other)

    assertShape (analyze form)
    assertShape (analyzeGuarded (freshEnv ()) form)

[<Fact>]
let ``located analysis handles deeply nested operator positions`` () =
    let depth = 100_000
    let span =
        { sourceName = "deep.ikr"
          startPosition = { offset = 0L; line = 1L; column = 1L }
          endPosition = { offset = 0L; line = 1L; column = 1L } }
    let mutable located : IronKernel.Source.LocatedValue =
        { kind = IronKernel.Source.LAtom "deep-operator"; span = span }
    for _ in 1..depth do
        located <- { kind = IronKernel.Source.LList [located]; span = span }

    let mutable analyzed = analyzeLocatedGuarded (freshEnv ()) "deep-operator" located
    for _ in 1..depth do
        match analyzed with
        | CLocated(_, Some "deep-operator", COperate(operator, [])) -> analyzed <- operator
        | other -> failwithf "expected located operator, got %s" (showCore other)
    match analyzed with
    | CLocated(_, Some "deep-operator", CVar "deep-operator") -> ()
    | other -> failwithf "expected located operator leaf, got %s" (showCore other)

[<Fact>]
let ``compilation handles deeply nested operator positions`` () =
    let depth = 100_000
    let mutable expression = CVar "deep-operator"
    for _ in 1..depth do
        expression <- COperate(expression, [])

    let compiled = compileToFunc expression
    Assert.NotNull(compiled)

[<Fact>]
let ``compilation handles deeply nested definitions`` () =
    let depth = 100_000
    let mutable expression = CLit Inert
    for _ in 1..depth do
        expression <- CDefine(CVar "deep-binding", expression)

    let compiled = compileToFunc expression
    Assert.NotNull(compiled)

[<Fact>]
let ``compilation handles deeply nested located operators`` () =
    let depth = 100_000
    let span =
        { sourceName = "deep.ikr"
          startPosition = { offset = 0L; line = 1L; column = 1L }
          endPosition = { offset = 0L; line = 1L; column = 1L } }
    let mutable expression = CVar "deep-operator"
    for _ in 1..depth do
        expression <- CLocated(span, Some "deep-operator", COperate(expression, []))

    let compiled = compileToFunc expression
    Assert.NotNull(compiled)

[<Fact>]
let ``partial evaluation handles deeply nested locations`` () =
    let depth = 100_000
    let env = freshEnv ()
    let span =
        { sourceName = "deep.ikr"
          startPosition = { offset = 0L; line = 1L; column = 1L }
          endPosition = { offset = 0L; line = 1L; column = 1L } }
    let mutable expression = COperate(CVar "+", [Obj(1 :> obj); Obj(2 :> obj)])
    for _ in 1..depth do
        expression <- CLocated(span, Some "+ 1 2", expression)

    let mutable optimized = partialEvaluate env expression
    for _ in 1..depth do
        match optimized with
        | CLocated(_, _, inner) -> optimized <- inner
        | other -> failwithf "expected located expression, got %s" (showCore other)
    match optimized with
    | CContractFold(_, Obj (:? int as value), _) -> Assert.Equal(3, value)
    | other -> failwithf "expected folded leaf, got %s" (showCore other)

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
let ``compiled define guard deoptimizes after binding changes`` () =
    let replacement = "(vau operands caller operands)"
    let rhsExpressions =
        [ "42"
          "(+ 20 22)"
          "(if #t 42 missing)"
          "((lambda (value) (+ value 1)) 41)" ]

    let assertScenario label createEnvironments mutate =
        for index, rhs in List.indexed rhsExpressions do
            let interpretedEnv, compiledEnv = createEnvironments ()
            let source = $"(define generated{index} {rhs})"
            let compiled = compileLispValGuarded compiledEnv (parseOk source)

            mutate interpretedEnv
            mutate compiledEnv

            let interpreted = evalRaw Interpreted interpretedEnv source |> observe
            let compiledResult = compiled.Invoke(compiledEnv, newContinuation compiledEnv) |> observe
            if interpreted <> compiledResult then
                failwithf
                    "%s define guard mismatch for %s\ninterpreted: %A\ncompiled: %A"
                    label
                    source
                    interpreted
                    compiledResult

    assertScenario
        "local mutation"
        (fun () -> freshEnv (), freshEnv ())
        (fun env -> ignore (evalIn env $"(define define {replacement})"))

    assertScenario
        "parent mutation"
        (fun () ->
            let interpretedParent = freshEnv ()
            let compiledParent = freshEnv ()
            newEnv [interpretedParent], newEnv [compiledParent])
        (fun env ->
            match env with
            | Environment record -> ignore (evalIn (List.head record.parents) $"(define define {replacement})")
            | _ -> failwith "expected child environment")

    assertScenario
        "child shadowing"
        (fun () ->
            let interpretedParent = freshEnv ()
            let compiledParent = freshEnv ()
            newEnv [interpretedParent], newEnv [compiledParent])
        (fun env ->
            let replacementValue = evalIn env replacement
            ignore (defineVar env "define" replacementValue))

[<Fact>]
let ``located compiled guard retains the generic fallback`` () =
    let env = freshEnv ()
    let compiled =
        match compileSourceLocated env "guarded-if.ikr" "(if #t 1 2)" with
        | Choice2Of2 [form] -> form.func
        | result -> failwithf "unexpected located compilation result: %A" result

    ignore (evalIn env "(define if (vau operands caller operands))")

    match compiled.Invoke(env, newContinuation env) with
    | Choice2Of2 (List [Bool true; Obj (:? int as one); Obj (:? int as two)]) ->
        Assert.Equal(1, one)
        Assert.Equal(2, two)
    | result -> failwithf "located guard did not use its generic fallback: %A" result

[<Fact>]
let ``toLispVal roundtrips analyzed if`` () =
    let original = parseOk "(if #t 1 2)"
    assertEqv (toLispVal (analyze original)) original

[<Fact>]
let ``toLispVal handles deeply nested core expressions`` () =
    let depth = 100_000
    let mutable expression = CLit Inert
    for _ in 1..depth do
        expression <- CDefine(CVar "deep-binding", expression)

    let mutable value = toLispVal expression
    for _ in 1..depth do
        match value with
        | List [Atom "define"; Atom "deep-binding"; rhs] -> value <- rhs
        | other -> failwithf "expected nested definition, got %s" (showVal other)
    match value with
    | Inert -> ()
    | other -> failwithf "expected inert leaf, got %s" (showVal other)

[<Fact>]
let ``expression tree helpers compile their live core shapes`` () =
    let env = freshEnv ()
    let invoke expression =
        compileToFunc expression
        |> fun compiled -> compiled.Invoke(env, newContinuation env)

    match invoke (CLit (Obj 1)) with
    | Choice2Of2 (Obj (:? int as value)) -> Assert.Equal(1, value)
    | result -> failwithf "unexpected literal result: %A" result

    match invoke (CVar "+") with
    | Choice2Of2 (Applicative _) -> ()
    | result -> failwithf "unexpected lookup result: %A" result

    match invoke (CIf(CLit (Bool true), CLit (Obj 2), CLit (Obj 3))) with
    | Choice2Of2 (Obj (:? int as value)) -> Assert.Equal(2, value)
    | result -> failwithf "unexpected conditional result: %A" result

    match invoke (CApp(CVar "+", [CLit (Obj 1); CLit (Obj 2)])) with
    | Choice2Of2 (Obj (:? int as value)) -> Assert.Equal(3, value)
    | result -> failwithf "unexpected application result: %A" result

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
let ``compiled named operator lookup preserves error parity`` () =
    assertParitySession ["(missing-combiner 42)"]

[<Fact>]
let ``compiled computed operator retains general evaluation`` () =
    assertParitySession ["((if #t + -) 20 22)"]

[<Fact>]
let ``generated programs preserve interpreter compiler and package parity`` () =
    let rec generateNumber depth seed =
        if depth = 0 then string ((seed % 19) - 9)
        else
            match seed % 5 with
            | 0 -> $"(+ {generateNumber (depth - 1) (seed + 1)} {generateNumber (depth - 1) (seed + 2)})"
            | 1 -> $"(- {generateNumber (depth - 1) (seed + 3)} {generateNumber (depth - 1) (seed + 4)})"
            | 2 -> $"(* {generateNumber (depth - 1) (seed + 5)} {generateNumber (depth - 1) (seed + 6)})"
            | 3 ->
                $"(if (< {generateNumber (depth - 1) (seed + 7)} {generateNumber (depth - 1) (seed + 8)}) {generateNumber (depth - 1) (seed + 9)} {generateNumber (depth - 1) (seed + 10)})"
            | _ ->
                $"((lambda (value) (+ value {generateNumber (depth - 1) (seed + 11)})) {generateNumber (depth - 1) (seed + 12)})"

    let names = [ for index in 0..63 -> $"generated{index}" ]
    let definitions =
        names
        |> List.mapi (fun index name -> $"(define {name} {generateNumber 3 (index * 13 + 1)})")
    let listArguments = String.concat " " names
    let source = String.concat "\n" (definitions @ [$"(list {listArguments})"])

    let runInterpreted () =
        match bootstrapEnv (), readExprListFromSource "generated-artifact.ikr" source with
        | Choice1Of2 error, _
        | _, Choice1Of2 error -> Choice1Of2 error
        | Choice2Of2 env, Choice2Of2 forms ->
            let mutable result = Choice2Of2 Inert
            for form in forms do
                match result with
                | Choice1Of2 _ -> ()
                | Choice2Of2 _ -> result <- eval env (newContinuation env) form
            result

    let runCompiled () =
        match bootstrapEnv () with
        | Choice1Of2 error -> Choice1Of2 error
        | Choice2Of2 env -> runSource env "generated-artifact.ikr" source

    let directory = Path.Combine(Path.GetTempPath(), $"ironkernel-artifact-{Guid.NewGuid():N}")
    Directory.CreateDirectory(directory) |> ignore
    let script = Path.Combine(directory, "generated.ikr")
    let package = Path.Combine(directory, "generated.ikc")
    try
        File.WriteAllText(script, source)
        match compileFileToPackage script package with
        | Choice1Of2 error -> failwithf "generated package compilation failed: %s" (showError error)
        | Choice2Of2 _ -> ()

        let interpreted = runInterpreted () |> observe
        let compiled = runCompiled () |> observe
        let packaged = loadIkc package |> observe
        Assert.Equal(interpreted, compiled)
        Assert.Equal(interpreted, packaged)
    finally
        Directory.Delete(directory, true)

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
