module IronKernel.Tests.ContractTests

open Xunit

open IronKernel.Ast
open IronKernel.Analyze
open IronKernel.Compiler
open IronKernel.Contracts
open IronKernel.Ir
open IronKernel.PartialEval
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``operative contract validates raw operand syntax`` () =
    withKernel (fun env ->
        ignore (evalIn env "(define raw (vau operands _ operands))")
        ignore (evalIn env "(contract raw operative (any) any pure #t)")
        assertEval env "(raw (+ 1 2))" (List [List [Atom "+"; Obj (1 :> obj); Obj (2 :> obj)]])

        match evalRaw Interpreted env "(raw 1 2)" with
        | Choice1Of2 (ContractViolation message) -> Assert.Contains("expected 1 operands", message)
        | result -> failwithf "unexpected operative contract result: %A" result)

[<Fact>]
let ``applicative contract validates argument and result values`` () =
    withKernel (fun env ->
        ignore (evalIn env "(define double (lambda (x) (+ x x)))")
        ignore (evalIn env "(contract double applicative (number) number pure #t)")
        assertEval env "(double 21)" (Obj (42 :> obj))
        assertEval
            env
            "(contract-of double)"
            (List
                [ Atom "applicative"
                  List [Atom "number"]
                  Atom "number"
                  Atom "pure"
                  Bool true
                  Atom "asserted" ])

        match evalRaw Interpreted env "(double \"wrong\")" with
        | Choice1Of2 (ContractViolation message) -> Assert.Contains("operand 1 expected number", message)
        | result -> failwithf "unexpected argument contract result: %A" result

        ignore (evalIn env "(define bad (lambda (x) \"wrong\"))")
        ignore (evalIn env "(contract bad applicative (number) number effectful #f)")
        match evalRaw Interpreted env "(bad 1)" with
        | Choice1Of2 (ContractViolation message) -> Assert.Contains("result expected number", message)
        | result -> failwithf "unexpected result contract outcome: %A" result)

[<Fact>]
let ``contract mode must match the combiner`` () =
    withKernel (fun env ->
        ignore (evalIn env "(define raw (vau operands _ operands))")
        match evalRaw Interpreted env "(contract raw applicative (any) any pure #t)" with
        | Choice1Of2 (ContractViolation message) -> Assert.Contains("mode does not match", message)
        | result -> failwithf "unexpected mode mismatch result: %A" result)

[<Fact>]
let ``wrap preserves contract metadata with eager argument policy`` () =
    evalSessionKernel
        [ "(define raw (vau operands _ operands))", Inert
          "(contract raw operative (number) list pure #t)", Inert
          "(define eager (wrap raw))", Inert
          "(eager (+ 1 2))", List [Obj (3 :> obj)] ]

[<Fact>]
let ``certified primitive literals are partially evaluated behind a guard`` () =
    let env = freshEnv ()
    let fallback = analyzeGuarded env (parseOk "(+ 1 2)")
    match partialEvaluate env fallback with
    | CContractFold (guard, Obj (:? int as value), COperate (CVar "+", _)) ->
        Assert.Equal(3, value)
        Assert.True(contractGuardMatches env guard)
    | other -> failwith (showCore other)

[<Fact>]
let ``contract fold falls back after rebinding`` () =
    let env = freshEnv ()
    let compiled = compileLispValGuarded env (parseOk "(+ 1 2)")

    ignore (evalIn env "(define + (vau operands _ operands))")

    match compiled.Invoke(env, newContinuation env) with
    | Choice2Of2 (List [Obj (:? int as one); Obj (:? int as two)]) ->
        Assert.Equal(1, one)
        Assert.Equal(2, two)
    | result -> failwithf "partial fold did not fall back: %A" result

[<Fact>]
let ``asserted contracts are never trusted for compile-time execution`` () =
    withKernel (fun env ->
        ignore (evalIn env "(define answer (lambda (x) 42))")
        ignore (evalIn env "(contract answer applicative (number) number pure #t)")
        match analyzeGuarded env (parseOk "(answer 1)") |> partialEvaluate env with
        | COperate (CVar "answer", _) -> ()
        | other -> failwithf "asserted contract was folded: %s" (showCore other))

[<Fact>]
let ``contracts preserve interpreter compiler parity`` () =
    assertParitySession
        [ "(load \"kernel.scm\")"
          "(define double (lambda (x) (+ x x)))"
          "(contract double applicative (number) number pure #t)"
          "(double 21)"
          "(+ 20 22)" ]
