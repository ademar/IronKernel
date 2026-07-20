module IronKernel.Tests.ConformanceTests

open Xunit

open IronKernel.Ast
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``interpreter and compiler agree on core evaluation`` () =
    assertParitySession
        [ "42"
          "(+ 20 22)"
          "(define x 4)"
          "(+ x 3)"
          "(if #t x missing)"
          "(define raw (vau (x) _ x))"
          "(raw (+ 1 2))"
          "((wrap (vau (x) _ x)) (+ 1 2))" ]

[<Fact>]
let ``interpreter and compiler report structured errors without corrupting state`` () =
    let cases =
        [ "(+ 1)", (function ContractViolation "+ expected 2 operands, found 1" -> true | _ -> false)
          "(car 42)", (function TypeMismatch ("pair", Obj _) -> true | _ -> false)
          "(if 1 2 3)", (function TypeMismatch ("bool", Obj _) -> true | _ -> false)
          "missing", (function UnboundVar (_, "missing") -> true | _ -> false)
          "((vau (x y) _ x) 1)", (function BadSpecialForm ("invalid arguments", List []) -> true | _ -> false) ]

    for mode in [Interpreted; Compiled] do
        let env = freshEnv ()
        match evalRaw mode env "(define stable 41)" with
        | Choice2Of2 Inert -> ()
        | result -> failwithf "%A failed to initialize the error session: %A" mode result

        for expression, isExpected in cases do
            match evalRaw mode env expression with
            | Choice1Of2 error when isExpected error -> ()
            | Choice1Of2 error ->
                failwithf "%A returned the wrong error for %s: %A" mode expression error
            | Choice2Of2 value ->
                failwithf "%A unexpectedly evaluated %s as %s" mode expression (showVal value)

        match evalRaw mode env "(+ stable 1)" with
        | Choice2Of2 (Obj (:? int as value)) -> Assert.Equal(42, value)
        | result -> failwithf "%A did not preserve state after errors: %A" mode result

[<Fact>]
let ``operative uses lexical scope and can explicitly evaluate in caller scope`` () =
    // Revised-1 Report sections 3.2 and 4.10.3.
    assertParityValueSession
        [ "(define list (wrap (vau xs _ xs)))"
          "(define x 1)"
          "(define lexical (vau () _ x))"
          "(define caller-value (vau (form) caller (eval caller form)))"
          "(define exercise (wrap (vau (x) _ (list (lexical) (caller-value x)))))"
          "(exercise 2)" ]
        (List [Obj (1 :> obj); Obj (2 :> obj)])

[<Fact>]
let ``environment lookup is depth first in parent order`` () =
    // Adapted from the KPLTS environment-concepts group (Kernel Report 3.2).
    assertParityValueSession
        [ "(load \"kernel.ikr\")"
          "(define a (bindings->environment (x 1) (y 2)))"
          "(define b (bindings->environment (x 3) (z 4)))"
          "(define c (make-environment a b))"
          "(list (remote-eval x c) (remote-eval y c) (remote-eval z c))" ]
        (List [Obj (1 :> obj); Obj (2 :> obj); Obj (4 :> obj)])

[<Fact>]
let ``wrapped combiner evaluates operands while operative receives syntax`` () =
    // Revised-1 Report sections 4.10.3 through 4.10.5.
    assertParityValueSession
        [ "(define list (wrap (vau xs _ xs)))"
          "(define raw (vau operands _ operands))"
          "(define cooked (wrap raw))"
          "(list (raw (+ 1 2)) (cooked (+ 1 2)))" ]
        (List
            [ List [List [Atom "+"; Obj (1 :> obj); Obj (2 :> obj)]]
              List [Obj (3 :> obj)] ])

[<Fact>]
let ``applicative operands evaluate from left to right`` () =
    assertParityValueSession
        [ "(load \"kernel.ikr\")"
          "(define trace (vector 0))"
          "(define record (lambda (x) (begin (vector-set! trace 0 (+ (* (vector-ref trace 0) 10) x)) x)))"
          "(+ (record 1) (record 2))"
          "(vector-ref trace 0)" ]
        (Obj (12 :> obj))

[<Fact>]
let ``applicatives dynamically evaluate expression-shaped combiners`` () =
    for combiner in [ "'target"; "'(if #t target missing)" ] do
        assertParityValueSession
            [ "(load \"kernel.ikr\")"
              "(define target (vau (x) _ x))"
              $"(define wrapped-target (wrap {combiner}))"
              "(wrapped-target (+ 1 2))" ]
            (Obj (3 :> obj))

[<Fact>]
let ``rebinding core names preserves combination semantics`` () =
    [ [ "(define if (vau xs e xs))"; "(if 1 2 3)" ]
      [ "(define quote (vau xs e xs))"; "(quote (+ 1 2))" ]
      [ "(define define (vau xs e xs))"; "(define x missing)" ]
      [ "(define vau (vau xs e xs))"; "(vau (x) e missing)" ]
      [ "(define eval (vau xs e xs))"; "(eval missing missing)" ]
      [ "(define sequence (vau xs e xs))"; "(sequence missing)" ]
      [ "(define begin (vau xs e xs))"; "(begin missing)" ]
      [ "(define reset (vau xs e xs))"; "(reset missing)" ] ]
    |> List.iter assertParitySession

[<Fact>]
let ``guarded if preserves continuation context`` () =
    assertParitySession
        [ "(load \"kernel.ikr\")"
          "(define saved #f)"
          "(+ 10 (if (call/cc (lambda (k) (begin (define saved k) #t))) 1 2))"
          "(saved #f)" ]
