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
