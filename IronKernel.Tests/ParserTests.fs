module IronKernel.Tests.ParserTests

open System
open Xunit
open IronKernel.Ast
open IronKernel.Errors
open IronKernel.Parser
open IronKernel.Source
open IronKernel.Tests.TestHelpers

let private choose (random: Random) values =
    List.item (random.Next(List.length values)) values

let rec private generatedDatumEqual left right =
    match left, right with
    | Obj (:? int as leftValue), Obj (:? int as rightValue) -> leftValue = rightValue
    | Bool leftValue, Bool rightValue -> leftValue = rightValue
    | Atom leftValue, Atom rightValue -> leftValue = rightValue
    | Keyword leftValue, Keyword rightValue -> leftValue = rightValue
    | List leftValues, List rightValues ->
        List.length leftValues = List.length rightValues
        && List.forall2 generatedDatumEqual leftValues rightValues
    | DottedList(leftHead, leftTail), DottedList(rightHead, rightTail) ->
        List.length leftHead = List.length rightHead
        && List.forall2 generatedDatumEqual leftHead rightHead
        && generatedDatumEqual leftTail rightTail
    | Vector leftValues, Vector rightValues ->
        Array.length leftValues = Array.length rightValues
        && Array.forall2 generatedDatumEqual leftValues rightValues
    | _ -> false

let rec private generateDatum (random: Random) depth =
    let generateLeaf () =
        match random.Next(4) with
        | 0 ->
            let value = random.Next(-100, 101)
            string value, Obj(value :> obj)
        | 1 ->
            let value = random.Next(2) = 0
            (if value then "#t" else "#f"), Bool value
        | 2 ->
            let name = choose random ["alpha"; "beta"; "gamma"; "delta"]
            name, Atom name
        | _ ->
            let name = choose random ["left"; "right"; "center"]
            $":{name}", Keyword name

    if depth = 0 then generateLeaf ()
    else
        match random.Next(5) with
        | 0 -> generateLeaf ()
        | 1 ->
            let values = [ for _ in 0..random.Next(3) -> generateDatum random (depth - 1) ]
            let source = values |> List.map fst |> String.concat " "
            $"({source})", List(List.map snd values)
        | 2 ->
            let values = [ for _ in 0..random.Next(3) -> generateDatum random (depth - 1) ]
            let source = values |> List.map fst |> String.concat "\n"
            $"[{source}]", Vector(values |> List.map snd |> List.toArray)
        | 3 ->
            let head = [ for _ in 0..random.Next(2) -> generateDatum random (depth - 1) ]
            let tailSource, tail = generateDatum random (depth - 1)
            let headSource = head |> List.map fst |> String.concat " "
            $"({headSource} & {tailSource})", DottedList(List.map snd head, tail)
        | _ ->
            let source, value = generateDatum random (depth - 1)
            $"'{source}", List [Atom "quote"; value]

[<Fact>]
let ``generated nested datums preserve structure and source spans`` () =
    let random = Random(0x5EED)
    for index in 0..255 do
        let source, expected = generateDatum random 5
        let sourceName = $"generated-{index}.ikr"
        match readLocatedExpr sourceName source with
        | Choice1Of2 error -> failwithf "generated datum failed to parse: %s\n%s" source (showError error)
        | Choice2Of2 located ->
            let actual = toLispVal located
            if not (generatedDatumEqual actual expected) then
                failwithf
                    "generated datum changed structure: %s\nactual: %s\nexpected: %s"
                    source
                    (showVal actual)
                    (showVal expected)
            Assert.Equal(sourceName, located.span.sourceName)
            Assert.Equal(0L, located.span.startPosition.offset)
            Assert.Equal(int64 source.Length, located.span.endPosition.offset)

[<Fact>]
let ``generated malformed datums return located parser errors`` () =
    let random = Random(0xBAD)
    for index in 0..127 do
        let inner, _ = generateDatum random 2
        let source =
            match index % 8 with
            | 0 -> $"({inner}"
            | 1 -> $"[{inner})"
            | 2 -> $"({inner} & )"
            | 3 -> $"({inner} & {inner} {inner})"
            | 4 -> $"'{inner})"
            | 5 -> "\"unterminated"
            | 6 -> "999999999999999999999999999999999999999999L"
            | _ -> "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFL"
        let sourceName = $"malformed-{index}.ikr"
        match readExprFromSource sourceName source with
        | Choice1Of2 (LocatedError(span, _, Parser _)) ->
            Assert.Equal(sourceName, span.sourceName)
        | Choice1Of2 error -> failwithf "malformed datum returned an unlocated error: %s\n%A" source error
        | Choice2Of2 value -> failwithf "malformed datum unexpectedly parsed: %s => %s" source (showVal value)

[<Fact>]
let ``parses integers and floats`` () =
    match parseOk "42" with
    | Obj (:? int as n) -> Assert.Equal(42, n)
    | v -> failwith (showVal v)
    match parseOk "-3" with
    | Obj (:? int as n) -> Assert.Equal(-3, n)
    | v -> failwith (showVal v)
    match parseOk "3.14" with
    | Obj (:? float as n) -> Assert.True(abs (n - 3.14) < 1e-9)
    | v -> failwith (showVal v)

[<Fact>]
let ``parses booleans inert and keywords`` () =
    assertEqv (parseOk "#t") (Bool true)
    assertEqv (parseOk "#f") (Bool false)
    assertEqv (parseOk "#inert") Inert
    match parseOk ":foo" with
    | Keyword "foo" -> ()
    | v -> failwith (showVal v)

[<Fact>]
let ``parses strings atoms and quote`` () =
    match parseOk "\"hi\\n\"" with
    | Obj (:? string as s) -> Assert.Equal("hi\n", s)
    | v -> failwith (showVal v)
    match parseOk "map" with
    | Atom "map" -> ()
    | v -> failwith (showVal v)
    match parseOk "'x" with
    | List [Atom "quote"; Atom "x"] -> ()
    | v -> failwith (showVal v)

[<Fact>]
let ``parses lists dotted lists and vectors`` () =
    assertEqv (parseOk "(a 1)") (List [Atom "a"; Obj 1])
    assertEqv (parseOk "()") (List [])
    match parseOk "(a & b)" with
    | DottedList ([Atom "a"], Atom "b") -> ()
    | v -> failwith (showVal v)
    match parseOk "[1 2 3]" with
    | Vector arr ->
        Assert.Equal(3, arr.Length)
        assertEqv arr.[0] (Obj 1)
        assertEqv arr.[2] (Obj 3)
    | v -> failwith (showVal v)

[<Fact>]
let ``parses nested combinations`` () =
    assertEqv
        (parseOk "(+ 1 (* 2 3))")
        (List [Atom "+"; Obj 1; List [Atom "*"; Obj 2; Obj 3]])

[<Fact>]
let ``parses deeply nested lists without exponential backtracking`` () =
    let depth = 250
    let source = String('(', depth) + "42" + String(')', depth)
    match readLocatedExpr "deep.ikr" source with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 value ->
        let mutable current = value
        for _ in 1..depth do
            match current.kind with
            | LList [nested] -> current <- nested
            | _ -> failwith "expected nested singleton list"
        match current.kind with
        | LLiteral (Obj (:? int as value)) -> Assert.Equal(42, value)
        | _ -> failwith "expected nested integer leaf"

[<Fact>]
let ``rejects unbalanced parentheses`` () =
    parseError "(+ 1 2"

[<Fact>]
let ``located parser records nested source spans without changing datum`` () =
    let source = "(+ 1\n  (* 2 3))"
    match readLocatedExpr "nested.ikr" source with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 outer ->
        assertEqv (toLispVal outer) (parseOk source)
        Assert.Equal("nested.ikr", outer.span.sourceName)
        Assert.Equal(1L, outer.span.startPosition.line)
        Assert.Equal(1L, outer.span.startPosition.column)
        Assert.Equal(2L, outer.span.endPosition.line)
        match outer.kind with
        | LList [_; _; inner] ->
            Assert.True(contains outer.span inner.span)
            Assert.Equal(2L, inner.span.startPosition.line)
            Assert.Equal(3L, inner.span.startPosition.column)
        | _ -> failwith "expected nested list"

[<Fact>]
let ``named parse diagnostics include position source and caret`` () =
    let source = "(+ 1\n  (* 2 3)"
    match readExprFromSource "broken.ikr" source with
    | Choice2Of2 value -> failwith ("unexpectedly parsed " + showVal value)
    | Choice1Of2 error ->
        let diagnostic = showError error
        Assert.Contains("broken.ikr:2:", diagnostic)
        Assert.Contains("(* 2 3)", diagnostic)
        Assert.Contains("^", diagnostic)
