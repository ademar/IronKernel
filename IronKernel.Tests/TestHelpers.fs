module IronKernel.Tests.TestHelpers

open System.IO

open IronKernel.Ast
open IronKernel.Eval
open IronKernel.Runtime
open IronKernel.Repl
open IronKernel.Choice
open IronKernel.Errors
open IronKernel.Parser

/// Isolated primitive environment (does not share REPL state).
let freshEnv () = makePrimitiveBindings ()

let evalIn env expr =
    evalString env (newContinuation env) expr

let expect eqvResult =
    match eqvResult with
    | Choice1Of2 e -> failwith (showError e)
    | Choice2Of2 (Bool true) -> ()
    | Choice2Of2 (Bool false) -> failwith "values are not eqv?"
    | Choice2Of2 other -> failwith ("expected Bool, got " + showVal other)

let assertEqv actual expected =
    expect (eqv' [actual; expected])

let assertEval env expr expected =
    assertEqv (evalIn env expr) expected

let assertEvalString expr expected =
    assertEval (freshEnv ()) expr expected

/// Run a session of (expr, expected) pairs in one fresh environment.
let evalSession (lines: (string * LispVal) list) =
    let env = freshEnv ()
    lines
    |> List.iter (fun (expr, expected) ->
        let actual = evalIn env expr
        match eqv' [actual; expected] with
        | Choice2Of2 (Bool true) -> ()
        | Choice2Of2 (Bool false) ->
            failwithf "expecting '%s' got '%s' for: %s" (showVal expected) (showVal actual) expr
        | Choice1Of2 e ->
            failwithf "eqv? failed (%s) for: %s => %s" (showError e) expr (showVal actual)
        | Choice2Of2 other ->
            failwithf "eqv? returned %s for: %s" (showVal other) expr)

/// Fresh env with kernel.scm loaded.
let withKernel (body: LispVal -> unit) =
    let env = freshEnv ()
    match evalIn env "(load \"kernel.scm\")" with
    | Status s -> failwith ("failed to load kernel.scm: " + s)
    | _ -> body env

/// Fresh env with kernel.scm + promises.scm.
let withKernelAndPromises (body: LispVal -> unit) =
    withKernel (fun env ->
        match evalIn env "(load \"promises.scm\")" with
        | Status s -> failwith ("failed to load promises.scm: " + s)
        | _ -> body env)

let evalSessionKernel (lines: (string * LispVal) list) =
    withKernel (fun env ->
        lines
        |> List.iter (fun (expr, expected) ->
            let actual = evalIn env expr
            match eqv' [actual; expected] with
            | Choice2Of2 (Bool true) -> ()
            | Choice2Of2 (Bool false) ->
                failwithf "expecting '%s' got '%s' for: %s" (showVal expected) (showVal actual) expr
            | Choice1Of2 e ->
                failwithf "eqv? failed (%s) for: %s => %s" (showError e) expr (showVal actual)
            | Choice2Of2 other ->
                failwithf "eqv? returned %s for: %s" (showVal other) expr))

let parseOk input =
    match readExpr input with
    | Choice2Of2 v -> v
    | Choice1Of2 e -> failwith ("parse failed: " + showError e)

let parseError input =
    match readExpr input with
    | Choice1Of2 _ -> ()
    | Choice2Of2 v -> failwith ("expected parse error, got " + showVal v)

let repoFile relative =
    let candidates =
        [ relative
          Path.Combine("..", relative)
          Path.Combine("..", "..", relative)
          Path.Combine(Directory.GetCurrentDirectory(), relative) ]
    candidates
    |> List.tryFind File.Exists
    |> Option.defaultWith (fun () -> failwith ("missing file: " + relative))
