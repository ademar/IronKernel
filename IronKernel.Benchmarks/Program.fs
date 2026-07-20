module IronKernel.Benchmarks.Program

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open IronKernel.Ast
open IronKernel.Compiler
open IronKernel.Emit
open IronKernel.Eval
open IronKernel.Interop
open IronKernel.Parser
open IronKernel.Runtime
open IronKernel.SymbolTable

let private parse expression =
    match readExpr expression with
    | Choice2Of2 value -> value
    | Choice1Of2 error -> invalidOp (sprintf "Benchmark expression failed to parse: %A" error)

[<MemoryDiagnoser>]
type CompilerBenchmarks() =
    let env = makePrimitiveBindings ()
    let literalCall = parse "(+ 20 22)"
    let dynamicCall = parse "(+ value 1)"
    let mutable compiledFolded = Unchecked.defaultof<KernelFunc>
    let mutable compiledGeneric = Unchecked.defaultof<KernelFunc>

    [<GlobalSetup>]
    member _.Setup() =
        defineVar env "value" (Obj 41) |> ignore
        compiledFolded <- compileLispValGuarded env literalCall
        compiledGeneric <- compileLispValGuarded env dynamicCall

    [<Benchmark>]
    member _.ColdCompile() =
        compileLispValGuarded env dynamicCall

    [<Benchmark(Baseline = true)>]
    member _.Interpreted() =
        eval env (newContinuation env) dynamicCall

    [<Benchmark>]
    member _.CompiledGeneric() =
        compiledGeneric.Invoke(env, newContinuation env)

    [<Benchmark>]
    member _.CompiledFolded() =
        compiledFolded.Invoke(env, newContinuation env)

[<MemoryDiagnoser>]
type SymbolLookupBenchmarks() =
    let mutable env = Nil

    [<Params(1, 10, 50)>]
    member val Depth = 1 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let root = newEnv []
        defineVar root "target" (Obj 42) |> ignore
        env <-
            [2..this.Depth]
            |> List.fold (fun parent _ -> newEnv [parent]) root

    [<Benchmark>]
    member _.LookupParentBinding() =
        getVar' env "target"

[<MemoryDiagnoser>]
type ClrResolutionBenchmarks() =
    [<Benchmark>]
    member _.ResolveAcrossAssemblies() =
        resolveTypeExact "IronKernel.Ast"

[<MemoryDiagnoser>]
type EqualityBenchmarks() =
    let values () = [1..16] |> List.map (fun value -> Obj (value :> obj))
    let scalarArgs = [Obj (42 :> obj); Obj (42 :> obj)]
    let flatListArgs = [List (values ()); List (values ())]
    let nestedListArgs =
        [ List [List (values ()); List (values ())]
          List [List (values ()); List (values ())] ]
    let dottedListArgs =
        [ DottedList (values (), Atom "tail")
          DottedList (values (), Atom "tail") ]

    [<Benchmark(Baseline = true)>]
    member _.ScalarEqual() =
        eqv' scalarArgs

    [<Benchmark>]
    member _.FlatListEqual() =
        eqv' flatListArgs

    [<Benchmark>]
    member _.NestedListEqual() =
        eqv' nestedListArgs

    [<Benchmark>]
    member _.DottedListEqual() =
        eqv' dottedListArgs

[<MemoryDiagnoser>]
type ControlFlowBenchmarks() =
    let lambdaCall = parse "((lambda (x) (* 5 x)) 4)"
    let callCc = parse "(call/cc (lambda (k) (* 5 (k 4))))"
    let shiftReset = parse "(reset (+ (shift (lambda (k) (k 7))) 1))"
    let effect =
        parse
            "(prompt effect (lambda (value k) (resume k (+ value 1))) (+ 1 (perform effect 40)))"
    let mutable env = Nil

    let evaluate expression =
        eval env (newContinuation env) expression

    [<GlobalSetup>]
    member _.Setup() =
        env <-
            match bootstrapEnv () with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> invalidOp (sprintf "Benchmark bootstrap failed: %A" error)
        match eval env (newContinuation env) (parse "(define effect (make-prompt-tag))") with
        | Choice2Of2 Inert -> ()
        | result -> invalidOp (sprintf "Benchmark effect setup failed: %A" result)
        for expression, expected in [(lambdaCall, 20); (callCc, 4); (shiftReset, 8); (effect, 42)] do
            match evaluate expression with
            | Choice2Of2 (Obj (:? int as value)) when value = expected -> ()
            | result -> invalidOp (sprintf "Benchmark control-flow check failed: %A" result)

    [<Benchmark(Baseline = true)>]
    member _.LambdaCall() =
        evaluate lambdaCall

    [<Benchmark>]
    member _.CallCcEscape() =
        evaluate callCc

    [<Benchmark>]
    member _.ShiftResetResume() =
        evaluate shiftReset

    [<Benchmark>]
    member _.EffectHandlerResume() =
        evaluate effect

[<EntryPoint>]
let main args =
    BenchmarkSwitcher.FromAssembly(typeof<CompilerBenchmarks>.Assembly).Run(args) |> ignore
    0
