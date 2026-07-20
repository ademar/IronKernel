module IronKernel.Benchmarks.Program

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open IronKernel.Ast
open IronKernel.Compiler
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

[<EntryPoint>]
let main args =
    BenchmarkSwitcher.FromAssembly(typeof<CompilerBenchmarks>.Assembly).Run(args) |> ignore
    0
