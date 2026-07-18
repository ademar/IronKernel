module IronKernel.Tests.ExampleTests

open System.IO
open Xunit
open IronKernel.Ast
open IronKernel.Emit
open IronKernel.Errors
open IronKernel.Tests.TestHelpers

let private examplesDir () =
    [ "Examples"
      Path.Combine("..", "Examples")
      Path.Combine("..", "..", "Examples")
      Path.Combine(Directory.GetCurrentDirectory(), "Examples") ]
    |> List.tryFind Directory.Exists
    |> Option.defaultWith (fun () -> failwith "Examples directory not found")

[<Fact>]
let ``hello.ikr packages without execution`` () =
    let path = Path.Combine(examplesDir (), "hello.ikr")
    let outp = Path.Combine(Path.GetTempPath(), "hello-ci.ikc")
    match compileFileToPackage path outp with
    | Choice1Of2 e -> failwith (showError e)
    | Choice2Of2 p -> Assert.True(File.Exists p)

[<Fact>]
let ``vau-dotnet.ikr packages without execution`` () =
    let path = Path.Combine(examplesDir (), "vau-dotnet.ikr")
    let outp = Path.Combine(Path.GetTempPath(), "vau-dotnet-ci.ikc")
    match compileFileToPackage path outp with
    | Choice1Of2 e -> failwith (showError e)
    | Choice2Of2 p -> Assert.True(File.Exists p)

[<Fact>]
let ``samples.ikr packages without execution`` () =
    let path = Path.Combine(examplesDir (), "samples.ikr")
    let outp = Path.Combine(Path.GetTempPath(), "samples-ci.ikc")
    match compileFileToPackage path outp with
    | Choice1Of2 e -> failwith (showError e)
    | Choice2Of2 p -> Assert.True(File.Exists p)
