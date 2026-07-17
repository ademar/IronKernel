module IronKernel.Tests.CliTests

open System
open System.IO
open Xunit

open IronKernel
open IronKernel.Emit
open IronKernel.Repl

let private tempPath extension =
    Path.Combine(Path.GetTempPath(), "ironkernel-" + Guid.NewGuid().ToString("N") + extension)

let private kernelString value =
    "\"" + value.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""

[<Fact>]
let ``script mode bootstraps the standard library`` () =
    let script = tempPath ".scm"
    try
        File.WriteAllText(script, "((lambda (x) x) 42)")
        Assert.Equal(0, runOne script [])
    finally
        File.Delete(script)

[<Fact>]
let ``script mode reports evaluation failure through exit code`` () =
    let script = tempPath ".scm"
    try
        File.WriteAllText(script, "(missing-combiner 42)")
        Assert.Equal(1, runOne script [])
    finally
        File.Delete(script)

[<Fact>]
let ``packaging validates but does not execute source`` () =
    let script = tempPath ".scm"
    let package = tempPath ".ikc"
    let marker = tempPath ".txt"
    try
        let source =
            sprintf "(. System.IO.File WriteAllText %s \"executed\")" (kernelString marker)
        File.WriteAllText(script, source)

        match compileFileToPackage script package with
        | Choice1Of2 error -> failwithf "packaging failed: %A" error
        | Choice2Of2 _ -> ()

        Assert.False(File.Exists(marker), "packaging executed user code")

        match loadIkc package with
        | Choice1Of2 error -> failwithf "package execution failed: %A" error
        | Choice2Of2 _ -> ()

        Assert.Equal("executed", File.ReadAllText(marker))
    finally
        File.Delete(script)
        File.Delete(package)
        File.Delete(marker)

[<Fact>]
let ``package API requires honest ikc extension`` () =
    let script = tempPath ".scm"
    let output = tempPath ".dll"
    try
        File.WriteAllText(script, "42")
        match compileFileToPackage script output with
        | Choice1Of2 _ -> ()
        | Choice2Of2 _ -> failwith "accepted a misleading .dll package name"
    finally
        File.Delete(script)
        File.Delete(output)

[<Fact>]
let ``truncated package returns a structured error`` () =
    let package = tempPath ".ikc"
    try
        File.WriteAllBytes(
            package,
            Array.concat
                [ Text.Encoding.ASCII.GetBytes("IKC1")
                  BitConverter.GetBytes(100) ])

        match loadIkc package with
        | Choice1Of2 _ -> ()
        | Choice2Of2 value -> failwithf "loaded truncated package as %A" value
    finally
        File.Delete(package)
