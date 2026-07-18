module IronKernel.Tests.CliTests

open System
open System.Diagnostics
open System.IO
open Xunit

open IronKernel
open IronKernel.Errors
open IronKernel.Emit
open IronKernel.Repl

let private tempPath extension =
    Path.Combine(Path.GetTempPath(), "ironkernel-" + Guid.NewGuid().ToString("N") + extension)

let private repoRoot =
    let dir = Directory.GetCurrentDirectory()
    if File.Exists(Path.Combine(dir, "IronKernel.sln")) then dir
    else Path.GetFullPath(Path.Combine(dir, "..", "..", "..", ".."))

let private buildConfiguration =
    let baseDir = AppContext.BaseDirectory
    if baseDir.Contains($"{Path.DirectorySeparatorChar}Release{Path.DirectorySeparatorChar}") then "Release"
    else "Debug"

let private runCli (args: string list) =
    let startInfo = ProcessStartInfo("dotnet")
    startInfo.WorkingDirectory <- repoRoot
    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardError <- true
    startInfo.RedirectStandardOutput <- true
    startInfo.ArgumentList.Add "run"
    startInfo.ArgumentList.Add "--project"
    startInfo.ArgumentList.Add "IronKernel"
    startInfo.ArgumentList.Add "-c"
    startInfo.ArgumentList.Add buildConfiguration
    startInfo.ArgumentList.Add "--no-build"
    startInfo.ArgumentList.Add "--"
    for arg in args do
        startInfo.ArgumentList.Add arg
    use child = Process.Start startInfo
    let stderr = child.StandardError.ReadToEnd()
    let stdout = child.StandardOutput.ReadToEnd()
    child.WaitForExit()
    child.ExitCode, stdout, stderr

let private kernelString (value: string) =
    "\"" + value.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""

[<Fact>]
let ``script mode bootstraps the standard library`` () =
    let script = tempPath ".ikr"
    try
        File.WriteAllText(script, "((lambda (x) x) 42)")
        Assert.Equal(0, runOne script [])
    finally
        File.Delete(script)

[<Fact>]
let ``script mode reports evaluation failure through exit code`` () =
    let script = tempPath ".ikr"
    try
        File.WriteAllText(script, "(missing-combiner 42)")
        Assert.Equal(1, runOne script [])
    finally
        File.Delete(script)

[<Fact>]
let ``packaging validates but does not execute source`` () =
    let script = tempPath ".ikr"
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
    let script = tempPath ".ikr"
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

[<Fact>]
let ``runtime diagnostics identify the failing top-level form`` () =
    match bootstrapEnv () with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 env ->
        let source = "(define ready #t)\n(missing-combiner 42)"
        match runSource env "runtime-error.ikr" source with
        | Choice2Of2 value -> failwithf "unexpectedly returned %A" value
        | Choice1Of2 error ->
            let diagnostic = showError error
            Assert.Contains("runtime-error.ikr:2:1", diagnostic)
            Assert.Contains("(missing-combiner 42)", diagnostic)
            Assert.Contains("^^^^", diagnostic)

[<Fact>]
let ``package validation reports named parse diagnostics`` () =
    let script = tempPath ".ikr"
    let package = tempPath ".ikc"
    try
        File.WriteAllText(script, "(define broken")
        match compileFileToPackage script package with
        | Choice2Of2 _ -> failwith "packaged invalid source"
        | Choice1Of2 error ->
            let diagnostic = showError error
            Assert.Contains(script + ":1:", diagnostic)
            Assert.Contains("(define broken", diagnostic)
            Assert.Contains("^", diagnostic)
    finally
        File.Delete(script)
        File.Delete(package)

[<Fact>]
let ``new with invalid kind reports project usage instead of running a script`` () =
    let exitCode, _, stderr = runCli [ "new"; "widget"; "demo" ]
    Assert.Equal(2, exitCode)
    Assert.Contains("Expected 'ik new <app|lib> <name>'", stderr)
    Assert.DoesNotContain("Getting an unbound variable", stderr)

[<Fact>]
let ``bare ikproj path runs the project instead of parsing XML as Kernel`` () =
    let root = Path.Combine(Path.GetTempPath(), "ironkernel-cli-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(root) |> ignore
    try
        Assert.Equal(0, IronKernel.Project.create "app" "demo" root)
        let projectPath = Path.Combine(root, "demo", "demo.ikproj")
        let exitCode, _, stderr = runCli [ projectPath ]
        Assert.Equal(0, exitCode)
        Assert.DoesNotContain("Parser error", stderr)
        Assert.DoesNotContain("Getting an unbound variable", stderr)
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``run with non-source args forwards them to the discovered project`` () =
    let root = Path.Combine(Path.GetTempPath(), "ironkernel-cli-args-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(root) |> ignore
    try
        Assert.Equal(0, IronKernel.Project.create "app" "demo" root)
        let projectDir = Path.Combine(root, "demo")
        File.WriteAllText(
            Path.Combine(projectDir, "src", "main.ikr"),
            "(if (eqv? (car args) \"--help\") #inert missing)\n")
        let startInfo = ProcessStartInfo("dotnet")
        startInfo.WorkingDirectory <- projectDir
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardError <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.ArgumentList.Add "run"
        startInfo.ArgumentList.Add "--project"
        startInfo.ArgumentList.Add (Path.Combine(repoRoot, "IronKernel"))
        startInfo.ArgumentList.Add "-c"
        startInfo.ArgumentList.Add buildConfiguration
        startInfo.ArgumentList.Add "--no-build"
        startInfo.ArgumentList.Add "--"
        startInfo.ArgumentList.Add "run"
        startInfo.ArgumentList.Add "--help"
        use child = Process.Start startInfo
        let stderr = child.StandardError.ReadToEnd()
        child.WaitForExit()
        Assert.Equal(0, child.ExitCode)
        Assert.DoesNotContain("Unable to find file", stderr)
        Assert.DoesNotContain("Parser error", stderr)
    finally
        Directory.Delete(root, true)
