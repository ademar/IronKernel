module IronKernel.Tests.CliTests

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open Xunit

open IronKernel
open IronKernel.Ast
open IronKernel.Errors
open IronKernel.Emit
open IronKernel.StaticEmit
open IronKernel.Repl

let private tempPath extension =
    Path.Combine(Path.GetTempPath(), "ironkernel-" + Guid.NewGuid().ToString("N") + extension)

let private repoRoot =
    let dir = Directory.GetCurrentDirectory()
    if File.Exists(Path.Combine(dir, "IronKernel.sln")) then dir
    else Path.GetFullPath(Path.Combine(dir, "..", "..", "..", ".."))

let private buildConfiguration =
    let attr = Assembly.GetExecutingAssembly().GetCustomAttribute<AssemblyConfigurationAttribute>()
    if isNull attr then "Release" else attr.Configuration

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
let ``managed artifact runs without Kernel source or runtime compilation`` () =
    let root = Path.Combine(Path.GetTempPath(), "ironkernel-managed-test-" + Guid.NewGuid().ToString("N"))
    let script = Path.Combine(root, "answer.ikr")
    let output = Path.Combine(root, "publish")
    Directory.CreateDirectory(root) |> ignore
    try
        File.WriteAllText(script, "(+ 20 22)")
        let artifact =
            match compileFileToManagedArtifact Minimal script output with
            | Choice1Of2 error -> failwith (showError error)
            | Choice2Of2 path -> path
        Assert.True(File.Exists artifact)
        Assert.Empty(Directory.GetFiles(output, "*.ikr", SearchOption.AllDirectories))
        Assert.True(File.Exists(Path.Combine(output, "IronKernel.Runtime.dll")))
        Assert.False(File.Exists(Path.Combine(output, "IronKernel.dll")))
        Assert.False(File.Exists(Path.Combine(output, "FParsec.dll")))

        let startInfo = ProcessStartInfo("dotnet")
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardError <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.ArgumentList.Add artifact
        use child = Process.Start startInfo
        let stdout = child.StandardOutput.ReadToEnd()
        let stderr = child.StandardError.ReadToEnd()
        child.WaitForExit()
        Assert.Equal(0, child.ExitCode)
        Assert.Equal("<obj 42 : Int32>", stdout.Trim())
        Assert.Equal("", stderr.Trim())
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``compile managed CLI publishes a runnable assembly`` () =
    let root = Path.Combine(Path.GetTempPath(), "ironkernel-managed-cli-" + Guid.NewGuid().ToString("N"))
    let script = Path.Combine(root, "cli-answer.ikr")
    let output = Path.Combine(root, "publish")
    Directory.CreateDirectory(root) |> ignore
    try
        File.WriteAllText(script, "42")
        let exitCode, stdout, stderr =
            runCli ["--profile"; "minimal"; "compile"; script; "--managed"; "-o"; output]
        Assert.Equal(0, exitCode)
        Assert.Contains("cli-answer.dll", stdout)
        Assert.Equal("", stderr.Trim())
        Assert.True(File.Exists(Path.Combine(output, "cli-answer.dll")))
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``native artifacts require the minimal profile`` () =
    let root = Path.Combine(Path.GetTempPath(), "ironkernel-native-profile-" + Guid.NewGuid().ToString("N"))
    let script = Path.Combine(root, "profile.ikr")
    Directory.CreateDirectory(root) |> ignore
    try
        File.WriteAllText(script, "42")
        match compileFileToNativeArtifact Safe "osx-arm64" script (Path.Combine(root, "publish")) with
        | Choice1Of2 (Default message) -> Assert.Contains("only the minimal profile", message)
        | result -> failwithf "unexpected native profile result: %A" result
        let exitCode, _, stderr =
            runCli
                [ "--profile"; "safe"; "compile"; script; "--native"; "osx-arm64"
                  "-o"; Path.Combine(root, "cli-publish") ]
        Assert.Equal(1, exitCode)
        Assert.Contains("only the minimal profile", stderr)
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``native artifact runs without dotnet or Homebrew dylibs`` () =
    if OperatingSystem.IsMacOS() && RuntimeInformation.OSArchitecture = Architecture.Arm64 then
        let root = Path.Combine(Path.GetTempPath(), "ironkernel-native-test-" + Guid.NewGuid().ToString("N"))
        let script = Path.Combine(root, "native-answer.ikr")
        let output = Path.Combine(root, "publish")
        Directory.CreateDirectory(root) |> ignore
        try
            File.WriteAllText(script, "(+ 20 22)")
            let artifact =
                match compileFileToNativeArtifact Minimal "osx-arm64" script output with
                | Choice1Of2 error -> failwith (showError error)
                | Choice2Of2 path -> path
            Assert.True(File.Exists artifact)
            Assert.False(File.Exists(artifact + ".dll"))

            let runInfo = ProcessStartInfo(artifact)
            runInfo.UseShellExecute <- false
            runInfo.RedirectStandardError <- true
            runInfo.RedirectStandardOutput <- true
            use child = Process.Start runInfo
            let stdout = child.StandardOutput.ReadToEnd()
            let stderr = child.StandardError.ReadToEnd()
            child.WaitForExit()
            Assert.Equal(0, child.ExitCode)
            Assert.Equal("<obj 42 : Int32>", stdout.Trim())
            Assert.Equal("", stderr.Trim())

            let dependencyInfo = ProcessStartInfo("otool")
            dependencyInfo.UseShellExecute <- false
            dependencyInfo.RedirectStandardOutput <- true
            dependencyInfo.ArgumentList.Add "-L"
            dependencyInfo.ArgumentList.Add artifact
            use dependencies = Process.Start dependencyInfo
            let linkedLibraries = dependencies.StandardOutput.ReadToEnd()
            dependencies.WaitForExit()
            Assert.Equal(0, dependencies.ExitCode)
            Assert.DoesNotContain("/opt/homebrew", linkedLibraries)
            Assert.DoesNotContain("/usr/local/opt", linkedLibraries)
        finally
            Directory.Delete(root, true)

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
let ``runtime diagnostics identify the failing operator`` () =
    match bootstrapEnv () with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 env ->
        let source = "(define ready #t)\n(missing-combiner 42)"
        match runSource env "runtime-error.ikr" source with
        | Choice2Of2 value -> failwithf "unexpectedly returned %A" value
        | Choice1Of2 error ->
            let diagnostic = showError error
            Assert.Contains("runtime-error.ikr:2:2", diagnostic)
            Assert.Contains("(missing-combiner 42)", diagnostic)
            Assert.Contains(" ^^^^^^^^^^^^^^^^", diagnostic)

[<Fact>]
let ``runtime diagnostics prefer the nested operator span`` () =
    match bootstrapEnv () with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 env ->
        let source = "(define ready #t)\n((missing-combiner) 42)"
        match runSource env "nested-runtime-error.ikr" source with
        | Choice2Of2 value -> failwithf "unexpectedly returned %A" value
        | Choice1Of2 error ->
            let diagnostic = showError error
            Assert.Contains("nested-runtime-error.ikr:2:3", diagnostic)
            Assert.Contains("((missing-combiner) 42)", diagnostic)
            Assert.Contains("  ^^^^^^^^^^^^^^^^", diagnostic)

[<Fact>]
let ``nested located errors render without overflowing`` () =
    let depth = 100_000
    let span =
        { sourceName = "deep.ikr"
          startPosition = { offset = 0L; line = 1L; column = 1L }
          endPosition = { offset = 0L; line = 1L; column = 2L } }
    let mutable error = Default "boom"
    for _ in 1..depth do
        error <- LocatedError(span, None, error)

    let diagnostic = showError error
    Assert.Equal(depth * 14 + 4, diagnostic.Length)
    Assert.StartsWith("deep.ikr:1:1: deep.ikr:1:1: ", diagnostic)
    Assert.EndsWith("boom", diagnostic)

    let nested =
        LocatedError(
            { span with sourceName = "outer.ikr" },
            Some "outer",
            LocatedError({ span with sourceName = "inner.ikr" }, Some "inner", Default "boom"))
    Assert.Equal(
        "outer.ikr:1:1: inner.ikr:1:1: boom\ninner\n^\nouter\n^",
        showError nested)

[<Fact>]
let ``runtime diagnostics identify the selected if branch`` () =
    match bootstrapEnv () with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 env ->
        let source = "(if #t\n    missing\n    42)"
        match runSource env "if-branch-error.ikr" source with
        | Choice2Of2 value -> failwithf "unexpectedly returned %A" value
        | Choice1Of2 error ->
            let diagnostic = showError error
            Assert.Contains("if-branch-error.ikr:2:5", diagnostic)
            Assert.Contains("    missing", diagnostic)
            Assert.Contains("    ^^^^^^^", diagnostic)

[<Fact>]
let ``runtime diagnostics identify the define value`` () =
    match bootstrapEnv () with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 env ->
        let source = "(define answer\n  missing)"
        match runSource env "define-value-error.ikr" source with
        | Choice2Of2 value -> failwithf "unexpectedly returned %A" value
        | Choice1Of2 error ->
            let diagnostic = showError error
            Assert.Contains("define-value-error.ikr:2:3", diagnostic)
            Assert.Contains("  missing", diagnostic)
            Assert.Contains("  ^^^^^^^", diagnostic)

[<Fact>]
let ``located guarded if does not evaluate the unused branch`` () =
    match bootstrapEnv () with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 env ->
        match runSource env "if-lazy.ikr" "(if #t 42 missing)" with
        | Choice2Of2 (IronKernel.Ast.Obj value) -> Assert.Equal(42, value :?> int)
        | result -> failwithf "unexpected located if result: %A" result

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
