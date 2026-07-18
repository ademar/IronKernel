module IronKernel.Tests.ProjectTests

open System
open System.IO
open System.IO.Compression
open Xunit

open IronKernel.Project

let private withProject body =
    let root = Path.Combine(Path.GetTempPath(), "ironkernel-project-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(root) |> ignore
    try
        Assert.Equal(0, create "app" "demo" root)
        let projectPath = Path.Combine(root, "demo", "demo.ikproj")
        match load projectPath with
        | Choice1Of2 error -> failwithf "project load failed: %A" error
        | Choice2Of2 project -> body project
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``new project loads runs tests and builds`` () =
    withProject (fun project ->
        Assert.EndsWith("src/main.ikr", project.main.Replace('\\', '/'))
        Assert.Equal(0, run project [])
        Assert.Equal(0, test project)
        Assert.Equal(0, build project)
        Assert.True(File.Exists(Path.Combine(project.outputPath, "demo.ikc"))))

[<Fact>]
let ``project package references can be added and removed`` () =
    withProject (fun project ->
        Assert.Equal(0, addPackage project.path "Example.Dependency" "1.2.3" "IronKernel")
        let reloaded =
            match load project.path with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwithf "reload failed: %A" error
        Assert.Contains(reloaded.packages, fun package ->
            package.id = "Example.Dependency"
            && package.version = "1.2.3"
            && package.kind = "IronKernel")
        Assert.Equal(0, removePackage project.path "Example.Dependency")
        let afterRemoval =
            match load project.path with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwithf "reload failed: %A" error
        Assert.Empty(afterRemoval.packages))

[<Fact>]
let ``project restore creates NuGet assets and lock file`` () =
    withProject (fun project ->
        Assert.Equal(0, restore project false)
        Assert.True(File.Exists(Path.Combine(project.directory, "obj", "project.assets.json")))
        Assert.True(File.Exists(Path.Combine(project.directory, "packages.lock.json")))
        Assert.Equal(0, restore project true))

[<Fact>]
let ``project pack emits NuGet package with ikr sources`` () =
    withProject (fun project ->
        Assert.Equal(0, pack project)
        let package =
            Directory.GetFiles(Path.Combine(project.directory, "bin"), "*.nupkg")
            |> Array.exactlyOne
        use archive = ZipFile.OpenRead(package)
        Assert.Contains(archive.Entries, fun entry ->
            entry.FullName.EndsWith("main.ikr", StringComparison.Ordinal)))

[<Fact>]
let ``NuGet IronKernel dependency sources load before project main`` () =
    let root = Path.Combine(Path.GetTempPath(), "ironkernel-deps-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(root) |> ignore
    try
        Assert.Equal(0, create "lib" "shared" root)
        let shared =
            match load (Path.Combine(root, "shared", "shared.ikproj")) with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwithf "shared load failed: %A" error
        File.WriteAllText(shared.main, "(define hello (lambda () \"from package\"))\n")
        Assert.Equal(0, pack shared)

        Assert.Equal(0, create "app" "consumer" root)
        let consumerPath = Path.Combine(root, "consumer", "consumer.ikproj")
        let consumer =
            match load consumerPath with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwithf "consumer load failed: %A" error
        File.WriteAllText(
            consumer.main,
            "(if (eqv? (hello) \"from package\") #inert missing)\n")
        Assert.Equal(0, addPackage consumer.path "shared" "0.1.0" "IronKernel")

        let feed = Path.Combine(shared.directory, "bin")
        File.WriteAllText(
            Path.Combine(root, "NuGet.config"),
            $"""<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="local" value="{feed}" />
  </packageSources>
</configuration>
""")
        let reloaded =
            match load consumer.path with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwithf "consumer reload failed: %A" error
        Assert.Equal(0, restore reloaded false)
        Assert.NotEmpty((resolveAssets reloaded).sources)
        Assert.Equal(0, run reloaded [])
    finally
        Directory.Delete(root, true)
