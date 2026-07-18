module IronKernel.Tests.ProjectTests

open System
open System.IO
open System.IO.Compression
open Xunit

open IronKernel.Ast
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

let private withDependencyGraph body =
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
        File.WriteAllText(
            Path.Combine(consumer.directory, "test", "main_test.ikr"),
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
        body root shared reloaded
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``NuGet IronKernel dependency sources load before project main`` () =
    withDependencyGraph (fun _ _ consumer ->
        Assert.Equal(0, restore consumer false)
        Assert.NotEmpty((resolveAssets consumer).sources)
        Assert.Equal(0, run consumer []))

[<Fact>]
let ``build concatenates dependency sources before main`` () =
    withProject (fun project ->
        File.WriteAllText(
            Path.Combine(project.directory, "src", "util.ikr"),
            "(define greeting \"ordered\")\n")
        File.WriteAllText(
            project.main,
            "(if (eqv? greeting \"ordered\") #inert missing)\n")
        let reloaded =
            match load project.path with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwithf "reload failed: %A" error
        Assert.Equal(0, run reloaded [])
        Assert.Equal(0, build reloaded)
        let combined =
            File.ReadAllText(Path.Combine(reloaded.directory, "obj", reloaded.name + ".combined.ikr"))
        let helperIndex = combined.IndexOf("define greeting", StringComparison.Ordinal)
        let mainIndex = combined.IndexOf("eqv? greeting", StringComparison.Ordinal)
        Assert.True(helperIndex >= 0 && mainIndex > helperIndex))

[<Fact>]
let ``build and test restore NuGet dependencies like run`` () =
    withDependencyGraph (fun _ _ consumer ->
        Assert.False(File.Exists(Path.Combine(consumer.directory, "obj", "project.assets.json")))
        Assert.Equal(0, test consumer)
        Assert.True(File.Exists(Path.Combine(consumer.directory, "obj", "project.assets.json")))
        Directory.Delete(Path.Combine(consumer.directory, "obj"), true)
        Assert.Equal(0, build consumer)
        Assert.True(File.Exists(Path.Combine(consumer.outputPath, "consumer.ikc"))))

[<Fact>]
let ``ensureRestored refreshes stale assets after project changes`` () =
    withDependencyGraph (fun _ _ consumer ->
        Assert.Equal(0, restore consumer false)
        let assets = Path.Combine(consumer.directory, "obj", "project.assets.json")
        File.SetLastWriteTimeUtc(consumer.path, File.GetLastWriteTimeUtc(assets).AddMinutes(1.0))
        Assert.Equal(0, ensureRestored consumer)
        Assert.True(File.GetLastWriteTimeUtc assets >= File.GetLastWriteTimeUtc consumer.path)
        let stamp = File.GetLastWriteTimeUtc assets
        Assert.Equal(0, ensureRestored consumer)
        Assert.Equal(stamp, File.GetLastWriteTimeUtc assets))

[<Fact>]
let ``discover reports multiple projects instead of none`` () =
    let root = Path.Combine(Path.GetTempPath(), "ironkernel-discover-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(root) |> ignore
    try
        File.WriteAllText(Path.Combine(root, "a.ikproj"), "<Project Sdk=\"Microsoft.NET.Sdk\" />")
        File.WriteAllText(Path.Combine(root, "b.ikproj"), "<Project Sdk=\"Microsoft.NET.Sdk\" />")
        match discover root with
        | MultipleProjects projects ->
            Assert.Equal(2, projects.Length)
            Assert.Contains(projects, fun path -> path.EndsWith("a.ikproj", StringComparison.Ordinal))
            Assert.Contains(projects, fun path -> path.EndsWith("b.ikproj", StringComparison.Ordinal))
        | other -> failwithf "expected MultipleProjects, got %A" other
        File.Delete(Path.Combine(root, "b.ikproj"))
        match discover root with
        | ProjectFound path -> Assert.EndsWith("a.ikproj", path)
        | other -> failwithf "expected ProjectFound, got %A" other
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``publish selects matching package id and version over lexical order`` () =
    withProject (fun project ->
        Directory.CreateDirectory(Path.Combine(project.directory, "bin")) |> ignore
        let older = Path.Combine(project.directory, "bin", "zzz.9.9.9.nupkg")
        let expected =
            Path.Combine(project.directory, "bin", sprintf "%s.%s.nupkg" project.name project.version)
        File.WriteAllText(older, "old")
        File.WriteAllText(expected, "new")
        File.SetLastWriteTimeUtc(older, DateTime.UtcNow.AddHours(1.0))
        File.SetLastWriteTimeUtc(expected, DateTime.UtcNow.AddHours(-1.0))
        match selectPackageForPublish project with
        | Some path -> Assert.Equal(expected, path)
        | None -> failwith "expected a package")

let private escapeJsonPath (path: string) =
    path.Replace("\\", "\\\\")

[<Fact>]
let ``resolveAssets searches all packageFolders`` () =
    withProject (fun project ->
        let objDir = Path.Combine(project.directory, "obj")
        Directory.CreateDirectory(objDir) |> ignore
        let emptyFolder = Path.Combine(project.directory, "empty-packages")
        let realFolder = Path.Combine(project.directory, "real-packages")
        Directory.CreateDirectory(emptyFolder) |> ignore
        let packageDir = Path.Combine(realFolder, "shared", "0.1.0")
        let sourceDir = Path.Combine(packageDir, "ironkernel", "src")
        Directory.CreateDirectory(sourceDir) |> ignore
        let sourcePath = Path.Combine(sourceDir, "main.ikr")
        File.WriteAllText(sourcePath, "(define hello (lambda () 1))\n")
        let assets =
            $"""{{
  "packageFolders": {{
    "{escapeJsonPath emptyFolder}/": {{}},
    "{escapeJsonPath realFolder}/": {{}}
  }},
  "libraries": {{
    "shared/0.1.0": {{
      "type": "package",
      "files": [ "ironkernel/src/main.ikr" ]
    }}
  }}
}}"""
        File.WriteAllText(Path.Combine(objDir, "project.assets.json"), assets)
        let resolved = resolveAssets project
        Assert.Contains(resolved.sources, fun path ->
            Path.GetFullPath(path) = Path.GetFullPath(sourcePath)))

[<Fact>]
let ``resolveAssets orders package sources by NuGet dependencies`` () =
    withProject (fun project ->
        let objDir = Path.Combine(project.directory, "obj")
        Directory.CreateDirectory(objDir) |> ignore
        let packages = Path.Combine(project.directory, "packages")
        let writePackage (id: string) (contents: string) =
            let sourceDir = Path.Combine(packages, id, "1.0.0", "ironkernel", "src")
            Directory.CreateDirectory(sourceDir) |> ignore
            let path = Path.Combine(sourceDir, "main.ikr")
            File.WriteAllText(path, contents)
            path
        // apple depends on zebra; alphabetical order would load apple first.
        let zebraPath = writePackage "zebra" "(define base 1)\n"
        let applePath = writePackage "apple" "(define uses-base base)\n"
        let assets =
            $"""{{
  "packageFolders": {{
    "{escapeJsonPath packages}/": {{}}
  }},
  "targets": {{
    "net10.0": {{
      "apple/1.0.0": {{
        "type": "package",
        "dependencies": {{ "zebra": "1.0.0" }}
      }},
      "zebra/1.0.0": {{
        "type": "package"
      }}
    }}
  }},
  "libraries": {{
    "apple/1.0.0": {{
      "type": "package",
      "files": [ "ironkernel/src/main.ikr" ]
    }},
    "zebra/1.0.0": {{
      "type": "package",
      "files": [ "ironkernel/src/main.ikr" ]
    }}
  }}
}}"""
        File.WriteAllText(Path.Combine(objDir, "project.assets.json"), assets)
        let resolved = resolveAssets project
        let expected = [ Path.GetFullPath zebraPath; Path.GetFullPath applePath ]
        let actual = resolved.sources |> List.map Path.GetFullPath
        Assert.Equal<string list>(expected, actual))

[<Fact>]
let ``parseProjectOptions finds project after flags`` () =
    let project, options =
        parseProjectOptions [ "--locked"; "demo.ikproj"; "--verbose" ]
    Assert.Equal(Some "demo.ikproj", project)
    Assert.Equal<string list>([ "--locked"; "--verbose" ], options)

[<Fact>]
let ``test loads IronKernelMain before test files`` () =
    withProject (fun project ->
        File.WriteAllText(project.main, "(define answer 42)\n")
        File.WriteAllText(
            Path.Combine(project.directory, "test", "main_test.ikr"),
            "(if (eqv? answer 42) #inert missing)\n")
        let reloaded =
            match load project.path with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwithf "reload failed: %A" error
        Assert.Equal(0, test reloaded))

[<Fact>]
let ``ensureRestored clears assets after all packages are removed`` () =
    withDependencyGraph (fun _ _ consumer ->
        Assert.Equal(0, restore consumer false)
        Assert.NotEmpty((resolveAssets consumer).sources)
        Assert.Equal(0, removePackage consumer.path "shared")
        let reloaded =
            match load consumer.path with
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwithf "reload failed: %A" error
        Assert.Empty(reloaded.packages)
        Assert.Equal(0, ensureRestored reloaded)
        Assert.Empty((resolveAssets reloaded).sources))

[<Fact>]
let ``ensureRestored refreshes when packages lock file is newer`` () =
    withDependencyGraph (fun _ _ consumer ->
        Assert.Equal(0, restore consumer false)
        let assets = Path.Combine(consumer.directory, "obj", "project.assets.json")
        let lockFile = Path.Combine(consumer.directory, "packages.lock.json")
        Assert.True(File.Exists lockFile)
        File.SetLastWriteTimeUtc(lockFile, File.GetLastWriteTimeUtc(assets).AddMinutes(1.0))
        Assert.Equal(0, ensureRestored consumer)
        Assert.True(File.GetLastWriteTimeUtc assets >= File.GetLastWriteTimeUtc lockFile))

[<Fact>]
let ``project profile override is honored by run`` () =
    withProject (fun project ->
        // Safe profile still bootstraps; override must be applied on the project record.
        let safeProject = { project with profile = Safe }
        Assert.Equal(0, run safeProject []))
