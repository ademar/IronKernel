namespace IronKernel

module Project =

    open System
    open System.Diagnostics
    open System.IO
    open System.IO.Compression
    open System.Reflection
    open System.Runtime.Loader
    open System.Text.Json
    open System.Xml.Linq

    open Ast
    open Emit
    open Errors
    open Runtime
    open SymbolTable

    type PackageReference = {
        id : string
        version : string
        kind : string
    }

    type IkProject = {
        path : string
        directory : string
        name : string
        version : string
        main : string
        profile : CapabilityProfile
        sources : string list
        tests : string list
        packages : PackageReference list
        outputPath : string
    }

    type ResolvedAssets = {
        sources : string list
        assemblies : string list
    }

    let private descendants name (document: XDocument) =
        document.Descendants(XName.Get name)

    let private property name fallback (document: XDocument) =
        descendants name document
        |> Seq.tryHead
        |> Option.map _.Value
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        |> Option.defaultValue fallback

    let private parseProfile = function
        | "minimal" -> Minimal
        | "safe" -> Safe
        | _ -> Unrestricted

    let private expandInclude projectDirectory (includeValue: string) =
        let normalized = includeValue.Replace('\\', '/')
        if normalized.Contains("**") then
            let prefix = normalized.Substring(0, normalized.IndexOf("**", StringComparison.Ordinal))
            let root = Path.GetFullPath(Path.Combine(projectDirectory, prefix))
            if Directory.Exists root then
                Directory.GetFiles(root, "*.ikr", SearchOption.AllDirectories)
                |> Array.sort
                |> Array.toList
            else []
        elif normalized.Contains("*") then
            let directory =
                Path.GetDirectoryName normalized
                |> fun value -> if String.IsNullOrEmpty value then projectDirectory else Path.Combine(projectDirectory, value)
            let pattern = Path.GetFileName normalized
            if Directory.Exists directory then
                Directory.GetFiles(directory, pattern, SearchOption.TopDirectoryOnly)
                |> Array.sort
                |> Array.toList
            else []
        else [Path.GetFullPath(Path.Combine(projectDirectory, includeValue))]

    let load projectPath =
        try
            let fullPath = Path.GetFullPath projectPath
            let directory = Path.GetDirectoryName fullPath
            let document = XDocument.Load fullPath
            let root = document.Root
            if obj.ReferenceEquals(root, null) || root.Name.LocalName <> "Project" then
                Choice1Of2 (Default "An .ikproj file must have a Project root element")
            else
                let sdk = root.Attribute(XName.Get "Sdk")
                if obj.ReferenceEquals(sdk, null)
                   || not (
                       sdk.Value.StartsWith("IronKernel.Sdk/", StringComparison.Ordinal)
                       || sdk.Value = "Microsoft.NET.Sdk") then
                    Choice1Of2 (Default "Project Sdk must be IronKernel.Sdk/<version> or Microsoft.NET.Sdk")
                else
                    let itemPaths itemName =
                        descendants itemName document
                        |> Seq.choose (fun element ->
                            let includeAttribute = element.Attribute(XName.Get "Include")
                            if obj.ReferenceEquals(includeAttribute, null) then None else Some includeAttribute.Value)
                        |> Seq.collect (expandInclude directory)
                        |> Seq.distinct
                        |> Seq.toList
                    let packages =
                        descendants "PackageReference" document
                        |> Seq.choose (fun element ->
                            let includeAttribute = element.Attribute(XName.Get "Include")
                            let versionAttribute = element.Attribute(XName.Get "Version")
                            if obj.ReferenceEquals(includeAttribute, null)
                               || obj.ReferenceEquals(versionAttribute, null) then None
                            else
                                let kindAttribute = element.Attribute(XName.Get "IronKernelKind")
                                Some
                                    { id = includeAttribute.Value
                                      version = versionAttribute.Value
                                      kind = if obj.ReferenceEquals(kindAttribute, null) then "IronKernel" else kindAttribute.Value })
                        |> Seq.toList
                    let name = property "PackageId" (Path.GetFileNameWithoutExtension fullPath) document
                    let mainRelative = property "IronKernelMain" "src/main.ikr" document
                    let main = Path.GetFullPath(Path.Combine(directory, mainRelative))
                    let sources = itemPaths "IronKernelSource"
                    let sources = if List.contains main sources then sources else sources @ [main]
                    if not (File.Exists main) then
                        Choice1Of2 (Default("Project main source does not exist: " + main))
                    else
                        Choice2Of2
                            { path = fullPath
                              directory = directory
                              name = name
                              version = property "Version" "0.1.0" document
                              main = main
                              profile = property "IronKernelProfile" "unrestricted" document |> parseProfile
                              sources = sources
                              tests = itemPaths "IronKernelTest"
                              packages = packages
                              outputPath = property "OutputPath" "bin" document |> fun value -> Path.GetFullPath(Path.Combine(directory, value)) }
        with ex ->
            Choice1Of2 (Default("Unable to load project: " + ex.Message))

    let discover startDirectory =
        let rec search directory =
            let projects = Directory.GetFiles(directory, "*.ikproj")
            match projects with
            | [|project|] -> Some project
            | [||] ->
                let parent = Directory.GetParent directory
                if obj.ReferenceEquals(parent, null) then None else search parent.FullName
            | _ -> None
        search (Path.GetFullPath startDirectory)

    let private runProcess command arguments workingDirectory =
        let startInfo = ProcessStartInfo(command)
        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        for argument in arguments do
            startInfo.ArgumentList.Add argument
        use childProcess = Process.Start startInfo
        childProcess.WaitForExit()
        childProcess.ExitCode

    let restore project locked =
        let args =
            [ "restore"; project.path; "--use-lock-file" ]
            @ (if locked then ["--locked-mode"] else [])
        runProcess "dotnet" args project.directory

    let private assetsPath project =
        Path.Combine(project.directory, "obj", "project.assets.json")

    let resolveAssets project =
        let path = assetsPath project
        if not (File.Exists path) then
            { sources = []; assemblies = [] }
        else
            use document = JsonDocument.Parse(File.ReadAllText path)
            let root = document.RootElement
            let packageRoot =
                root.GetProperty("packageFolders").EnumerateObject()
                |> Seq.tryHead
                |> Option.map _.Name
            match packageRoot with
            | None -> { sources = []; assemblies = [] }
            | Some packageRoot ->
                root.GetProperty("libraries").EnumerateObject()
                |> Seq.fold (fun (sources, assemblies) library ->
                    let value = library.Value
                    if value.GetProperty("type").GetString() <> "package" then sources, assemblies
                    else
                        let packageDirectory =
                            Path.Combine(packageRoot, library.Name.ToLowerInvariant())
                        let files =
                            match value.TryGetProperty("files") with
                            | true, values -> values.EnumerateArray() |> Seq.map _.GetString() |> Seq.toList
                            | _ -> []
                        files
                        |> List.fold (fun (sourceAcc, assemblyAcc) relative ->
                            let fullPath = Path.Combine(packageDirectory, relative.Replace('/', Path.DirectorySeparatorChar))
                            if relative.StartsWith("ironkernel/src/", StringComparison.Ordinal)
                               && relative.EndsWith(".ikr", StringComparison.OrdinalIgnoreCase) then
                                fullPath :: sourceAcc, assemblyAcc
                            elif relative.StartsWith("lib/", StringComparison.Ordinal)
                                 && relative.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                                 && not (relative.Contains("/ref/")) then
                                sourceAcc, fullPath :: assemblyAcc
                            else sourceAcc, assemblyAcc) (sources, assemblies)) ([], [])
                |> fun (sources, assemblies) ->
                    { sources = List.sort sources
                      assemblies = List.distinct assemblies }

    let private loadAssemblies paths =
        paths
        |> List.iter (fun path ->
            try AssemblyLoadContext.Default.LoadFromAssemblyPath(Path.GetFullPath path) |> ignore
            with :? FileLoadException -> ())

    let private runFiles env files =
        files
        |> List.fold (fun state path ->
            match state with
            | Choice1Of2 _ -> state
            | Choice2Of2 _ -> runSourceFile env path) (Choice2Of2 Inert)

    let run project args =
        if project.packages <> [] && not (File.Exists(assetsPath project)) then
            let restoreExit = restore project false
            if restoreExit <> 0 then restoreExit
            else
                let assets = resolveAssets project
                loadAssemblies assets.assemblies
                match bootstrapEnvForProfile project.profile with
                | Choice1Of2 error ->
                    eprintfn "Project startup error: %s" (showError error)
                    1
                | Choice2Of2 standardEnv ->
                    let env =
                        bindVars standardEnv
                            [ "args", List(List.map (fun arg -> Obj(arg :> obj)) args) ]
                    let projectSources = project.sources |> List.filter ((<>) project.main)
                    match runFiles env (assets.sources @ projectSources @ [project.main]) with
                    | Choice1Of2 error ->
                        eprintfn "Project error: %s" (showError error)
                        1
                    | Choice2Of2 _ -> 0
        else
            let assets = resolveAssets project
            loadAssemblies assets.assemblies
            match bootstrapEnvForProfile project.profile with
            | Choice1Of2 error ->
                eprintfn "Project startup error: %s" (showError error)
                1
            | Choice2Of2 standardEnv ->
                let env =
                    bindVars standardEnv
                        [ "args", List(List.map (fun arg -> Obj(arg :> obj)) args) ]
                let projectSources = project.sources |> List.filter ((<>) project.main)
                match runFiles env (assets.sources @ projectSources @ [project.main]) with
                | Choice1Of2 error ->
                    eprintfn "Project error: %s" (showError error)
                    1
                | Choice2Of2 _ -> 0

    let test project =
        let assets = resolveAssets project
        loadAssemblies assets.assemblies
        project.tests
        |> List.fold (fun failures testFile ->
            match bootstrapEnvForProfile project.profile with
            | Choice1Of2 error ->
                eprintfn "Test startup error: %s" (showError error)
                failures + 1
            | Choice2Of2 env ->
                let support = project.sources |> List.filter ((<>) project.main)
                match runFiles env (assets.sources @ support @ [testFile]) with
                | Choice1Of2 error ->
                    eprintfn "FAIL %s\n%s" testFile (showError error)
                    failures + 1
                | Choice2Of2 _ ->
                    printfn "PASS %s" testFile
                    failures) 0
        |> fun failures -> if failures = 0 then 0 else 1

    let build project =
        Directory.CreateDirectory project.outputPath |> ignore
        let objectDirectory = Path.Combine(project.directory, "obj")
        Directory.CreateDirectory objectDirectory |> ignore
        let combinedPath = Path.Combine(objectDirectory, project.name + ".combined.ikr")
        let source =
            project.sources
            |> List.map File.ReadAllText
            |> String.concat Environment.NewLine
        File.WriteAllText(combinedPath, source)
        let output = Path.Combine(project.outputPath, project.name + ".ikc")
        match compileFileToPackageForProfile project.profile combinedPath output with
        | Choice1Of2 error ->
            eprintfn "Build error: %s" (showError error)
            1
        | Choice2Of2 path ->
            printfn "Wrote %s" path
            0

    let private projectXml name =
        $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
    <RestorePackagesWithLockFile>true</RestorePackagesWithLockFile>
    <PackageId>{name}</PackageId>
    <Version>0.1.0</Version>
    <Authors>TODO</Authors>
    <Description>IronKernel project</Description>
    <PackageTags>ironkernel</PackageTags>
    <IronKernelMain>src/main.ikr</IronKernelMain>
    <IronKernelProfile>unrestricted</IronKernelProfile>
  </PropertyGroup>
  <ItemGroup>
    <IronKernelSource Include="src/**/*.ikr" />
    <IronKernelTest Include="test/**/*.ikr" />
  </ItemGroup>
</Project>
"""

    let create kind name parentDirectory =
        try
            let directory = Path.Combine(parentDirectory, name)
            if Directory.Exists directory then
                eprintfn "Directory already exists: %s" directory
                1
            else
                Directory.CreateDirectory(Path.Combine(directory, "src")) |> ignore
                Directory.CreateDirectory(Path.Combine(directory, "test")) |> ignore
                File.WriteAllText(Path.Combine(directory, name + ".ikproj"), projectXml name)
                let main =
                    if kind = "lib" then
                        "(provide! (hello)\n  (define hello (lambda () \"hello\")))\n"
                    else
                        "(Console.write-line \"Hello from IronKernel!\")\n"
                File.WriteAllText(Path.Combine(directory, "src", "main.ikr"), main)
                File.WriteAllText(Path.Combine(directory, "test", "main_test.ikr"), "(if #t #inert missing)\n")
                File.WriteAllText(Path.Combine(directory, ".gitignore"), "bin/\nobj/\n*.ikc\n")
                File.WriteAllText(
                    Path.Combine(directory, "README.md"),
                    "# " + name + "\n\nAn IronKernel " + kind + " project.\n")
                printfn "Created %s" directory
                0
        with ex ->
            eprintfn "Unable to create project: %s" ex.Message
            1

    let private saveDocument (path: string) (document: XDocument) =
        use writer =
            System.Xml.XmlWriter.Create(
                path,
                System.Xml.XmlWriterSettings(Indent = true))
        document.Save writer

    let addPackage (projectPath: string) id version kind =
        let document = XDocument.Load projectPath
        let existing =
            descendants "PackageReference" document
            |> Seq.tryFind (fun element ->
                let attribute = element.Attribute(XName.Get "Include")
                not (obj.ReferenceEquals(attribute, null)) && attribute.Value = id)
        match existing with
        | Some _ ->
            eprintfn "Package already referenced: %s" id
            1
        | None ->
            let group =
                document.Root.Elements(XName.Get "ItemGroup")
                |> Seq.tryHead
                |> Option.defaultWith (fun () ->
                    let value = XElement(XName.Get "ItemGroup")
                    document.Root.Add value
                    value)
            group.Add(
                XElement(
                    XName.Get "PackageReference",
                    XAttribute(XName.Get "Include", id),
                    XAttribute(XName.Get "Version", version),
                    XAttribute(XName.Get "IronKernelKind", kind)))
            saveDocument projectPath document
            printfn "Added %s %s" id version
            0

    let removePackage (projectPath: string) id =
        let document = XDocument.Load projectPath
        let matches =
            descendants "PackageReference" document
            |> Seq.filter (fun element ->
                let attribute = element.Attribute(XName.Get "Include")
                not (obj.ReferenceEquals(attribute, null)) && attribute.Value = id)
            |> Seq.toList
        matches |> List.iter _.Remove()
        saveDocument projectPath document
        if matches = [] then
            eprintfn "Package not found: %s" id
            1
        else
            printfn "Removed %s" id
            0

    let tree project =
        let path = assetsPath project
        if not (File.Exists path) then
            eprintfn "Run 'ik restore' first."
            1
        else
            use document = JsonDocument.Parse(File.ReadAllText path)
            document.RootElement.GetProperty("libraries").EnumerateObject()
            |> Seq.map _.Name
            |> Seq.sort
            |> Seq.iter (printfn "%s")
            0

    let pack project =
        let objectDirectory = Path.Combine(project.directory, "obj")
        let outputDirectory = Path.Combine(project.directory, "bin")
        Directory.CreateDirectory objectDirectory |> ignore
        Directory.CreateDirectory outputDirectory |> ignore
        let generatedProject = Path.Combine(objectDirectory, project.name + ".pack.csproj")
        let manifestPath = Path.Combine(objectDirectory, "ironkernel.package.json")
        let readmePath = Path.Combine(project.directory, "README.md")
        if not (File.Exists readmePath) then
            File.WriteAllText(readmePath, "# " + project.name + Environment.NewLine)
        let mainRelative = Path.GetRelativePath(project.directory, project.main).Replace('\\', '/')
        let manifest =
            JsonSerializer.Serialize(
                {| schemaVersion = 1
                   name = project.name
                   version = project.version
                   entry = mainRelative
                   profile = project.profile.ToString().ToLowerInvariant() |},
                JsonSerializerOptions(WriteIndented = true))
        File.WriteAllText(manifestPath, manifest)
        let packageItems =
            project.sources
            |> List.map (fun source ->
                let relative = Path.GetRelativePath(project.directory, source).Replace('\\', '/')
                let packageRelative =
                    if relative.StartsWith("src/", StringComparison.Ordinal) then
                        relative.Substring(4)
                    else relative
                sprintf "    <None Include=\"%s\" Pack=\"true\" PackagePath=\"ironkernel/src/%s\" />" source packageRelative)
            |> String.concat Environment.NewLine
        let references =
            project.packages
            |> List.map (fun package ->
                sprintf "    <PackageReference Include=\"%s\" Version=\"%s\" />" package.id package.version)
            |> String.concat Environment.NewLine
        let generated =
            $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
    <IncludeBuildOutput>false</IncludeBuildOutput>
    <PackageId>{project.name}</PackageId>
    <Version>{project.version}</Version>
    <Authors>IronKernel</Authors>
    <Description>IronKernel source package</Description>
    <PackageTags>ironkernel</PackageTags>
    <PackageReadmeFile>README.md</PackageReadmeFile>
    <NoWarn>NU5128</NoWarn>
  </PropertyGroup>
  <ItemGroup>
{packageItems}
    <None Include="{manifestPath}" Pack="true" PackagePath="ironkernel/package.json" />
    <None Include="{readmePath}" Pack="true" PackagePath="" />
{references}
  </ItemGroup>
</Project>
"""
        File.WriteAllText(generatedProject, generated)
        runProcess "dotnet" ["pack"; generatedProject; "-o"; outputDirectory] project.directory

    let publish project source apiKey =
        let outputDirectory = Path.Combine(project.directory, "bin")
        let package =
            if Directory.Exists outputDirectory then
                Directory.GetFiles(outputDirectory, "*.nupkg")
                |> Array.sortDescending
                |> Array.tryHead
            else None
        match package with
        | None ->
            eprintfn "No package found. Run 'ik pack' first."
            1
        | Some package ->
            runProcess
                "dotnet"
                [ "nuget"; "push"; package; "--source"; source; "--api-key"; apiKey ]
                project.directory

    let doctor () =
        printfn "IronKernel project doctor"
        let dotnetExit = runProcess "dotnet" ["--version"] (Directory.GetCurrentDirectory())
        let stdlib =
            [ "kernel.ikr"; "promises.ikr" ]
            |> List.forall (fun name ->
                File.Exists name || File.Exists(Path.Combine(AppContext.BaseDirectory, name)))
        printfn "stdlib: %s" (if stdlib then "ok" else "missing")
        if dotnetExit = 0 && stdlib then 0 else 1
