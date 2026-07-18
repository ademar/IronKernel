# IronKernel for Visual Studio Code

Language support and a local playground backed by the real IronKernel runtime.
The extension never implements a second evaluator in JavaScript.

## Features

- Syntax highlighting for lists, vectors, improper lists, keywords, `vau`, and CLR interop
- Distinct colors for **operatives** (unevaluated operands) vs **applicatives** (args evaluated first), matching the website token sets
- Bracket matching, comments, indentation, and IronKernel snippets
- **Run Current File** and **Compile Current File to IKC** commands
- **Run Project** and **Build Project** for `.ikproj` (nearest project, picker, or explorer context menu)
- Source-range diagnostics in VS Code's Problems view
- A playground with runnable examples, validation, cancellation, output limits, and timeouts

## Development

```bash
cd editors/vscode
npm install
npm run typecheck
npm test
npm run package
```

Open this directory in VS Code and press `F5` to launch an Extension Development
Host, or install the generated `.vsix` with **Extensions: Install from VSIX…**.

## Runtime discovery

The extension resolves IronKernel in this order:

1. The absolute path in `ironkernel.executablePath`
2. `dotnet run --project IronKernel/IronKernel.fsproj --` in an IronKernel workspace
3. `IronKernel` (`IronKernel.exe` on Windows) on `PATH`

Use `ironkernel.projectPath` when the .NET runtime project lives elsewhere in
the workspace. Use `ironkernel.ikprojPath` to pin an IronKernel `.ikproj` for
Run/Build Project; otherwise the extension walks up from the active file, then
searches the workspace (with a picker if several match).
Standalone release binaries must remain beside `kernel.ikr` and `promises.ikr`.
Set `ironkernel.profile` to `minimal`, `safe`, or `unrestricted` to control the
host authority available to editor commands and playground runs.

## Commands

- `IronKernel: Run Current File`
- `IronKernel: Compile Current File to IKC`
- `IronKernel: Run Project` — `run <project.ikproj>` (plus `ironkernel.runArgs`)
- `IronKernel: Build Project` — `build <project.ikproj>` → `bin/*.ikc`
- `IronKernel: Open Playground`
- `IronKernel: Show Output`

Right-click a `.ikproj` in the explorer for Run/Build Project as well.

The playground executes arbitrary IronKernel and therefore arbitrary permitted
.NET operations. Execution is disabled in Restricted Mode. It uses direct
process argument arrays, never a command shell, and limits source size, output,
and execution time.

## `.ikr` association

`.ikr` is IronKernel's primary source extension and is registered automatically.
Legacy `.scm` files are intentionally not claimed because doing so conflicts
with Scheme tooling. To edit a legacy file as IronKernel, add:

```json
{
  "files.associations": {
    "*.scm": "ironkernel"
  }
}
```
