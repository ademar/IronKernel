# IronKernel for Visual Studio Code

Language support and a local playground backed by the real IronKernel runtime.
The extension never implements a second evaluator in JavaScript.

## Features

- Syntax highlighting for lists, vectors, improper lists, keywords, `vau`, and CLR interop
- Bracket matching, comments, indentation, and IronKernel snippets
- **Run Current File** and **Compile Current File to IKC** commands
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

Use `ironkernel.projectPath` when the project lives elsewhere in the workspace.
Standalone release binaries must remain beside `kernel.scm` and `promises.scm`.
Set `ironkernel.profile` to `minimal`, `safe`, or `unrestricted` to control the
host authority available to editor commands and playground runs.

## Commands

- `IronKernel: Run Current File`
- `IronKernel: Compile Current File to IKC`
- `IronKernel: Open Playground`
- `IronKernel: Show Output`

The playground executes arbitrary IronKernel and therefore arbitrary permitted
.NET operations. Execution is disabled in Restricted Mode. It uses direct
process argument arrays, never a command shell, and limits source size, output,
and execution time.

## `.scm` association

Other Scheme extensions may also claim `.scm`. If VS Code chooses a different
language mode, add this workspace setting:

```json
{
  "files.associations": {
    "*.scm": "ironkernel"
  }
}
```
