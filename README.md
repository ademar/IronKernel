# IronKernel

IronKernel is a dialect of [John N. Shutt’s Kernel](http://web.cs.wpi.edu/~jshutt/kernel.html) for **.NET 10**.

Kernel is Scheme-like but more homoiconic: combiners and environments are first-class, and the core abstraction is the operative (`vau`) rather than macros.

This tree is a **hybrid CLR runtime**: programs are analyzed to a Core IR and compiled to CLR delegates while preserving runtime combiner dispatch, with a trampolined CPS interpreter for full Kernel semantics (`vau`, first-class `eval`/environments, `call/cc`, `shift`/`reset`).

## Build & test

```bash
dotnet build
dotnet test
```

Requires the .NET 10 SDK.

### CI & releases

- **CI** (`.github/workflows/ci.yml`) runs `dotnet test` on Ubuntu for pushes/PRs to `main`/`master`.
- **Release** (`.github/workflows/release.yml`) triggers on tags `v*` (e.g. `v0.2.0`): tests on Linux, then publishes self-contained single-file binaries for `linux-x64`, `win-x64`, `osx-arm64`, and `osx-x64`, attached as `ironkernel-<rid>.tar.gz` (binary + `kernel.scm` / `promises.scm`).

### Website

The promotional site and docs live in [`website/`](website/) and target **[ironkernel.org](https://ironkernel.org)** (`Iron` = *I run on .NET*; the `.net` TLD was taken).

```bash
# local preview
python3 -m http.server -d website 8080
```

GitHub Pages deploys from `website/` via `.github/workflows/pages.yml` (enable Pages → “GitHub Actions” in repo settings, then point the domain’s DNS at GitHub).

## Getting started

With the .NET 10 SDK:

```bash
dotnet build
dotnet run --project IronKernel -- Examples/hello.scm
```

For a release archive, extract it and run `./IronKernel` (`IronKernel.exe` on
Windows) from the extracted directory. The archive includes the standard
library files required by the runtime.

See the [getting-started guide](website/docs/getting-started.html) for the full
REPL, script, and package workflow, and [`Examples/README.md`](Examples/README.md)
for runnable programs.

## REPL

```bash
dotnet run --project IronKernel
```

Loads `kernel.scm` and `promises.scm`, then presents an interactive prompt. Type `quit` to exit.

## Run a script

```bash
dotnet run --project IronKernel -- path/to/program.scm arg1 arg2
# Equivalent explicit form:
dotnet run --project IronKernel -- run path/to/program.scm arg1 arg2
```

Script mode loads `kernel.scm` and `promises.scm` in a fresh environment, then
binds command-line arguments to `args`. Evaluation and startup errors are written
to stderr and produce a non-zero exit code.

## Compile to an IKC package

```bash
dotnet run --project IronKernel -- compile path/to/program.scm -o program.ikc
dotnet run --project IronKernel -- run program.ikc
```

Compilation validates and packages the source without executing it. An **IKC1**
file is an IronKernel package containing the source payload; it is not a CLR
assembly. At run time IronKernel loads the standard library, compiles the payload
to delegates, and executes it. Omitting `-o` writes `<source-name>.ikc`.

Use `--help` for all commands and `--version` for the runtime version.

## Diagnostics

Parse and runtime failures include the source path, line and column, offending
line, and a caret range. CLI modes write diagnostics to stderr and return a
non-zero exit code:

```text
program.scm:2:1: Getting an unbound variable: 'missing'
(missing 42)
^^^^^^^^^^^^
```

## Capability profiles and generated CLR bindings

IronKernel can construct root environments with different host authority:

| Profile | Host access |
|---|---|
| `minimal` | Kernel evaluation and data primitives only |
| `safe` | Minimal profile plus reviewed generated CLR wrappers |
| `unrestricted` | Raw CLR reflection, source loading, and host I/O (default) |

```bash
dotnet run --project IronKernel -- --profile safe Examples/safe-clr.scm
```

Safe wrappers such as `Console.write-line`, `String.concat`, and `Math.sqrt`
are generated from [`manifests/safe-clr-bindings.json`](manifests/safe-clr-bindings.json).
The generator resolves one exact public static signature and emits direct typed
calls—there is no runtime overload selection or reflection in the generated
path:

```bash
dotnet fsi tools/generate-clr-bindings.fsx \
  manifests/safe-clr-bindings.json IronKernel/Generated/Bindings.Safe.fs
```

Child environments intersect their parents' capability sets. Imported or stolen
interop values still check the authority of the environment where they are
invoked, so copying a binding cannot grant host access. See
[`docs/capabilities.md`](docs/capabilities.md) for the security model and limits.

## VS Code extension and playground

The extension in [`editors/vscode/`](editors/vscode/) provides IronKernel syntax
highlighting, snippets, run/compile commands, Problems diagnostics, and a
playground backed by the real CLI. Build a local VSIX with:

```bash
cd editors/vscode
npm install
npm run package
```

The playground requires a trusted workspace because IronKernel programs can
invoke .NET APIs. See the [extension README](editors/vscode/README.md) for
runtime discovery and `.scm` file-association guidance.

## Syntax

IronKernel keeps a **LISP / Kernel S-expression surface** (parentheses are intentional). Notable surface forms:

| Form | Meaning |
|------|---------|
| `(…)` | Lists / combinations |
| `[…]` | Vectors |
| `(a & b)` | Improper / dotted lists (`&` instead of `.`) |
| `:keyword` | Keywords |
| `#t` `#f` `#inert` | Booleans and inert |
| `λ` / `ϝ` | Aliases for `lambda` / `vau` |

## Architecture

| Module | Role |
|--------|------|
| `Parser.fs` | FParsec → homoiconic `LispVal` |
| `Ir.fs` / `Analyze.fs` | Core IR + binding-aware guarded analysis |
| `Eval.fs` | Trampolined CPS interpreter (TCO-capable) |
| `Compiler.fs` | Guarded Expression-tree compiler with generic fallback |
| `Emit.fs` | IKC package emit / load |
| `Runtime.fs` | Primitive operatives & applicatives |
| `kernel.scm` | Stdlib (`lambda`, `let`, modules, …) |

Compiler fast paths are guarded by stable binding-cell identity and version.
Rebinding or shadowing a primitive invalidates its guard before any specialized
work begins, so execution falls back to ordinary Kernel combiner dispatch.

## License

Apache 2.0 — see [COPYING](COPYING).
