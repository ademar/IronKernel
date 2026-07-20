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
- **Release** (`.github/workflows/release.yml`) triggers on tags `v*` (e.g. `v0.3.0`): tests on Linux, then publishes self-contained single-file binaries for `linux-x64`, `win-x64`, `osx-arm64`, and `osx-x64`, attached as `ironkernel-<rid>.tar.gz` (binary + `kernel.ikr` / `promises.ikr`).

### Website

The promotional site and docs live in [`website/`](website/) and target **[ironkernel.org](https://ironkernel.org)** (`Iron` = *It runs on .NET*; the `.net` TLD was taken).

```bash
# local preview
python3 -m http.server -d website 8080
```

GitHub Pages deploys from `website/` via `.github/workflows/pages.yml` (enable Pages → “GitHub Actions” in repo settings, then point the domain’s DNS at GitHub).

## Getting started

**Preferred:** install the `ik` global tool from NuGet.org (requires the
[.NET 10 SDK](https://dotnet.microsoft.com/download)), then use the CLI directly.
Self-contained binaries are also on
[GitHub Releases](https://github.com/ironkernel-lang/IronKernel/releases).
Full steps and VS Code runtime discovery are in the
[getting-started guide](website/docs/getting-started.html).

```bash
dotnet tool install -g IronKernel.Tool
# Ensure ~/.dotnet/tools is on PATH
ik --version
ik new app hello
```

Packages on NuGet.org:
[`IronKernel.Tool`](https://www.nuget.org/packages/IronKernel.Tool) (`ik`) and
[`IronKernel.Sdk`](https://www.nuget.org/packages/IronKernel.Sdk) (MSBuild SDK for
`.ikproj`). If `dotnet tool install` reports a missing `DotnetToolSettings.xml`,
install or select a .NET 10 SDK — that error is the usual symptom of an older SDK
trying to install a `net10.0` tool.

For contributors building this repository:

```bash
dotnet build
dotnet run --project IronKernel -- Examples/hello.ikr
```

See [`Examples/README.md`](Examples/README.md) for runnable programs.

## Projects and packages

`.ikproj` files are MSBuild-compatible IronKernel projects. Use the `ik` tool
(or the same subcommands on the `IronKernel` release binary):

```bash
ik new app hello
cd hello
ik run
ik test
ik restore
ik add Acme.IronKernel.Http 1.2.0
ik add Npgsql 9.0.0 --clr
ik tree
ik build
ik pack
```

Projects use standard NuGet `PackageReference` entries and commit
`packages.lock.json`. Restored IronKernel package sources under
`ironkernel/src/**/*.ikr` load before project source; declared CLR runtime
assemblies are loaded for interop.

Public packages use NuGet.org initially. See
[`docs/packages.md`](docs/packages.md) for package layout and
[`ADR 0001`](docs/adr/0001-source-project-and-package-conventions.md) for the
extension and ecosystem decision.

## REPL

```bash
ik
# or: IronKernel
```

Loads `kernel.ikr` and `promises.ikr`, then presents an interactive prompt. Type `quit` to exit.

## Run a script

```bash
ik path/to/program.ikr arg1 arg2
# Equivalent explicit form:
ik run path/to/program.ikr arg1 arg2
```

Script mode loads `kernel.ikr` and `promises.ikr` in a fresh environment, then
binds command-line arguments to `args`. Evaluation and startup errors are written
to stderr and produce a non-zero exit code.

## Compile to an IKC package

```bash
ik compile path/to/program.ikr -o program.ikc
ik run program.ikc
```

Compilation validates and packages the source without executing it. An **IKC1**
file is an IronKernel package containing the source payload; it is not a CLR
assembly. At run time IronKernel loads the standard library, compiles the payload
to delegates, and executes it. Omitting `-o` writes `<source-name>.ikc`.

Use `--help` for all commands and `--version` for the runtime version.

## Diagnostics

Parse and runtime failures include the source path, line and column, offending
line, and a caret range. CLI modes write diagnostics to stderr and return a
non-zero exit code. Compiled source reports the narrowest retained span, such as
an unbound operator or the selected branch of a guarded `if`:

```text
program.ikr:2:2: Getting an unbound variable: 'missing'
(missing 42)
 ^^^^^^^
```

Syntax constructed as a runtime `LispVal` and passed to `eval` has no original
source span. Errors from such code use the nearest enclosing source location
when one is available.

## Capability profiles and generated CLR bindings

IronKernel can construct root environments with different host authority:

| Profile | Host access |
|---|---|
| `minimal` | Kernel evaluation and data primitives only |
| `safe` | Minimal profile plus reviewed generated CLR wrappers |
| `unrestricted` | Raw CLR reflection, source loading, and host I/O (default) |

```bash
ik --profile safe path/to/safe-clr.ikr
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

## Tagged handlers and async

Prompt tags are unforgeable runtime values. Tagged `shift` selects the nearest
matching `reset`, while `prompt` installs a deep effect handler:

```scheme
(define request (make-prompt-tag))

(prompt request
  (lambda (value k) (resume k (+ value 1)))
  (+ 1 (perform request 40)))
; ⇒ 42
```

`perform` supplies the operation value and a one-shot resumption to the handler.
Resuming reinstalls the same tagged handler; attempting to resume twice is an
error. Existing untagged `(reset body)` and `(shift handler)` remain multi-shot
and backward compatible.

The unrestricted profile also provides `(task-delay milliseconds value)` and
`(await-task task)`. Task callbacks only publish an outcome; `Eval.runAsync`
resumes the trampoline serially rather than evaluating on a CLR callback thread.
See [`Examples/effects-async.ikr`](Examples/effects-async.ikr).

## Operative contracts and partial evaluation

Contracts are optional combiner metadata. They distinguish raw operative
operands from evaluated applicative arguments and validate fixed value shapes:

```scheme
(define double (lambda (x) (+ x x)))
(contract double applicative (number) number pure #t)

(define raw (vau operands _ operands))
(contract raw operative (any) any pure #t)
```

Supported shapes are `any`, `number`, `integer`, `string`, `boolean`, `atom`,
`list`, `prompt-tag`, and `resumption`. The final fields declare the effect
summary (`pure` or `effectful`) and whether the combiner is intended to be
inlineable.

User contracts are asserted metadata and are never executed by the compiler.
Reviewed primitive contracts are certified; pure literal calls such as
`(+ 20 22)` may be folded behind a binding-cell/version/structural contract snapshot
guard. Rebinding the operator selects the untouched generic combination before
any operand effects occur. Dynamic `eval`, control effects, CLR calls, and async
operations remain residual.

See [`Examples/contracts.ikr`](Examples/contracts.ikr).

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
runtime discovery and `.ikr` file-association guidance.

## CLR namespaces and Clojure-style calls

Under the `unrestricted` profile you can open namespaces and use short type names:

```scheme
(clr-open System System.IO)
(clr-alias SB System.Text.StringBuilder)

(Guid/NewGuid)                 ; static method
(StringBuilder.)               ; constructor
(.Append sb "hi")              ; instance method
(.-Length sb)                  ; field / property
(clr-type Path)                ; first-class System.Type value
```

Full names such as `System.Console` remain valid. Ambiguous short names after
`clr-open` raise an error; use `clr-alias` or a full name to disambiguate.

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
| `kernel.ikr` | Stdlib (`lambda`, `let`, modules, …) |

Compiler fast paths are guarded by stable binding-cell identity and version.
Rebinding or shadowing a primitive invalidates its guard before any specialized
work begins, so execution falls back to ordinary Kernel combiner dispatch.

## License

Apache 2.0 — see [COPYING](COPYING).
