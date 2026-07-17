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
| `Ir.fs` / `Analyze.fs` | Core IR + analysis |
| `Eval.fs` | Trampolined CPS interpreter (TCO-capable) |
| `Compiler.fs` | Expression-tree hybrid compiler |
| `Emit.fs` | IKC package emit / load |
| `Runtime.fs` | Primitive operatives & applicatives |
| `kernel.scm` | Stdlib (`lambda`, `let`, modules, …) |

## License

Apache 2.0 — see [COPYING](COPYING).
