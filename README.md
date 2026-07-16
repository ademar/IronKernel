# IronKernel

IronKernel is a dialect of [John N. Shutt’s Kernel](http://web.cs.wpi.edu/~jshutt/kernel.html) for **.NET 10**.

Kernel is Scheme-like but more homoiconic: combiners and environments are first-class, and the core abstraction is the operative (`vau`) rather than macros.

This tree is a **hybrid CLR compiler**: programs are analyzed to a Core IR, lowered through Expression trees where safe, and fall back to a trampolined CPS interpreter for full Kernel semantics (`vau`, first-class `eval`/environments, `call/cc`, `shift`/`reset`).

## Build & test

```bash
dotnet build
dotnet test
```

Requires the .NET 10 SDK.

### CI & releases

- **CI** (`.github/workflows/ci.yml`) runs `dotnet test` on Ubuntu for pushes/PRs to `main`/`master`.
- **Release** (`.github/workflows/release.yml`) triggers on tags `v*` (e.g. `v0.2.0`): tests on Linux, then publishes self-contained single-file binaries for `linux-x64`, `win-x64`, `osx-arm64`, and `osx-x64`, attached as `ironkernel-<rid>.tar.gz` (binary + `kernel.scm` / `promises.scm`).

## REPL

```bash
dotnet run --project IronKernel
```

Loads `kernel.scm` and `promises.scm`, then presents an interactive prompt. Type `quit` to exit.

## Compile to an IKC package

```bash
dotnet run --project IronKernel -- compile path/to/program.scm -o program.dll
```

Writes an **IKC1** package (Kernel source payload + in-process `Reflection.Emit` runner type). Reload with the runtime helpers in `IronKernel.Emit`.

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
