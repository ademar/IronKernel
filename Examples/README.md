# IronKernel examples

Run an example from the repository root:

```bash
dotnet run --project IronKernel -- Examples/hello.scm
```

The CLI loads `kernel.scm` and `promises.scm` before each script. Compile any
finite example to an `.ikc` package with:

```bash
dotnet run --project IronKernel -- compile Examples/hello.scm -o hello.ikc
dotnet run --project IronKernel -- run hello.ikc
```

| Example | Demonstrates | Notes |
|---|---|---|
| `hello.scm` | Environments, `lambda`, and CLR output | Prints `Hello,world!` |
| `safe-clr.scm` | Generated, allowlisted CLR bindings | Run with `--profile safe` |
| `vau-dotnet.scm` | User-defined control forms and .NET interop | Guided feature tour |
| `samples.scm` | Recursion and the `let` family | Prints intermediate values |
| `continuations.scm` | Full continuations | Defines local output helpers |
| `effects-async.scm` | Tagged deep handlers and CLR Task suspension | Requires `unrestricted` profile |
| `coroutines.scm` | Cooperative scheduling experiment | Historical example; not yet covered by the compatibility suite |
| `zipper.scm` | Delimited continuations | Functional zipper traversal |
| `sqrt.scm` | Operative-based numeric procedure | Defines `sqrt`; intentionally produces no output |
| `yingyang.scm` | Classic yin-yang continuation loop | Intentionally non-terminating; do not use it to test packaging |

Errors identify the source file and failing range:

```text
Examples/demo.scm:2:1: Getting an unbound variable: 'missing'
(missing 42)
^^^^^^^^^^^^
```
