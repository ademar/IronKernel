# Iron Lantern

A small **multi-file IronKernel project** that serves HTTP with .NET's
`HttpListener`. It shows how larger programs are structured: ordered source
files, a declared entry point, CLR interop, and Kernel-native control forms.

```text
lantern/
  lantern.ikproj          ← project file (entry + profile + source globs)
  src/
    10-prelude.ikr        ← clr-open, helpers, guestbook cells
    20-markup.ikr         ← HTML DSL (vau)
    30-router.ikr         ← dispatch + tagged not-found effect
    40-pages.ikr          ← route handlers
    50-http.ikr           ← accept loop
    main.ikr              ← entry point (always loaded last)
  test/
    router_test.ikr       ← routing checks (see below)
```

## How projects work

1. **`.ikproj`** declares:
   - `IronKernelMain` — entry file (`src/main.ikr`)
   - `IronKernelSource` — glob of modules (`src/**/*.ikr`)
   - `IronKernelProfile` — capability profile (`unrestricted` here, for raw CLR)
2. **Load order:** stdlib → NuGet package sources → project files **sorted by
   path** → **main last**. Numbered prefixes (`10-`, `20-`, …) keep dependencies
   ahead of dependents.
3. **One shared environment:** later files see earlier `define`s. There is no
   per-file import; use `provide!` / `import!` when you want Kernel modules.
4. **`args`** is bound by `ik run` to the CLI argument list (CLR strings).

## Run

From the repository root (uses the local tool build):

```bash
# Forever on http://127.0.0.1:8742/
dotnet run --project IronKernel -- run Examples/lantern/lantern.ikproj

# Custom port
dotnet run --project IronKernel -- run Examples/lantern/lantern.ikproj 9090

# Serve three requests then exit (handy for scripts)
dotnet run --project IronKernel -- run Examples/lantern/lantern.ikproj 8742 3
```

If you have the `ik` tool installed:

```bash
cd Examples/lantern
ik run
ik run -- 9090
ik build          # → bin/IronLantern.ikc
```

Then open [http://localhost:8742/](http://localhost:8742/) or
[http://127.0.0.1:8742/](http://127.0.0.1:8742/) (both are registered), try
`/health`, `/time`, and sign the guestbook.

## What it showcases

| Piece | Where |
|-------|--------|
| Project entry + source glob | `lantern.ikproj`, `main.ikr` |
| `clr-open` + Clojure-style calls | `10-prelude.ikr`, `50-http.ikr` |
| HTML DSL via `vau` | `20-markup.ikr` |
| Tagged `prompt` / `perform` for 404 | `30-router.ikr` |
| Contracts | `str=?` in `10-prelude.ikr` |
| Mutable cells (`vector-set!`) | guestbook + hit counter |
| .NET `HttpListener` | `50-http.ikr` |

## Tests

`ik test` loads `main.ikr` before test files, which would block on
`GetContext`. Router checks are therefore run as a **one-shot program** that
reuses the modules by evaluating them in a harness, or manually in the REPL
after loading sources.

Quick smoke (serves one request):

```bash
# terminal A
dotnet run --project IronKernel -- run Examples/lantern/lantern.ikproj -- 18770 1

# terminal B
curl -s http://127.0.0.1:18770/health
```
