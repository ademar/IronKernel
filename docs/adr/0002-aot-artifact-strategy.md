# ADR 0002: Ahead-of-time artifact strategy

Status: Accepted

## Decision

IronKernel distinguishes three artifact classes:

- `.ikc` remains a portable source package. It is validated when built and
  parsed, analyzed, and compiled when loaded.
- Managed artifacts contain generated CLR methods and embedded typed values.
  They require a compatible .NET runtime but do not contain or parse the user’s
  Kernel source at startup.
- Native artifacts are RID-specific executables produced from the managed
  artifact model with .NET NativeAOT. They do not require an installed runtime.

The static backend may retain the evaluator for dynamic Kernel semantics such
as operatives, first-class environments, and `eval`. Retaining an AOT-compiled
evaluator does not make an artifact source-based: the defining requirements are
that user source is absent and startup performs no parsing, analysis, or dynamic
code generation.

Unsupported constants and Core IR forms fail artifact construction. The backend
must not silently embed user source as a fallback.

## Initial managed milestone

`ik compile program.ikr --managed -o publish` produces a framework-dependent
managed executable. The backend emits literals, variables, quoted values,
statically named combinations, top-level definitions, lazy conditionals, and
sequencing. Named combinations preserve Kernel combiner dispatch through the
runtime, including operative handling of raw operands. Generated binding guards
select specialized `if` and `define` paths while those names retain their
primitive identities, then fall back to raw combination semantics after
rebinding.

Generated projects reference `IronKernel.Runtime`, which owns values,
environments, evaluation, primitive operations, capabilities, and generated safe
CLR bindings. Parser-backed `load`, `read`, and `read-all` services are injected
by the full tool and are absent from managed artifact dependencies. Managed
outputs therefore exclude the compiler assembly and FParsec.

Artifact construction analyzes `kernel.ikr` and `promises.ikr` before the user
program and emits their forms as ordinary generated startup functions. Runtime
initialization evaluates those functions in the primitive environment without
loading or parsing standard-library source files.

## NativeAOT progression

The `minimal` profile can be published as a NativeAOT executable with an explicit
RID. Runtime primitive tables use concrete lists rather than generic maps of
curried functions to avoid NativeAOT generic expansion. FSharp.Core still emits
trim-analysis warnings for its structured formatting and reflection internals;
the generated application does not promote those framework warnings to errors.

On macOS, the publisher stages static Homebrew OpenSSL and Brotli archives for
linking. The resulting executable depends only on operating-system libraries and
frameworks, not Homebrew dylibs.

Remaining progression:

1. Replace remaining unrestricted reflection paths with generated bindings or
   explicit preservation metadata.
2. Remove dynamic expression compilation from compiler-hosted execution paths
   on all generated startup paths.
3. Extend NativeAOT publishing to the `safe` profile.
4. Require generated binding manifests or preservation metadata for unrestricted
   CLR reflection.

Managed and native artifacts retain source names, spans, and relevant source
lines as metadata for diagnostics, but not complete source payloads.

## Consequences

`.ikc` stays architecture-neutral and suitable for source distribution. Managed
and native outputs are deployment artifacts rather than dependency packages.
Dynamic language features remain available through statically linked runtime
services, while unsupported host reflection is diagnosed during native publish
instead of failing unpredictably after deployment.