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
managed executable. The initial backend emits literals, variables, quoted
values, statically named combinations, and top-level sequencing. Named
combinations preserve Kernel combiner dispatch through the runtime, including
operative handling of raw operands.

The generated project currently references the installed compiler assembly and
its managed dependencies. A dedicated runtime library package will replace this
temporary coupling before the managed artifact format is declared stable.

## NativeAOT progression

1. Split parser/compiler/tooling from a trim-safe runtime library.
2. Replace reflection-based helper discovery and dynamic expression compilation
   on all generated startup paths.
3. Compile and embed standard-library initialization rather than copying `.ikr`
   files.
4. Publish `minimal` and `safe` profiles with `PublishAot=true` and explicit RID
   selection.
5. Require generated binding manifests or preservation metadata for unrestricted
   CLR reflection.

Managed and native artifacts retain source names and spans as metadata for
diagnostics, but not complete source payloads.

## Consequences

`.ikc` stays architecture-neutral and suitable for source distribution. Managed
and native outputs are deployment artifacts rather than dependency packages.
Dynamic language features remain available through statically linked runtime
services, while unsupported host reflection is diagnosed during native publish
instead of failing unpredictably after deployment.