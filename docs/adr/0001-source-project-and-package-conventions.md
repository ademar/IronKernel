# ADR 0001: IronKernel source, project, and package conventions

Status: Accepted

## Decision

- `.ikr` is the primary IronKernel source extension.
- `.ikc` remains the compiled source-package extension.
- `.ikproj` is the MSBuild-compatible project format.
- `ik` is the project command exposed by the `IronKernel.Tool` .NET tool.
- IronKernel and CLR dependencies use NuGet packages and feeds.
- `packages.lock.json` is committed; CI restores in locked mode.
- NuGet.org is the initial public registry. `ironkernel.org` may provide a
  curated catalog without operating separate package storage.

## Rationale

`.scm` causes persistent Scheme language-mode ambiguity. `.ik` already belongs
to Ioke, `.krn` is shared with strict Kernel and Humdrum music, and `.ikn`
belongs to IKinema. `.ikr` has no programming-language registration and is
short, brand-specific, and distinct from `.ikc`.

Reusing MSBuild and NuGet provides transitive dependency resolution, private
feeds, package-source mapping, credentials, signatures, content hashes, and
repeatable lock files. IronKernel should not implement an independent resolver
or registry before its requirements differ materially from NuGet.

## Compatibility

The CLI continues to execute `.scm` paths during the transition and emits a
deprecation warning. New projects, runtime libraries, examples, releases, and
editor associations use `.ikr`.
