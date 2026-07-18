# IronKernel packages

IronKernel packages are ordinary NuGet dependency packages tagged
`ironkernel`. A package may contain both IronKernel source and CLR assemblies.

## Layout

```text
package.nupkg
├── ironkernel/package.json
├── ironkernel/src/**/*.ikr
├── ironkernel/lib/**/*.ikc
├── ironkernel/bindings/*.json
├── lib/net10.0/*.dll
└── buildTransitive/*.targets
```

The project loader reads restored `project.assets.json`, loads declared CLR
runtime assemblies, then evaluates `ironkernel/src/**/*.ikr` in deterministic
path order before project sources.

`ironkernel/package.json` may declare entry modules, exports, required runtime
version, capability requirements, and generated CLR binding manifests. The
initial `ik pack` command packages project sources and NuGet dependencies; a
schema-backed package manifest is the next format revision.

## Repositories and repeatability

- Public packages are published to NuGet.org.
- Private packages use standard NuGet feeds and `NuGet.config`.
- Projects enable `RestorePackagesWithLockFile`.
- Commit `packages.lock.json`.
- CI uses `ik restore --locked`.
- Use NuGet package-source mapping to prevent dependency confusion.

Custom NuGet package types are intentionally avoided because ordinary
Visual Studio and NuGet installation paths do not support them consistently.
