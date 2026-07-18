# IronKernel.Sdk

MSBuild conventions for `.ikproj` projects. The SDK supplies `.ikr` source
collection, lock-file restore defaults, package layout, main-file validation,
and IronKernel project properties.

Published on NuGet.org as
[`IronKernel.Sdk`](https://www.nuget.org/packages/IronKernel.Sdk). Projects can
reference it:

```xml
<Project Sdk="IronKernel.Sdk/0.1.0">
```

`ik new` currently emits a `Microsoft.NET.Sdk`-based project with the same
properties for maximum restore compatibility; you can switch the `Sdk=` attribute
to `IronKernel.Sdk/<version>` when you want the packaged conventions.
