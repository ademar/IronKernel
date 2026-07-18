# IronKernel.Sdk

MSBuild conventions for `.ikproj` projects. The SDK supplies `.ikr` source
collection, lock-file restore defaults, package layout, main-file validation,
and IronKernel project properties.

Projects can use the published SDK:

```xml
<Project Sdk="IronKernel.Sdk/0.1.0">
```

During repository development, `ik new` emits a `Microsoft.NET.Sdk`-based
project with the same properties so restore works before this SDK is published.
