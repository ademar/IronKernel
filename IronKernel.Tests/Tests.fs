module IronKernel.Tests.AssemblyInfo

open Xunit

// Kernel environments are mutable; keep the suite single-threaded.
[<assembly: CollectionBehavior(DisableTestParallelization = true)>]
do ()
