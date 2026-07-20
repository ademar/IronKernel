# ADR 0003: Portable Core package format

Status: Accepted

## Decision

`.ikc` files use the IKC2 format: an architecture-neutral serialization of
IronKernel Core IR and typed Kernel constants. Package construction parses and
analyzes source once. Package loading decodes Core, compiles it to runtime
delegates, and executes it without parsing or analyzing the packaged program
source. Standard-library initialization remains a separate runtime concern.

IKC2 uses an explicit binary schema rather than CLR object serialization. The
format begins with the ASCII magic `IKC2`, followed by a form count and tagged
Core nodes. Numeric fields use the fixed BinaryReader/BinaryWriter little-endian
representation. Strings are length-prefixed UTF-8. Constants have explicit tags
for atoms, lists, dotted lists, vectors, booleans, inert, nil, keywords, null,
strings, bytes, 32-bit and 64-bit integers, and single- and double-precision
floating-point values.

The package stores source offsets, lines, columns, and relevant source lines for
diagnostics. It does not store the build-machine source path; the loader assigns
the `.ikc` path as the diagnostic source name. This metadata and raw operand
syntax mean IKC2 is not an obfuscation or source-confidentiality format.

Optimization guards do not serialize binding cell IDs or versions. They store
only the primitive name and identity, then rehydrate a fresh guard against the
load-time environment. Contract folds serialize their unfused fallback because
contract certificates are runtime binding identities rather than portable data.

The decoder rejects unknown tags, trailing bytes, oversized strings and
collections, and Core or value nesting deeper than 256 levels. Runtime objects,
environments, continuations, delegates, ports, and arbitrary CLR objects cannot
be serialized as package constants.

## Compatibility

IKC1 embedded the original UTF-8 source payload. IKC2 intentionally replaces
that contract. IKC1 packages fail with an explicit instruction to rebuild; the
loader does not retain a source-parser compatibility path.

Future incompatible schema changes require a new magic/version and an explicit
migration decision. Readers must reject unknown versions rather than guessing a
layout.

## Consequences

Packages remain portable across operating systems, processor architectures, and
compatible IronKernel runtimes. Startup avoids parsing and analyzing the packaged
program while retaining runtime compilation, dynamic Kernel semantics,
standard-library initialization, command-line arguments, guarded specialization,
and located errors.

The Core schema is now a persisted compatibility boundary. Changes to Core must
either map to existing portable tags, introduce a new package version, or lower
to a supported representation before serialization.