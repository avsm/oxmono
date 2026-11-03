# OxMono import

Imported ocaml-stdint 0.7.2 from its Git tag. `lib/`, the package file,
licence, changelog and README are unchanged. Tests, the code generator and
specs are omitted. Capnp needs it for fixed-width integers.

On refresh, compare `lib/` with the new tag and build
`vendor/stdint/lib/stdint.cmxa` with the OxCaml switch.
