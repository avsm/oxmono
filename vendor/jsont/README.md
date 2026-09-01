# Jsont 0.4.0 for OxCaml

This directory is based on upstream Jsont `v0.4.0`, commit
`f99b5cf4b3ff37d16af897998bab115cf06cba43`. The four core files and the two
files under `bytesrw/` were copied from that release before applying the local
OxCaml port.

The 0.4 release's optional `jsont.typegist` library is not included because
this workspace neither vendors nor uses Typegist. The optional Brr codec,
command-line tools, upstream documentation, and upstream tests are also out of
scope. The local Dune files build `jsont` and `jsont.bytesrw`.

## Upstream changes now included

Moving from the previous 0.2 snapshot brings in the corrected exact-integer
interchange range and migration codecs, `Json.remove_mem`, the fix for decoding
case objects whose members precede their tag, and all other 0.3 and 0.4 core
fixes.

## OxCaml port

`'a Jsont.t` is a `portable contended` value. A description can be defined
once at module scope, captured by a portable route, and safely used by several
domains. Its representation enforces that contract:

- Every callback retained by a description is required at `portable`.
- Constants retained by `null`, `const`, setters, enums, and case tags are
  additionally required to be safe at `contended`.
- Ordinary missing-member defaults, query defaults, `todo` stubs, and fold
  initial values are portable `unit -> value` factories. Each decode can
  therefore construct fresh mutable output rather than sharing it between
  domains. An absent case discriminator remains a portable, contention-safe
  constant because it selects a fixed branch.
- Recursive descriptions use `Base.Portable_lazy`; the generic JSON codec can
  be forced concurrently.
- Descriptor maps use immutable `Base.Map.Poly` values through
  `Jsont.String_map`, including Base's portable, value-restriction-free
  polymorphic empty map.
- Generative type witnesses use process-unique atomic integer identifiers.
- Array and bigarray encoding loops use OxCaml `let mutable`, avoiding escaping
  closures around mutable iteration state.

`Object.map` still accepts ordinary immutable constants for empty objects. A
map that constructs mutable output should start with a portable constructor
function, as record-shaped maps normally do. `enum` deliberately scans its
immutable association list in linear time; this avoids embedding an
arbitrary-key comparator structure in a portable description. In particular,
Base maps only cross portability when the entire key and value *types* have
portable kinds, while `enum` remains more general by accepting specific
portable values of any contention-safe type. A private immutable-tree probe
showed that logarithmic lookup is possible without a cast, but duplicating a
balanced-tree implementation was not judged worthwhile for this descriptor
constructor.

## Audited unsafe boundaries

The core has one portability assertion: the compiler-created `format6`
constant for `"%.17g"` is immutable, but the standard library type does not
expose that fact, so it is promoted with `Obj.magic_portable`. The atomic type
identifier implementation uses `Obj.magic Equal` only after two unique
identifiers compare equal; this is the same hidden type-equality proof supplied
by upstream's non-portable first-class generative modules.

The streaming codec also depends on the separately documented, isolated
`Bytes.Slice.eod` portability assertion in `../bytesrw`.

## Validation

`proffer/jsont/test/test_proffer_jsont.ml` checks upstream integer-boundary and
nested-case behaviour, proves mutable defaults are fresh, captures
module-level Jsont media codecs in portable routes, and forces and round-trips
shared generic and typed codecs concurrently on four domains. The workspace
test suite exercises both the Proffer and Fetch adapters.

Run the compiler and tests through the OxCaml switch:

```sh
opam exec --switch=5.2.0+ox -- dune build @all
opam exec --switch=5.2.0+ox -- dune runtest
opam exec --switch=5.2.0+ox -- dune build --profile release-check @all
```

## Re-vendoring

1. Copy `src/jsont.{ml,mli}`, `src/jsont_base.{ml,mli}`, and
   `src/bytesrw/jsont_bytesrw.{ml,mli}` from the new upstream tag.
2. Reapply the OxCaml changes described above, reviewing every newly stored
   callback and value for portability and contention safety.
3. Decide explicitly whether any new optional upstream libraries and
   dependencies belong in this workspace.
4. Update the version, commit, and checked date here, in `jsont.opam`, and in
   the root `VENDORED.md`.
5. Run all three commands in the validation section.
