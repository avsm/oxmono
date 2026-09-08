# Uunf 17.0.0

The core and string normalization modules come from upstream tag `v17.0.0`,
commit `90d53ea98504631e01cc76685cad1dffd3a42e5c`. The initial source was the
opam switch's `uunf.17.0.0` archive. This is a release snapshot, not main's tip.
[../upstreams.json](../upstreams.json) records the base for future updates.

## OxCaml changes

- Generated lookup arrays, including decomposition and composition entries,
  are immutable arrays. The data values are unchanged.
- Private trie modules retain only types, constants and lookup operations.
  Generator-only builders and formatters are omitted.
- The normalization algorithm reads immutable entries. `Uunf.decomp` copies
  an entry to preserve its public `int array` result without exposing a table.
  Each streaming normalizer still owns its mutable accumulator.
- Public functions are portable. Dune builds the unwrapped `uunf` library,
  including `Uunf_string`. The optional `unftrip` executable is omitted.

Portable URL codecs in Fetch and Matrix need this port through HTTPz's IDNA
normalization. Compiler checking verifies the portability annotations.

## Verify and update

Run the differential against a pristine upstream 17.0.0 checkout or archive:

```sh
opam exec --switch=5.2.0+ox -- python3 vendor/uunf/check-port.py UPSTREAM
```

It builds both implementations in a temporary workspace and compares all four
normalization forms across every Unicode scalar and 20,000 mixed sequences.
The initial port passed 84,356 comparisons. It does not modify either tree.

Consumer regression tests run outside the inert vendor aliases:

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check \
  --force bleeding/httpz/test bleeding/fetch/lib/tests
```

When updating, import the complete core snapshot, repeat the immutable-table
conversion, preserve the mutable public return type of `decomp`, and rerun
the differential against that pristine version. Update the version and
manifest only after the comparison and consumer tests pass.
