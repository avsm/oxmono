# Jsonm 1.0.2

Imported `src/jsonm.ml` and `src/jsonm.mli` from the pristine opam 1.0.2
archive. Upstream tag `v1.0.2` resolves to
`6e32959c508bba67e6e4e773ca8160b4723f3ee0` in
https://github.com/dbuenzli/jsonm. JMAP uses its token stream to validate I-JSON.

## Local changes

Minimal Dune and opam metadata replace the upstream build. `pp_error`,
`decoder`, `decode` and `decoded_range` expose compiler-checked portable
operations. Shared polymorphic variant constants have closed type annotations.
The internal decode type aliases move before those annotations. No decoding
or encoding algorithm changes.

## Refresh and verify

Copy the two source files from the recorded release or a reviewed newer base.
Reapply only the annotations above and update `../upstreams.json`.
Run the differential against pristine source and JMAP's consumer tests.
Vendor test aliases are inert in this workspace.

```sh
opam exec --switch=5.2.0+ox -- python3 vendor/jsonm/check-port.py UPSTREAM
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check bleeding/jmap
```
