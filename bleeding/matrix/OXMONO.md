# OxMono import

Imported `f150bc14804d8b9263c9cdecdf42967d6ec3f0b4` from `/home/avsm2/src/git/knot/ocaml-matrix`.
[oxmono/upstream.json](oxmono/upstream.json) records the repository and base.
The upstream layout, package name `matrix-chat`, examples and tests are retained.
The source checkout is not modified.

## Local adaptations

The build targets OxCaml 5.2.0+ox and the monorepo's Jsont 0.4 API.
Dune opens a private compatibility module with the OCaml 5.5 List, Option and
Result helpers used by upstream. JSON defaults use factories and retained
callbacks have checked portable interfaces. Portable map functors and a few
eta expansions preserve the original computations.

The implementation, public interfaces, examples and tests use Uriz directly.
`compat/matrix_compat.ml` contains only compiler compatibility helpers.
Path arguments use segment encoding. Query bindings use Uriz's ordered pairs,
with independent key/value encoding and no comma-list convention. OAuth query
and fragment parameters use form decoding exactly once. Invalid URI syntax is
rejected at construction, as a JSON decoding error or as a CLI argument error.
The HTML sanitizer drops malformed URI attributes. Existing reserved escapes
are preserved, including `%2F`.

The OAuth loopback route receives its promises through Proffer's explicit
request environment. Its port and completion flag are atomic. Its listener
still binds loopback and shuts down after the first accepted callback.

The `matrix-chat.proto` package remains independent of HTTPz and Eio.
The vodozemac oracle, Synapse and rendezvous suites remain opt-in. Follow the
integration section of `README.md` and `test/integration/synapse.sh` for the
server fixtures. Set an absolute
`VODOZEMAC_ORACLE` and `MATRIX_REQUIRE_ORACLE=1` when requiring the Rust oracle.
Upstream's standalone `test/release-check.sh` assumes a separate workspace.
Use the scoped commands below in oxmono.

## Build and test

From the oxmono root:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/matrix/all
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check \
  --force bleeding/matrix
```

The full Matrix test dependencies include `qcheck-alcotest.0.91`.
Unset `MATRIX_TEST_HOMESERVER` and `MATRIX_REQUIRE_HOMESERVER` to run offline.

## Update from upstream

1. Read the recorded revision. Review the source checkout's changes since that
   commit. Export them with `git diff --binary BASE..NEW` in that checkout.
2. Check the patch with `git apply --check --directory=bleeding/matrix PATCH`
   from oxmono, then apply it with the same directory prefix. Resolve conflicting
   hunks against `git show BASE:path` in the source checkout, preserving the
   adaptations listed above and the local `compat/` and `oxmono/` directories.
3. Run the scoped build and tests. Dune regenerates the opam file from the
   locally adapted `dune-project`. Update the recorded revision only after the
   complete upstream change has been integrated.

Upstream documentation and CI describe the standalone OCaml 5.5 build.
This file records the monorepo-specific commands and differences.
