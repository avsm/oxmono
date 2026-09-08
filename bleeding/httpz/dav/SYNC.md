# Keeping WebDAV support synchronized

The same DAV implementation is maintained in OxMono, `oxcaml-httpz`, and
`ocaml-httpz`. OxMono's paths have a `bleeding/` prefix; the standalone trees
start directly at `httpz/`, `fetch/` and `proffer/`.

The shared scope is `httpz/dav/`, `fetch/dav/`, `proffer/dav/`,
`fetch/test/webdav/`, and `fetch/WEBDAV.md`. This includes tests, fixtures and
these maintenance notes. The protocol library stays below Fetch; the client
lives in `fetch.dav`; `proffer.dav` only re-exports its types and exceptions.

From OxMono's root, check the default sibling locations with:

```sh
python3 bleeding/httpz/dav/check_sync.py
```

From a standalone checkout, use `python3 httpz/dav/check_sync.py`. All locations
can be supplied explicitly with `--monorepo`, `--oxcaml` and `--ocaml`. The
checker is read-only, compares file sets as well as contents, and exits nonzero
on drift. It also checks the private XML codec against OxMono's `vendor/xmlm`.

The standalone OxCaml DAV files are byte-identical to OxMono's. The stock port
removes floating portability annotations and abstract kind constraints,
converts `Null`/`This` to ordinary options in the two URI-consuming modules, and
uses `Hashtbl.Make`. Its fixture uses stock X509's `chain_of_trust` instead of
the OxCaml-only portable `chain_of_trust_no_crl`, with the same no-CRL policy.
These narrow transformations are explicit in `stock_source` in the checker;
it is not a general-purpose OxCaml-to-OCaml converter.

When changing DAV support:

1. Start with a committed source revision and review the change in the protocol
   layer before its client and Proffer integration. Keep the three layers acyclic.
2. Port the shared code and regressions to both siblings. Keep existing work in
   each checkout intact, and update the private XML copy and the monorepo vendor
   together if changing the codec.
3. Update each project's package dependencies and generated opam files. The
   monorepo has separate Dune projects; standalone repositories share a project.
   These intentionally different root manifests are not copied by the checker.
4. Run the protocol, mock and facade aliases, the package installation targets,
   and the standalone full suites. Use `5.2.0+ox` with `--profile release-check`
   for OxCaml and `5.5.0` for stock OCaml. Run all three clients against the
   disposable Docker fixture as described in the client guide.
5. Run the parity checker before committing. Record each completed port's exact
   revision in the repository synchronization notes; do not advance a broader
   history-review marker for this selective DAV port.

The live Fastmail observations are regression context. Routine port validation
uses the disposable fixture; live-account tests remain explicit opt-in commands.
