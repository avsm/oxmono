# Vendored components

This file covers libraries embedded inside the HTTPz source tree. The separate
TLS/X.509 dependency closure under the repository-level `vendor/` directory,
and its synchronization with `oxcaml-httpz`, are documented in
[`HTTPZ_SYNC.md`](../../HTTPZ_SYNC.md).

Three libraries under `httpz/` began life as separate projects and were copied
into this tree rather than depended on. They are maintained here now, so an
upstream security fix will not reach this repository on its own: check the
sources below when auditing.

The Public Suffix List *data* is different. It is not a fork but a periodic
snapshot of an upstream file, and it records exactly which snapshot it is.

## Provenance

Everything below is what the repository itself substantiates. The import
predates any vendoring metadata: there is no submodule, no subtree, and no
recorded upstream revision, so the **upstream commit for each copy is
unknown**. The upstream repository is known because the imported opam and
`dune-project` files named it.

### `httpz/punycode` — RFC 3492 Punycode and the IDNA helper

| | |
|---|---|
| Upstream | `https://tangled.org/anil.recoil.org/ocaml-punycode` (package `punycode`) |
| Upstream commit | unknown |
| Upstream version | unknown; the imported opam file carried no version |
| Imported in | `96c0766` ("init", 2026-08-30), as `bleeding/punycode` |
| Moved to `httpz/punycode` in | `f731b6a` ("Reorganize packages for initial release") |
| Upstream license | ISC, the same as this repository |

Local changes since the import: renamed to the `httpz.punycode` and
`httpz.punycode.idna` libraries; `uutf` and `domain-name` dropped as
dependencies (`38e939d`, `a98116d`); unused IDNA helpers and inert options
removed (`a98116d`, `8475615`); `7f6f973`, `33a4af7`. The second-pass security
work in the working tree touches `punycode_idna.ml` and the tests as well.

### `httpz/pubsuffix` — Public Suffix List lookup

| | |
|---|---|
| Upstream | `https://tangled.org/anil.recoil.org/ocaml-publicsuffix` (package `publicsuffix`) |
| Upstream commit | unknown |
| Upstream version | unknown; the imported opam file carried no version |
| Imported in | `96c0766`, as `bleeding/publicsuffix` |
| Moved to `httpz/pubsuffix` in | `f731b6a` |
| Upstream license | ISC |

Local changes since the import: renamed from `Publicsuffix` to `Pubsuffix`;
the `domain-name` dependency dropped; the separate `lib/cmd` and `psl_test`
drivers folded into the binary and the test (`b99f800`, `355ea03`); the
generator and the trie reworked (`497f4bf`, `9b8393f`, `230fbbf`, `85a2570`,
`2d12e97`, `9720df3`, `9a06bc3`).

### `httpz/cookie` — RFC 6265 cookies and the client jar

| | |
|---|---|
| Upstream | `https://tangled.sh/@anil.recoil.org/ocaml-cookeio` (package `cookeio`) |
| Upstream commit | unknown |
| Upstream version | unknown; the imported opam file carried no version |
| Imported in | `96c0766`, as `bleeding/cookeio` |
| Moved to `httpz/cookie` in | `f731b6a` |
| Upstream license | ISC |

Local changes since the import: renamed from `Cookeio`/`Cookeio_jar` to the
`httpz.cookie` and `httpz.cookie.jar` libraries; retargeted at
`httpz.pubsuffix`; `61b343e`, `121ed14`, `a8fead2`. The first- and second-pass
security work in the working tree rewrites parts of `cookie.ml` and
`cookie_jar.ml`.

## `httpz/pubsuffix/data/public_suffix_list.dat` — upstream data, not a fork

| | |
|---|---|
| Upstream | `https://publicsuffix.org/list/public_suffix_list.dat` |
| Snapshot | the `// VERSION:` and `// COMMIT:` lines at the head of the file |
| Local changes | none; byte-for-byte upstream, checked 2026-09-03 |
| License | MPL-2.0, which is why this repository is `ISC MPL-2.0` |

The generator reads those two header lines and embeds them in the generated
module, so the snapshot a build shipped is recoverable from the binary. The
MPL-2.0 header at the top of the file must be preserved on every refresh. See
`httpz/pubsuffix/README.md` for the refresh procedure; the list moves weekly
and a stale copy is a cross-tenant cookie-scoping hole, not merely stale data.

## If a component is ever re-synced with upstream

Record the upstream commit here at that point, so the next audit has a base to
diff against.
