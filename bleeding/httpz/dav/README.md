# httpz.dav

`Httpz_dav` provides immutable WebDAV protocol values and bounded XML codecs:
property fragments, multistatus responses, request XML, hrefs, lock discovery,
opaque tokens and DAV If conditions. Link `httpz.dav`, installed by the `httpz`
opam package. It depends on `httpz.uri` and has no HTTP transport or Eio dependency.

The [specification](SPEC.md) describes the supported RFC 4918 subset and resource
limits. The [public interface](httpz_dav.mli) exposes encoding and namespace
values without exposing the private XML codec. The Fetch client is
[`fetch.dav`](../../fetch/dav/README.md); Proffer applications can use its
[`proffer.dav`](../../proffer/dav/README.md) re-export.

The protocol tests generate prefix collisions, namespace shadowing and detached
fragments, verify stable reserialization, and exercise a 32,000-declaration
scope at its exact node limit. Inherited attributes use hash lookups and count
against the node budget. `test/bench_namespaces.exe` measures scaling separately
from pass/fail tests. The bundled XML codec also has direct encoding, whitespace
and namespace regressions. See the [client testing commands](../../fetch/dav/README.md).

The private `xmlm.ml` and `xmlm.mli` come from Xmlm revision
`2f942d16c2874d52ac4d2923073728156182eff5` (`v1.4.0-6-g2f942d1`), with the OxMono
portability, CDATA attribute normalization, whitespace-output and reserved
namespace fixes. Unused bindings were cleaned up for normal project warnings.
The original ISC notices are retained. Packaging the corrected codec privately
keeps installed applications independent of an unpatched external Xmlm and
avoids exporting another global `Xmlm` module.

The OxCaml copy matches OxMono's `vendor/xmlm`; the stock OCaml copy removes mode
and kind annotations and uses `Hashtbl.Make`. Both retain identical XML behavior.
[`check_sync.py`](check_sync.py) checks the three DAV trees and this vendor copy.

The [synchronization procedure](SYNC.md) records the shared paths, supported
stock-OCaml transformations and validation order.
