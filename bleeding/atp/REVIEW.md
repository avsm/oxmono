# ATP stack implementation review

Reviewed 2026-09-07: XRPC transport and credentials, shared CLI authentication,
IPLD/syntax primitives, CAR and MST, Hermest's generation paths and generated
codecs, and Bluesky/Tangled/Standard Site consumers. Generated libraries were
reviewed through their generator plus representative output, not by treating
every generated declaration as independent handwritten code.

## Open findings, in priority order

1. **P1 — MST mutations do not construct a protocol-canonical tree.**
   `atp/lib/mst.ml:279` (`add`) and 325 (`remove`) gather all leaves and place
   them in one node, even when their hash-derived layers differ. The node's
   layer depends on the most recently added key. This produces incompatible
   root CIDs and invalid repository updates. The rightmost-subtree lookup bug
   was fixed, but mutation still needs a canonical builder, including required
   intermediate empty nodes, and upstream root-CID vectors. The
   [repository specification](https://atproto.com/specs/repository#mst-structure)
   defines the required deterministic shape.

2. **P1 — Loaded MST nodes are not adequately validated.**
   `Mst.Raw.decode_entry` (91) supplies defaults for missing or mistyped `p`,
   `k`, and child links. `raw_to_node` (172) clamps oversized prefix lengths,
   does not validate global ordering/layers, and lazily follows unverified
   child references without traversal budgets. A malformed tree can yield
   misleading lookups or excessive traversal. CAR import now checks block
   hashes, but arbitrary blockstores and MST loading still need validation of
   both content hashes and tree invariants.

3. **P1 — Generated nullable fields lose required/null/absent semantics.**
   Both object-generation paths (`hermest/lib/codegen_jsont.ml:301`, 1153)
   turn nullable fields into `opt_mem` of the non-null codec. A required nullable
   string accepts `{}` but rejects `{"field":null}`. Optional nullable fields
   cannot distinguish absent from explicitly null. Represent the three states
   when necessary; use a required nullable member for required/null fields.
   Regenerate interfaces and consumers with dedicated nullable fixtures.
   [Lexicon object fields](https://atproto.com/specs/lexicon#object) distinguish
   required and nullable independently.

4. **P1 — Standard Site updates replace fields the caller did not edit.**
   `bin/standard-site/lib/standard_site_api.ml:157` rebuilds publications with
   `icon=None`, `basic_theme=None`, and default preferences. `update_document`
   (297) clears `content` and any omitted optional fields. Neither fetches the
   original record nor uses `swap_record`, so an ordinary update can erase
   metadata and overwrite concurrent changes. Preserve the original JSON and
   patch selected fields, with an explicit clear operation and optimistic CID
   comparison. Generated codecs also currently drop unknown members.

5. **P2 — High-level reads hide errors and return incomplete lists.**
   Standard Site `get_publication`/`get_document` and Tangled `get_repo`/
   `get_profile` catch every XRPC error as `None`, including authorization,
   rate limits, malformed responses, and transport failures. Record-list
   decoding silently drops malformed entries. The list helpers fetch only the
   first 100 records and discard cursors, so later records disappear from
   lookup, clone, and pipeline summary operations. Distinguish missing records
   from failures and share bounded, cycle-aware cursor iteration.

6. **P2 — Generated codecs are representations, not schema validators.**
   `gen_jsont_ref`/`gen_unified_jsont_ref` ignore string formats, enums,
   constants, length/range constraints, and many union constraints. `$type`
   decoding ignores the supplied discriminator and supplies defaults when
   absent. Several unions and recursive references fall back to unrestricted
   `Jsont.json`, including scalar values where Lexicon requires an object.
   Keep the lossless fallback explicit and add separate validation rather than
   claiming these types enforce the schema. The incorrect JSON bytes encoding
   was fixed in both generator paths and regenerated outputs.

7. **P2 — Integer and blob codecs need an AT Protocol validation layer.**
   `atp/lib/lex.ml:162` passes integers through OCaml floats, losing precision
   beyond 2^53; values near the int64 boundary can wrap on conversion.
   Hermest uses `Jsont.int64` for some fields, which may emit a JSON string for
   large integers. Generic DAG-CBOR permits floats while the AT Protocol model
   forbids them. `Blob_ref.jsont` (97) ignores the supplied `$type` and does not
   validate positive size, MIME type, or raw CID codec; its JSON path lacks the
   legacy form supported by its CBOR path. Define and test the boundary between
   generic IPLD and [AT Protocol data](https://atproto.com/specs/data-model).

8. **P2 — TID creation is not monotonic or guaranteed distinct locally.**
   `atp/lib/tid.ml:95`/106 use wall time and the default Random state. Calls in
   the same microsecond, clock rollback, or separate processes can collide;
   multiplying unchecked millisecond timestamps can overflow before range
   validation. Use a stateful, injected-clock generator with a stable random
   clock ID and a monotonically increasing last timestamp. Keep explicit
   timestamp conversion separate. [TID generation guidance](https://atproto.com/specs/tid)
   calls for avoiding repeats and handling clock adjustments.

9. **P2 — Filesystem blockstore writes are not atomic or durable.**
   `atp/lib/blockstore.ml:98` truncates the final block path in place and `sync`
   is a no-op. An interrupted or concurrent write can expose a partial block.
   Direct `put`/`get` do not check CID/data correspondence. Use atomic immutable
   block publication and define verification/durability responsibilities.
   The swallowed directory-creation exception and invalid cache capacities were
   fixed; session/password files now use atomic private replacement.

10. **P2 — Discovery and multi-service mutations remain application policy.**
    `xrpc/xrpc_types.ml:26` discards `didDoc`; it does not extract or validate the
    PDS endpoint despite the comment saying extraction happens separately.
    Clients use the explicitly configured service. Tangled repository creation
    writes to the PDS before the knot, and deletion does the reverse, without a
    resumable operation record. A second-service failure leaves partial state;
    retrying creation may create another record. Add an explicit identity/PDS
    resolution policy and resumable multi-step operations with stable record
    keys. Plain app-password login remains distinct from AT Protocol OAuth.

## Fixes applied during this review

- Added `Xrpc.Client.of_fetch`; `create` remains the curl convenience path.
  Normalized service URLs and validated NSIDs before network access.
- Replaced unbounded `Eio.Flow.read_all` and ad hoc JSON conversion with
  `Fetch.decode`/`Fetch.Json.v`/`Fetch.encode`: 16 MiB configurable JSON/binary
  response limits, Fetch JSON nesting limits, and 64 KiB diagnostic limits.
- Reject write redirects for every procedure/upload variant. Snapshot refreshed
  tokens per request through origin/path-scoped `Fetch.Credential` wrappers.
- Encoding failures and mismatched codec/value arguments stop before requests.
  Map transport/decoding errors into XRPC's public API while preserving
  cancellation and exceptions from caller code.
- Added `procedure_unit` for no-output endpoints and switched Tangled knot writes
  and session deletion to it. Empty successful responses no longer become parse
  failures after mutation.
- Reused one Fetch pool per credential manager. Serialize refresh/state changes,
  update every retained client with current tokens, reject stale account/PDS
  use, refresh against the injected Eio clock, and treat missing JWT expiration
  as expired. Logout uses the refresh token and clears locally on failure while
  preserving cancellation.
- Validate profile/application paths, honour XDG and current profiles, preserve
  the selected profile across refresh, activate the first login, and save
  credentials by syncing/renaming mode-0600 temporary files. Malformed sessions
  are errors. Tangled's optional saved password uses the same private writer
  and preserves its exact bytes.
- Fix varint signed-integer overflow and negative offsets; CAR read-ahead loss,
  malformed roots, zero/truncated frames, and frames above 64 MiB. CAR import
  verifies each hash before storage; export raises on missing blocks.
- Fix CBOR integer canonical-boundary checks and unsigned 32-bit reads. Reject
  overflowing integers/lengths, invalid UTF-8, and duplicate encoded map keys;
  bound decoding to configurable bytes (16 MiB) and nesting (128) by default.
- Fix MST final-right-subtree lookup and case-insensitive handle equality.
- Add the shared AT Protocol `$bytes` base64 codec; use it in both Hermest
  generation paths and regenerate Bluesky/Tangled lexicons.
- Remove Bluesky's JSON encode-to-string/decode round trip. Add missing direct
  package dependencies and correct README CLI examples and experimental CID
  format guidance. Current repositories use standard CAR/DAG-CBOR links, as
  described by [CAR serialization](https://atproto.com/specs/repository#car-file-serialization)
  and [DAG-CBOR links](https://ipld.io/specs/codecs/dag-cbor/spec/#links).

## Simplicity and validation

Fetch should continue to own HTTP lifetime, codecs, credentials, redirects,
retry policy, and backend restrictions. XRPC owns NSID construction, status
payloads, and protocol session state. A raw binary response can be bounded today;
a future streaming API should use a response callback so ownership cannot escape.
Do not add another retry loop to each application helper.

The next structural simplification should consolidate duplicated generator
field/type decisions into one representation shared by the individual/unified
module and interface emitters. Make nullable semantics and unknown-field
preservation explicit there, then regenerate all libraries. Keep repository
validation separate from the generic CBOR reader.

Passed with the requested switch:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release \
  @bleeding/atp/runtest \
  bleeding/atp/bin/bluesky/bsky.exe \
  bleeding/atp/bin/tangled/cli/main.exe \
  bleeding/atp/bin/standard-site/cli/main.exe
```

The previously missing syntax fixtures are now vendored with licenses and pinned
upstream revision `afbd27e5d9f19907cb630e6eda3bf7b134966d23`: all 357 syntax/
structure cases pass. The 12 existing CID-format cases, generator regressions,
new XRPC transport/credential tests, session filesystem tests, and binary parser
regressions pass. The two affected generated libraries were regenerated and all
three CLI consumers built. Combined apubt and Fetch-signature tests also pass.

Tests cover redirects, credential scope, response limits/media types, encoding
failure before I/O, cancellation, current-token propagation, logout, PDS mismatch,
profile persistence, CAR sequential reads/import integrity, CBOR boundaries and
limits, varint overflow, and rightmost MST lookup. Tests for the experimental CID
format are round trips, not evidence of current network interoperability.

Validation used the release profile because the dev profile has an existing
unrelated fatal unused-value warning in `bleeding/httpz/uri/uri_template.ml:66`.
Existing unsafe-multidomain alerts remain; the isolated session tests also warn
about setting their temporary configuration environment. No live account changes,
full repository conformance suite, standalone opam install, or full-monorepo test
run was performed. The open findings above remain open in the resulting worktree.
