# Eio-native OCaml IMAP client and synchronization foundations

Design and implementation handoff, 2026-09-26.

This document specifies the `bleeding/imap` library family, aligned with
`bleeding/jmap`, with a path to a durable synchronizer and IMAP/JMAP adapters.
It began as a pre-implementation handoff. A first client foundation now exists;
its supported scope and limitations are documented in
[bleeding/imap/README.md](bleeding/imap/README.md). The research corpus and an
independent live Cyrus probe remain included with this handoff. Sections below
describe the intended full design, including work beyond the current library.

Implementation checkpoint (2026-09-26): `imap` and `imap.eio` provide checked
protocol types, incremental parsing, TLS/SASL, UID-first selected operations,
CONDSTORE/QRESYNC, UIDPLUS, IDLE and capability-gated modern extensions,
including RFC 8970 PREVIEW. `imap.store` uses versioned SQLite snapshots,
staged scans, an operation journal and synced SHA-256 blobs. `imap.sync`,
`imap.watch`, `imap.maildir`, `imap.policy`, `imap.flag-sync`,
`imap.delete-sync` and `imap.bridge` provide a continuous scan supervisor,
disk-paged local inventories, durable paired transfer, three-way flag merge
and opt-in, targeted deletion propagation. `imap.cli` provides bounded
one-shot sync cycles, read-only paged journal and deletion-plan views,
directional deletion policy, retention markers, and explicit APPENDUID,
FLAGS and deletion repair commands. Cyrus, Dovecot and Stalwart Docker fixtures
exercise real IMAP behavior. M7 remains incomplete: uncertainty repair,
exhaustive crash injection, retention policy, large-mailbox live interoperability and proxy
state remain open. See §17 for the precise checkpoint.

The shared `Sqlite3_eio` worker now handles cancellation before a SQLite
statement starts as well as during execution. A cancelled queued worker skips
the statement; an active worker receives repeated `sqlite3_interrupt` calls
until it exits. This closes a race that could leave a sync shutdown waiting on
an unbounded query. The SQLite cancellation regression and live Dovecot bridge
suite pass with this behavior.

The first deliverable is a reliable client, followed by a restartable remote
mirror. Bidirectional synchronization and either direction of proxy are separate
deliverables built on those foundations. Do not implement a proxy by translating
individual commands without persistent identity and synchronization state.

## 1. Decisions to retain throughout implementation

1. Use direct-style Eio APIs with caller-owned switches, flows, clocks and
   filesystem capabilities. No Lwt, Async, public promises or blocking socket I/O.
2. Separate protocol types/codecs, connection management, mailbox mirroring,
   storage, synchronization policy and cross-protocol adaptation.
3. Treat a connection as a stateful protocol session. Initially allow one command
   in flight. Allow concurrent callers safely; scale across bounded connections.
   Add pipelining only after a command-conflict scheduler has independent tests.
4. Represent UID, UIDVALIDITY, sequence number, MODSEQ and object ID as distinct
   types. Persist message identity with account and mailbox generation. Never
   use sequence numbers or Message-ID headers as durable identity.
5. Stream literals with bounded memory from the first implementation. A literal
   is a byte count, not a line or a UTF-8 string.
6. Match JMAP's module vocabulary and lifetime/error conventions; do not force
   IMAP into JMAP's JSON request, Chain, ID or state-token types.
7. Share mail flag semantics, while adding strict, lossless IMAP wire types.
   Make cross-protocol conversion explicit and report fidelity limitations.
8. Prefer QRESYNC, then CONDSTORE, then complete UID/flag reconciliation. A
   capability is an optimization, not a prerequisite for avoiding data loss.
9. Commit data, deletions, receipts and the cursor together. A socket disconnect
   must never advance the durable checkpoint past uncommitted observations.
10. Never transparently replay a mutation after an uncertain outcome. APPEND,
    COPY and MOVE require an operation journal and reconciliation.
11. Preserve exact message octets. MIME interpretation is a derived view.
12. Use the existing Cyrus image and JMAP harness for integration; use a scripted
    adversarial server for cases a real server cannot reliably reproduce.

## 2. Evidence, references and research boundaries

### 2.1 Repository revisions inspected

| Repository | Revision | Relevant material |
| --- | --- | --- |
| OxMono | `8af1731a7619dabc0b009e9540c15b99275a5726` | JMAP, mail-flag, Eio, TLS, bytesrw-eio, SQLite and oracle setup |
| `../stalwart` | `02503015580abd6706923d34e9d8e501e76f8634` | Rust IMAP protocol/server implementation and IMAP integration cases |
| `../isync` | `45b11c63a503dd53674ead6f8617af61e7563b78` | IMAP/Maildir drivers, synchronization journal and regression scenarios |

The local isync checkout is an older implementation: its interfaces, terminology
and documented authentication support are not a current interoperability target.
Use its journal, identity and recovery lessons; do not copy its historical advice
to edit a saved UIDVALIDITY after a mismatch. That can propagate deletions to
unrelated messages.

Stalwart is primarily a server reference. Its command receiver is not a client
response parser. Its request buffering and permissive parsing choices are not
automatically appropriate for large response literals or strict outbound syntax.
Both references have different licenses from JMAP's ISC code: Stalwart source
headers specify AGPL-3.0-only OR LicenseRef-SEL; isync specifies GPL. Write original
code and independently authored tests from protocol requirements; do not transplant
their implementation or test code into an ISC library.

### 2.2 Local standards corpus

[bleeding/imap/spec/sources.json](bleeding/imap/spec/sources.json) records source
URLs, retrieval date, byte sizes and SHA-256 checksums for 60 RFC texts, one pinned
Internet-Draft and three IANA registries. Read the actual ABNF and applicable
sections when implementing each feature. The RFC files retain their notices.
JMAP RFC 8620 and 8621 already live in [bleeding/jmap/spec](bleeding/jmap/spec).

| Area | Local references under `bleeding/imap/spec/` | Intended use |
| --- | --- | --- |
| Base protocol | [9051](bleeding/imap/spec/rfc9051.txt), [3501](bleeding/imap/spec/rfc3501.txt), 4466, 5530 | rev2 target, rev1 compatibility, extensible grammar and response codes |
| Client engineering | 2683, [4549](bleeding/imap/spec/rfc4549.txt) | interoperability and disconnected synchronization |
| Incremental sync | [7162](bleeding/imap/spec/rfc7162.txt) | CONDSTORE, QRESYNC, safe checkpoint rules in §6 |
| Mutation identity | 4315, 6851, 3502 | UIDPLUS, MOVE, MULTIAPPEND |
| Session modes | 2177, 3691, 5161, 4959 | IDLE, UNSELECT, ENABLE, SASL-IR |
| Octets and names | 7888, 3516, 6855 | LITERAL-/+, BINARY, UTF8=ACCEPT |
| Mailbox discovery | 2342, 5258, 5819, 6154, 8438, 8457, 9979 | namespace, extended LIST, LIST-STATUS, roles, status size, flags |
| Search and paging | 4731, 5182, 5256, 9394, 10022 | ESEARCH, SEARCHRES, SORT/THREAD, PARTIAL, UIDBATCHES |
| Resource limits | 7889, 9738 | APPENDLIMIT, MESSAGELIMIT/SAVELIMIT |
| Cross-protocol IDs | [8474](bleeding/imap/spec/rfc8474.txt), [9698](bleeding/imap/spec/rfc9698.txt) | OBJECTID, JMAPACCESS |
| Optional modern modes | 9586; `draft-ietf-mailmaint-imap-objectid-bis-06.txt` | experimental UIDONLY; draft OBJECTID+ |
| Later features | 4978, 5465, 5464, 4314, 9208, 8970, 2971 | compression, NOTIFY, metadata, ACL, quota, preview, ID |
| Authentication/TLS | 4422, 4616, 7628, 2595, 8314, 7817 | SASL framework, PLAIN/OAuth, STARTTLS, modern transport policy and peer identity |
| Message format | 5322, 6532, 2045, 2046, 2047, 2231, 4648 | raw messages, UTF-8 headers, MIME and encodings |
| Synthetic delivery | 2033 | LMTP harness |

Current-status decisions, checked on the design date:

- [RFC 9979](https://www.rfc-editor.org/rfc/rfc9979.html) supersedes the old
  messageflag/mailboxattribute draft cited by mail-flag. Audit its semantics and
  update documentation in the flag work, rather than copying the old draft.
- [RFC 10022](https://www.rfc-editor.org/rfc/rfc10022.txt) specifies UIDBATCHES.
  It is useful for bounded scans, but does not remove the need for fallback.
- [RFC 9586](https://www.rfc-editor.org/rfc/rfc9586.html) and
  [RFC 9738](https://www.rfc-editor.org/rfc/rfc9738.html) are Experimental.
  Implement behavior only when negotiated; do not call them base rev2 features.
- [OBJECTID+ draft -06](https://www.ietf.org/archive/id/draft-ietf-mailmaint-imap-objectid-bis-06.txt)
  is work in progress, dated 22 July 2026. The inspected Stalwart advertises
  `OBJECTID+`; the inspected Cyrus advertises RFC 8474 `OBJECTID`. They need
  separate capability and grammar paths. Pin the draft revision in tests.
- [Verified erratum 8635](https://www.rfc-editor.org/errata/eid8635) corrects an
  RFC 9698 example: the command is `GETJMAPACCESS`, not `JMAPACCESS`.
  Use the command definition/ABNF, not the erroneous example.
- Recheck the relevant RFC's errata before coding that feature. This handoff is
  not a complete errata audit; the RFC 7162 errata page was rate-limited during
  research. Do not apply Reported or Held errata as if they were verified rules.

### 2.3 Existing OCaml code to read first

| File(s) | Reuse or design consequence |
| --- | --- |
| `bleeding/jmap/eio/jmap_eio.mli`, `client.mli` | Auth/Transport/Client/Sync/Push/Profile/Cli vocabulary; result and `_exn` APIs |
| `bleeding/jmap/lib/core/mirror.mli`, `mirror.ml` | storage-independent staged publication and transactional cursor contract |
| `bleeding/jmap/eio/calendars.mli` | fidelity-aware archival receipts and adapter design |
| `bleeding/jmap/eio/auth.ml`, `secret_file.ml`, `profile.ml` | lazy credentials and protected named profile files |
| `bleeding/jmap/mail-flag/lib/{keyword,mailbox_attr}.mli` | existing shared semantics and intentional normalization |
| `bleeding/jmap/lib/mail/mail_keyword.mli` | JMAP-valid keyword subset and validation |
| `bleeding/jmap/lib/mail/mail_{email,body,address,header}.mli` | typed JMAP projections; these are not a MIME parser |
| `bleeding/bytesrw-eio/src/bytesrw_eio.mli` | bounded byte readers/writers over arbitrary Eio flows |
| `vendor/tls/eio/tls_eio.mli` | TLS flow upgrade, explicit RNG entry point, host/IP verification hooks |
| `bleeding/mqttz/{eio,tls}` | local precedent for a pure codec plus an Eio session and authenticated TLS |
| `bleeding/sqlite3/lib_eio/sqlite3_eio.mli` | offloading SQLite I/O with cancellation; later storage adapter |
| `bleeding/jmap/scripts/oracle-{up,env,down}.sh` | existing Cyrus lifecycle and upload-size fix |
| `bleeding/jmap/test/oracle/{oracle_harness.ml,oracle_harness.mli,README.md}` | JMAP connection, LMTP delivery and eventual-index polling |

`Jmap.Mirror` requires opaque account-wide states and JMAP changes semantics.
IMAP has per-mailbox epochs, membership and partial metadata updates. Reuse its
transactional *contract*, not its cursor type or its source callbacks unchanged.

### 2.4 Specific reference implementation lessons

- Stalwart `crates/imap-proto/src/{receiver.rs,protocol,parser}` separates token
  framing from typed operations. Adopt that separation, adding streaming response
  literals instead of collecting all arguments in a vector.
- Stalwart `crates/imap/src/op/{select,enable,fetch,store,copy_move,idle}.rs`
  demonstrates selected-session state and extension-specific behavior. Its
  `enable.rs` makes QRESYNC imply CONDSTORE; activation is distinct from discovery.
- Stalwart `tests/src/imap/{condstore,copy_move,objectid,body_structure,uidonly,uidbatches,messagelimit}.rs`
  provides a list of useful behaviors to test independently. In particular,
  OBJECTID+ activation and UIDONLY's changed response grammar cannot be aliases.
- isync `src/sync.c` journals paired identities, UIDVALIDITY, transfer intent and
  completion, uses a temporary transfer identifier, and recovers unfinished work.
  Preserve the principle that intent is durable before remote effects. Its
  `X-TUID` message rewriting is not a default for an exact-byte archive.
- isync `src/{drv_imap.c,drv_maildir.c,isync.h}` separates endpoint drivers from
  reconciliation policy. `src/run-tests.pl` supplies scenarios involving flags,
  expiration and deletions; implement analogous model-based scenarios afresh.

## 3. Package and dependency structure

Create one `imap` opam package initially, following JMAP's multiple public
libraries convention. Do not declare unused future package dependencies.

```text
bleeding/imap/
  dune-project                 # Dune 3.21; OxCaml workspace, OCaml >= 5.2
  imap.opam                    # generated from dune-project
  README.md  CHANGES.md  LICENSE.md  OXMONO.md
  lib/                         # public imap, root module Imap
    proto/                     # scalars, capabilities, commands, responses
    wire/                      # incremental decoder and typed encoder
    core/                      # selected-state reducer and Mirror planner
  eio/                         # public imap.eio, root Imap_eio
    auth.ml[i] transport.ml[i] client.ml[i] selected.ml[i]
    sync.ml[i] push.ml[i] profile.ml[i] cli.ml[i]
    session.ml[i]              # private reader/dispatcher implementation
  codec/                       # public imap.codec: Jsont cursor/receipt codecs
  jmap/                        # public imap.jmap: explicit typed projections
  test/{proto,wire,eio,mirror,interop,oracle}/
  examples/{1-connect,2-mailboxes,3-fetch,4-append,5-watch,6-mirror}/
  spec/                        # this handoff's standards corpus
```

Dependencies flow in one direction:

```text
mail-flag <- imap <- imap.eio <- application / later mail-sync
               ^        ^
         imap.codec     TLS + Eio + bytesrw-eio
               ^
            imap.jmap -> jmap
```

More precisely, `imap.codec` depends on `imap` and `jsont`; `imap.jmap` depends
on `imap`, `jmap` and codec support as needed. Neither is a dependency of the
protocol library. `imap.eio` does not depend on JMAP, Fetch or HTTPz for sockets.
Its minimum dependencies include `eio`, `bytesrw`, `bytesrw-eio`, `tls-eio`,
`ca-certs`, `x509`, domain/IP types, duration/time types and formatting. Reuse
the workspace's authenticated TLS implementation patterns without introducing
an HTTP transport dependency solely for TLS. Keep `eio_main` at executable
boundaries where possible; follow existing env polymorphism in convenience APIs.

Pure protocol types should follow OxMono's portable/immutable interface
conventions where valid. Connection state is domain-confined and safe between
fibers in that domain. Do not promise domain portability for Eio resources,
mutable decoders, selected handles or callbacks. Borrowed bytes cannot escape
their documented lifetime; copy/globalize only at ownership boundaries.

Later add `mail-sync` (planner and journal), `mail-sync.eio`, a SQLite adapter,
Maildir adapter, and proxy executables after the client/mirror gates pass.
Do not entangle the IMAP protocol package with a database or Maildir layout.

## 4. Protocol model and JMAP compatibility

### 4.1 Scalars and sets

| Type | Representation and invariant |
| --- | --- |
| `Uid.t` | abstract checked `int64`, 1..4294967295 |
| `Uidvalidity.t` | separate abstract type, same numeric range |
| `Seq.t` | separate checked sequence number; never interchangeable with UID |
| `Modseq.t` | 1..9223372036854775807, signed OCaml int64 is sufficient for RFC 7162's unsigned **63-bit** range |
| size/offset | checked nonnegative int64 with grammar-specific upper limits; never narrow through machine `int` |
| `Object_id.t` | opaque, validated IMAP object identifier, independently scoped from a JMAP ID |
| `Tag.t` | internal unique command token; no reuse within a connection |
| mailbox name | raw wire form + encoding mode + decoded UTF-8 result, not just a display string |

Some grammar positions permit zero (counts, partial offsets, CHANGEDSINCE and
UNCHANGEDSINCE sentinels, STATUS HIGHESTMODSEQ when unavailable); use separate
constructors/options rather than allowing UID zero or a normal MODSEQ zero.
Reject arithmetic overflow before allocating or encoding. JSON checkpoints encode
large integers as decimal strings, not Jsont/JavaScript floating-point numbers.

Have two set families:

- Wire sequence/UID expressions can contain ranges, descending endpoints and
  `*` where legal. Preserve positional correspondence in COPYUID mappings.
- Durable `Uid_set.t` uses finite normalized intervals, with lazy iteration,
  union/difference/intersection and overflow-checked cardinality. Never expand
  `1:4294967295` into a list. Represent COPYUID as paired ordered ranges;
  normalizing the two sides independently can corrupt the mapping.

`*` is the last message/UID under the applicable command semantics; it is not an
infinity sentinel. In particular, `UID FETCH (last+1):*` can include the old last
UID when there is no new mail. Prefer fixed finite ranges from UIDNEXT snapshots,
and filter any open-ended discovery result against the saved frontier. Handle
UIDNEXT=1 and UID exhaustion without generating UID 0 or overflowing.

### 4.2 Lossless flags and mailbox names

Add additive strict wire APIs to `mail-flag`, retaining existing public behavior:

```ocaml
(* Names are proposed; settle these before implementing either consumer. *)
module Imap_flag : sig
  type t = private
    | System of [ `Seen | `Answered | `Flagged | `Deleted | `Draft ]
    | Recent
    | Keyword of string
    | Extension of string
  val of_wire : string -> (t, string) result
  val to_wire : t -> string
  val semantic : t -> Mail_flag.Keyword.t option
end
```

Inside mail-flag itself use the local `Keyword` module, not a cyclic reference
to its root module. This sketch describes the external type relationships.

The strict parser must distinguish `Seen`, `$seen` and `\Seen`: the first two
are keyword spellings, the third is a system flag. Existing
`Mail_flag.Keyword.of_string` deliberately recognizes bare/sigilled familiar
names and drops the backslash on unknown system flags. It cannot be used as
the authoritative IMAP parser. Preserve spelling for custom flags and unknown
system extensions; compare flags using the applicable case-insensitive semantics.
`\Recent` is session metadata in rev1, never a writable/persisted flag; `\*`
is a PERMANENTFLAGS permission marker, not a message flag.

Expose a strict attribute token type or retain original LIST attribute strings
beside semantic `Mail_flag.Mailbox_attr.t`. Existing attribute normalization
(including `\Spam` and unknown backslashes) cannot produce a lossless transcript.
Keep all attributes, multiple special-use hints, subscriptions and selectable
status. Never emit `\Inbox`; identify INBOX by its reserved name. Resolve the
single-role constraints of JMAP only inside an adapter.

Mailbox names need a dedicated modified UTF-7 codec for rev1, including `&-`,
UTF-16 surrogate handling and modified base64. Once UTF-8/rev2 is enabled,
validate UTF-8 according to that mode. Do not Unicode-normalize mailbox identity,
case-fold arbitrary names, assume `/` as a separator, or split a NIL delimiter.
Only INBOX has the reserved case-insensitive behavior. Keep namespace prefixes
and LIST delimiters explicit. A malformed received name is preservable raw data
with a decode error, not silently replaced with U+FFFD and later renamed.

### 4.3 Mail data

Model ENVELOPE, BODYSTRUCTURE, addresses/groups, sections, internal date, size,
flags and identifiers in `Imap.Proto`. Preserve NIL versus empty string, absent
versus returned-empty attributes, multipart nesting, message/rfc822 parts and
extension fields. A FETCH response is a **partial update**; missing FLAGS or
MODSEQ must not clear stored values. INTERNALDATE is independent of the Date
header; preserve its instant and source offset/spelling for receipts.

Use typed section paths: whole message, HEADER, TEXT, numbered MIME parts,
MIME headers, HEADER.FIELDS/NOT, and partial ranges. Never concatenate caller
strings into a FETCH item. Default archival reads to BODY.PEEK[], not BODY[],
and do not use BINARY's transfer-decoded output as the canonical raw message.

`imap.jmap` supplies explicit adapters for flags, roles, addresses and metadata,
returning a value plus limitations. JMAP Email has account-wide identity and
possibly multiple mailbox memberships; an IMAP UID is one mailbox occurrence.
Do not fill mandatory JMAP properties with fabricated IDs, threads or zero
sizes. Use partial projections until a store/backend can supply them.

## 5. Incremental wire codec

### 5.1 Required architecture

Implement an incremental octet decoder with resumable states, not a regular
expression over `read_line`. Separate framing/tokenization from typed response
parsing and session-state reduction. The pure decoder accepts chunks and reports
consumed bytes plus one of need-input, event, literal-start/chunk/end, or error.
An EOF input finalizes the state and distinguishes a clean boundary from a
truncated quoted string, CRLF, literal header or literal payload.

Byte slices are borrowed until the next decoder step. Metadata returned from a
public command owns its memory. A streamed literal chunk is valid only during
its delivery; a consumer retaining it must copy. Do not store borrowed buffers
in promises, selected-state tables or cross-fiber event queues.

Parse these categories from the beginning:

- greeting OK/PREAUTH/BYE, tagged OK/NO/BAD, continuations and unsolicited data;
- CAPABILITY/ENABLED, status response codes with typed known forms and unknowns;
- LIST/LSUB/NAMESPACE/STATUS, including extensions and literal mailbox names;
- EXISTS/RECENT/EXPUNGE, SEARCH/ESEARCH, FETCH, VANISHED;
- ENVELOPE, BODYSTRUCTURE and arbitrary ordering of FETCH data items;
- APPENDUID/COPYUID/MODIFIED/HIGHESTMODSEQ/NOMODSEQ/CLOSED;
- IDLE's continuation and completion, then UIDONLY's UIDFETCH in its later mode.

Unknown values are bounded syntax trees of atoms, strings/literals, NIL,
numbers and lists where an extension grammar permits them. Unknown FETCH or
LIST fields must not be discarded just because there is no semantic decoder.
Preserve opaque literals through a sink/spool reference. If unknown syntax
cannot be framed safely, return an explicit protocol/unsupported-syntax error
and close; guessing the end of a response risks desynchronization.

Human-readable response text has its own grammar: text ending in `{123}` does
not automatically introduce a literal. Likewise brackets in BODY[HEADER.FIELDS
(...)] require the FETCH grammar. A generic parenthesized S-expression parser
alone does not implement IMAP.

### 5.2 Literals and encoder

Consume exactly the declared octet count, including embedded CRLF, parentheses,
NUL where literal8 permits it, and text resembling tags. Resume the surrounding
response after the final byte; one response can contain several literals.
Zero-length literals still participate in the surrounding grammar.

For client output:

- Validate atoms, flags, tags and numbers; reject CR/LF injection before any
  command bytes are sent. Quote and escape where allowed; otherwise use a literal.
- `{n}` requires a continuation even when n=0. Send no literal bytes before
  `+`; a tagged rejection instead of `+` completes that command without upload.
- `{n+}` is usable with LITERAL+, or for n<=4096 with LITERAL-/rev2 semantics.
  Never use it for a larger rev1 LITERAL- literal.
- Received server literals do not have `+`. Treat a server `{n+}` as a protocol
  violation rather than silently extending the accepted grammar.
- Literal8 (`~{n}`) and binary APPEND are distinct from ordinary literals;
  rev2 includes BINARY FETCH support but does not imply binary APPEND support.
- An encoder emits syntax fragments and literal-source requests. The Eio
  layer owns the continuation handshake and actual flow copying.

A typed `Command.'a t` binds command arguments, legal states, capability
requirements and its result accumulator. Avoid an unrestricted public
`command : string -> string list -> ...` escape hatch in v1. Future vendor
extensions can use a validated builder with explicit effects and response shape.

### 5.3 Limits

Start with configurable defaults: 64 KiB I/O chunks, 64 KiB outbound command
syntax budget, 1 MiB per control value/nonstreamed textual response, nesting
depth 64, 16 MiB aggregate collected metadata per command, bounded event and
command queues (e.g. 256 events and 64 waiting calls). Validate limit settings.
Keep collected-string APIs at a conservative limit such as 16 MiB.

Streaming SEARCH UID lists and FETCH rows must not require a complete response
line in memory. Token/value limits still apply; a large result can be streamed
to a caller-supplied staging store. Expose total-result/time budgets separately
from memory limits. Large streamed body literals use an explicit policy limit
(initially 1 GiB by default, caller-adjustable), not the metadata limit.
The scalar codec must still support the protocol's full size range.

Oversize/over-depth input yields a typed limit error and closes the connection
unless a specifically tested bounded drain can restore framing. Never allocate
based only on an untrusted length. Allow large-body tests to set small metadata
limits and prove the two are independent.

## 6. Eio connection ownership and concurrency

### 6.1 State machine and resources

Track transport state separately from protocol state:

```text
Connecting -> Greeting -> Not_authenticated / Authenticated(PREAUTH)
Not_authenticated -> TLS upgrade -> Not_authenticated
Not_authenticated -> Authentication -> Authenticated
Authenticated -> Selecting -> Selected(mailbox, generation)
Selected -> UNSELECT -> Authenticated
Selected -> SELECT/EXAMINE -> Selecting -> Selected or Authenticated on failure
Authenticated/Selected -> LOGOUT -> Closed
any live state -> BYE / framing error / EOF / fatal timeout -> Closed
```

STARTTLS is only legal in its specified unauthenticated state. A failed SELECT
cannot leave the old mailbox handle valid. CLOSED and UIDVALIDITY changes update
the selected generation. EXAMINE returns a read-only handle; SELECT must still
inspect READ-ONLY, ACL-related errors and PERMANENTFLAGS.

`Client.connect ~sw transport endpoint` owns its socket under a child connection
switch. One dedicated reader fiber owns input/decoder state and is active even
when no command is running. The writer is serialized across an entire command,
including all continuation/literal phases. Tags associate completions, but
unsolicited responses always update selected state before command completion is
resolved. Unknown or duplicate tagged completions are fatal protocol errors.

Transport upgrades are exclusive reader/writer barriers. After STARTTLS's tagged
OK, the reader parks before issuing another read; the handshake owns the raw flow
until it installs the new TLS flow and decoder input. It is not sufficient for
the command caller to wrap the socket while the background reader continues to
read it. Use the same handoff discipline for any later compression upgrade.

Normal connection failure resolves pending operations with a structured error
and wakes waiters; it must not crash unrelated clients through an uncaught daemon
exception. Parent switch cancellation still propagates and closes everything.
All shutdown paths are idempotent and bounded. Graceful LOGOUT reads BYE and the
tagged completion; emergency close never sends CLOSE or EXPUNGE.

### 6.2 Selected mailbox lease

Provide a scoped exclusive selection lease. The client must prevent one fiber
from selecting mailbox B between another fiber's SELECT A and UID FETCH.
`Selected.t` carries connection identity and a generation check on every call.
Invalidate it when its scope ends, after reselection, reconnect or UIDVALIDITY
change. An escaped OCaml handle is rejected even if the type system cannot
express its dynamic lifetime.

Proposed public shape (illustrative signatures, not generated source):

```ocaml
module Client : sig
  type t
  type error
  val connect : sw:Eio.Switch.t -> ?auth:Auth.t ->
    Transport.t -> Endpoint.t -> (t, error) result
  val capabilities : t -> Capability.Set.t
  val with_mailbox : t -> mode:[ `Read_only | `Read_write ] ->
    Mailbox_name.t -> (Selected.t -> ('a, error) result) -> ('a, error) result
  val close : t -> unit
end

module Selected : sig
  type t
  val info : t -> Selection.t
  val uid_search : t -> Search.t -> (Uid_set.t, Client.error) result
  val uid_search_iter : t -> Search.t ->
    on_uid:(Uid.t -> unit) -> (Search.completion, Client.error) result
  val fetch_iter : t -> Uid_set.t -> Fetch.request ->
    on_row:(Fetch.metadata -> unit) -> (Fetch.completion, Client.error) result
  val fetch_to : t -> uid:Uid.t -> section:Section.t ->
    _ Eio.Flow.sink -> (Fetch.receipt, Client.error) result
  val store : t -> Uid_set.t -> ?unchanged_since:Modseq.t ->
    Store.change -> (Store.receipt, Client.error) result
end
```

`append_flow` belongs to Client since APPEND names a destination without requiring
selection. Give it a known byte length, flags, optional INTERNALDATE and a source
flow. Also offer bounded `append`/`fetch_string` convenience functions and `_exn`
wrappers consistent with JMAP. Expose advanced operations through typed
`Client.call`/`Selected.call`, sharing the same legality checks.

The collected search form is bounded and may return a limit error. Its iterator
form and fetch callbacks are synchronous, backpressured consumers. They must not
reenter the same connection or retain borrowed literals. Detect obvious reentry
and report it instead of deadlocking. Callbacks may perform bounded staging I/O;
their failures abort the current operation, normally closing its connection.

If later adding pipelining, give each command a response/effect class and an
explicit conflict matrix. Selection, authentication, mode/transport upgrades,
IDLE, SEARCHRES mutation and continuation ownership are barriers. Untagged FETCH,
LIST or SEARCH results are not generally tagged; disjoint command tags alone do
not make attribution safe. Overlapping FETCH/STORE on the same UIDs and operations
depending on mutable sequence numbers must remain serialized unless independently
proven safe. Start with a very small tested whitelist and bounded in-flight bytes.
Do not imitate JMAP Chain's result references: IMAP has no equivalent wire feature.

### 6.3 Event delivery and IDLE

Separate internal authoritative processing from public notifications. Never
block the reader indefinitely because a UI has stopped reading an event stream.
Reduce state first. Public events use a bounded queue with a sticky overflow/
`Resync_required` flag; when full, coalesce hints and ensure the flag is visible
even if no overflow marker can be enqueued. Events include connection epoch and
mailbox identity. A lossy notification stream is never a durable change journal.

`Push.subscribe ~sw` owns a dedicated connection/selection lease by default.
It enters IDLE, waits for `+`, processes events, sends untagged `DONE` when
stopping, then awaits the IDLE tag before sending another command. A request to
stop before `+` must wait for it or abort the connection, not send DONE early.
Reissue IDLE before the RFC 2177 29-minute interval; use 25 minutes initially.
If IDLE is absent, poll NOOP/select/status with configured intervals.

On reconnection, authenticate, renegotiate and resynchronize from durable state
before publishing that the watch is current. IDLE events are wakeups, not a
replacement for reconciliation. Watching many mailboxes uses a bounded pool,
polling or later NOTIFY; do not create an unbounded connection per mailbox.

### 6.4 Cancellation, timeouts and errors

Use monotonic time. Separate queue/lease acquisition timeout, connect/TLS/auth
deadline, command deadline, literal progress timeout and IDLE heartbeat. An idle
watch is not failed by a normal short command timeout. A streaming operation can
have both a caller-set overall deadline and a per-progress timeout.

Before dispatch, cancellation removes the queued call with no wire effect.
After any command byte is written, the first version closes that connection on
caller cancellation or timeout; it does not leave a reader consuming responses
for a cancelled owner and then reuse the socket. Cancellation remains Eio
cancellation, not `Error Cancelled`. Cleanup must execute in a bounded protected
region. Pending durable operation intent remains available for reconciliation.

Errors distinguish:

- endpoint/TLS/authentication/authorization and transport failures;
- state misuse, stale selected handle and unsupported capability;
- NO/BAD with tag, structured response code and bounded server text;
- parser/limit errors with offset/state but redacted sensitive data;
- read-only mailbox, conflict (`MODIFIED` UID subset), partial completion;
- uncertain mutation outcome after partial writes, disconnect or cancellation.

Track operation phase: queued, writing syntax, awaiting continuation, writing
literal, awaiting completion, complete. An I/O write failure may occur after
bytes reached the peer. Do not label it definitely unsent merely because the
write call raised. A final NO is also not a universal all-or-nothing guarantee:
COPY/MULTIAPPEND and MOVE have different semantics. Retain per-command receipts
and any observed COPYUID/APPENDUID even when the larger operation is incomplete.

## 7. Negotiation, TLS, authentication and profiles

`Transport.t` is configuration, not a live connection: network capability,
monotonic/wall clocks, RNG, TLS authenticator and dial policy. Supply an injectable
dialer/flow constructor for deterministic tests. No implicit global environment.

Connection sequence:

1. Resolve configured host and connect with a deadline. For implicit TLS (default
   port 993), authenticate the TLS peer before reading the IMAP greeting.
2. Parse greeting including PREAUTH/BYE and optional CAPABILITY response code.
3. For required STARTTLS (default port 143), require advertised STARTTLS, issue
   it, read its tagged OK, then upgrade. Never downgrade on failure. Ensure the
   plaintext parser hands off at the exact boundary: no buffered plaintext is
   interpreted as authenticated IMAP. Reject unexpected trailing plaintext;
   keep any transport handoff behavior explicit and tested.
4. Discard pre-TLS capabilities and query again after upgrade. Authenticate using
   the chosen supported mechanism. Requery capabilities after authentication,
   including when authentication supplies a replacement list.
5. Negotiate protocol mode and extensions, observing ENABLED confirmation.
   Record advertised, effectively supported and enabled capabilities separately.

If both IMAP4rev1 and IMAP4rev2 are advertised, issue ENABLE IMAP4rev2 before using
rev2 behavior (RFC 9051 Appendix A). A server advertising only rev2 is handled
according to that base protocol. Explicitly model implied rev2 functionality;
do not require separate tokens for features incorporated into rev2. Conversely,
rev2 does **not** imply CONDSTORE, QRESYNC, full BINARY APPEND, MULTIAPPEND or every
LIST-EXTENDED option. Keep the implication table tied to RFC 9051 Appendices B/C/E.

For rev1, enable UTF8=ACCEPT only when supported; otherwise use modified UTF-7.
Enable QRESYNC when offered, which also enables CONDSTORE. Avoid enabling
UIDONLY or OBJECTID+ implicitly in the initial release. ENABLE may succeed while
ignoring unknown tokens; only the actual ENABLED response changes those modes.

TLS verifies the configured DNS hostname or IP SAN using system roots or an
explicit test CA/pinning authenticator. Use DNS SNI appropriately; do not derive
the authenticated peer name from a greeting or reverse DNS. Reuse the vendored
TLS explicit-RNG path where practical. TLS failure never triggers cleartext
fallback. A plaintext test endpoint requires an explicit insecure setting.

Initial authentication: SASL PLAIN over TLS, CRAM-MD5 for the required
Dovecot deployment, plus LOGIN compatibility when permitted (honor
LOGINDISABLED). PLAIN, OAUTHBEARER and LOGIN require TLS unless a caller
explicitly opts into insecure transport; Auto cannot fall back to plaintext
LOGIN. Add OAUTHBEARER with proper challenge-error
completion per RFC 7628; vendor XOAUTH2 is a separately named extension. Support
SASL-IR only when available and handle multi-step challenges/cancellation. Do not
present an HTTP Basic header or copy JMAP's ASCII/colon credential restrictions
into SASL. Validate NUL/separator and mechanism-specific fields correctly.
SCRAM and SASL security layers are deferred; report unsupported mechanisms.

Auth constructors parallel JMAP (`password`, `oauth_bearer`, file-backed and
refreshing secrets, `none` for explicitly trusted PREAUTH). Refresh at connection
authentication/reconnection boundaries, not on every IMAP command. Retry an
authentication failure only under an explicit bounded refresh policy. Redact
the entire secret, including in transcripts, errors and pretty-printers.

Profiles live under `$XDG_CONFIG_HOME/imap/profiles/NAME`, retaining JMAP's
size/permission/atomic-save guarantees. Include host, port, TLS mode, username,
mechanism and secret source; do not put secrets in endpoint URLs or command-line
examples. Existing JMAP profiles must continue to load unchanged. If extracting
shared protected-file code, introduce a small neutral library and preserve the
JMAP API with wrappers; do not make JMAP depend on IMAP.

Disable automatic referrals and arbitrary endpoint following. JMAPACCESS is a
discovery hint with a protocol identity guarantee, not authority to send a token
to any URL. Require configured/trusted endpoint mapping, HTTPS outside the local
oracle, and JMAP `Transport.restrict` origin confinement.

## 8. Command support and mutation receipts

Implement the following in order, with a documented capability/fallback matrix:

| Group | Required commands/features | Behavior |
| --- | --- | --- |
| Session | CAPABILITY, NOOP, LOGOUT, STARTTLS, AUTHENTICATE, LOGIN, ENABLE | explicit state transitions and capability epochs |
| Discovery | LIST, LSUB, STATUS, NAMESPACE; LIST-STATUS when supported | preserve delimiter, names, rights/role hints and unknown fields |
| Selection | SELECT, EXAMINE, UNSELECT; CONDSTORE/QRESYNC parameters | selected lease and generation |
| Read | UID SEARCH/ESEARCH, UID FETCH, BODY.PEEK, ENVELOPE, BODYSTRUCTURE | typed metadata and streamed octets |
| Flags | UID STORE, +/-FLAGS[.SILENT], UNCHANGEDSINCE | return MODIFIED conflicts; don't blindly replace whole flag sets |
| Copy/append | APPEND, UID COPY, UIDPLUS receipts; then MULTIAPPEND | distinguish known success, mapping absent, partial and unknown outcome |
| Move/delete | UID MOVE, UID EXPUNGE | capability-gated exact targets and explicit destructive policy |
| Mailboxes | CREATE, DELETE, RENAME, SUBSCRIBE, UNSUBSCRIBE | no heuristic renames or silent parent creation |
| Notifications | IDLE and NOOP fallback | hints with resynchronization |
| Identity | OBJECTID, GETJMAPACCESS | independent IDs; explicit trusted bridge |

A missing APPENDUID/COPYUID does not turn a tagged success into failure: the
server may lack UIDPLUS or omit mappings under relevant conditions. Return
success with unknown destination identity, and let the sync layer reconcile.
Recognize UIDNOTSTICKY; refuse a persistent UID-based mirror for that mailbox
unless an explicit disposable-snapshot mode is selected.

COPYUID source/destination UID cardinalities must match. COPY preserves source
messages. MOVE can partially succeed and can return COPYUID in untagged status
before its tagged completion; collect all relevant receipts. Destination
UIDVALIDITY in a receipt is not necessarily the selected mailbox's UIDVALIDITY.

Fallback MOVE is a journaled sequence: UID COPY, establish destination receipt,
mark exactly the source UIDs deleted, then UID EXPUNGE if supported and permitted.
If safe targeted expunge is unavailable, leave a pending deletion or return
unsupported-safe-expunge. Never quietly issue mailbox-wide EXPUNGE or CLOSE:
another client may have marked unrelated mail `\Deleted`. Expose those commands
only as explicitly destructive low-level operations. UNSELECT is the preferred
release path; if unavailable, close the socket without expunging.

Record PERMANENTFLAGS and whether arbitrary keywords are allowed. Do not strip
unknown flags while updating known ones. Silent STORE still has state effects
and may have unsolicited responses. Read-only selection is enforced locally,
but server authorization errors remain authoritative.

Later: UIDBATCHES and PARTIAL for efficient enumeration; MESSAGELIMIT/SAVELIMIT
must be parsed before claiming a scan is complete. A tagged OK with
`[MESSAGELIMIT ...]` can be only partial completion. Resume by the RFC 9738
processed-UID boundary and operation semantics, not by replaying the whole
mutation. If v1 cannot safely resume an extension response, return a structured
partial result and retain the old mirror checkpoint.
The current staged scanner resumes bounded metadata UID FETCH and UID SEARCH
windows after an OK MESSAGELIMIT response with a valid processed-UID boundary.
It requests only lower UIDs on each continuation and publishes only after
both windows finish. A missing or contradictory boundary fails the scan;
mutation continuation remains separate journal work.
The same bounded continuation now applies to staged CONDSTORE CHANGEDSINCE
windows. Changed rows remain provisional until the independent full UID SEARCH
membership pass finishes; an empty partial page still advances solely by the
server's processed-UID boundary. A scripted durable follow-up scan covers a
partial changed window and verifies that unchanged seeded rows survive.
For mutations, a processed-UID boundary in a tagged NO or an earlier untagged
NO MESSAGELIMIT makes the outcome uncertain; close that connection and retain
the journal entry for reconciliation. RFC 9738 explicitly makes a limit-rejected
COPY/UID COPY atomic, so its tagged NO is a clean rejection unless the server
has already reported partial progress. Preserve the MESSAGELIMIT reason in the
error rather than replacing it with a generic lost-response error.

UIDBATCHES returns descending ranges and requires a requested batch size of at
least 500. Cache them and obey RFC 10022's restrictions on recomputing batches;
do not query them on every fetch. PARTIAL result positions are not a stable
mailbox snapshot. UIDONLY changes FETCH to UIDFETCH and expunge behavior, and
forbids the QRESYNC sequence-match parameter. Implement it as a negotiated mode
with its own test matrix, not merely a flag that makes commands use UID.

COMPRESS=DEFLATE is an explicit post-authentication upgrade at a command
boundary. It requires continuous raw DEFLATE streams and decompressed-size
limits, and has interactions with secret/message compression. Activate it
between mailbox leases, after any TLS upgrade; see §17 for implementation and
transport-boundary validation.

## 9. Body streaming and source preservation

`fetch_to` delivers the requested literal directly to a sink with backpressure.
Its receipt becomes successful only after the enclosing FETCH response and
command tagged completion are validated. A sink may already contain bytes on
error: recommend a caller-owned temporary file followed by durable publication.
Check returned UID, section and partial origin. Do not assume UID appears before
the literal in a FETCH response. A one-UID convenience fetch can provisionally
stream to its temporary sink, then validate the later UID before publication.
Multi-message fetches need per-response temporary spools until identity is known.

Do not turn missing/NIL section data into a successful empty message. An empty
literal, unavailable section and vanished message are distinct outcomes.
Partial body reads validate returned offsets and observed length. Do not make
one huge message consume the global metadata memory budget.

`append_flow ~length` sends exactly length bytes without buffering the whole
source. Short source EOF closes the connection and marks the operation
uncertain. Define that bytes beyond the declared slice remain in the caller's
source; never wait for EOF after length on a potentially live flow. For unknown
length input, require a seekable file or explicit bounded-disk spool API.
Spools need per-operation and total quotas and cleanup on every failure path.

Once a non-synchronizing literal is started, an early rejection cannot be treated
as proof that the sender may safely stop at an arbitrary byte. Follow framing or
close the connection. The first implementation should close on such interrupted
uploads and reconcile, rather than attempt clever recovery.

Archive exact BODY.PEEK[] octets, content hash, byte length, IMAP identity,
INTERNALDATE and metadata receipts. Never normalize line endings, unfold headers,
decode MIME or insert recovery headers into the canonical archive. A MIME parser
can be introduced later as a neutral library for preview/proxy needs; existing
JMAP body types are useful targets but do not perform RFC 5322 parsing.

## 10. Restartable mailbox mirror

### 10.1 Boundary and persistent model

`Imap.Mirror` is storage independent. Use a pure planner/reducer driven by typed
completed read results; `Imap_eio.Sync` executes those actions. Network commands
can emit many rows into staging, so command batches and durable checkpoints are
different concepts. Commit bounded row batches to staging if necessary, but
publish a completed-command receipt only after the tag is received. An interrupted
scan has no authority to delete objects absent from its partial results.

Cursor schema v1 must include:

```text
schema_version, endpoint/account scope, local mailbox key
mailbox raw name + encoding, optional MAILBOXID
UIDVALIDITY, generation, phase
completed MODSEQ anchor (optional; absent for NOMODSEQ)
UID discovery frontier / fixed upper UID bound for current round
inventory generation and continuation/spool reference
staging identifier, selected feature mode, revision/CAS token
```

Keep the full known UID inventory in the store, keyed by generation. A JSON
cursor must not contain millions of UIDs. Persist compressed UID intervals or a
table plus iterator; generate bounded command sets on demand. Store cursor and
MODSEQ as validated decimal strings in `imap.codec`. Reject incompatible future
schema versions and impossible phase combinations instead of resetting silently.

An occurrence key is `(account, local_mailbox_key, UIDVALIDITY, UID)`. MAILBOXID
helps retain mailbox identity through rename, but does not remove UIDVALIDITY.
Email/thread object identifiers and blob hashes are separate optional indices.
Mailbox names alone cannot identify a rename reliably.

The update contract parallels JMAP Mirror: stage replacement snapshots while the
previous published generation remains visible; atomically commit mutations,
deletions, receipts, cursor and revision; publish only a complete generation.
Expose `more` and `Resync_required`/`Restart reason`. An adapter must not claim
that a degraded view of an IMAP account has JMAP's account-wide atomic state.

Use explicit planner phases such as `New`, `Inventory`, `Fetch_metadata`,
`Catch_up`, `Publish` and `Live`, with independent action state for new-UID
discovery and optional body hydration. A concrete interface sketch is:

```ocaml
(* Pure imap core: Completed contains bounded summaries and opaque staging
   references, not all rows or message bodies from the wire. *)
val plan : cursor -> inventory_summary -> action
val reduce : cursor -> action -> completed ->
  (transition, consistency_error) result

(* imap.eio: execute an action using selected-session operations and a
   caller-supplied staging sink; cursor persistence remains with the caller. *)
val step : source -> staging -> cursor -> (transition, error) result
```

An action has a stable local ID, expected mailbox generation, coverage bounds and
requested attributes. The staging interface has `begin_action`, bounded
`ingest_rows`/`ingest_removals`, blob staging, `finish_action` and `abort_action`.
`finish_action` records the verified completion and resulting summary; `reduce`
rejects completions for another action/generation. A transition carries a next
cursor, staging reference, publication decision, receipts, `more` and any restart
reason. The store applies it under a revision check in one transaction. On crash,
unfinished action staging is discarded or safely replayed without publishing it.

Event rows required by the mirror go through this authoritative staging path,
not the public bounded notification queue. A staged metadata page may be reused
after a crash only if its identity, generation and completed coverage can be
verified. Body hydration has an explicit status (`Not_requested`, `Pending`,
`Complete`, `Unavailable`); publishing a metadata-only mirror is permitted, but
an archive requiring bodies cannot claim completion while any required body is
pending. This distinction avoids coupling every flag sync to attachment download.

### 10.2 Initial scan

1. EXAMINE/SELECT with CONDSTORE if available; capture UIDVALIDITY, UIDNEXT,
   HIGHESTMODSEQ or NOMODSEQ, permanent flags, optional MAILBOXID and read mode.
   Persist the starting anchor only as an incomplete snapshot parameter.
2. Fix a finite upper UID bound from UIDNEXT-1 for this scan. Concurrent new
   messages beyond it belong to subsequent discovery. Do not infer actual UIDs
   from counts or assume no gaps.
3. Enumerate membership in that bound. Prefer cached UIDBATCHES/ESEARCH when
   supported; baseline `UID SEARCH` can stream UID tokens to a staging inventory.
   For a server/result budget that cannot support that response, use finite
   numeric UID windows, allowing slower operation rather than unbounded memory.
   Issue chunked UID FETCH for metadata and optional bodies with count and
   encoded-command-length limits. Missing UIDs are allowed if mail vanished.
4. Catch up flag/membership changes from the starting anchor, and discover new
   messages in a later finite UID range. Keep newer per-message MODSEQ data when
   an older observation arrives. Without MODSEQ, preserve response order within
   the session and use a later reconciliation pass.
5. Publish only after all required rows/blobs and coverage receipts have completed.
   A snapshot is an eventually convergent mirror, not a claim of server snapshot
   isolation across separate commands. Record its covered UID bound/anchor.

For baseline scans, a complete final inventory reconciliation plus ordered
unsolicited events catches expunges during earlier fetching. Any unresolved
sequence-to-UID event requires another inventory pass. Deletions need complete
coverage for the same generation, never just a reduced EXISTS count. Continuous
mail arrival must not prevent publication forever: keep finite scan bounds and
schedule the next round after publication.

### 10.3 QRESYNC path

On reconnect, ENABLE QRESYNC, then SELECT/EXAMINE with saved UIDVALIDITY and the
last **durably completed** MODSEQ. Supply known UIDs only if the encoded set fits
the command budget; otherwise omit the optional set (meaning full mailbox
resynchronization), or fall back to bounded CONDSTORE/inventory work. Do not
truncate the set and later claim full coverage. Sequence-match data is an
optional optimization and is deferred initially.

Process `VANISHED (EARLIER)` as historical UID removals, not sequence-number
EXPUNGE operations: it does not decrement the just-reported EXISTS count. Process
live VANISHED according to RFC 7162's selected-state rules, including deduplicated
UID sets. Merge partial FETCH updates. Record explicit HIGHESTMODSEQ codes and
completion boundaries in wire order. Fetch newly discovered UIDs separately;
QRESYNC's update of known messages is not a complete new-message body scan.

QRESYNC SELECT may itself return a very large delta. Stream it into staging with
bounded memory. If time/total-work limits interrupt it, retain the previous cursor
and retry or fall back; staging row commits alone never advance the anchor.

### 10.4 CONDSTORE and baseline paths

With CONDSTORE but not QRESYNC, fetch `UID FLAGS MODSEQ` using CHANGEDSINCE for
updates, **and** perform complete UID inventory reconciliation for expunges.
CONDSTORE alone provides no VANISHED history. Discover new UIDs independently.
If HIGHESTMODSEQ has not changed, use the RFC's guarantees conservatively;
UIDNEXT/count checks never replace all deletion reasoning on baseline servers.

Without persistent MODSEQ, enumerate complete UID membership and refetch current
flags in bounded batches each round. This is slower but correct. A count equal
to the previous count does not imply equal membership. UIDNEXT is a discovery
hint, not a message list or deletion detector. Partial/filtered mirrors must
retain their coverage scope so filtered-out messages do not become tombstones.

### 10.5 The checkpoint rule that must have a regression test

RFC 7162 §6 permits a safe HIGHESTMODSEQ update at a completed command boundary
after full synchronization. An explicit HIGHESTMODSEQ response governs that
boundary **even when observed FETCH MODSEQ values are higher**. Without that
explicit response, the applicable maximum FETCH MODSEQ is considered only at
the completed boundary, with all required observations incorporated.

Do not checkpoint the maximum number seen on the socket, or a later STATUS/get
value. During batched catch-up, retain the original anchor until **all** batches
covering it have completed and their rows are durable. The new-message inventory
frontier advances only after the corresponding UID range is covered; a later
MODSEQ does not certify that every new message has been fetched.

Regression scenario: receive MODSEQ 103 and 104, but explicit HIGHESTMODSEQ 99
because a lower-MODSEQ VANISHED is delayed. Crash before that VANISHED arrives.
Resume from 99; storing 104 would lose a deletion. Also inject crashes between
every staging/receipt/cursor commit and require the result after restart to match
a fresh full reconciliation.

### 10.6 Epoch changes and invalidation

On UIDVALIDITY mismatch, invalidate all live UID references for that mailbox,
clear MODSEQ/inventory cursors, and stop pending mutations against the old epoch.
Keep old archive data in a quarantined generation while building the new view;
do not treat every old UID as a deletion to propagate to another endpoint.
Reassociation using object IDs/content is an explicit reconciliation process.

On NOMODSEQ, clear the incremental anchor and use baseline reconciliation. A
regressing MODSEQ under unchanged UIDVALIDITY, missing required identity, unknown
sequence-number EXPUNGE, event overflow or incompatible cursor causes explicit
resynchronization rather than a guessed update. Preserve the prior published
snapshot until replacement completes. Report failure to callers and telemetry.

Mailbox catalog synchronization is separate: LIST all relevant namespaces,
reconcile subscriptions/roles and identify renames by MAILBOXID where possible.
Without it, a disappeared name and a new name are distinct observations, not a
proven rename. Connection identity and auth scope are part of every cursor key.

## 11. Durable bidirectional syncer design

Implement only after read mirroring and fault tests pass. Support IMAP↔Maildir
first, then IMAP↔IMAP and JMAP adapters through endpoint capabilities. The driver
boundary offers inventory/deltas, raw-message reads, append, flag deltas, optional
copy/move, targeted delete and mailbox operations. It returns receipts and limits;
it does not decide conflict or deletion policy.

Persist:

- endpoint/mailbox generations and cursors;
- paired occurrence identities and last common synchronized flags;
- immutable blobs, hashes and lengths, with durability state;
- tombstones, local retention decisions and conflict records;
- operation intents, source/destination epochs, desired changes, receipts and
  state transitions: Prepared → Sent/uncertain → Observed → Committed.

Use a SQLite implementation over `sqlite3-eio` as the first concrete journal,
with a serialized writer/transaction lock. SQLite's per-call serialized mode
does not make a multi-statement transaction safe from interleaving fibers.
Do not hold a transaction across network calls. Define schema migration,
exclusive sync ownership/CAS and a crash-safe state directory from the outset.

For a file-backed blob: write temporary file, verify hash/length, fsync, rename
atomically within the filesystem, fsync its directory, then commit the database
reference. Recover orphan files and incomplete intents. For Maildir, use unique
names and tmp/new/cur semantics, reconcile filename flag changes and guarantee
durability before acknowledging copies. IMAP keywords use Dovecot's standard `dovecot-keywords` mapping. Explicitly reject
keywords beyond its 26 filename slots rather than silently dropping them.

Use three-way flag reconciliation against the last common state. Compute added
and removed flags separately per endpoint. Preserve unknown flags and surface
conflicting desired changes. With CONDSTORE, use UNCHANGEDSINCE and retry a
MODIFIED subset only after rereading it. Without conditional updates, prefer
explicit +/- operations, report weaker conflict guarantees, then verify.

Default destructive policy is conservative: no deletion propagation without a
complete identity-matched inventory; `\Deleted` is distinct from expunged;
local expiration/size limits/partial downloads are distinct from user deletion;
retain tombstones to prevent resurrection. Make New/Flags/Delete/Expunge direction,
retention, trash-before-delete and dry-run plans explicit configuration, inspired
by isync's policy separation. Mailbox deletion requires its own policy.

Uncertain APPEND/COPY is the key recovery problem. IMAP supplies no general
idempotency key. An APPENDUID observed before a lost tag is useful evidence, not
proof of completed local commit. Record the intended blob, flags, date, destination
epoch and pre-operation frontier before dispatch; on restart inspect candidates
using receipts/object IDs and exact content where available. Message-ID alone,
or even identical bytes among multiple legitimate copies, cannot disambiguate
every case. Leave ambiguous operations pending for an explicit policy decision;
never claim exactly-once delivery when the server cannot provide it.

An optional transfer header/custom keyword can be offered for cooperative stores,
with explicit fidelity/capability consequences. Do not introduce isync's X-TUID
header implicitly. Keep duplicate messages as distinct occurrences even if their
content-addressed blob is shared. Commit copy verification before source deletion.

Acceptance for this stage: restart after every persistence/network boundary,
including a committed server append with a lost response, converges without
unexplained duplicates or lost mail. Ambiguities must be visible, stable pending
operations rather than a retry loop.

## 12. IMAP/JMAP bridge and future proxy boundaries

There are two different server products. Neither is part of the first client
release; the client architecture must make both possible.

### 12.1 JMAP interface backed by IMAP

Build a durable account-wide materialized view with its own opaque JMAP IDs,
monotonic change journal, query state and tombstone retention. IMAP mailbox
MODSEQ cannot be used as a JMAP Email state. UID alone cannot be a JMAP Email ID.
Generate stable mappings across proxy restarts; expiry of the changes journal
returns cannotCalculateChanges rather than an incomplete delta.

When OBJECTID and trusted JMAPACCESS are present, use their defined identity
correlation with verified account scope. RFC 9698 does not by itself supply an
ACCOUNTID or authorize an arbitrary origin. OBJECTID+ adds account context in
its pinned draft; keep this path opt-in. Without interoperable IDs, allocate local
IDs per occurrence initially; aggregate memberships only with defensible identity
evidence. Do not merge messages simply because Message-ID or a body hash matches.

| Difference | Required mapping policy |
| --- | --- |
| Multiple IMAP occurrences vs JMAP mailboxIds | persistent occurrence↔Email mapping, with copy/dedup evidence |
| `\Deleted` vs JMAP membership/destruction | retain IMAP deletion state separately; JMAP has no `$deleted` keyword |
| IMAP special uses vs one JMAP role | resolve multiple hints deterministically; report conflicts |
| IMAP THREAD vs JMAP threadId | stable local thread index or verified server identity; no assumed equivalence |
| BODYSTRUCTURE vs JMAP bodyStructure/bodyValues | derived MIME projection with explicit decoding errors/fidelity |
| INTERNALDATE vs receivedAt; Date vs sentAt | keep separate and preserve timezone/precision limitations |
| Search filters and ordering | explicit supported subset; never silently weaken a filter |
| ACL/rights, quotas and subscriptions | translate only supported semantics; expose limits accurately |

JMAP /set may produce partial per-object results. Coordinate those with the
operation journal; do not promise distributed atomicity across IMAP commands.
SMTP submission is separate: IMAP APPEND is not sending mail. Do not advertise
EmailSubmission support without a submission backend. Blob IDs resolve durable
raw bytes, not an arbitrary current UID. Reuse JMAP protocol codecs and Proffer
HTTP infrastructure for the future server surface, not the JMAP client transport.

### 12.2 IMAP interface backed by JMAP

Requires a command decoder/server session engine in addition to the client
response decoder. Reuse low-level tokens and semantic types, with separate
directional grammars. Maintain durable per-mailbox UIDVALIDITY, increasing UIDs,
sequence order, flags and expunge history, independent of JMAP opaque IDs.
Support selected-session unsolicited responses/IDLE from the local change journal.

A JMAP message can be in multiple mailboxes; per-mailbox `\Deleted` and UID
semantics need local state. A MOVE/removal of one membership must not globally
destroy an Email still in other mailboxes. Preserve exact raw download bytes
for IMAP fetch/append behavior. Advertise only extensions whose guarantees the
proxy store can uphold. QRESYNC needs retained expunge history; UIDVALIDITY must
change if mappings are lost rather than reusing old UIDs.

A common future `Mail_store` projection can share blobs, membership and flag
semantics, but retain backend-specific capability and fidelity information.
Do not refactor JMAP's typed model wholesale into a lowest-common-denominator
mail model before both adapters demonstrate actual common requirements.

## 13. End-to-end Cyrus environment

### 13.1 What has actually been verified

The existing running `jmap-oracle` uses the image digest:

```text
ghcr.io/cyrusimap/cyrus-docker-test-server@sha256:fd71ef73ba0104077e4b87493fc8a4730a8cffe0a2522f0616d78ee0ac66ad7c
```

Container IMAP listens on **8143**, LMTP on 8024, JMAP HTTP on 8080. The current
JMAP script publishes only HTTP/LMTP/management, so 8143 is not host-accessible.
The image creates user1…user5 at example.com and uses fake password validation.
Its configuration permits plaintext; it is not an authentication/TLS security
oracle. It advertises both rev1/rev2, CONDSTORE, QRESYNC, UIDPLUS, MOVE, OBJECTID,
JMAPACCESS, LITERAL+, UIDONLY and UIDBATCHES, among many others.

An isolated copy of that digest was started with ephemeral loopback host ports,
and then removed after the probe. The pre-existing container was not altered.
[cyrus-probe.json](bleeding/imap/spec/cyrus-probe.json) records its capabilities,
message hashes, APPENDUID/COPYUID and QRESYNC observations. The probe used Python's
IMAP/HTTP/LMTP libraries, not an OCaml client, and verified:

- five APPENDs: plain text, MIME attachment, duplicate Message-ID with different
  content, UTF-8 text and a 2,106,180-byte message;
- exact BODY.PEEK[] byte/hash equality, with no incidental `\Seen`;
- all five messages visible via JMAP in the same mailbox;
- IMAP EMAILID matching the JMAP Email IDs;
- reconnection with QRESYNC yielding changed flags and VANISHED (EARLIER);
- LMTP delivery becoming visible in JMAP.

This does not verify the whole advertised extension set, TLS, production auth,
the future OCaml parser, bounded-memory performance or crash-safe synchronization.
The image's JMAPACCESS URL uses its internal HTTP port; a host-side test needs
an explicit trusted endpoint override. Do not generalize test URL rewriting
into production discovery behavior.

### 13.2 Changes to make with the first integration milestone

Extend `bleeding/jmap/scripts/oracle-up.sh` and `oracle-env.sh` to publish/export
IMAP while preserving existing JMAP behavior and the explicit 51200 KiB upload
fix. Use `JMAP_ORACLE_IMAP_PORT` default 18143, mapping `127.0.0.1:18143:8143`;
export `IMAP_ORACLE_HOST=127.0.0.1`, `IMAP_ORACLE_PORT`, and explicit insecure-test
mode. Retain existing JMAP URL/LMTP exports. Add an opt-in IMAP readiness check
(greeting/auth/CAPABILITY) as well as the JMAP session check.

Pin the image digest in CI or a fixture lock file. Permit image overrides for
interoperability runs, and record digest, advertised capabilities and server ID
in test artifacts. Do not assume today's bookworm tag is immutable.

The current script unconditionally removes a container with its chosen name.
For the shared fixture work, add ownership labels and refuse to remove unrelated
containers. Allow unique test-run names and explicit/ephemeral ports. Make teardown
owned and reliable on exit. Do not reset a developer's existing oracle to run a
suite. Never use production profiles for tests.

Extract protocol-neutral synthetic-message and LMTP helpers from the private
JMAP oracle harness into a test-only library (`mail_oracle_support`, or similar).
Keep `Oracle_harness` as a compatible JMAP wrapper. IMAP's runtime library must
not depend on the JMAP test harness. The mixed oracle executable can link
`imap.eio`, `imap.jmap`, `jmap.eio` and both wrappers.

Use the local fixture variables below; follow JMAP's opt-in semantics:

| Variable | Meaning |
| --- | --- |
| `IMAP_ORACLE_HOST`, `IMAP_ORACLE_PORT` | explicit test endpoint; unset host means offline skip |
| `IMAP_ORACLE_USER`, `IMAP_ORACLE_PASSWORD` | synthetic test login, default user1/x |
| `IMAP_ORACLE_TLS` | explicit `plain-test`, `starttls` or `implicit` mode |
| existing `JMAP_ORACLE_URL`, `_LMTP`, `_DOMAIN` | same store's JMAP and synthetic delivery |
| `IMAP_ORACLE_REQUIRED=1` | CI fails if fixture/configuration/capabilities required by its suite are absent |

Ordinary `dune runtest` remains offline. An opt-in live suite must visibly skip
when unconfigured, and must fail on connection/setup errors once configured.
CI must not turn an accidentally missing fixture into a green skipped run.

### 13.3 Synthetic corpus to implement

Create deterministic RFC 5322 fixture bytes with fixed dates and seeded payloads,
plus unique per-run mailbox names/identifiers. Save a manifest of expected lengths,
hashes, headers, initial flags and mailbox memberships. Use APPEND for exact-byte
comparisons (LMTP may add trace/delivery headers); use LMTP for delivery behavior.

| Fixture group | Cases |
| --- | --- |
| Message syntax | ASCII, UTF-8 headers/body, encoded words, empty body, folded/repeated headers, missing/duplicate Message-ID |
| MIME | multipart alternative/mixed/related, base64 binary with every octet, quoted printable, filename continuations, nested message/rfc822 |
| Literal edges | payload containing CRLF, braces, parentheses, fake tags, leading `+`, byte-boundary UTF-8; sizes 0 in wire tests, 4095/4096/4097 and 50 MiB+ in transfer tests |
| Mailboxes | empty, hierarchy, spaces/quotes/ampersand/non-ASCII, INBOX, subscription and special-use cases |
| Flags | system flags, bare `Seen` keyword, `$custom`, case variants, unknown flags; PERMANENTFLAGS restrictions in scripted tests |
| Identity | sparse UIDs after expunge, same Message-ID/different bytes, identical bytes in two occurrences, COPY/MOVE |
| Scale | default fast ~40-message corpus; optional 10,000-message stress set and generated large bodies |

Do not store 50 MiB fixtures in Git. Generate large sources and compute hashes
while streaming. Use separate test-owned mailbox prefixes and explicit cleanup;
never assume the account is empty. Reserve users or mailbox scopes per suite.
Use two independent IMAP connections and a JMAP client for concurrent mutation.
Poll observable state with a deadline; never rely on an arbitrary sleep for indexing.

The JMAP oracle README documents a per-account lock with long-lived EventSource
streams. Use its polling mode or separate users/phases for mixed push tests.
Do not misdiagnose that known fixture behavior as an IMAP synchronization bug.
TLS/auth negative tests require a controlled TLS server/test CA or a separately
configured Cyrus variant, since fake SASL accepts arbitrary passwords here.

## 14. Required verification matrix

### 14.1 Offline protocol and Eio tests

Every requirement below needs named tests with RFC section comments. Use generated
fragmentation/property cases and deterministic mock clocks, not only happy-path
snapshots. Test RFC-derived behavior with independently authored transcripts.

- Split each transcript at every byte boundary for small fixtures; use seeded
  random fragmentation for large ones. Coalesce many responses in one read.
- Round-trip typed outbound commands where directional grammars permit; test
  malformed atoms, quoted escapes, nested values, NIL/empty differences, literal
  truncation, octet count overflow, deep nesting and terminal-control redaction.
- Exercise UIDs at 1 and 4294967295, MODSEQ above 2^53 and at 2^63-1, large sizes,
  reversed UID ranges and non-expanding range operations.
- Test a FETCH literal before its UID, multiple literals, unsolicited FETCH
  interleaved with command results, unknown extension values and missing fields.
- Fail during greeting, TLS upgrade, SASL challenge, literal header/payload,
  IDLE entry/DONE, tagged completion and switch shutdown. Assert no leaked fibers,
  sockets, queued promises or reusable desynchronized connections.
- Test two fibers selecting different mailboxes, stale handles, failed SELECT,
  unsolicited BYE, duplicate/unknown tags, cancellation while queued versus sent.
- Saturate a notification queue while completing commands: reader must not
  deadlock, and clients must see `Resync_required`.
- Test literal source/sink exceptions, short source, early server rejection,
  progress timeout and partial outputs that are never published as complete.
- Exercise capability refresh after TLS/auth, dual rev1/rev2 enablement, missing
  ENABLED token, LITERAL- 4096 boundary, UTF-7/UTF-8 and all fallback paths.
- Test unknown and partial response codes, especially MESSAGELIMIT tagged OK,
  conflicting UNCHANGEDSINCE/MODIFIED subsets and COPYUID cardinality errors.

Use a scripted Eio server/duplex mock that controls reads, writes, scheduling,
fragmentation, disconnect points and extensions. Hiding a capability token while
forwarding all other traffic is not sufficient simulation of a rev1 server:
fallback fixtures must emit the matching grammar and behavior.

### 14.2 Mirror and sync correctness tests

Build a small reference mailbox model with UID generations, flags and tombstones.
Drive random append/flag/copy/move/expunge/rename and crash schedules; compare the
eventual published result to a fresh full model read. Required directed cases:

1. initial scan with append, flag change and expunge during separate fetch batches;
2. QRESYNC changed flags, EARLIER/live VANISHED and newly discovered UIDs;
3. CONDSTORE without QRESYNC, proving deleted UIDs are reconciled;
4. no MODSEQ/no UIDPLUS, with equal counts but different membership;
5. explicit HIGHESTMODSEQ lower than a FETCH MODSEQ, then disconnect;
6. UIDVALIDITY reset with queued mutation: old action never reaches new message;
7. mailbox rename/recreation with and without MAILBOXID;
8. missing UID/body between inventory and fetch, NOMODSEQ and event overflow;
9. partial/oversized result or missing tagged completion: no false deletions;
10. crash at every file durability and journal/cursor commit boundary;
11. lost APPEND/COPY response and duplicate candidates: no blind retry;
12. local retention vs user deletion, custom flags and concurrent edits;
13. byte-identical duplicates stay distinct occurrences; no Message-ID dedup;
14. failed or stale CAS writer cannot publish a newer worker's staging generation.

### 14.3 Live integration suites

- Session/list/select/status, rev1 and explicitly enabled rev2.
- APPEND with INTERNALDATE/flags; BODY.PEEK[] hash equality and unchanged Seen;
  BODY[] seen semantics tested only in its explicit test.
- JMAP import → IMAP discover/fetch and IMAP append → JMAP query/get/download.
- JMAP keyword update → IMAP flags and reverse, excluding Deleted/Recent and
  reporting unrepresentable keywords rather than losing them.
- COPY/MOVE/UID EXPUNGE with destination receipts and unrelated deleted mail
  preserved; mailbox rename and object identity correlation.
- Disconnect/change/reconnect with QRESYNC; forced CONDSTORE and baseline modes.
- IDLE wakeup, DONE completion, reconnect and full delta reconciliation.
- UIDBATCHES/UIDONLY/PARTIAL in separately gated suites when implemented.
- Large-message transfer into a hashing file sink/source with bounded memory.

Use Cyrus as the mandatory shipping-server oracle. Run the client against the
pinned Stalwart v0.15.5 fixture for independent baseline interoperability and
the separate v0.16.23 fixture for live OBJECTID+ coverage. Rust
source inspection is not evidence that the OCaml client interoperates with
Stalwart. RFC requirements take precedence over an isolated server quirk.

### 14.4 Performance and validation commands

A 100 MiB literal must not increase live managed payload memory proportionally
to its size. Record baseline and peak memory and throughput for 1/10/100 MiB
transfers into a sink; target <8 MiB additional live payload buffering with default
settings, excluding deliberately retained metadata/TLS runtime overhead. A
million-UID interval must stay compact. A large UID enumeration may use disk
staging but must not collect a million-element OCaml list accidentally.
Optimized delta sync should be proportional to changes plus bounded discovery;
baseline full reconciliation is deliberately proportional to mailbox size.

From the monorepo root, after the target directories exist:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/imap/all
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check bleeding/imap
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check bleeding/jmap
# With the shared fixture explicitly configured:
IMAP_ORACLE_REQUIRED=1 opam exec --switch=5.2.0+ox -- \
  dune build --force --profile release-check @bleeding/imap/test/oracle/runtest
```

Run JMAP tests whenever changing shared flags, profiles or oracle helpers.
Run dependent application builds for public API changes. Broaden to the workspace
release-check build at integration milestones; keep routine development scoped.
Current implementation checks are recorded in §17; the commands above remain
the repeatable verification entry points.

## 15. Implementation milestones and acceptance gates

Work in this order. Each milestone should be reviewable, with public `.mli`
documentation, tests and examples updated together. Do not start by implementing
all optional RFC extensions or a generalized proxy framework.

| Stage | Concrete work | Gate before moving on |
| --- | --- | --- |
| M0: interfaces | package skeleton, scalar/set types, capabilities, error/receipt model, strict mail-flag additions, RFC 9979 audit | release-check builds; old flag/JMAP tests pass; exact wire vs semantic conversions tested |
| M1: codec | incremental framing, literal streaming, typed response grammar, command encoder and limits | fragmentation, malformed-input, boundary-number and unknown-field tests; no unbounded literal allocation |
| M2: Eio session | injected transport, TLS/auth, reader/dispatcher, one-command scheduling, selection lease, cancellation, basic reads | scripted concurrency/failure tests and connect/list/fetch example; no leaks or reselect race |
| M3: client completeness | core rev1/rev2 commands, negotiation matrix, mutation receipts, append/fetch flows, IDLE | explicit supported-command matrix and relevant rev2 parser/behavior coverage; no false rev2 claim |
| M4: live oracle | extend shared scripts/harness, deterministic corpus, IMAP↔JMAP cross-checks and TLS negative fixture | opt-in Cyrus suites pass and unconfigured offline tests remain hermetic; fixture CI fails on skips |
| M5: mirror | staged state machine, cursor codec, QRESYNC/CONDSTORE/baseline, inventory/frontier rules | model/crash tests and live reconnect convergence; no lost deletion in low-HIGHESTMODSEQ case |
| M6: operational client | bounded pool, profiles/CLI, retry/backoff policy, metrics, examples, resource-limit handling | large-body/large-inventory resource tests, redacted diagnostics, stable public API |
| M7: syncer | endpoint interface, SQLite journal, Maildir, three-way flags, retention/tombstones, uncertain-outcome recovery | fault-injected IMAP↔Maildir tests; explicit unresolved ambiguities; dry-run plans |
| M8: extensions/second server | UIDBATCHES/PARTIAL, MESSAGELIMIT continuation, UIDONLY, optional OBJECTID+, Stalwart live fixture | capability-mode matrix and independent interoperability evidence |
| M9: proxy | choose direction; durable identity/change view and server protocol frontend | honest capability surface, cross-protocol mutation/fidelity tests and restart-stable IDs |

M3/M4 may be developed incrementally alongside M2 to expose interoperability
mistakes early. Mirror memory/receipt requirements must influence M1/M2; do not
postpone streaming until M5. M8 optimizations can be pulled forward once their
base dependencies are sound, but must not delay correct fallback behavior.

For a focused first implementation task, complete M0–M2 and the minimal M4
append/read smoke case. For a usable synchronization foundation, complete M0–M6.
M7 is required before describing the project as a solid isync-like syncer.
M9 is a separate product decision, with both directions outlined above.

## 16. Handoff checklist and unresolved choices

The implementation agent should start by reading this document, the listed JMAP
interfaces, RFC 9051's grammar/compatibility appendices and RFC 7162 §6. Read
the current repository status and `bleeding/imap/README.md` before editing; this
document's milestones describe more than the currently implemented libraries.
References in sibling checkouts are informational;
the library build must not depend on those paths being present.

Decisions already made here: package/module layering, UID-first public APIs,
single-command initial scheduling, explicit selection leases, bounded streaming,
safe mutation receipts, separate IMAP mirror cursor, staged durable publication,
lossless flag layer, conservative deletion policy, shared Cyrus fixture and
separate proxy stages. Do not reopen them merely to simplify the first parser.

Choices to settle with evidence at the relevant stage:

- exact spelling of additive mail-flag modules, after checking downstream callers;
- callback/iterator ergonomics after implementing a real streaming FETCH case;
- whether shared secret/profile helpers merit a neutral library (preserve the
  existing JMAP file format either way);
- MIME parser dependency versus a new neutral mail-format package, needed for
  rich projections/proxy but not for exact-byte synchronization;
- provider-specific OAuth/XOAUTH2 requirements and a pinned Stalwart test runner;
- product policy for genuinely ambiguous transfer recovery, and which proxy
  direction to implement first.

These choices do not block the client and mirror stages. Record any design change
with its motivating failing test or interoperability evidence. A successful
command transcript alone is not evidence of crash safety, exact identity,
complete synchronization, bounded memory or protocol-wide conformance.

## 17. Implementation checkpoint (2026-09-27)

The review fixes consolidate temporary transfer ownership in `imap.io`:
`Imap_io.with_spool` exclusively creates a provisional file, scopes its open
resource to a callback, and removes only successfully acquired files on exit,
including Eio cancellation. Blob/Maildir publication retains rename and fsync
ordering with explicit temporary-file ownership. Selected handles serialize
commands independently of the outer mailbox lease; callers must join their
command fibers before leaving the callback. Lease expiry closes the connection
if a command is still running. APPEND accepts bounded unsolicited responses
before continuation, and IDLE enforces aggregate metadata/count budgets.
Shared durable flag-set equality handles case-insensitive keyword spelling;
FLAGS settlement compares INTERNALDATE instants. Indexed pending-operation
lookup replaces mailbox-wide materialization. Shared scope validation, typed
FETCH collection, metadata reads, and CLI connection/evidence helpers reduce
repeated invariants. CLI configurations are private records constructed by
validation. The obsolete response classifier has been removed.

Review-fix validation: the full IMAP release-check build and scoped protocol,
resource, store, mirror, Maildir, policy, flag/deletion, bridge-fault, CLI, and
shared mail-flag suites pass. Live isolated Docker tests pass against Dovecot
(20 tests, including CRAM-MD5/TLS and equivalent-offset FLAGS settlement) and
Cyrus (2 tests). Resource regressions cover exclusive-creation collisions and
success, exception, and cancellation cleanup. Protocol regressions cover
concurrent selected commands, expired active/queued leases, unsolicited APPEND
responses, and aggregate APPEND/IDLE budgets.


RFC 5256 SORT/THREAD now has typed command constructors, strict ordered
response parsing and capability-gated selected UID APIs. SORT keys retain
priority and per-key direction; THREAD trees retain dummy grouping parents and
expanded parent/child chains. Parsing limits both total nodes (100,000) and
ancestry depth (100, including flat wire chains). Missing, duplicate, malformed
and MESSAGELIMIT-partial results fail rather than appearing as empty complete
views. Stalwart's single trailing separator on an empty THREAD is accepted as
an explicit interoperability exception. These APIs do not publish durable
membership or synthesize JMAP thread IDs. Scripted cases cover capability
refusal, UIDONLY use, grammar/limit failures and partial responses. Live Dovecot
uses four synthetic messages plus an expunged UID gap to verify subject sort,
reverse ties, REFERENCES/ORDEREDSUBJECT trees, filtered dummy parents and empty
results. The release-check build, 25 pure protocol tests, Eio scripted suites
and all 21 live Dovecot tests pass with these APIs.

RFC 5267 is now in the checksum-verified corpus. `uid_sort_extended` adds ESORT
MIN/MAX/COUNT/ALL summaries and sorted UID lists, plus positive positional
PARTIAL when CONTEXT=SORT is advertised. It requests COUNT as a completeness
check, correlates the UID ESEARCH to the command tag, and distinguishes an
unrequested UID list from a verified empty list. Numeric ranges expand in
ascending order even when written with reversed endpoints; comma-element order
is retained. The decoder checks requested fields, counts, duplicates, page
length and covered boundary UIDs, bounding expansion to 100,000 UIDs. COUNT-only
results may describe larger sets. ESEARCH itself rejects duplicate known fields
and out-of-range numeric data. Sorted contexts with UPDATE/CANCELUPDATE and
additional registered sort/thread extensions remain separate work; repeated
positional pages do not establish a durable mailbox snapshot.
The ESORT release-check build, 27 pure protocol tests, Eio scripted suites,
49 bridge fault tests and all 21 live Dovecot tests pass. Dovecot directly
verifies ESORT summary boundaries, sorted ALL and empty results; positional
CONTEXT=SORT pages are verified with scripted transcripts.

RFC 5182 SEARCHRES now has explicit saved-set command constructors and opaque
selected-lease handles. SAVE requests COUNT and requires a correlated UID
ESEARCH response before issuing a handle. Handle validation shares the command
mutex with dispatch; another SAVE, raw UID SEARCH, or lease expiry invalidates
it. A fixed grouped `uid_search_saved` query safely filters the saved set
without replacing it. Empty sets are valid and EXPUNGE removes their members;
the captured count is not a current inventory. Saved metadata FETCH may include
unsolicited rows and therefore does not prove membership. STORE/COPY/MOVE and
targeted EXPUNGE share ordinary command serialization, capability checks and
uncertain-outcome handling. UIDVALIDITY/CLOSED notifications during an active
selection close the connection, including during IDLE; dispatched mutations
report an uncertain outcome. These handles are not durable journal entries,
and no automatic mutation replay is introduced.
Shared mutation-receipt validation also treats contradictory receipts after
successful completion as uncertain and closes the connection, including
duplicate COPYUID responses.
Validation passes the release-check build, 28 pure protocol tests, all Eio
scripted suites, 49 bridge fault tests and all 22 live Dovecot tests. The new
live fixture verifies saved subset queries followed by FETCH/STORE/COPY/MOVE,
empty results after expunge, targeted deletion, raw-search invalidation and
handles escaping their mailbox callback. Scripted tests additionally exercise
concurrent invalidation, rejected SAVE, identity changes during mutations and
IDLE, and contradictory mutation receipts.

RFC 3516 BINARY FETCH now has typed numeric-section constructors and decoded
response accessors. The selected API streams BINARY.PEEK to a provisional sink
with a byte budget, validates UID/section/partial origin and declared versus
received length, and distinguishes NIL from empty content. Bounded BINARY.SIZE
queries do not download the body or run automatically before a stream. Native
or enabled IMAP4rev2 supplies these FETCH features without a separate BINARY
capability; rev2 restricts them to leaf MIME parts. Partial offsets and counts
use decoded octets and signed-63-bit bounds. BINARY.SIZE accepts the same
nonnegative range to follow rev2's general size expansion despite its retained
`number` ABNF spelling. Raw BODY archival stays transfer-encoded and exact;
decoded results never replace canonical blobs.
The release-check build, 30 pure protocol tests, all Eio scripted suites,
49 bridge fault tests and all 23 live Dovecot tests pass. The decoded MIME
fixture verifies NUL/high-bit bytes, quoted-printable text, partial reads and
EOF, decoded size, missing UID, unchanged raw archival and preserved unseen
state. Shared streaming now tolerates metadata-only unsolicited FETCH updates
and checks declared literal lengths before writing bytes. Scripted cancellation
after a provisional prefix closes the connection.

The Eio `Rejected` error now retains an optional typed response code alongside
tag, status and bounded server text. RFC 5530 failure codes, UNKNOWN-CTE,
TRYCREATE and COMPRESSIONACTIVE have explicit constructors; unrecognized codes
remain `Other_code`. Known codes with malformed arguments are rejected by the
parser. Ordinary commands, both APPEND phases and all IDLE completion paths
preserve the code. Authentication redaction is centralized: only a whitelist
of standard codes without parameters survives, with fixed explanatory text;
unknown or parameterized codes cannot carry echoed credentials into errors.
The formatter prints only canonical known code names, without arguments.
This supplies recovery information without introducing retries or changing
partial-mutation uncertainty rules.
Validation passes the release-check build, 31 pure protocol tests, all Eio
scripted suites (including 68 rejection scenarios), 49 bridge fault tests and
all 24 live Dovecot tests. Dovecot verifies AUTHENTICATIONFAILED with redacted
text, ALREADYEXISTS, NONEXISTENT and TRYCREATE, followed by successful commands
on the same connection after ordinary rejections. Scripted tests also cover
unknown codes, codes without text payloads, parameterized authentication-code
redaction, both APPEND phases, IDLE and typed UNKNOWN-CTE after provisional
BINARY output.

Binary APPEND now has an explicit literal8 constructor, reached through
`Client.append ~binary:true`. It requires advertised BINARY; rev2 alone
supplies only the FETCH side of the extension.
The shared APPEND implementation preserves flags/date validation, destination
OBJECTID+ guards, command locking, exact input length, unread source suffixes,
and uncertain/cancelled outcome handling. The server may transform CTE while
preserving decoded content, so callers must fetch and verify the stored bytes
before assigning a canonical digest. The existing durable bridge uses ordinary
APPEND and does not assume binary-input byte equality. A multiple-UID receipt
for either single-message APPEND form now closes the connection and returns
`Uncertain`, rather than being accepted as an absent receipt.
Validation passes the release-check build, 32 pure protocol tests, all Eio
scripted suites, 49 bridge fault tests and all 25 Dovecot tests. A recording
transport checks literal8 framing, exact source slicing, capability refusal
before reading, absence of replay, and cancellation/uncertain outcomes. The
live binary APPEND fixture verifies decoded NUL/high-bit content, APPENDUID
epoch, flags and INTERNALDATE, allowing storage-side CTE conversion.

RFC 4978 COMPRESS=DEFLATE is an explicit authenticated connection upgrade.
Negotiation reads one plaintext byte at a time until the tagged completion,
preserving any compressed tail in the underlying flow or TLS read buffer.
Only successful completion activates the codec; rejection preserves the
original framing and transport. The codec uses the existing pure OCaml
`decompress.de` library with independent raw DEFLATE streams, fixed 64 KiB
buffers, bounded queues/windows and flushes after writes. Incoming history
persists across blocks; outgoing LZ77 history restarts per 64 KiB chunk because
the dependency exposes no LZ77 sync-flush operation. No final block is emitted
per command. Decoded output is delivered before awaiting more network input,
including short responses. More than 16 MiB of compressed input without output
fails, and decoding yields between input batches for cancellation. Existing
IMAP decoded metadata/literal limits still apply. Codec failures or cancelled
operations close the owned transport, preserving dispatched-mutation
uncertainty. Compression remains opt-in and is never used during credential
exchange; TLS must be established first. RFC 1951 was added to the reference
corpus, whose 64 document checksums and sizes verify offline.

Compression validation completed with a successful release-check build, the
protocol and scripted Eio suites, 49 bridge-fault tests and 27 live Dovecot
tests. Live cases exercise compression over plaintext, implicit TLS and
required STARTTLS with CRAM-MD5, large APPEND/FETCH, tagged rejection followed
by reuse, and compressed IDLE. The disposable compression fixture was stopped
after validation. The current production acceptance audit and remaining gates
are recorded in [PRODUCTION-GATES.md](bleeding/imap/PRODUCTION-GATES.md).

The milestones above remain the acceptance criteria. `bleeding/imap` now has a
pure incremental protocol and typed command/response layer, an Eio client with
TLS and SASL negotiation, UID-first selected operations, LIST/NAMESPACE
discovery, IDLE, QRESYNC-assisted scans, durable SQLite staging, exact-octet
blob archiving, an operation journal, and IMAP↔Maildir transfer with paired
flag synchronization. The client also has explicit bounded body hydration
from a published SQLite snapshot: an indexed missing-reference query pages
UIDs under cursor and UIDVALIDITY checks, `hydrate_once` preflights
RFC822.SIZE, then streams each BODY.PEEK[] through an exclusive provisional
spool to a synced content-addressed blob. Invocation count, individual body
size and aggregate bytes are bounded. It returns `more` for another pass; a
caller must schedule those passes. Live Dovecot verifies budget refusal,
paged progress, exact digests, idempotence and spool cleanup. The
`imap-sync hydrate` command exposes a single pass against an existing
published inventory and uses exit status 2 for more work. The opt-in
`sync --hydrate-bodies` mode performs one such pass after a completed,
conflict-free bridge cycle. Live Dovecot evicts cache references for already
paired messages and confirms a later sync rehydrates both without duplicating
Maildir occurrences; background scheduling remains external. The
offline `audit-cache` command now rehashes bounded published-reference pages
and conditionally detaches corrupt or missing cache references without
changing inventory or local message files. Its UID continuation is pinned to
the reported cursor revision. Live Dovecot corrupts a same-length blob,
observes one invalidation through both the API and CLI, and verifies exact
rehydration. Audit scheduling remains external. The
protocol has capability-gated UIDONLY, UIDBATCHES, PARTIAL and MESSAGELIMIT
support and a bounded typed ENVELOPE decoder plus selected-mailbox fetch API.
The same API exposes bounded typed BODYSTRUCTURE trees, including nested
message parts and extension fields. `imap.eio` also exposes a switch-owned,
bounded Eio connection pool. It retires clients after uncertain/protocol/
transport outcomes and callback exceptions while retaining a connection after
a clean tagged rejection. Long IDLE leases should use a dedicated connection.
`imap.watch` compares a new SELECT with the published cursor to close
the scan-to-IDLE race and caps exponential retries after repeated connection
or scan failures. Watch connections and complete scans have finite Eio
deadlines; a timed-out scan discards its provisional SQLite stage and retains
the published cursor. `imap.maildir` publishes through fsynced tmp/new/cur
files and stores keyword letters through `dovecot-keywords`. Its inventory is staged into a
temporary SQLite index and read in bounded ID-ordered pages; it requires a
single writer.
The disk-backed staged scanner now uses CONDSTORE when a same-epoch completed
MODSEQ anchor exists. It copies the prior published UID rows into a SQLite
stage, overlays `CHANGEDSINCE` metadata while preserving a newer saved MODSEQ,
and fetches new UID ranges. It still performs a complete SEARCH inventory
before the cursor and snapshot commit atomically; vanished UIDs are pruned
only from that proof. NOMODSEQ, resets and missing anchors take the full
FETCH/SEARCH path. Incremental CHANGEDSINCE windows now continue RFC 9738
partial responses by processed UID before staging changed rows; missing
boundaries abort the stage. A SQLite regression covers older out-of-order
delta rows, new flags and an expunged UID; a scripted wire regression asserts an actual
`CHANGEDSINCE` command and NOMODSEQ fallback. A subprocess test exits after
fsynced seeded delta rows and confirms the old published cursor and flags
survive restart while the abandoned stage stays inert. Live Dovecot bridge
tests exercise successive scans.
`imap.policy` provides three-way flag and deletion plans. `imap.flag-sync`
journals a conditional UID STORE and a local Maildir flag change, verifies
both sides, and CAS-commits the common flag baseline. The bridge applies the
flag plan by default, holding `\\Deleted`. `imap.delete-sync` journals paired
deletions and verifies surviving byte digest, length and flags. The bridge
defaults to preserving disappearances; `deletion_policy=Propagate` permits
local removal or conditional `\\Deleted` plus targeted UID EXPUNGE on a
server advertising UIDPLUS and CONDSTORE. No global EXPUNGE is used. A
permanent advisory Maildir writer lease spans each bridge cycle. A bridge
receipt counts held flag/deletion decisions and returns up to 100 pair IDs;
the CLI exits 4 for these holds rather than claiming convergence.
Under the default preserve policy, the bridge now walks tombstoned pairs,
counts a one-sided disappearance as a held deletion, and maintains one durable
`Deletion_hold` conflict per pair. A repeated complete scan retains its ID;
when both sides are absent or an opt-in deletion commits, it resolves. A live
Dovecot crash-recovery test verifies the hold is reported and later cleared.
When propagation is opted in but UIDPLUS or CONDSTORE is missing, targeted
remote deletion is also reported as a durable hold, without dispatching a
mutation or creating a deletion journal entry. A scripted capability test
verifies repeated scans keep the same conflict ID.
The optional `min_absence_scans` policy delays an otherwise eligible
deletion until N *additional* complete remote scan generations have been
published after the first observed absence. The first-generation evidence is
stored in the pair's remote or local absence tombstone and is retained across
later scans; incomplete stages cannot advance this timer. With N=1 the first
complete scan records a durable `Grace_period` hold and a second complete
scan can proceed after live identity checks. N=0 preserves the existing
opt-in propagation behavior. Both offline deletion planners use the same
generation test and remain read-only. A preexisting local absence tombstone
without generation evidence stays held for N>0, rather than treating its age
as proven. The bridge fault suite tests first-scan hold, later eligibility,
and retention of the initial observation generation.
SQLite schema v13 records the latest complete scan that saw each side present
after an earlier absence. If a side reappears, a later disappearance replaces
the old first-absence generation. Before that replacement is published, the
offline planners treat the old tombstone as superseded and hold deletion even
with zero grace. The presence witness and renewed absence are separate
durable transactions; a crash between them cannot make the old absence
actionable. Bridge fault and live Dovecot tests cover disappearance,
reappearance and renewed disappearance; migration tests cover v1 through v12.
For a local `Local_absence` tombstone, the bridge first verifies the restored
occurrence's exact body digest, length and INTERNALDATE. A revision-checked
SQLite transition then retires that obsolete tombstone, allowing normal
three-way flag reconciliation again. An altered body or date keeps its
conflict and prevents deletion; a live Dovecot fixture verifies a restored
local `\Seen` change reaches the remote message.

The bridge publishes a complete remote UID inventory, checks paired
UIDVALIDITY, and archives an unpaired remote occurrence before reserving a
Maildir ID. It journals that ID before the local write, then verifies content
hash and flags before atomically publishing the pair. For an unpaired local
occurrence it archives exact bytes, journals APPEND, requires a UIDPLUS
receipt, fetches the resulting flags, and atomically publishes the pair. It
refuses an initially populated pair of endpoints unless the caller explicitly
allows duplicates. SQLite schema v8 stores an immutable digest and length on
each new pair. Older pairs without this evidence are held from destructive
propagation. A work budget limits copies, flag updates and deletions per call. An unresolved
operation stops subsequent copies; it must never be blindly retried. The
legacy APPEND intent and the paired operation journal are distinct records
with the same operation ID.

The protocol library now has a validated `Internal_date.t` for IMAP's
`date-time` syntax, including leap days, optional leading day space, leap
seconds and signed offsets. `FETCH INTERNALDATE` yields this type;
the `Imap.Fetch_item.Internal_date` item requests it; and the
Eio client accepts it on APPEND. `Imap_sync.Engine.append_blob_journaled` persists
the intended date before dispatch, including when an APPEND completes without
an attributable APPENDUID. Protocol, scripted crash-journal, and live Dovecot
APPEND/FETCH round-trip tests cover these paths. The bridge's snapshot and
Maildir occurrence model now differ: the staged remote snapshot does not carry
dates, so the bridge fetches `INTERNALDATE` for each unpaired remote UID before
copying it. The imported Maildir occurrence stores the represented instant in its file
mtime, set and verified before fsync/publication. Flag renames preserve that
timestamp and the basename. Paged local inventory retains the date in UTC;
local uploads pass it to APPEND and verify the returned instant. Original
protocol timezone spellings remain in SQLite journals and pair baselines.
Verification compares instants rather than textual zones. File timestamps
are rounded down to whole seconds for IMAP, and
source checks reject a file whose mtime changes during archival. Invalid or
out-of-range timestamps stop the upload rather than silently substituting
the server's current time. A fixed-mtime live Dovecot fixture verifies this
policy. Future staged remote snapshots should carry
the date if this metadata is needed for proxy queries or offline inspection.
SQLite schema v9 saves the scanned Maildir mtime alongside an unpaired APPEND
operation before the network write. Recovery requires the saved preimage and
compares it with the current file timestamp before committing a pair. Older
pending uploads without that evidence remain held. A fault test changes only
mtime across restart, verifies that no pair is published, restores the original
timestamp, and completes without replaying APPEND. Read-only inspection still
accepts a v8 database before a writer migrates it. External writers that ignore
the Maildir lease can still race after the last source check; fully closing
that race requires a cooperative writer or stronger filesystem coordination.
SQLite schema v10 also stores the validated `INTERNALDATE` on each newly
committed pair. The once-bound value survives restart and flag updates; a later
local mtime mismatch creates one durable `Identity_conflict` and
stops the bridge before flag or deletion propagation. Repeated scans retain
its conflict ID, and restoring the baseline instant resolves it. Existing v8
and v9 pairs keep an unknown date until safely backfilled; both old schemas
remain readable without migration through the read-only opener. A live
Dovecot fixture exercises date drift and restoration.
Crash recovery also compares the published local file timestamp with the source UID,
or the lower APPEND intent's saved date with the destination UID, before
committing a recovered pair. The Dovecot bridge fixture exercises dated
imports, Maildir reopen, dated uploads, and normalization to UTC.
Keep the message's RFC 5322 Date header distinct from IMAP `INTERNALDATE`.
Ambiguous APPEND inspection now requests `INTERNALDATE` when the intent saved
one, compares the instant across numeric timezone offsets, and skips bodies
at a different instant before charging the aggregate body-byte budget. It
remains diagnostic: even an exact date, flags, size and digest match cannot
prove which client sent an APPEND. A live Dovecot process-crash test covers
two identical bodies at the intended instant and a third identical body one
second later; only the first two remain candidates.

When pairs or active journal operations exist, the bridge passes the previously
published UIDVALIDITY into the staged scanner. SELECT must match before any
new stage is written. A reset
returns `Uidvalidity_changed` and preserves the prior published cursor and UID
inventory for diagnosis; a bridge regression test verifies this with a
scripted server. Reassociation across epochs remains an explicit future step.

The next agent should complete M7 before calling this an isync-like syncer:

1. Expand confirmed-UIDPLUS `Append` recovery and fault injection at each
   remaining durability boundary. `Local_append` already reconciles a
   published reserved occurrence without duplicate writes. For a pending
   `Sent` or `Ambiguous` local append whose reserved occurrence is absent,
   `repair-local-append` provides explicit operator recovery under the
   cross-process Maildir writer lease. It requires a matching scope and
   UIDVALIDITY, complete published UID membership, live matching flags,
   exact archived body digest and length, and a live INTERNALDATE. It writes
   the original reserved ID with durable date metadata, then observes and
   pairs the operation. A crash after publication is handled by ordinary
   reconciliation. SQLite schema v11 now records the remote source date in
   the `Local_append` intent before Maildir publication. Restart recovery
   rejects a missing or altered source timestamp, and operator repair checks the
   saved date against the live source. Older intents without a date preimage
   retain their legacy recovery rule. Scripted recovery tests cover missing
   and altered timestamps; a live Dovecot test covers persisted source date,
   success, scope/evidence guards,
   remote flag drift, exact bytes and date. Divergent existing occurrences
   remain held for manual intervention. For an APPEND with
   no attributable receipt, `inspect_append_candidates` and the CLI now
   inspect only the saved pre-send UID range under UID-count and aggregate
   body-byte budgets, comparing epoch, flags, RFC822.SIZE and body SHA-256
   without changing the journal.
   A live Dovecot test creates two identical remote bodies and confirms that
   both remain candidates; matching bytes never establish attribution. Where
   attribution remains ambiguous, expose a durable conflict and
   require a caller decision. A prepared copy is now rejected on restart before
   any send using a prepared-only SQLite transition, and
   `record_appenduid_evidence` accepts an independently
   attributable operator receipt; the bridge verifies the nominated UID's
   body, length and flags before committing a pair. Never infer identity from
   matching bytes alone. The immediate APPENDUID success path now performs the
   same exact remote-body read-back before pairing. A scripted server that
   changes one byte after APPENDUID leaves the operation `Observed` with its
   receipt and no pair; a later scan may reconcile it without replaying APPEND.
   The read-back hashes a provisional spool without creating an orphan blob.
   Recovery of a confirmed UIDPLUS APPEND now uses the same digest-only
   read-back. A mismatched server body cannot be attached to the durable
   remote blob cache before the saved digest is verified; a scripted recovery
   test confirms the operation remains unpaired and the cache reference absent.
   Live Dovecot, pinned Stalwart and Cyrus uploads pass this path.
   A tagged APPEND without APPENDUID now saves a bounded explanation in the
   durable bridge operation receipt. The same journal distinguishes a failure
   before the lower-layer APPEND intent existed: a corrupt archived blob is
   proven unsent and its bridge operation is rejected instead of being left
   ambiguously pending. Scripted tests cover both outcomes and reopen the
   database to verify the missing-APPENDUID reason survives restart.
   Restart recovery now also rejects a bridge `Sent`/`Ambiguous` APPEND if no
   lower-layer intent exists, or if that intent is still `Prepared` or already
   `Rejected`: the lower layer durably marks its intent `Sent` before writing
   any APPEND bytes. A scripted crash-boundary test confirms that the old
   operation is rejected and exactly one fresh APPEND is attempted; a separate
   test with a genuinely sent lower intent remains pending without replay.
2. Harden the implemented paired flag reconciler for mid-flight MODSEQ,
   UIDVALIDITY and external Maildir writes. SQLite schema v8 atomically saves
   the local FLAGS preimage alongside the paired operation and saved pair
   revision. On restart, one-sided recovery may finish a local flag rename
   only if the pair revision and UIDVALIDITY still match, the remote UID has
   the exact target flags, and the Maildir occurrence retains its saved
   digest, length and exact flag preimage. It rechecks both endpoints before
   commit and never resends an uncertain remote STORE. Older operations with
   no preimage and divergent local edits remain pending. A new FLAGS intent
   now verifies the paired local body digest and length before dispatch, and
   rechecks content around the Maildir flag rename. A same-length replacement
   is rejected without creating an intent; a mid-flight replacement leaves
   the sent intent pending instead of advancing the shared baseline. Recovery
   checks local content before a network read and persists a stable
   `Flag_conflict` for a changed body; a scripted test repeats recovery and
   confirms the same conflict ID and pending intent. A pre-dispatch mismatch
   now records a distinct durable `Content_conflict` without creating a FLAGS
   operation. A complete scan clears it after the paired bytes are restored,
   even if there is no flag delta. The CLI reports the pair ID with conflict
   exit status 4; a live Dovecot CRAM-MD5 test covers mismatch, inspection
   and restored-byte clearance. The read-only `plan-sync` view emits saved
   content conflicts as pair holds and suppresses misleading FLAGS candidates
   for those pairs. The offline `verify-local` command now hashes all paired
   local bodies with saved content evidence through a paged Maildir inventory
   under the writer lease. It records stable content conflicts even when
   flags are unchanged, clears verified restorations, and reports absent or
   legacy-unverifiable occurrences separately. A live Dovecot case exercises
   its conflict and clearance exit statuses. A reappearing local occurrence
   with changed bytes now opens a durable content conflict even if its flags
   match. Subsequent disappearance keeps that conflict open; live deletion
   and both offline planners hold the pair until exact paired bytes return.
   A scripted fault case and live Dovecot cycle verify this sequence. Add actionable
   conflict records and operator repair paths. Keep `\\Deleted` held unless
   policy permits it. A `Prepared` FLAGS operation is now rejected before
   validating a possibly changed pair, using a SQLite transition that cannot
   reject an operation already marked `Sent`. A held `\\Deleted` change now
   creates one revision-checked `Policy_conflict` row per pair, refreshes the
   same record on repeated scans, and resolves it only after a complete scan
   shows the hold is gone or the pair is tombstoned. A live Dovecot test
   verifies the row survives a read-only reopen and its ID stays stable.
   Divergent sent FLAGS operations now also create or refresh a durable
   `Flag_conflict` with the verified reason. Repeated recovery retains its
   conflict ID; a successful FLAGS pair/operation commit resolves it in the
   same SQLite transaction, provided no other FLAGS operation for the pair is
   active. `imap-sync settle-flags` now gives an operator an explicit repair
   after manually aligning both endpoints. It verifies the paired content,
   date, UIDVALIDITY, optional OBJECTID+ binding, exact flags and stable remote
   MODSEQ; one SQLite transaction rejects the superseded intent, advances the
   common baseline and resolves the flag conflict. A live Dovecot CRAM-MD5
   case checks refusal before alignment and success through the CLI afterward.
   The bridge maps an immediately pending FLAGS write to its pending
   journal result so the CLI reports exit 3 rather than a generic IMAP error.
3. Extend per-side tombstones with retention and conflict handling. The
   opt-in deletion pass now uses complete inventories, unchanged survivor
   content and flags, a journal, conditional UID STORE and targeted UID
   EXPUNGE. It rechecks flags and MODSEQ immediately before expunging the
   target; a changed target remains pending.
   `imap-sync` now exposes separate `--propagate-remote-deletions` and
   `--propagate-local-deletions` switches (the older
   `--propagate-deletions` enables both). An offline
   `mark-local-retention --pair-id ID --evidence TEXT` command verifies a
   complete Maildir scan under the writer lease, requires the paired local
   occurrence to be absent and no pending operation, then records a durable
   `Retention` tombstone. The deletion planner holds remote deletion for
   that tombstone even when both directions are enabled. This is an explicit
   operator attestation, not automatic inference from file absence.
   `plan-deletions` now streams a read-only candidate plan using the last
   complete published remote inventory and a fresh paged Maildir inventory.
   It reports pending journal work and directional policy holds, with a
   bounded display and full counts. Candidate actions still require the
   next sync to refresh remote state and revalidate live survivor identity,
   bytes and flags. An incompatible absence tombstone now creates a durable
   `Unverified_absence` hold instead of reaching the mutation path or making
   the preview claim a candidate. `plan-sync` extends this same offline,
   bounded-memory view to unpaired copy candidates, three-way paired flag
   deltas, `\Deleted` holds, pending work and populated-bootstrap refusal.
   A full live dry-run of New/Flags/Delete/Expunge,
   general retention rules, and trash-before-delete remain to be implemented.
   Inspect and repair pending ambiguous deletes without replaying them.
   An unresolved delete now reaches the bridge's pending-operation
   result rather than being reported as a generic IMAP failure; the journal
   remains ambiguous until complete inventory proves the outcome. IMAP cannot
   make STORE and EXPUNGE one transaction, so document the
   concurrent remote-edit window and support operational conflict handling.
   A `Prepared` delete likewise resolves as unsent despite a later pair
   revision, while the transition refuses to reject `Sent` work. A live
   Dovecot subprocess test now exits after conditional `UID STORE` and
   targeted `UID EXPUNGE` succeed but before SQLite observes the result;
   restart commits the target tombstone from a complete inventory without
   replay and leaves an unrelated UID present. A separate subprocess test
   exits after Maildir unlink but before local-delete journal commit, then
   recovers the explicit local tombstone without recreating the occurrence.
   If a `Sent` or `Ambiguous` local deletion still has its exact Maildir file,
   `repair-local-delete` is an explicit operator action. It holds the Maildir
   writer lease, verifies the saved pair revision and identity, complete
   published and live read-only remote UID absence, and the local digest,
   length and flags before unlinking and committing the journal. The bridge
   continues to hold this uncertainty; it never replays the unlink itself.
   A matching `reject-remote-delete` command now handles the opposite
   non-mutating resolution: a sent/ambiguous remote DELETE is rejected only
   if the target UID is still present in the published inventory and live
   read-only selection with the saved UIDVALIDITY, original body digest,
   length, flags and stable MODSEQ, while the local occurrence remains
   absent. The store transition atomically checks the saved pair revision
   and journal identity. It sends no STORE or EXPUNGE. A live Dovecot
   CRAM-MD5 case verifies changed flags block rejection and the CLI succeeds
   after they return to the baseline. For a target with exactly the original
   flags plus `\Deleted`, `finish-remote-delete` provides the separate
   explicit operator decision. It verifies complete published UID membership,
   exact body digest and length, local absence, saved mailbox binding, and
   stable remote MODSEQ/flags; then it transactionally records the operator
   evidence and `Ambiguous` state before issuing targeted `UID EXPUNGE`.
   Successful UID absence commits an expunge tombstone. A lost response stays
   pending and a later complete inventory can commit without replay. The
   recovery receipt preserves the saved operator evidence. Live Dovecot
   tests reject extra flags and exercise successful CLI completion; scripted
   crash recovery verifies an attested ambiguous delete commits without
   replay after the UID vanishes.
4. The bridge now pages active operations by ID and queries pending work for
   one pair directly. Subprocess-exit tests cover PREPARED, SENT, fsynced
   Maildir publication and OBSERVED local-copy boundaries, plus an interrupted
   staged scan. Add crash/fault injection at the remaining journal, fsync,
   network send, receipt and pair-commit boundaries. The cross-process Maildir
   lease is implemented; verify all external writers obey it and define
   process-level recovery and repair commands beyond the current CLI's
   bounded sync, inspection and APPENDUID attestation. Inspection now accepts
   an operation ID for a direct, scope-checked SQLite lookup, including
   terminal rows; this avoids paging a large journal while investigating one
   mutation.
5. Test concurrent flag edits and lost APPENDUID. The default resource regression pages 100,001 actual Maildir
   occurrences; an opt-in gate has twice passed with 1,000,001 occurrences
   and 100,001 SQLite pair/operation rows. The second million-file run took
   267 seconds including streaming cleanup, raised inventory VmHWM by
   4,376 KiB, and reached 22,204 KiB process VmHWM before cleanup. Retain
   and expand this gate. Digest-pinned Stalwart v0.15.5 and v0.16.23 live fixtures
   now cover UIDPLUS, CONDSTORE, QRESYNC, exact bytes and bridge transfer;
   v0.16.23 also covers OBJECTID+ over pinned-certificate IMAPS. Add further
   independent mutation and recovery scenarios. A live Dovecot
   bridge test now verifies that two distinct Maildir occurrences with
   identical bytes receive separate UIDs and remain stable on the next pass.
   A separate live Dovecot bootstrap test starts with the same bytes on both
   sides: the default refuses before any copy or journal mutation, explicit
   opt-in creates two distinct pairs, and the next pass is stable.
   A scripted transfer test removes a remote UID after inventory publication
   but before body archival: `fetch_to` returns typed `Missing_uid` without
   closing a clean connection, the bridge returns `Source_vanished` before
   reserving a local ID or journal, and a new scan converges on absence.
   The symmetric Maildir path now verifies an inventory occurrence's identity,
   filename, location, length and flags around byte archival. A vanished or
   renamed occurrence, or content that changes between the hash and blob
   write, returns `Local_source_changed` before reserving an APPEND journal
   ID. The CLI uses the same bounded rescan budget as a vanished remote UID.
   External writers that ignore the Maildir lease can still mutate a file
   after verification; end-to-end concurrent-writer fault tests remain useful.
   The CLI now runs Maildir startup recovery under its writer lease before
   connecting. Recovery removes only abandoned owned temporary files and
   inventory indexes. Standard Dovecot metadata remains intact. Inventory
   staging uses bounded directory batches under the Dovecot metadata lock,
   with a single bounded keyword map and SQLite rows for message observations.
   The default 100,001-occurrence resource gate covers the resulting inventory.
   Remote-to-Maildir imports now pass the staged inventory into `append`,
   replacing a full Maildir duplicate-ID scan per message with an indexed
   lookup plus IDs published during that cycle. The same index handles the
   post-append digest check. Standalone append retains its full-scan
   duplicate guard when no staged inventory is supplied. The resource gate
   now imports 100 additional messages into the 100,001-occurrence fixture
   through the indexed path. Inventory handles are bound to their Maildir
   handle so a caller cannot accidentally use another endpoint's index.

The scoped release-check build and IMAP/JMAP offline tests passed on this
checkpoint. The Cyrus live oracle passed an import of four messages, one
Maildir-to-IMAP upload, durable pair count, stable second pass, recovery of a
local write left in `Sent`, recovery from a persisted APPENDUID,
inventory-proven local/remote tombstones, three-way flag merging and the
default `\\Deleted` hold. It also passed opt-in local and targeted remote
deletion, proving an unrelated `\\Deleted` UID survives. It verifies a
trusted operator APPENDUID after first holding an ambiguous upload; five
consecutive isolated Cyrus runs passed after making fixture message selection
deterministic. The live
Dovecot suite passed CRAM-MD5, selected extensions, IDLE wakeups, the
scan-to-watch gap, durable bridge import/upload, conditional flag merge and
both directions of opt-in deletion. The FLAGS fixture additionally verifies
one-sided recovery from a reopened SQLite journal against real Dovecot and
holds a divergent local edit. Its subprocess APPEND test now exits
after a real server acceptance but before either SQLite journal records the
UIDPLUS receipt; restart holds the operation without a duplicate, and
operator APPENDUID evidence is checked before the single pair commits.
The Dovecot fixture also checks CRAM-MD5 over verified implicit TLS and
required STARTTLS, and LOGIN over both protected transports, using a
disposable CA and IP SAN certificate, including
untrusted-CA and wrong-host rejection. The client now refuses Auto's
plaintext LOGIN fallback, as well as explicit LOGIN over plaintext, unless
the caller opts into insecure transport.
The selected client now parses the RFC 4315 untagged NO `UIDNOTSTICKY`
response code and refuses the selected lease before invoking callers. The
connection closes because the server has already selected a mailbox whose
UIDs cannot safely support a durable mirror. Scripted parser/client tests
verify the callback is not reached.
The client also exposes LSUB, RENAME, SUBSCRIBE and UNSUBSCRIBE using the
negotiated mailbox-name encoding. Mailbox mutations retain uncertain-outcome
semantics on a lost tagged completion. A live Dovecot test creates,
subscribes, lists, renames, unsubscribes and deletes a synthetic mailbox;
durable reassociation of a renamed mirror remains separate work.
RFC 8474 OBJECTID now has a bounded typed selected-mailbox fetch path for
EMAILID and THREADID. It requires the exact legacy OBJECTID capability and
selected MAILBOXID, and rejects incomplete or contradictory identifier rows.
The independent OBJECTID+ capability does not activate this path. A live
Cyrus test confirms a real MAILBOXID and typed EMAILID for an appended UID;
the older Cyrus round-trip fixture was updated to save the APPEND source-mtime
preimages now required by recovery.
An isolated Dovecot CLI smoke appended a synthetic 145-byte message with
CRAM-MD5, then `imap-sync sync` imported exactly one byte-identical Maildir
occurrence; `inspect` reported revision 1 and no pending work. The CLI's
read-only SQLite opener accepts existing v8 stores without the later optional
operation indexes, and paginates open conflicts and active operations.
The independent, digest-pinned Stalwart v0.15.5 live suite passed UIDPLUS,
CONDSTORE, QRESYNC and exact-byte bridge import/upload. That release does not
advertise CRAM-MD5, so the test verifies explicit CRAM-MD5 refusal while the
Dovecot fixture remains the successful CRAM-MD5 test.
The draft OBJECTID+ -06 path is now distinct from RFC 8474 OBJECTID. The
client explicitly enables it before selection, parses compound SELECT
ACCOUNTID/MAILBOXID, STATUS OBJECTID and bounded UID FETCH EMAILID/THREADID
results, and retains unknown identifier keys. Scripted tests cover ENABLE,
SELECT, STATUS, FETCH, CREATE and RENAME receipts, identity fallback, and empty
or malformed compounds. The pinned Stalwart v0.15.5 fixture does not advertise
the capability. The separate digest-pinned v0.16.23 fixture bootstraps the new
JSON datastore, creates a synthetic account through JMAP recovery mode and
requires live OBJECTID+ over certificate-pinned IMAPS. The live suite validates
ENABLE, CREATE and RENAME identities, STATUS, identifier-based SELECT and
message EMAILID/THREADID, plus the baseline protocol and bridge checks. It
also binds an identity in SQLite, renames the mailbox, creates a replacement
under the old name, and confirms the next staged scan refuses publication
without changing the old cursor; the original message remains in the renamed
mailbox. The
client can now select
by `(ACCOUNTID, MAILBOXID)` and rejects the draft's fallback-to-name result
when it resolves to a different mailbox, before invoking the selected
callback. Typed CREATE and RENAME APIs return tagged compound identities and
report an uncertain outcome when a successful mutation omits identity. The
next step for a durable rename-aware mirror is to re-associate names only
after matching the account and mailbox IDs across STATUS/LIST and SELECT;
current sync state is still scoped by configured mailbox name and UIDVALIDITY.
SQLite schema v12 now persists a verified OBJECTID+ `(ACCOUNTID, MAILBOXID)`
binding per logical mailbox scope, with a uniqueness constraint against
binding the same remote mailbox to two local scopes. The staged scanner
activates OBJECTID+ when advertised, pins the saved identity on each new
connection, and refuses a SELECT fallback to another mailbox before scanning
or publishing. If an enabled server omits the compound account/mailbox
identity on first SELECT, the scanner refuses to stage or publish an unbound
cursor. The first successful selection can bind the identity before
the scan, so a crash leaves a durable safety guard. The scanner and journaled
APPEND also verify that the configured name still resolves to that identity
with STATUS; the pinned Eio client checks direct APPEND too. A known rename or
replacement halts the cycle before a mutation.
Standalone local append/delete repair, FLAGS settlement, UID archival and
APPEND candidate inspection now use the same saved-binding guard on their
fresh client connections. Scripted replacement tests show the repairs leave
their journal entries pending and diagnostic inspection refuses the wrong
mailbox.
An upgraded live cursor without a prior binding also refuses to first-bind
when UIDVALIDITY has changed, since that observation could belong to a
replacement mailbox rather than the old cursor's mailbox.
This does not yet re-associate a changed mailbox name, and IMAP cannot make a
STATUS check atomic with a later name-based APPEND. Other name-based mutations
still need a rename-aware destination policy.
Offline fault tests reopen SQLite across pending operations and verify that
ambiguous APPEND is held, a reserved local write is recovered once, divergent
bytes block pair commit, and confirmed UIDPLUS can recover without a replay.
They also verify that an ambiguous remote delete with its target still
present is reported as pending without replay.
The deletion recovery suite covers prepared, sent and ambiguous states; the
Maildir suite covers process contention and cancellation of the writer lease.
Subprocess-exit tests cover four local-copy journal states and an incomplete
staged scan. They do not yet simulate kills or lost packets at every write boundary.
These checks establish the listed behavior, not full
crash-safe bidirectional synchronization or proxy readiness.


## 18. Structure and durability review checkpoint

The source tree now has five public libraries under `bleeding/imap/lib/`:
`protocol` (`imap`), `eio`, `maildir`, `store` and `sync`. The former separate
workflow libraries are modules in `Imap_sync`: Engine, Bridge, Flags,
Deletion and Watch. Pure policy is `Imap.Sync_policy`; Spool is private.
The CLI support library is private under `bin/`, with the same installed
`imap-sync` executable. Unit and integration tests are grouped under `test/`.
This supersedes the original directory proposal in section 3. Detailed review
findings and remaining splits are in [IMAP-STRUCTURE-REVIEW.md](IMAP-STRUCTURE-REVIEW.md).

The first review batch covered Session, Maildir and Store. Fixes protect
statement and directory cleanup from cancellation, register transaction rollback
before BEGIN, retain control literals for IDLE/APPEND/authentication metadata,
compare Maildir flag sets semantically, reserve temporary inventory files
exclusively, serialize conflict lookup, preserve pending-journal blob GC roots,
serialize schema version inspection with migrations, reject quarantined epochs
as current remote presence evidence, and index pair pagination by scope and ID.

The release-check build and Eio, Maildir, store, bridge-fault, CLI, flag, deletion,
policy and spool suites pass after these changes. New regressions exercise
IDLE literal names and limits, noncanonical flag ordering, and pending journal
bodies surviving store reopen and garbage collection. Dedicated migration-race
and cancellation-acquisition tests remain required; existing migration/crash
coverage is not a substitute for these schedules.

The standard Maildir format conversion is implemented as described below. So are the full private
session/transport interfaces, scoped writer API, internal Store/Engine splits,
remaining per-module review and stronger public journal invariants. The consolidation is not a production-readiness claim.


### Standard Maildir metadata checkpoint

Custom flag/date sidecars have been removed. Maildir uses `dovecot-keywords`,
filename flags and file mtime. Keyword maps are validated, additive, and atomically
published under `dovecot-uidlist.lock` before referencing filenames. Basenames,
Passed flags and trailing extension fields survive flag updates. Observations
are private records with inode/change-time checks in addition to filename,
length, flags and mtime; disk-staged inventories carry these checks and reject
escaped handles after their callback expires.

The metadata lock is acquired without waiting. A lock naming a dead local PID
can be recovered; foreign-host, malformed and live-owner locks are left intact.
Directory staging refreshes and checks lock identity every 256 entries and again
before releasing it. This does not provide safe concurrent operation with tools
that ignore Dovecot locking. Filesystem failures and lock loss abort the scan.

The final release-check validation passes 11 Maildir tests, all 49 bridge-fault
cases, all 27 live Dovecot cases and the 100,001-occurrence scale regression
(20.243 seconds). Tests cover sparse keyword mappings, overflow without partial
publication, case aliases, malformed mappings, metadata-lock contention, legacy
rejection, expired inventory handles and same-name inode replacement. The live
suite verifies CRAM-MD5 sync/recovery using this local storage format; direct
co-access of the same Maildir by Dovecot still needs its own interoperability
scenario. There is no automatic legacy sidecar migration yet.


### Client correctness review checkpoint

SEARCH now rejects missing, repeated and unrelated tagged result evidence before
expanding UID ranges. Explicit empty results are accepted. UID window bounds and
normalization apply both with and without MESSAGELIMIT. COPYUID receipts retain
ordered compact correspondence ranges alongside membership sets, respecting
RFC 4315 comma order and ascending range semantics without expanding large copies.
Structured FETCH preserves request order and rejects duplicate ENVELOPE values.

Authentication diagnostics now sanitize server protocol failures and provider
exceptions as well as tagged rejections. TLS/capability preflight remains local
and descriptive, and cancellation propagates. Failed SELECT validation or failed
UNSELECT closes the connection. Transport closure is idempotent and protected
from cancellation, and a failed TLS upgrade closes the transport before propagating
the original failure.

The release-check build, scripted Eio suites, all 49 bridge-fault cases and all
27 live Dovecot tests passed after these changes. The new review regression suite
also exercises a billion-UID COPYUID mapping as one compact range. The disposable
fixture was removed after validation. Public internal capabilities and remaining
structural splits are still tracked in the structure review.


### Eio interface boundary checkpoint

The public Eio root now seals Auth, Transport, Selected, Client and Pool
signatures, preserving cross-module type sharing without exposing secret
resolution, raw flow mutation, or lease constructors. An installed private
implementation library supports low-level tests; it is not another supported
public API. Session has an explicit internal interface, and unused command
collection/callback modes are removed. Compile tests cover supported entry
points and rejection of eleven internal operations. The broader Store and
Maildir structural work and remaining production gates are still open.


### Durable operation evidence checkpoint

Preparing a paired journal operation now requires the exact stored local ID
and matching supplied remote UID/UIDVALIDITY. Content evidence must be complete
and a valid lowercase SHA-256; FLAGS/deletion content preimages must agree with
the pair, and supplied deletion flags must match its baseline.

An atomic pair commit validates source identity for local append/FLAGS/deletion
or destination identity from the receipt for remote creation, plus reserved
local ID, scope, any expected destination epoch, supplied content and requested
flags. Local append checks saved INTERNALDATE by instant, accepting equivalent
UTC timestamps. FLAGS/deletion commits preserve unrelated pair fields, and
deletions require the corresponding tombstone. Revision conflicts, including
legacy missing preconditions, leave the operation observed for reconciliation.
No schema migration is required. Existing inconsistent pending intents are
rejected for repair instead of silently publishing contradictory pairs.

`test/store/test_operation_evidence.ml` exercises contradictory identity,
content, flags, missing and wrong-epoch receipts, missing source dates,
unrelated tombstone changes, transaction non-publication and valid commits.
This validates the store boundary; it does not prove server-side observation
or implement a multi-pair COPY/MOVE synchronization workflow.


### Direct Dovecot Maildir interoperability

The optional `IMAP_DOVECOT_SHARED=1` fixture mode shares only a generated mail
tree, uses the host UID/GID for Dovecot mail processes, and discovers INBOX via
`doveadm mailbox path`. The live test writes with Imap_maildir and reads through
Dovecot, then changes flags/keywords through IMAP and reads them locally. It
checks exact body bytes, system flags, additive keyword mapping, INTERNALDATE,
local flag renaming after Dovecot changes, and targeted expunge of the same file.
All 28 live Dovecot tests passed in this mode; the fixture was removed afterward.
Log: `/tmp/imap-maildir-coaccess-final.log`. Dune now tracks fixture environment
variables explicitly. This proves sequential interoperability on shared files;
concurrent lock-loss and process-crash schedules remain separate open gates.
Legacy sidecar migration and the structural ownership/inventory work remain open.


### Scoped dotlock ownership and recovery correction

Metadata locking is now a private `Dotlock` module, separating filesystem
ownership from Maildir occurrence/keyword handling. An outer cancellation-protected
finalizer owns the descriptor immediately after open, including failures before
stat completes. Refresh is serialized, writes through the owned descriptor,
and checks path identity before and after. Refresh handles expire at scope exit.
Directed tests cover return, exception, cancellation, subsequent acquisition,
escaped refresh, concurrent refresh without owner-content corruption, and
replacement before explicit refresh or callback cleanup.

This review supersedes the preceding automatic stale-owner recovery checkpoint.
The bounded reader did not solve the check/unlink race between two reclaimers:
one could unlink the other's newly created live lock. Automatic stale lock
reclamation has therefore been removed, not declared production-safe. All
existing lock files now produce Writer_lock_busy, including dead-owner files;
recovery currently requires offline removal while every Maildir user is stopped.
Safe unattended recovery interoperating with Dovecot remains an explicit open
production requirement. Inode checks also cannot make cleanup atomic against
an uncooperative process replacing paths between syscalls.

Release-check, the directed dotlock executable, 12 Maildir cases and 49
bridge-fault cases pass. Log: `/tmp/imap-dotlock-lifetime-final.log`.


### RFC 3502 MULTIAPPEND checkpoint

`Client.append_message` describes borrowed streams and
`Client.Multiappend.append_many` streams 1..1000 nonempty messages with
per-message flags and INTERNALDATE under one connection lock. Its witness
requires MULTIAPPEND; no sequential
fallback weakens atomicity. Every argument is validated before APPEND dispatch,
with 1 MiB total syntax and bounded per-argument syntax. The existing OBJECTID+
destination guard applies before mutation. Single and batch APPEND share one
64 KiB streaming implementation, synchronizing each literal and carrying reply
budgets across the whole exchange.

Receipts retain input-message UID order, reject duplicates/cardinality mismatch,
and bound expansion by message count. Missing APPENDUID means known success with
unknown identity. A tagged rejection before a later literal consumes none of
that source and remains a definite atomic rejection. Short source, lost response,
invalid UID correspondence, unexpected continuation after the final literal,
or a partial notice followed by success closes and reports uncertainty;
cancellation closes and propagates. This is a low-level API, not a durable batch
journal or automatic replay mechanism. Current sync retains individual journaled
APPEND operations.

Release-check, all scripted Eio/protocol suites, 49 bridge-fault cases and all 29
live Dovecot cases passed. Directed tests cover exact wire separation, bounded
source reads, capability/preflight rejection, later-literal NO, missing/invalid
receipts, giant ranges, cancellation in the second source, aggregate reply
budgets and contradictory completion. Dovecot confirms input-order receipts,
exact message bodies, per-message flags and INTERNALDATE. Fixture removed.
Logs: `/tmp/imap-multiappend-live.log`, `/tmp/imap-multiappend-final.log`.


### Connection lifecycle checkpoint

Client now provides NOOP and graceful LOGOUT; Selected provides lease-scoped
NOOP. Polling retains unsolicited updates in wire order and does not advance
durable checkpoints. LOGOUT requires BYE followed by the matching tagged OK,
rejects malformed/repeated continuations and missing completion, and closes
on every outcome, including rejection and cancellation. Its metadata and reply
count budgets match the other session exchanges. Immediate Client.close remains
available; callers apply Eio timeout scopes when shutdown needs a deadline.

Scripted tests cover ordinary polling, selected update order and expired leases,
normal logout, missing/repeated BYE, wrong tags, continuations, rejection, EOF,
cancellation and response limits. Release-check, all scripted Eio suites and
29 live Dovecot tests pass. The live authentication case now verifies connection
and selected NOOP followed by graceful logout. Fixture removed after validation.
Log: `/tmp/imap-lifecycle-live.log`. Remaining production gates are unchanged.


### RFC 9738 capability and saved-result limits

Corrected the acceptance audit: SAVELIMIT is a capability restricting COPY and
APPEND, not a separate response code and not a SEARCH SAVE limit. Both capability
forms use the MESSAGELIMIT response code. MULTIAPPEND now checks advertised
MESSAGELIMIT/SAVELIMIT counts before dispatch, rejects malformed advertised
values, and permits batches exactly at the limit.

Directed SEARCHRES tests prove tagged or untagged partial MESSAGELIMIT SAVE
completion cannot mint a handle and invalidates the previous saved handle.
SAVELIMIT alone does not restrict SEARCH. SAVE COUNT validation also rejects an
unexpected ESEARCH PARTIAL field rather than treating it as complete evidence.
Release-check and all scripted Eio suites pass; these capability/error paths
were verified with controlled transcripts rather than a new live-server run.
Log: `/tmp/imap-save-limits-final.log`.


### Negotiated non-synchronizing APPEND literals

APPEND and MULTIAPPEND now use non-synchronizing literals up to 4096 octets when
LITERAL-, LITERAL+, or effective IMAP4rev2 permits them. Larger literals remain
synchronizing even with LITERAL+; this is permitted by RFC 7888 and avoids
unbounded speculative uploads. A dual-revision advertisement requires successful
rev2 activation unless an explicit literal capability independently permits it.
Binary APPEND still separately requires BINARY. Batch messages choose their
literal mode independently and share the same streaming buffer/reply budgets.

Directed tests cover 4095/4096/4097 bytes with rev1, LITERAL-, LITERAL+ and rev2;
failed/successful dual-revision activation; literal8 markers; mixed-mode batches;
tagged rejection and illegal continuations. Release-check, all scripted Eio and
protocol tests, 49 bridge-fault cases and 29 live Dovecot cases passed. The live
suite exercises sync/crash recovery and single, binary and multi-message uploads
through the changed path. Fixture removed. Logs:
`/tmp/imap-literal-modes-final.log`, `/tmp/imap-literal-modes-live.log`.


### Notification overflow regression checkpoint

Reviewed RFC 5465 section 5.8 against NOTIFY setup, IDLE and Watch. Existing
NOTIFY methods report setup overflow; IDLE retains the typed overflow update,
and Watch runs a fresh durable scan after every wakeup rather than advancing a
checkpoint from notification data. No runtime change was required for those
paths. The selected API now explicitly documents that overflow disables NOTIFY
registration and requires reconciliation before re-registration.

New directed tests place NOTIFICATIONOVERFLOW both before and after the IDLE
continuation, verify completion updates retain wire order, then issue NOOP and
UNSELECT to prove tagged completion was consumed. Notification floods before
continuation and during completion hit the aggregate response budget and close
the connection. Release-check and all scripted Eio suites pass.
Log: `/tmp/imap-notify-overflow.log`. This is controlled-transcript evidence;
whole-watcher crash/checkpoint schedules remain a separate production gate.


### Store ownership and journal decomposition

The store implementation now has private purpose-based modules:

- `Database` owns the shared SQLite handle, mutex, statement lifetimes and
  transaction helper.
- `Schema` opens/configures connections, validates supported schemas and applies
  migrations under that same transaction owner.
- `Record_codec` defines persisted IMAP scalar/scope representations.
- `Sync_journal` owns occurrence pairs, conflicts and mutation evidence.
- `Imap_store` retains mailbox staging/publication, the legacy append journal
  and blob operations; its public API is unchanged.

There are no extra public libraries, additional connection owners or nested
transaction boundaries. The former ~2,000-line implementation root is now
850 lines; blob and mailbox publication decomposition remains open.

Review identified existing initialization defects and fixed them during the
split. Invalid blob directories are checked before acquiring SQLite. Failed
initialization closes the acquired handle immediately while preserving the
original exception/backtrace. Read-only version and schema checks share one
read transaction, preventing mixed snapshots during concurrent migration.
Schema validation now checks the expected unique occurrence index definitions,
not merely column/index names; missing, nonunique or differently filtered pair
identity indexes are rejected.

Release-check, all 25 existing store cases (including v2..v13 migrations and
process restarts), directed journal/schema guard tests, flag/deletion suites and
49 bridge-fault cases pass. The new guard test repeatedly opens damaged schemas
within one long-lived switch and, on Linux, confirms no database descriptors
remain. Log: `/tmp/imap-store-boundaries-final.log`. Live server tests were not
repeated for this database boundary change; concurrent migration scheduling and
remaining structural/production gates are still open.


### Blob and command journal boundaries

`Blob_store` and `Operation_intent` now own content-addressed files/references
and command recovery respectively. Both are private modules within `imap.store`,
with explicit interfaces and the same `Database.t` owner. Cursor decoding lives
in `Record_codec`, so neither module depends on the public facade. Public type
constructors, `Imap_store.Blob`, and all existing entry points remain compatible.
No schema or transaction behavior changed. `Imap_store` is now 455 lines,
focused on mailbox staging and publication plus public re-exports.

Release-check and the store, bridge-fault, flag-sync and deletion suites pass,
including migration, process restart and >100k-row staging coverage. Validation
log: `/tmp/imap-store-purpose-check.log`. Live servers were not rerun for this
internal extraction. Unbounded orphan inventory and the remaining Maildir
ownership/inventory work remain open.


### APPEND journal metadata validation

New command intents now require a 64-character lowercase SHA-256 digest and
parse supplied INTERNALDATE values with the protocol date parser before
insertion. This closes the two validation gaps identified during the command
journal extraction review. Rejected preparation leaves no persisted intent and
the ID can immediately be reused for valid preparation.

Historical rows remain readable without rewriting their evidence. A directed
restart test injects an old invalid digest/date, reopens the store, verifies
inspection and pending-intent enumeration, and explicitly rejects that intent.
New tests also cover digest length/alphabet/prefix/control errors, impossible
dates, invalid clocks/zones, and preservation of valid timezone spellings.
Release-check, the new validation test, all store tests and 49 bridge-fault
cases pass (`/tmp/imap-intent-validation.log`). No schema migration or live
server behavior changed. Other production gates remain open.


### Bounded blob orphan collection

`Imap_store.Blob.iter_orphan_candidates` and `reap_orphans_iter` now scan
archives with batches of at most 256 names and indexed per-hash reachability
checks. Snapshot references and unresolved entries in either journal remain GC
roots. Write-open creates two auxiliary partial indexes without changing the
schema version or stored row representation. Existing list-returning APIs are
compatible wrappers and explicitly retain their unbounded result-list cost.

Directory handles close on normal return, callback exceptions and cancellation.
Reaping syncs the directory after any attempted unlink, including when a callback
fails or is cancelled. Callbacks run before that final sync and therefore do not
constitute durable removal receipts. All writers must still remain quiescent,
including writers in other processes; these APIs do not acquire a global writer
lease.

Release-check, store and 49 bridge-fault tests pass. A directed 1,031-file test
crosses multiple batches, rejects duplicate/missing visits, reads the store from
callbacks, interrupts reaping by exception and cancellation, checks directory
handle closure on Linux, and finishes collection afterward. SQLite query-plan
checks verify indexed SEARCH for all three root lookups. Existing store tests
verify roots in both journals survive reopening and collection. Logs:
`/tmp/imap-blob-stream-final.log` and `/tmp/imap-blob-query-plan.log`.
Large-scale peak-memory measurements and global writer ownership remain open;
this change establishes a bounded inventory implementation, not completion of
those broader production gates.


### Live Dovecot validation after store and GC changes

The required Dovecot suite was rerun against a fresh pinned 2.4.5 fixture with
shared filesystem mode enabled after the store decomposition, APPEND intent
validation and streaming blob GC changes. All 29 cases passed in 11.741 seconds
(`/tmp/imap-store-gc-live.log`). This includes successful/rejected CRAM-MD5,
verified TLS and STARTTLS, compression, ordered MULTIAPPEND receipts, direct
Dovecot/local Maildir metadata interchange, and durable APPEND/UID EXPUNGE
process-crash recovery. The disposable `imap-store-gc-check` container and its
owned certificate/Maildir tree were removed afterward.

This establishes the current Dovecot integration baseline. It does not close
the full crash-point matrix, adversarial concurrent Maildir access, large-body
memory measurements, or the separate Cyrus/Stalwart revalidation gates.


### Large-body streaming measurement baseline

The new opt-in Dovecot `body_memory.exe` probe runs in a fresh process per
transport/size, generates bytes incrementally, and checks exact APPEND/FETCH
lengths and SHA-256. All 1/10/100 MiB combinations passed over plain, TLS,
DEFLATE and TLS+DEFLATE with CRAM-MD5. The largest additional sampled live
managed heap was 80,408 bytes; additional sampled RSS peaked at 6,025,216 bytes.
Reproducible instructions and checked-in CSV evidence are in
`bleeding/imap/test/dovecot/README.md` and `body-memory-results.csv`.

The probe samples at MiB boundaries and forces collection for live-word counts.
This establishes a retained-memory baseline with a compressible payload, not
absolute peaks under normal GC or incompressible input. Those measurements,
STARTTLS-specific measurements and other streaming paths remain open. The
owned Dovecot fixture was removed after all 12 cases completed successfully.


### Review-checkpoint JMAP and Cyrus verification

A forced JMAP regression run passed 798 cases; 43 opt-in live cases skipped
without a configured fixture (`/tmp/imap-jmap-checkpoint-tests.log`). Against a
fresh isolated Cyrus fixture, the required IMAP oracle passed both cases
(`/tmp/imap-review-cyrus.log`) and the separate JMAP oracle passed all 45 cases
with no skips (`/tmp/imap-review-jmap-live.log`). The owned fixture was removed.

Inspection corrected an evidence overstatement: the current IMAP oracle has no
JMAP/LMTP calls. Its existing tests and separate JMAP tests do not establish
bidirectional body/keyword round trips. Those tests still need implementation;
`IMAP-TODO.md` and the oracle README now state that explicitly. Shared protocol
regressions pass, but the cross-protocol acceptance gate remains open.


### Cross-protocol oracle implemented

`test/oracle/test_cross_protocol.ml` now exercises JMAP import → IMAP exact body,
IMAP APPEND → JMAP exact download, JMAP keyword changes → IMAP SEARCH and IMAP
STORE → JMAP Email/get. The test checks BODY.PEEK leaves the initially unseen
message unseen, validates the destination mailbox and cleans up its isolated
mailbox through JMAP. It reuses the JMAP harness source in the private test build
without making the harness a public library.

All three Cyrus cases passed in a fresh fixture (`/tmp/imap-cross-live.log`).
Release-check passes. Separate executable checks confirm hermetic skip without
configuration and required-mode failure when either endpoint is missing.
The owned fixture was removed. This supersedes the earlier missing-roundtrip
finding for these paths; unrepresentable keywords and the full cross-protocol
MIME/duplicate corpus remain open in `IMAP-TODO.md`.


### Cross-protocol flag fidelity

The oracle now distinguishes the custom keyword `seen` from system Seen across
JMAP import, IMAP SEARCH/STORE and JMAP Email/get. It asserts the RFC 8621 §4.1.1
rule that IMAP Deleted messages disappear from JMAP get/query, then return with
keywords intact after removal of Deleted. The first attempted test incorrectly
expected the Email to remain visible; both Cyrus evidence and the local RFC
confirmed the required invisibility behavior, and the assertion was corrected.
A future proxy must implement visibility/count semantics, not merely discard
Deleted during keyword conversion.

Offline tests preserve an overlength IMAP keyword while rejecting its JMAP
validation, reject false semantic aliases for Recent/unknown system flags and
colliding names, and exclude Deleted from shared-to-JMAP conversion. These do
not constitute a complete proxy mapping API. Release-check and all four oracle
cases pass (`/tmp/imap-cross-flags-final.log`); the fixture was removed.
Mailbox-count and broader MIME/duplicate fidelity coverage remain open.
