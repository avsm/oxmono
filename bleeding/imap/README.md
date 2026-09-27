# IMAP for OCaml and Eio

The library is organized by purpose under `lib/`:

- `imap` contains pure protocol values, codecs, mirror planning and sync policy.
- `imap.eio` owns authenticated connections, selected mailbox leases and pools.
- The sibling [`maildir`](../maildir/README.md) package provides local
  message storage.
- `imap.store` provides SQLite snapshots, journals and blob archives.
- `imap.sync` contains `Engine`, `Bridge`, `Reconcile`, `Flags`, `Deletion` and
  `Watch` modules for durable synchronization.

Maildir system flags live in filenames; custom keywords use lowercase filename
letters and the standard `dovecot-keywords` mapping. INTERNALDATE lives in file
mtime. Flag changes preserve the basename, timestamps, Passed flag and extra
filename fields. Keyword-map updates and directory scans coordinate through
`dovecot-uidlist.lock`; a busy lock fails immediately. Pending journal state and
remote UID mappings remain in SQLite. More than 26 mapped keywords, unknown
system flags and unrepresentable dates fail explicitly. Existing nonempty
`.imap-flags` or `.imap-dates` directories require offline migration and are
never silently ignored or removed. No migration utility is provided yet.

Tests live under `test/`. The `imap-sync` executable and its private support
library live under `bin/`. Further internal module splits and
interface narrowing are tracked in [the structure review](../../IMAP-STRUCTURE-REVIEW.md).

The
`Imap_sync.Bridge` driver recovers a verified local publication or confirmed UIDPLUS
APPEND after a crash. It stops on unresolved operations rather than replaying a
possibly completed APPEND. A prepared copy that was never dispatched is
rejected on restart. If an operator has an independently attributable
APPENDUID receipt, `Imap_sync.Bridge.record_appenduid_evidence` records it; the
next cycle still verifies the remote UID's exact bytes, length and flags
before committing a pair. Matching message bytes alone are insufficient
evidence of ownership. Complete remote and local scans record durable
absence tombstones for established pairs. The default deletion policy
preserves both sides. Explicit `Imap.Sync_policy.Propagate` requires complete
inventories, paired content hashes and unchanged flags before removing a
survivor. Remote deletion requires UIDPLUS and CONDSTORE and uses targeted
UID EXPUNGE. Ambiguous deletion operations remain pending until a later
complete inventory proves the target absent.
An optional `min_absence_scans` grace period delays opt-in deletion until
that many additional complete remote scan generations have confirmed the
recorded absence. `imap-sync --min-absence-scans N` applies the same rule to
live sync and offline deletion plans; its default is zero.
SQLite schema v13 records complete-scan presence after an absence, so a
restored paired occurrence resets the grace clock if it disappears again.
When the restored Maildir occurrence has the paired bytes and INTERNALDATE,
the bridge retires its local absence tombstone and resumes flag reconciliation.
After a successful UIDPLUS APPEND, the bridge reads back the nominated UID's
exact bytes and checks its digest, length, flags and date before pairing it.
An altered server body leaves the identified operation pending for diagnosis.
Directional `Propagate_remote` and `Propagate_local` policies limit which
endpoint may lose its surviving copy. `imap-sync mark-local-retention` records
an explicit local eviction tombstone; even full propagation holds the remote
survivor for that pair. The bridge holds a cross-process Maildir writer lease
for each cycle; other writers must use the same lease.
`imap-sync plan-deletions` streams an offline candidate plan from the last
complete published remote inventory and a fresh local inventory. It does not
authorize mutation; the next sync revalidates against live IMAP state.
`imap-sync plan-sync` adds paged copy and paired flag candidates to that
read-only view, while respecting the populated-bootstrap hold.
Paired FLAGS writes verify the local body's saved digest before dispatch.
A changed body produces a durable `content` conflict without sending STORE;
a complete bridge scan clears the conflict once the original body is restored.
`imap-sync verify-local` performs an explicit paged hash pass over established
local pairs, detecting same-length body changes even when flags never change.
It records or clears those content conflicts without connecting to IMAP.
If a tombstoned local occurrence reappears with different bytes, the bridge
opens a durable content conflict. Its later disappearance does not clear the
conflict or authorize remote deletion; restoring the paired bytes clears it.
`imap-sync reject-remote-delete` can settle a sent or ambiguous remote DELETE
only when a live read-only verification proves that the same UID still has
the paired bytes, flags and stable MODSEQ. It rejects the old intent in SQLite
without sending another STORE or EXPUNGE.
`imap-sync finish-remote-delete` handles the complementary, explicitly
authorized case where a pending target still has exactly the paired bytes
and original flags plus `\Deleted`. It persists operator evidence before
targeted UID EXPUNGE and leaves uncertain outcomes pending for inventory
recovery.
Bridge receipts count held flag and deletion decisions, and `imap-sync`
returns a conflict exit status with affected pair IDs when requested work is
held.
`Imap_sync.Reconcile` inspects uncertain APPEND outcomes without replaying them.
The [imap-sync command](bin/README.md) runs bounded bridge cycles with
CRAM-MD5 or the client's negotiated authentication, inspects journal work
through a read-only SQLite connection, and records an operator-supplied
APPENDUID for later verification.
It also exposes explicit operator repairs for an uncertain local deletion
or a pending remote-to-Maildir copy whose reserved occurrence is absent, and
an explicit `settle-flags` action after both endpoints have been manually
aligned. The flag action verifies the paired content, date, UIDVALIDITY and
stable remote MODSEQ before replacing the common baseline without replaying
an uncertain STORE.
Provisional transfer files use a private scoped spool: exclusive creation grants
ownership, and callback exit closes and removes only the file that was
successfully created, including on Eio cancellation. Durable publication paths
retain their separate rename/fsync protocol. IMAP flag-set comparisons share
case-insensitive keyword semantics through `Mail_flag.Imap_flag`.

The code lives beside the [JMAP client](../jmap/) and shares `Mail_flag`
semantics. The
[design and standards handoff](../../IMAP-SPEC.md) describes the path to a
complete bidirectional syncer and IMAP/JMAP proxy. The local RFC corpus is in
[`spec/`](spec/).

The Eio client supports authenticated implicit TLS (the default), required
STARTTLS, and an explicit plaintext mode for test fixtures. It discovers
capabilities and authenticates with a lazily resolved password. Under TLS,
automatic selection prefers advertised SASL PLAIN, then CRAM-MD5, then LOGIN;
explicit plaintext transport prefers CRAM-MD5. Callers can pin PLAIN or
CRAM-MD5 with `Auth.password ~mechanism`, or supply a bearer token with
`Auth.bearer`/`Auth.refreshing_bearer` for OAUTHBEARER. Explicit PLAIN and
OAUTHBEARER require TLS unless the caller opts into insecure transport for an
isolated fixture. SASL-IR is used only when advertised. A failed mechanism is
never retried as another mechanism; OAUTHBEARER error continuations are
acknowledged before the tagged failure. Commands are serialized across fibers, including commands sharing a selected
mailbox lease. Join selected command fibers before leaving `with_mailbox`; an
escaped in-flight command closes the connection when its lease expires.
A rejected selection metadata check or failed UNSELECT closes the connection;
a failed lease release never returns a still-selected connection for reuse.
The client discovers NAMESPACE and extended LIST results, correlates optional
LIST-STATUS replies, lists/creates/renames/deletes mailboxes, manages
subscriptions through SUBSCRIBE/UNSUBSCRIBE and LSUB, and APPENDs message bytes
from an Eio source. It selects a mailbox for UID search, fetch, conditional flag STORE,
COPY, MOVE and targeted UID EXPUNGE. Selection requests CONDSTORE when offered;
it refuses RFC 4315 `UIDNOTSTICKY` mailboxes before handing a selected lease
to a caller, since their UIDs cannot support durable pairing.
QRESYNC can be enabled and selected with a saved checkpoint. IDLE waits for a
change on a dedicated selected connection. Capability-gated ACL, QUOTA,
METADATA and NOTIFY operations return typed responses; selected NOTIFY filters
use the active mailbox lease. SETQUOTA replaces the complete limit list for its
root. `fetch_to`
streams `BODY.PEEK[]` into a sink and verifies the UID and literal length at
tagged completion. Its output is provisional until it returns `Ok ()`; discard
it on error. Selected handles expire when `with_mailbox` returns. The callback
holds the session lock, so do not call another `Client` operation on the same
connection from that callback. Mailbox arguments are UTF-8 and are encoded as
modified UTF-7 on rev1 connections unless UTF-8 mode was enabled. Each LIST
and LSUB row carries its decoded `name`, whose `utf8` field is the decoded
name and whose `raw` field is the exact wire name. An interrupted APPEND
returns an uncertain outcome and must be reconciled before retrying.
Metadata FETCH is one typed call. `Selected.fetch` takes up to 1,000 UIDs and
a list of `Imap.Fetch_item.t` and returns one `Selected.row` per reported UID
in request order, and `Selected.fetch_range` does the same for a UID window
in ascending order. Rows for one UID merge, unsolicited UIDs are ignored, and
a conflicting value for an immutable item is a protocol error. RFC 8970
PREVIEW is the capability-gated `Preview` item, which preserves the
difference between an absent preview, `NIL` and empty text.
RFC 8474 OBJECTID is the `Emailid` and `Threadid` items, requiring the exact
`OBJECTID` capability and selected MAILBOXID. The independent
[OBJECTID+ draft -06](spec/draft-ietf-mailmaint-imap-objectid-bis-06.txt)
has an explicit `Client.enable_objectid_plus` mode and the `Objectid` item.
It parses the compound SELECT
ACCOUNTID/MAILBOXID, STATUS OBJECTID and message EMAILID/THREADID, retaining
unknown keys for future versions. STATUS OBJECTID requires prior activation,
including when requested through LIST-STATUS. The optional [with_mailbox]
identity argument selects by account/mailbox ID and refuses a name fallback to
a different mailbox. Typed CREATE and RENAME methods return the tagged compound
identity, or an uncertain outcome if the server omits it after success. This
draft mode does not activate legacy OBJECTID. Neither
path assumes an EMAILID is a JMAP Email ID without account identity evidence.

Ordinary and paged UID SEARCH require exactly one explicit SEARCH or matching
UID ESEARCH result. A tagged OK alone is not evidence of an empty mailbox.
Repeated result responses are rejected before UID expansion, and range searches
validate bounds on every capability path. Explicit empty SEARCH/ESEARCH replies
remain valid.

COPYUID receipts expose compact `mapping` ranges in server correspondence order.
Each range maps `source_first + i` to `destination_first + i` for
`0 <= i < length`. The separate source/destination sets describe membership;
independently sorting and zipping them does not preserve the mapping.

RFC 5256 sorting and threading are exposed as `Selected.uid_sort` and
`Selected.uid_thread`. SORT takes priority-ordered typed keys with per-key
ascending/descending order and preserves the server's result order. THREAD
supports advertised REFERENCES and ORDEREDSUBJECT algorithms, retaining parent,
child, sibling and dummy-parent structure. Both require an explicit charset
and typed `Imap.Search.t` criteria, reject missing/duplicate/partial results,
and bound results to 100,000 nodes (thread ancestry depth 100). These views
describe a server search at command time; they are not durable inventory
checkpoints or JMAP thread IDs. Typed criteria have no sequence-set key, and
under UIDONLY a `Raw` criterion that starts with a sequence set is refused.
`Selected.uid_sort_extended` adds RFC 5267 ESORT summaries and ordered UID
results. It always requests COUNT, correlates the UID ESEARCH reply to its
command tag, and validates requested fields against that count. MIN/MAX mean
first/last in sort order. UID range expansion follows ESORT's ascending-range
rule while preserving comma-element order. Positive positional PARTIAL pages
require `CONTEXT=SORT`; results remain bounded to 100,000 expanded UIDs, while
COUNT-only summaries can describe larger mailboxes. Page positions can change
between calls and are not durable scan checkpoints.

RFC 5182 SEARCHRES uses opaque `Selected.saved_search` handles. A successful
`uid_search_save` binds the server's saved set to the current mailbox lease;
saved FETCH, STORE, COPY, MOVE and targeted EXPUNGE validate the handle under
the command mutex. Another SAVE or a raw UID SEARCH invalidates it. The fixed
`uid_search_saved` operation queries a subset while preserving the handle.
EXPUNGE can shrink the set, so `saved_search_count` is the count captured at
SAVE completion. Metadata FETCH can include unsolicited updates; use the
correlated search API when membership matters. These handles are ephemeral,
and their mutation methods do not journal or retry uncertain outcomes.

`Selected.fetch_binary_to` streams transfer-decoded MIME sections using
BINARY.PEEK, with optional decoded byte offsets and a caller-supplied output
limit. It verifies UID, section, offset and length before reporting success;
sink bytes remain provisional until then. NIL and an empty section are
distinct results. The `Binary_size` FETCH item queries decoded lengths for
bounded UID lists without downloading their bodies. These operations require
BINARY or effective IMAP4rev2, whose requests are limited to leaf MIME parts.
Choose parts using BODYSTRUCTURE and let the server validate them. BINARY
output is for decoded content consumption; `fetch_to` and durable archival
retain the original transfer-encoded message.
`Client.append_binary_flow_receipt` explicitly opts into RFC 3516 literal8
APPEND and requires the BINARY capability, even under IMAP4rev2. It shares the
ordinary APPEND lock, destination identity check and uncertainty handling.
Servers may rewrite transfer encodings without changing decoded content;
fetch the stored representation before assigning a canonical body digest.
The durable exact-byte bridge continues to use ordinary APPEND.

`Error.Rejected` includes the optional typed server response code, so callers
can distinguish UNKNOWN-CTE, TRYCREATE, quota and other failures without parsing
explanatory text. Unknown extensions retain their raw code separately.
Authentication protocol failures and credential-provider exceptions use fixed
messages, so BYE text, malformed replies and provider diagnostics cannot echo
credentials into client errors. Capability and TLS preflight failures retain
their local explanations. Cancellation propagates.
Authentication errors redact server text and arbitrary code payloads while
preserving a whitelist of standard failure codes. No rejection automatically
triggers a retry; uncertain mutations still require reconciliation.

`Client.compress_deflate` explicitly activates RFC 4978 after authentication
and before acquiring a mailbox lease. It wraps the current transport, including
TLS, with continuous raw DEFLATE streams. Activation preserves the exact
plaintext/compressed boundary even when both arrive in one packet. Rejected
negotiation leaves the original transport usable. Compression uses bounded
buffers and preserves existing decoded-response and body limits; cancellation
or malformed streams close the connection. Outbound compression restarts its
LZ77 history per 64 KiB chunk, while inbound history persists. Activation is
opt-in because compression can expose lengths when secrets and attacker-chosen
data share a stream; it is never enabled during credential exchange.

`Imap_store` keeps cursor and UID snapshot changes in a single SQLite
transaction with revision and scope checks. Its operation journal records
prepared and uncertain APPEND intents so a restart can reconcile them; it does
not promise exactly-once APPEND. Its blob store streams exact octets into
content-addressed SHA-256 files, syncs file and directory metadata, and tracks
references by UIDVALIDITY and UID. `Imap_sync.Engine.archive_uid` publishes a blob
reference only after a verified BODY.PEEK[] completion.
`Imap_sync.Engine.hydrate_once` fills missing blob references from a published
snapshot in UID pages. It preflights RFC822.SIZE and bounds each pass by
message count, per-message size and total body bytes; an insufficient total
budget returns `more=true` without starting that body. It streams each body
to an exclusive provisional spool, then attaches the synced blob only after
a verified FETCH completion. Callers provide unique spool IDs and invoke
further passes until `more=false`.
The [imap-sync `hydrate` command](bin/README.md) runs one such pass from an
existing published inventory and exits 2 when another pass is needed.
The optional `sync --hydrate-bodies` mode runs one bounded hydration pass
after a bridge cycle finishes without pending transfers or conflicts.
The offline [audit-cache command](bin/README.md) rehashes cached blobs in
revision-pinned UID pages. It conditionally invalidates missing or corrupt
references, which a later hydration pass can refill from IMAP.
`Imap_sync.Engine.append_blob_journaled` verifies a durable source blob and records
the pre-send UID frontier, byte length and wire flags before APPEND.
`Imap_sync.Reconcile.inspect_append` can scan later UIDs and compare exact body
digests after a lost receipt. Its report is evidence, not an automatic commit:
another client may have appended identical bytes, or the original may already
have been expunged.
`Imap_sync.Engine.run_once` scans a finite UID range,
reconciles complete membership, excludes session-only `\Recent`, and publishes
through that store only after every command succeeds. It uses an opening
HIGHESTMODSEQ as a conservative checkpoint when CONDSTORE is available. On a
subsequent QRESYNC selection, it applies changed rows and fetches new UIDs,
then verifies complete UID membership before publication. Its
in-memory row/window limits are explicit. `Imap_sync.Engine.run_once_staged` uses
SQLite staging for larger mailboxes and atomically publishes a cursor plus row
count without returning a full OCaml snapshot. It currently performs a full
UID membership scan. With a same-epoch CONDSTORE anchor it seeds prior rows
inside SQLite and fetches only changed and new metadata; otherwise it fetches
all metadata. Abandoned stages remain inert after a crash until explicitly
discarded.
`Imap_sync.Watch.run` reconnects for each scan and IDLE wait, compares the new
selection against its published cursor to close the scan-to-watch gap, and
renews IDLE on a timer. A wakeup always triggers durable reconciliation before
the caller receives a publication receipt.

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let endpoint = Imap_eio.Transport.v
  ~net:(Eio.Stdenv.net env) ~host:"imap.example.org" () in
let auth = Imap_eio.Auth.password
  ~username:"alice" ~password:"secret" () in
match Imap_eio.Client.connect ~sw ~auth endpoint with
| Error error ->
    failwith (Imap_eio.Client.error_to_string error)
| Ok client ->
    match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
      (fun selected ->
        Imap_eio.Selected.uid_search selected ~criteria:Imap.Search.All) with
    | Ok uids ->
        List.iter (fun uid -> print_endline (Imap.Uid.to_string uid)) uids
    | Error error ->
        failwith (Imap_eio.Client.error_to_string error)
```

The pure library has checked UID/UIDVALIDITY/sequence/MODSEQ values, finite UID
sets, an incremental literal framer, typed response metadata, strict command
encoding, modified UTF-7/UTF-8 mailbox-name conversion, and a storage-independent
`Imap.Mirror` baseline reconciliation planner. The planner produces typed
add/change/remove deltas for atomic publication. Unknown FETCH fields retain
their raw syntax. Validated `Imap.Internal_date.t` values can be requested by
the `Imap.Fetch_item.Internal_date` item and supplied to
`Client.append_flow_receipt`; journaled APPEND saves the intended date before
the network write. Remote bridge imports set the message file's modification
time to the server's INTERNALDATE before syncing and publishing it. Local
uploads use that timestamp in UTC and verify the resulting instant. SQLite
schema v9 saves the source mtime before APPEND, v10 saves the paired date
baseline, and v11 saves the source date before local publication. Recovery
rejects timestamp drift instead of silently substituting the current time.
Schema v12 stores a first-verified compound
OBJECTID+ account/mailbox identity for each logical scope. On servers that
advertise OBJECTID+ and ENABLE, the staged sync scan activates the mode,
requires a complete SELECT account/mailbox identity before staging or
publishing, binds that identity, and uses it on later selections across
reconnects. A
different selected identity or reuse of one remote identity by another local
scope stops publication. It checks the configured name with STATUS before
scanning a bound mailbox and before journaled or directly pinned APPEND,
holding a known rename
or replacement rather than sending to the wrong destination. A saved binding
is also rechecked before standalone local append/delete repair, flag
settlement, UID archival or APPEND candidate inspection. A migrated live
cursor with changed UIDVALIDITY cannot acquire its first OBJECTID+ binding
without operator reconciliation. UIDONLY,
UIDBATCHES, PARTIAL and MESSAGELIMIT are capability
gated. A staged scan resumes RFC 9738 partial metadata FETCH and UID SEARCH
by the server's processed-UID boundary, and refuses to publish when that
boundary is missing or contradictory.
Incremental CONDSTORE CHANGEDSINCE windows use the same continuation rule;
publication still waits for a complete independent UID membership scan.
For mutations, a processed-UID MESSAGELIMIT boundary is surfaced as uncertain
and closes the connection; an atomic limit rejection of UID COPY remains a
normal rejection.
Typed ENVELOPE and BODYSTRUCTURE fetching is available for selected UIDs,
including bounded literal fields, address groups and nested MIME parts.
An Eio-native connection pool bounds authenticated clients, reuses healthy
sessions and replaces those closed by uncertain outcomes. Its clients belong
to the creation switch; a long IDLE wait should have its own connection.
Full QRESYNC delta reduction,
additional SASL methods, background hydration scheduling, automatic resolution of
ambiguous APPEND outcomes, and full bidirectional synchronization remain to be
implemented. Keep server credentials on
authenticated TLS outside isolated test fixtures.
The IDLE watch supervisor backs off repeated connection or scan failures from
5 seconds to a configurable 300-second cap, then resets after a successful
publication and wait. Connection setup and complete scans have configurable
30-second and one-hour deadlines. A timeout closes the connection and drops
the incomplete SQLite stage while retaining the last published cursor.

Run the scoped checks with
`opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/imap/all`.
The [Cyrus oracle instructions](test/oracle/README.md) explain the opt-in live
round trip, including exact message-octet verification, baseline publication,
flag delta checks, synced blob archiving, journaled APPEND, IMAP↔Maildir
transfer, paired flag merging and absence tombstones. The
[Dovecot fixture](test/dovecot/README.md) covers CRAM-MD5 and extension behavior.
The [digest-pinned Stalwart fixture](test/stalwart/README.md) independently
checks UIDPLUS, CONDSTORE, QRESYNC, exact bytes and bridge transfer. The
[Stalwart v0.16 fixture](test/stalwart_v16/README.md) additionally requires
live OBJECTID+ over certificate-pinned IMAPS. The
[resource-scale regression](test/scale/README.md) pages 100,001 real Maildir
occurrences and a durable SQLite journal.


The supported Eio API is defined in `lib/eio/imap_eio.mli`. Use
`Client.with_mailbox` to obtain a scoped `Selected.t`; its constructors and the
raw transport upgrade operations are internal. `imap_eio_core` is an installed
private implementation dependency, used directly only by low-level repository
tests. It is not a supported application API.


Maildir metadata locking rejects every existing `dovecot-uidlist.lock`, including
stale owners. A stale lock currently requires offline removal with all Maildir
users stopped. Automatic stale reclamation is disabled because pathname
check/unlink races can remove another writer's newly acquired lock. Unattended
crash recovery for this case remains a production gate.


For atomic multi-message uploads, construct borrowed streams with
`Imap_eio.Client.append_message` and send them using `Client.append_messages`.
Multiple messages require MULTIAPPEND. Optional receipt UIDs correspond to input
order; absent receipts and uncertain outcomes require reconciliation. The API
streams each literal with bounded buffers and does not journal or replay batches.


`Client.noop` polls connection updates between mailbox leases; `Selected.noop`
does so within a mailbox lease. Updates are returned in wire order.
`Client.logout` performs graceful shutdown and always releases the transport,
including errors or cancellation. Wrap it in an Eio timeout when a deadline is
required. `Client.close` remains immediate transport cleanup without LOGOUT.


APPEND automatically negotiates non-synchronizing literals for messages up to
4096 octets with LITERAL-, LITERAL+, or effective IMAP4rev2. Larger messages
continue to wait for permission before streaming. Binary APPEND additionally
requires BINARY, and MULTIAPPEND can mix literal modes within one batch.


Store internals separate SQLite ownership (`Database`), schema/migrations
(`Schema`), stored value encodings (`Record_codec`), synchronization evidence
(`Sync_journal`), command recovery (`Operation_intent`) and content-addressed
files (`Blob_store`). Mailbox staging and publication remain in `Imap_store`.
These modules are private; all operations retain the same
connection and transaction mutex behind the existing `Imap_store` API.


For large blob archives, use `Imap_store.Blob.iter_orphan_candidates` or
`reap_orphans_iter`. They scan names in bounded batches and check snapshot and
journal references through SQLite indexes. The older list-returning wrappers
collect all results in memory. Collection requires all writers to remain
quiescent. Reaping syncs deletions before return, including on callback failure
or cancellation; removal callbacks run before the final sync.
