# IMAP interface and structure review

This is the interface-first review requested on 2026-09-27. Its scope is
`bleeding/imap`, with the JMAP library boundaries and shared `mail-flag` API
as compatibility references. It does not propose reorganizing unrelated
projects or vendored dependencies in this monorepo.

The review reads Dune files, package declarations and interfaces. Implementation
correctness and dead-code conclusions require the subsequent implementation
review. The report below records the original interface review. Implementation progress
is recorded at the end; original file references describe the pre-move tree.

## 1. Current library and module map

An arrow below means “depends on”. External dependencies are listed separately.
All thirteen public libraries use Dune's default wrapping. None declares private
modules, preprocessing or locally disabled warnings.

| Public library | Modules | Internal dependencies | External dependencies |
| --- | --- | --- | --- |
| `imap` | `Proto`, `Wire`, `Response`, `Command`, `Mailbox_name`, `Internal_date`, `Mirror` | None | `mail-flag` |
| `imap.eio` | `Imap_eio`, `Auth`, `Error`, `Transport`, `Client`, `Selected`, `Pool`, `Session`, `Deflate_flow` | `imap` | Eio, Cstruct, TLS/TLS-Eio, X509, CA certificates, domain-name, IP address, Mirage RNG, Base64, decompress.de |
| `imap.io` | `Imap_io` | None | Eio, Cstruct, Digestif |
| `imap.store` | `Imap_store` | `imap` | SQLite3-Eio, Eio/Eio.Unix, Unix, Digestif, Cstruct |
| `imap.maildir` | `Imap_maildir` | `imap` | Eio/Eio.Unix, Unix, threads, Cstruct, Optint, Digestif, mail-flag, SQLite3-Eio |
| `imap.policy` | `Imap_policy` | None | mail-flag |
| `imap.sync` | `Imap_sync` | `imap`, `io`, `eio`, `store` | mail-flag, Eio, Optint |
| `imap.reconcile` | `Imap_reconcile` | `imap`, `io`, `eio`, `store` | mail-flag, Eio |
| `imap.flag-sync` | `Imap_flag_sync` | `imap`, `eio`, `store`, `sync`, `maildir`, `policy` | mail-flag |
| `imap.delete-sync` | `Imap_delete_sync` | `imap`, `io`, `eio`, `store`, `sync`, `maildir`, `policy` | mail-flag, Eio |
| `imap.bridge` | `Imap_bridge` | `imap`, `io`, `eio`, `store`, `sync`, `maildir`, `policy`, `flag-sync`, `delete-sync` | mail-flag, Eio |
| `imap.watch` | `Imap_watch` | `imap`, `eio`, `store`, `sync` | Eio |
| `imap.cli` | `Imap_cli` | `imap`, `eio`, `store`, `sync`, `bridge`, `flag-sync`, `maildir`, `policy` | mail-flag, Eio/Eio.Unix, Cstruct |

`imap-sync` is the sole installed executable. Its `Main` module depends on
`imap.cli` and `eio_main`. The protocol library and CLI explicitly enumerate
their modules. Other libraries use implicit module selection. `Imap_eio`'s
explicit interface re-exports Auth, Error, Transport, Client, Selected and Pool.
Session has no `.mli`; Deflate_flow has one but is not in that facade. An omitted
facade alias is not a substitute for a Dune private-module declaration.

Tests live both below source libraries and below `test/`. Unit groups cover
protocol, mirror, store, Eio, temporary I/O, Maildir, policy, flag synchronization,
deletion and CLI. Cross-layer groups cover bridge faults and scale. Integration
groups cover Cyrus (`oracle`), Dovecot and Stalwart, with fixture locks in Dune.
Test modules do not need public interfaces. There are no IMAP `.mld` pages.

The graph is acyclic. The issue is excessive public boundaries, not an existing
dependency cycle. Most sync libraries expose each other's types and cannot be
understood as independent products. The whole opam package also installs the
SQLite and Eio dependencies even for users of the pure protocol library.

JMAP provides a useful precedent: one pure `jmap` library with purpose-based
source subdirectories and one `jmap.eio` library with an explicit facade and
private modules. `mail-flag` already serves both protocols. Keep that common
type identity rather than introduce another representation of mail flags.

## 2. Interface findings

### Protocol and Eio client

- `bleeding/imap/eio/dune:1`: Session is implicitly included without an interface or private-module declaration, leaving its implementation boundary unspecified.
- `bleeding/imap/eio/selected.mli:6`: Public `create` accepts `Session.t`, and `invalidate` exposes lease administration that should belong solely to the client.
- `bleeding/imap/eio/transport.mli:19`: Raw flow reads, writes, compression activation and TLS upgrade share the public endpoint-configuration interface despite requiring session-controlled protocol boundaries.
- `bleeding/imap/eio/auth.mli:21`: Password/token resolution and SASL response construction are exported alongside credential constructors although they serve the authentication engine.
- `bleeding/imap/eio/client.mli:5`: Error printers live on Client rather than Error, requiring an unrelated module to format a shared failure type.
- `bleeding/imap/eio/client.mli:156`: APPEND accepts raw string flags while selected STORE accepts validated `Mail_flag.Imap_flag.t`, creating inconsistent mutation interfaces.
- `bleeding/imap/eio/selected.mli:43`: Selected operations mix bare int64 UIDs, raw UID-set strings and validated `Proto.Uid_set.t`, making identifier validation inconsistent across the safe client surface.
- `bleeding/imap/lib/mailbox_name.mli:6`: A freely constructible record allows `raw`, `mode` and decoded `utf8` to disagree; use a private record or abstract type.
- `bleeding/imap/lib/proto.mli:3`: Abstract scalar identifiers lack named equality, ordering and printers, encouraging callers to compare converted representations.
- `bleeding/imap/lib/internal_date.mli:22`: POSIX timestamp conversion is one-way, preventing Maildir timestamp writes through the validated date API.
- `bleeding/imap/lib/wire.mli:15`: Public resource limits have no documented defaults or invalid-argument behavior.
- `bleeding/imap/eio/pool.mli:17`: Borrowing is scoped by documentation but returns the full Client capability; preserve and verify rejection of escaped uses rather than assuming a callback alone enforces a lifetime.

Keep `Imap.Mirror` in the pure library. Its placement matches `Jmap.Mirror` and
allows a storage-independent planner. The fact that it is not a wire codec is
not sufficient reason to force SQLite or Eio onto its users.

### Storage and synchronization

- `bleeding/imap/maildir/imap_maildir.mli:4`: Custom flag and date sidecars are part of the public storage contract, preventing ordinary Maildir tools from maintaining the same metadata.
- `bleeding/imap/maildir/imap_maildir.mli:14`: A freely constructible occurrence record is subsequently accepted as mutation and identity evidence; make observations private and separate their identity from mutable file attributes.
- `bleeding/imap/maildir/imap_maildir.mli:31`: The writer lease is an independently callable convention while mutation accepts an unrestricted `t`; a scoped writer capability would make the precondition explicit.
- `bleeding/imap/maildir/imap_maildir.mli:70`: Maildir owns a temporary SQLite inventory and exposes it to mutation APIs, coupling the storage format to the sync algorithm.
- `bleeding/imap/maildir/imap_maildir.mli:105`: Upload date conversion belongs in the IMAP adapter rather than the format-neutral filesystem endpoint.
- `bleeding/imap/maildir/imap_maildir.mli:140`: Recovery exposes counters for custom formats that disappear with the standard Maildir redesign.
- `bleeding/imap/store/imap_store.mli:70`: Snapshot staging, durable intent state, pair state and blob storage share one large public interface; their transaction coordination should remain shared while their caller-facing responsibilities become separate modules.
- `bleeding/imap/flag_sync/imap_flag_sync.mli:29`: Pure flag planning and journaled network/filesystem execution share an interface and therefore require the complete synchronization dependency stack.
- `bleeding/imap/flag_sync/imap_flag_sync.mli:44`: Pair reconciliation requires the caller to hold a Maildir writer lease without representing that requirement in its parameters.
- `bleeding/imap/io/imap_io.mli:3`: A public library exists for just scoped spooling and file hashing, both implementation services for synchronization.
- `bleeding/imap/cli/dune:1`: The application configuration/runner is published as a library even though its purpose is the installed sync command and its tests.

### Additional synchronization findings

- `bleeding/imap/store/imap_store.mli:117` and `:254`: APPEND is represented in two public operation models; implementation review should determine whether their states and evidence can be unified without losing attribution or migration semantics.
- `bleeding/imap/store/imap_store.mli:179` and `:258`: Persisted pair and operation records expose identity/revision/evidence fields for caller construction; distinguish validated creation inputs from private readable persisted state.
- `bleeding/imap/store/imap_store.mli:17` and `:212`: The documented read-only schema ceiling disagrees with the later pre-v13 compatibility description.
- `bleeding/imap/store/imap_store.mli:434`: Orphan enumeration has an unbounded list interface, unlike paged inventory; assess behavior for large archives during implementation review.
- `bleeding/imap/sync/imap_sync.mli:25`: Identity guarding, scanning, journaled APPEND, caching and cache audit need separate purpose-named modules within synchronization.
- `bleeding/imap/reconcile/imap_reconcile.mli:40` and `bleeding/imap/bridge/imap_bridge.mli:209`: APPEND candidate inspection has two entry points with different evidence shapes; consolidate inspection while keeping explicit repair separate.
- `bleeding/imap/bridge/imap_bridge.mli:43`: The bridge interface combines transfer, integrity checking, preview and operator repair, rather than presenting these as separate operations with distinct authority.
- `bleeding/imap/policy/imap_policy.mli:56`: Eight related deletion booleans permit contradictory evidence; use structured endpoint observations and verified absence state.
- `bleeding/imap/watch/imap_watch.mli:18`: Explicit switch and clock parameters are good boundaries, but connection-factory ownership and callback exception behavior need documentation.
- `bleeding/imap/cli/imap_cli.mli:9`: A single configuration record spans fourteen commands and overlapping deletion switches; use command-specific payloads and one deletion policy.

### Documentation

- `bleeding/imap/eio/imap_eio.mli:1`: The facade has no synopsis, ownership overview or entry-point example.
- `bleeding/imap/eio/auth.mli:10`: Credential constructors lack individual documentation and optional-argument defaults.
- `bleeding/imap/lib/command.mli:5`: Many exported command encoders lack individual contracts, while grouped comments mix protocol requirements with session implementation details.
- `bleeding/imap/eio/client.mli:143`: The important non-reentrant lease and cancellation rules should survive the rewrite and become the central ownership contract.

These are interface findings, not claims of implementation-level races or dead
code. The latter need a separate implementation audit after the plan checkpoint.

## 3. Proposed restructuring

### Target public boundaries

| Library | Responsibility | Dependencies within this subsystem |
| --- | --- | --- |
| `imap` | Pure protocol values, framing, codecs and mirror planner | None |
| `imap.eio` | Authenticated connections, selected leases and pooling | `imap` |
| `imap.maildir` | Standard Maildir files, keyword mapping, timestamps and scoped filesystem operations | None; retain shared `mail-flag` |
| `imap.store` | Durable snapshots, operation journal, pair state and blob archive | `imap` |
| `imap.sync` | Scanning, reconciliation, Maildir bridge, repair and watch loop | `imap`, `imap.eio`, `imap.store`, `imap.maildir` |

This reduces thirteen public libraries to five. It preserves dependency isolation
for protocol users, network-client users and local-storage users. It does not
merge these five into one library or add an endpoint functor before a second
backend establishes the required abstraction.

Suggested source layout:

```text
bleeding/imap/
  lib/
    protocol/          # dune public_name imap
    eio/               # dune public_name imap.eio
    maildir/           # dune public_name imap.maildir
    store/             # dune public_name imap.store
    sync/              # dune public_name imap.sync
  bin/                 # private CLI library and imap-sync executable
  test/
    protocol/ eio/ maildir/ sync/ cli/
    bridge_faults/ scale/ oracle/ dovecot/ stalwart/
  doc/
  spec/
```

Directories express library boundaries; files within them express module
responsibilities. Further source subdirectories are appropriate only when they
group several related modules, using one library stanza where possible.

### Module moves and splits

1. Keep the existing protocol module names and `Imap.Mirror`. Add an explicit
   `Imap` facade interface. Keep Command and Response as coherent codec
   interfaces until implementation review identifies a supported split.
2. Keep Eio's public Auth, Transport, Client, Selected, Pool and Error names.
   Introduce private Session, Connection/transport operations, authentication
   operations and Deflate_flow modules. Give Session an explicit internal
   `.mli`. Use an internal Selected implementation with a narrower exported
   signature so only Client can construct/invalidate a lease. Private-module
   types must not leak into the installed signatures.
3. Split `Imap_sync` into Scan, Cache and Append; consolidate Reconcile,
   Bridge, Flags, Deletion, Watch, Preview and Repair in the same wrapped
   library. Move pure flag/deletion planning into `Imap.Sync_policy`. Keep command orchestration separate from
   policy decisions even though they share a library.
4. Retain `imap.store` as a reusable persistence boundary for syncers and
   proxies without requiring the Maildir driver. Separate snapshot operations,
   journal operations, pair state and blob archive into focused modules backed
   by one private database owner and shared transaction implementation. Preserve
   atomic commits across these responsibilities; do not open one independent
   database per module. Expose only the inspection/configuration types needed
   by users of the sync engine and operator repair API.
5. Move `Imap_io` into private synchronization modules named for their purpose,
   such as Spool. Keep one ownership implementation for each resource kind.
6. Move SQLite-backed local inventory staging into synchronization. Maildir
   provides bounded enumeration and guarded access to observed occurrences.
   Inventory staging continues to detect duplicates and prove complete scans
   without materializing a large mailbox in OCaml memory.
7. Keep Maildir as an independent storage endpoint. Represent timestamps as
   POSIX instants; convert to/from `Imap.Internal_date` in the sync adapter.
   Split filename parsing and Dovecot keyword-map management into private
   modules. Use private occurrence observations and a scoped writer capability.
8. Move CLI code into `bin/`, with a private library for configuration and test
   access. Keep the installed `imap-sync` command and its behavior.

### Eio ownership contract

- A switch owns network connections and persistent database resources.
- A selected-mailbox callback owns an expiring lease. Session internals issue
  and revoke it; applications cannot manufacture one.
- A Maildir writer callback receives the capability required by mutations.
  Escaped handles fail after the callback. Application writer coordination and
  the short Dovecot metadata lock have separate purposes and lifetimes.
- Temporary inventories and message spools are callback-scoped and are removed
  on return, exception and cancellation. Cancellation propagates.
- A durable operation outcome remains distinct from a resource lifetime.
  Closing a connection cannot turn an uncertain APPEND/STORE/EXPUNGE into a
  safe automatic retry.
- Environmental I/O exceptions are documented. Expected conflicts and format
  limits have typed outcomes. Do not catch cancellation as an ordinary error.

### Maildir format change

Adopt Dovecot-compatible Maildir metadata: system flags in `:2,` filenames,
keyword letters through `dovecot-keywords`, and INTERNALDATE through file
mtime. Keep remote UID mappings, operation journals and original protocol
timestamp spellings in SQLite. Maildir++ folder layout alone does not specify
the arbitrary-keyword mapping or remote sync journal.

Preserve message basenames during flag changes and preserve mtime. Publish the
keyword map durably before a filename refers to a new mapping. Respect Dovecot's
metadata locking conventions. Treat the 26-letter keyword limit and timestamps
that cannot be represented by the filesystem as explicit unsupported outcomes,
before publishing a partial message. Unknown IMAP system flags must not silently
become ordinary keywords.

Existing nonempty custom sidecar stores must fail with an actionable format
error until an explicit migration is available. Never silently ignore metadata,
reinterpret it as absent, or delete it during ordinary open. If migration is
implemented, preflight the whole conversion, journal it, preserve identities,
and make it restartable before removing legacy metadata.

### API break costs and affected callers

This is an intentional source-level API change for the unreleased IMAP tree.
It need not change the existing SQLite schema merely to reorganize OCaml code.

| Change | Callers to update |
| --- | --- |
| Remove `imap.io`, `imap.policy`, `imap.reconcile`, `imap.flag-sync`, `imap.delete-sync`, `imap.bridge`, `imap.watch`, `imap.cli` public names | All IMAP Dune stanzas, CLI, unit/fault/scale/integration tests, README and IMAP-SPEC examples |
| Replace workflow modules with `Imap_sync` submodules; reorganize `Imap_store` submodules | Sync drivers, CLI, repair paths and all storage/bridge tests |
| Hide lease construction, SASL internals and flow upgrades | Eio siblings and low-level tests; test internal modules via a private test-support boundary rather than public escape hatches |
| Validate client identifiers and APPEND flags consistently | Sync/bridge code, client tests and examples; retain raw syntax only in explicitly low-level codecs |
| Introduce private Maildir observations and scoped writers | Bridge, flag/deletion/recovery paths and Maildir fixtures |
| Move staged inventory to synchronization and remove sidecar recovery fields | Bridge/scale tests, Maildir tests, sync inventory code |
| Use mtime rather than stored timezone spelling | Date comparisons, restart and crash tests; compare instants, not original text |

No Dune consumers of the IMAP libraries were found outside `bleeding/imap` in
the non-vendored workspace. Unknown external consumers would need the same
source migration. Avoid retaining nine compatibility libraries merely to
preserve this prerelease layout. Shared `mail-flag` and JMAP public names remain. Keeping `imap.store` is
intentional: a proxy or offline archive should not acquire a Maildir/network
workflow dependency just to access durable state. It becomes a multi-module
library, not another single-module directory.

### Implementation sequence and verification

1. Review implementations against this interface plan, one module per review
   agent, grouping only trivial modules as permitted by the review skill.
2. Perform mechanical library consolidation and facade narrowing first. Build
   the entire IMAP subtree and run unit tests before changing storage behavior.
3. Introduce scoped storage capabilities and move inventory staging with its
   existing bounded-memory and crash tests.
4. Replace Maildir sidecars and test keyword-map failure ordering, timestamp
   preservation, stable basename/UID identity, overflow and legacy rejection.
5. Run protocol/Eio tests, store/bridge fault tests, 100k-message scale coverage,
   and the live Cyrus/Dovecot suites. Specifically verify external Dovecot
   visibility of flags/keywords/dates and UID stability after flag changes.
6. Build installed interfaces and documentation; check private modules and
   test-only operations are absent from the supported facade.

The completed pre-restructure COMPRESS release-check build passed, including
49 bridge-fault tests and all 27 live Dovecot tests. Its disposable container
has been stopped. Those results are a baseline, not validation of this plan.

## 4. Redocumentation plan

Rewrite all IMAP public interfaces under the requested `doc-style` skill,
starting with the five facade interfaces and their ownership contracts.
Protocol files are `proto.mli`, `wire.mli`, `response.mli`, `command.mli`,
`mailbox_name.mli`, `internal_date.mli` and `mirror.mli`. Eio files are
`imap_eio.mli`, `auth.mli`, `transport.mli`, `client.mli`, `selected.mli`,
`pool.mli` and `error.mli`. Rewrite the Maildir interface and the new sync
module interfaces after their boundaries are settled. Internal interfaces
receive concise internal contracts, not public tutorials.

Each public value needs its own contract, defaults and failure behavior.
Preserve observable resource limits, uncertainty semantics, completeness
requirements and cancellation rules. Move implementation history and codec
workarounds out of public API prose. Add `.mld` entry pages with one scoped
client example, one durable-sync example and an explanation of format limits.
Update README and IMAP-SPEC so they describe the final layout and contain no
remaining promises of custom sidecar metadata.

## Implementation checkpoint

Work continued following the subsequent instruction to continue. The five public
library boundaries now exist under `bleeding/imap/lib/{protocol,eio,maildir,store,sync}`.
Workflow modules are Engine, Bridge, Reconcile, Flags, Deletion and Watch.
Sync_policy is in the pure library. Spool is private. CLI support is private
under `bin/`; library unit tests have moved under `test/`.

The first implementation-review batch covered Session, Maildir and Store.
It found and fixed cancellation-sensitive cleanup, unretained control literals,
flag-order-sensitive occurrence checks, nonexclusive temporary inventory file
creation, missing journal GC roots, an unlocked conflict query, migration
version inspection outside the migration transaction, quarantined-epoch presence
checks, and the missing pair-pagination ordering index. Regression tests cover
control literals and limits, flag-order equivalence, and journal-root survival
after reopen and collection. Existing schema-migration and bridge fault tests
also pass. The concurrency-specific fixes still need dedicated race regressions.

Remaining work includes the standard Maildir format, scoped writer capabilities,
private Session/transport/auth machinery, purpose-based splits within Store and
Engine, stronger persisted journal invariants, and the rest of the per-module
implementation review. This checkpoint does not claim the full plan complete.


### Second review batch and Maildir conversion

The standard metadata conversion is implemented and validated. Maildir no longer
creates custom flag/date sidecars; its occurrence records are private and include
inode/change-time identity. Writer capabilities and relocation of SQLite inventory
staging remain unfinished.

Client, Selected and Transport were reviewed without edits by the review agents.
The following concrete findings were identified at the then-current Eio source paths:

- `client.ml:130`: authentication protocol/provider diagnostics can expose secrets.
- `client.ml:499`: dangling else breaks METADATA-SERVER capability gating.
- `client.ml:645` and `:678`: failed SELECT validation or UNSELECT can leave an open selected session outside its lease.
- `client.ml:2`: stored secure field is unused.
- `selected.ml:742`: normalizing COPYUID source and destination independently loses positional correspondence.
- `selected.ml:140` and `:318`: repeated ESEARCH expansions can exceed the aggregate allocation bound before checking it.
- `selected.ml:140` and `:327`: missing SEARCH replies can be mistaken for empty inventories.
- `selected.ml:348`: non-MESSAGELIMIT range search bypasses bounds/order validation.
- `selected.ml:428` and `:457`: structured FETCH reorders requested UIDs despite the interface contract.
- `selected.ml:975`: missing BINARY UID returns Protocol rather than documented Missing_uid.
- `transport.ml:61` and `:77`: direct public TLS operations have unsafe failure cleanup/reuse; hide these behind session ownership.

Preserve these as correctness work before further mechanical interface hiding.
The full reports also identify duplicated range and response decoders, but their
consolidation must retain completeness and uncertainty checks.


### Client and Selected correctness fixes

The second-batch correctness findings are now addressed. Authentication exchanges
sanitize protocol/provider/transport diagnostics while preserving cancellation;
METADATA-SERVER gating is corrected; failed SELECT validation and UNSELECT close
the connection; the unused secure field is removed. SEARCH requires one explicit
result before expansion and validates ordinary range bounds as well as limited
pages. COPYUID carries ordered compact correspondence ranges. Structured FETCH
retains first-occurrence request order and rejects repeated ENVELOPE data.
BINARY payloads missing UID return the documented typed error and close safely.
Transport close is idempotent and cancellation-protected; failed TLS upgrade
closes the original flow, and upgrade returns unit instead of the same handle.

Release-check, the full scripted Eio suites, 49 bridge-fault tests and all 27 live
Dovecot tests pass. New regressions cover secret-bearing authentication errors,
provider exceptions, failed lease cleanup, metadata scope, missing/repeated/wrong-tag
SEARCH responses, explicit empty results, ordinary bounds, COPYUID comma order,
reversed ranges, a billion-UID compact mapping, request order and duplicate
ENVELOPE rejection. Existing BINARY tests now assert the documented missing-UID
error for a payload without UID.

Public internal Session/Transport/Selected construction boundaries still need
narrowing. Decoder/loop consolidation and the remaining per-module reviews are
also pending; the correctness fixes do not finish the structural plan.


### Sealed Eio resource boundary

`lib/eio/imap_eio.mli` now defines the supported public signatures explicitly.
Auth exposes credential construction and policy, Transport exposes endpoint
configuration, and Selected exposes mailbox commands on leases supplied by
Client. Credential extraction/encoding, raw transport mutation, and selected
lease construction/invalidation are absent. Public abstract types are shared
across Client, Selected and Pool but have no exposed equality to core types.

The implementation sits in the package-private `imap_eio_core` library in the
same directory. It is installed for linking and deliberately used by low-level
in-project tests; this is an API boundary, not a security sandbox against callers
manually accessing private installed artifacts. There is no additional supported
public library. Internal per-module interfaces remain implementation contracts;
`imap_eio.mli` owns the supported API documentation. Session now has an explicit
internal interface. Its unused untagged callback and optional response-discard
mode were removed, retaining unconditional response-count accounting.

API break: callers of Selected.create/invalidate, Transport flow functions or
Auth secret/encoding functions must stop using those internals. Existing client,
mailbox, pool and sync callers require no changes. Encoder, codec and session
unit tests explicitly depend on the private core and do not mix its types with
public handles. Compile regressions exercise supported entry points and reject
11 formerly exposed internal functions.

Validation: release-check builds all IMAP targets; all scripted Eio suites,
public API compile checks and 49 bridge-fault tests pass. Live server tests were
not repeated for this interface-only boundary change.


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

Validation for the durable evidence checkpoint: release-check, store and directed
evidence tests, flag/deletion suites, 49 bridge-fault cases and all 27 live Dovecot
cases pass. The isolated Dovecot fixture was removed after verification.


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


### Bounded stale metadata lock inspection (superseded below)

Maildir stale-owner inspection now opens a nonblocking descriptor, verifies its
regular-file identity against the inspected path, and reads at most 1,025 bytes
before deciding whether a lock is eligible for recovery. This removes the
unbounded reopen/read race when the path is replaced or the lock grows. The
final unlink still requires the inspected inode to remain at the lock path.

Directed tests recover a dead local PID and preserve live, foreign-host,
malformed, oversized, empty, FIFO, directory and symlink locks. Release-check,
all 12 Maildir cases and 49 bridge-fault cases pass (`/tmp/imap-lock-recovery.log`).
This does not complete adversarial concurrent replacement, cancellation or
crash-point coverage; those remain open production gates.


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

`Client.append_message` describes borrowed streams and `Client.append_messages`
streams 1..1000 nonempty messages with per-message flags and INTERNALDATE under
one connection lock. Multiple messages require MULTIAPPEND; no sequential
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

Independent implementation reviews found no extraction regressions or new
resource owners. The command journal still accepts nonempty, noncanonical
digests and printable dates without parsing them at preparation time. These
pre-existing validation gaps remain follow-up work; tightening them requires
checking compatibility with persisted legacy intents and their callers.


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
