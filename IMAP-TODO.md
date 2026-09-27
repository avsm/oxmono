# IMAP remaining work

Updated 2026-09-27. This is the current review/handoff checklist, not a claim of
production readiness. Design requirements remain in [IMAP-SPEC.md](IMAP-SPEC.md)
§§14–15; historical evidence is in
[PRODUCTION-GATES.md](bleeding/imap/PRODUCTION-GATES.md). Start a code review with
[IMAP-REVIEW-CHECKPOINT.md](IMAP-REVIEW-CHECKPOINT.md).

## 0. Restructuring in progress (accepted 2026-09-27)

This section is the resumable state of the interface restructuring accepted
from the Phase 1 interface review. A resumed session starts here: read the
environment rules, find the first step whose status is not `done`, and
continue it. Every step ends with a clean build, a clean test run, one commit,
and this table updated with the commit hash. A subagent doing a step updates
its own row and the step notes below, and nothing else in this file.

### Environment

All dune commands run in the OxCaml switch, from the repository root. The
`imap` package and the sibling `maildir` package at `bleeding/maildir/` build
and test together:

    opam exec --switch=5.2.0+ox -- dune build @bleeding/imap/all @bleeding/maildir/all
    opam exec --switch=5.2.0+ox -- dune build @bleeding/imap/runtest @bleeding/maildir/runtest --force

From a git worktree under `.claude/worktrees/`, dune otherwise resolves
to the parent checkout's workspace, so add `--root .` to both commands
there, as in `dune build --root . @bleeding/imap/all`.
Live-server tests skip without their environment variables. ocamlformat is not
usable in that switch, so match the surrounding formatting by hand and keep
lines within 80 columns. Never regenerate a golden file to make a test pass.
Work is on branch `minus39`. One commit per step, with a one-line imperative
message and no trailers. Prefer Eio operations over `Unix` wherever Eio provides the operation
(`Eio.Path` for open, stat, rename, unlink and directory listing, `Eio.File`
for descriptors and sync); keep `Unix` only for what Eio lacks, such as
`lockf`, `utimes` and directory fsync, and say so in a comment at the call.
When an `Eio.Io` is caught to be re-raised, re-raise it with
`Eio.Exn.reraise_with_context` naming the operation and the path, so a trace
shows where it happened. New library I/O failures are `Eio.Io` values with a
printer registered through `Eio.Exn.register_pp`, not `Failure` strings.
Interfaces follow the `doc-style` rules: `[f x] is`,
full sentences, no colons or em dashes joining clauses, defaults stated for
every optional argument, no history in the prose. Implementations carry no
comments unless the code cannot say it.

### Review pause, 2026-09-27, after step 4

The session stopped here at the user's request so the work so far can be
reviewed before steps 5 to 15 run. Tree state: `minus39` at 71411e8c1,
58 commits since the baseline b4084133b. Both packages build with
`dune build @bleeding/imap/all @bleeding/maildir/all` and all 16 test
suites pass with `--force`. Nothing is uncommitted.

What to review, in order:

1. The new standalone package at `bleeding/maildir/`, especially
   `lib/maildir.mli`: the typed `error`, the `with_writer` capability, the
   `fold`, and the exceptions kept for concurrency conditions.
2. `bleeding/imap/lib/protocol/capability.mli` and the `Unsupported` and
   `Not_enabled` variants in `bleeding/imap/lib/eio/imap_eio.mli`, which
   are the base of the extension-aware interface.
3. The "Step F notes" below for each library, which list every behaviour
   change the fix wave made.
4. `git log --oneline b4084133b..HEAD` for the commit-by-commit record.

Decisions the agents took that deviate from the brief, each documented in
its step or F note and worth a look:

- Store: a tombstone can be replaced only by the same or a more permanent
  reason, rather than never, because Bridge renews absence tombstones.
- Eio: ENABLE is gated on advertised IMAP4rev2, not effective rev2, so a
  dual-revision server still gets `ENABLE IMAP4rev2`; STATUS DELETED also
  accepts rev2; refresher exceptions become `State "invalid credentials"`.
- Maildir: publication and flag changes stay `rename`, with duplicates
  refused under the metadata lock; the dovecot-keywords parser tolerates
  blank lines and a missing final newline but still rejects malformed
  lines; an unknown flag letter still fails the scan so the syncer never
  treats an unreadable entry as absent.
- Sync: when only the stored flag baseline is behind, it advances with a
  direct `put_pair` and no operation; an oversized message is skipped by
  hydration and audit with its UID reported.
- Step 4: `Local_inventory` stages a `Marshal`led occurrence blob keyed by
  id in `<spool_dir>/local-inventory-im-<hex>.sqlite3` because the
  occurrence record is private; the alternative is a public constructor
  and typed columns. The offline plan and verify commands now take and
  create `--spool-dir`. Bridge, Flags and Deletion gained a
  `Maildir of Maildir.error` case.

Open questions for the user before resuming:

- Keep the Marshal staging blob, or add a `Maildir.occurrence` constructor
  and keep typed columns.
- `Imap_store.forget_epochs` exists but nothing calls it yet; step 12 wires
  it and the orphan collector into the CLI. Confirm that quarantined epochs
  may be dropped by an explicit command.
- The two APPEND journals stay this round; unifying them is a schema v14
  migration recorded as follow-up.
- Step 11 removes `Engine.run_once`, `Imap_store.publish` and `load`, the
  `Mirror.complete` and `publish` chain and `Reconcile`, all test-only.

Resumed 2026-09-27 at step 5 after the user reviewed. Steps 5 to 16 run
one agent per step in the main checkout, verifying both packages after
each step. Step 16 is the OxCaml performance pass the user asked for, to
run only once everything else works.

### Steps

| # | Step | Status | Commit |
|---|------|--------|--------|
| 0 | Baseline commit of the untracked IMAP tree and its shared-library edits | done | b4084133b |
| R | Phase 2 implementation review by subagent, one per module, findings in section 0.R | done, 21 reviews, 336 findings | |
| F | Apply Phase 2 correctness fixes in severity order, then dead code, redundancy, comments | done; 235 findings fixed, the remainder annotated to their steps; CLI findings fold into step 12 | e449db4a4 |
| 1 | Plan item 8: strip duplicated docs from core Eio `.mli` and private store `.mli` to one-line internal contracts; rename `Imap_store.Sync` to `Journal` | done | d948dc830 |
| 2 | Plan items 1 to 3: `Imap.Capability`, typed `Response.Capability`/`Enabled`, `Error.Unsupported`, typed `Client.capabilities`/`enabled`/`has`/`enable` | done | a21c8b091 |
| 3 | Plan item 12: `spool` and `database` as private support libraries shared by their library and their tests; drop the copy_files rules in test/io and test/store/database | done | 722b25c41 |
| 4 | Plan item 10: standalone `maildir` package at `bleeding/maildir/`; no `imap` or `sqlite3-eio` dependency; `Local_inventory` in sync; `with_writer` capability; typed errors; `Dotlock` public | done | 68af6a250 |
| 5 | Plan item 5a: dissolve `Proto` into `Imap.Uid`, `Uidvalidity`, `Modseq`, `Uid_set` with `equal`, `compare`, `pp`; unify identifier shapes across `Selected` | done | 7dbd4c0ca |
| 6 | Plan item 5b: move vocabulary types out of `Command`; `Command.error` a real type; label mailbox arguments; `Mailbox_name.t` private; `Client.list` returns `Mailbox_name.t` | done | 2415590b9 |
| 7 | Plan item 5c: `Imap.Search` and `Imap.Fetch_item`; one `Selected.fetch` replacing the six fifty-UID fetchers | done | 5681e332f |
| 8 | Plan item 6: one `Client.append` and `append_many`; typed flags on APPEND | done | b03f5b918 |
| 9 | Plan item 4: extension witness submodules on `Client` and `Selected`, each with `require` | done | 42eb28cab |
| 10 | Plan item 9: `with_mailbox` reentrancy returns `State` instead of blocking | done | 286cc6993 |
| 11 | Sync moves: `Ctx` record, single `Imap_sync.Error.t`, `Repair` module, `Plan` module, one APPEND inspection, drop `Engine.run_once` if unused | todo | |
| 12 | CLI on cmdliner with one term per command and a single `deletion_policy` option; also applies every `bin/imap_cli.ml` finding from 0.R and wires the blob orphan collector and `forget_epochs` into startup under the writer lease | todo | |
| 13 | `imap.mli` facade, `.mld` pages, `(documentation)` stanza, dune-project dependency fixes | todo | |
| 14 | Plan item 7: `Imap_eio.Mailbox` strategy layer | todo | |
| 15 | Redocumentation pass under doc-style over every public interface | todo | |
| 16 | OxCaml pass after everything works: load the `oxcaml` skill, then annotate `portable`, `contended` and `local` modes and stack-allocate hot-path values where the compiler proves it and a measurement shows a gain; every `.mli` mode claim comes from a compiler probe, never from memory | todo | |

Decisions taken: extension witnesses rather than plain submodules; `maildir`
becomes its own package now; the `imap` package split into protocol, eio and
sync packages is deferred to match the JMAP sibling; the strategy layer is
the last step.

### Step notes

Step 1. The facade `lib/eio/imap_eio.mli` is the single documented copy of
Auth, Error, Transport, Client, Selected and Pool. Each core `.mli` keeps its
signature and gets a one-sentence synopsis plus one-line value docs at most.
The same for `lib/store/sync_journal.mli`, `operation_intent.mli` and
`blob_store.mli`, which `lib/store/imap_store.mli` re-exports. Guard:
`test/api/check.sh` must still pass.

Done: the core Eio and private store `.mli` files now carry a synopsis
naming their documented facade, the signature, and one sentence where a
contract is not obvious. `Session` and `Deflate_flow` have no facade copy
and keep one-sentence internal contracts. The facades gained the 16 MiB
compressed-input budget in `Client.compress_deflate` and the `Blob`
synopsis. `publish_stage` now states the current rule, an anchor of
`None` without an explicit HIGHESTMODSEQ. `Imap_store.Sync` is
`Imap_store.Journal` in lib/sync, bin/imap_cli.ml and every test.
`Operation_intent` takes `Database.t` directly, so `imap_store.ml` includes
it instead of restating the intent types, which settles the remainder of
the imap_store.ml:29 finding. Every touched `.mli` parses cleanly under
`-w +50`, and the facades are within 80 columns. Build and runtest are
clean, 15 suites and 216 test cases.

Step 2. `Imap.Capability` in `lib/protocol/` with constructors carrying
parameters where the wire does: `Auth of string`, `Thread of algorithm`,
`Compress of [ `Deflate ]`, `Utf8 of [ `Accept | `Only ]`,
`Context of [ `Search | `Sort ]`, `Messagelimit of int64`,
`Savelimit of int64`, and `Other of string`. Provide `of_wire`, `to_wire`,
`equal`, `compare`, `pp`, and a set with `mem`, `of_list`, `to_list`.
Capability names compare case-insensitively per RFC 9051. Seed the
constructor list from the capability strings enumerated by the Phase 2
reviews of `client.ml` and `selected.ml`. `Error.Unsupported of
Imap.Capability.t` replaces every `State`/`Limit` text returned for a missing
or unenabled extension. `Client.enable` is the general RFC 5161 ENABLE.

Done: `Imap.Capability` types every token the reviews named plus the
standard ones, keeps anything else as `Other` in its received spelling, and
compares tokens case-insensitively. Its `Set` stores an `Other` token as
`of_wire` reads it. The typed lookups are `messagelimit`, `savelimit`,
`auth_mechanisms`, `thread_algorithms` and `quota_resources`.
`implied_by_rev2` holds for ENABLE, IDLE, NAMESPACE, UIDPLUS, MOVE,
SEARCHRES, ESEARCH, LIST-EXTENDED, LIST-STATUS, UNSELECT, SASL-IR and
LITERAL- (RFC 9051 Appendix E item 2) and STATUS=SIZE (item 3). BINARY is
excluded, and the FETCH side of BINARY keeps its own rev2 gate in
`Selected`. `malformed_limit` keeps the `Protocol` error for an invalid
advertised MESSAGELIMIT or SAVELIMIT. `Response.Capability` and `Enabled`
carry deduplicated typed lists, and `[CAPABILITY ...]` is a typed code.
`Command.enable` encodes ENABLE and refuses a token that is not an atom.
Session stores typed sets, and `Session.has`, `require` and
`require_enabled` are the one gate in lib/eio. Every missing-extension
`State` text is now `Error.Unsupported c` and every missing mode
`Error.Not_enabled c`. A missing MESSAGELIMIT is `Unsupported (Other
"MESSAGELIMIT")`. `Client` gains `has`, `is_enabled` and `enable`, and
`enable_uidonly`, `enable_objectid_plus` and the three automatic enables
go through it. Every gate now folds effective IMAP4rev2 the same way, so
SASL-IR, several LIST patterns and the lib/sync UIDPLUS test accept a
rev2 server without the token. ENABLE sends the canonical uppercase
spelling, and ENABLE while selected is one `State` message. lib/sync
engine, deletion, flags and watch use `Client.has` and `is_enabled`.
Deletion and flags treat `Unsupported` and `Not_enabled` from a
conditional STORE as not applied, as they treated `State`. test/stalwart,
test/dovecot and test/oracle read the typed sets. Twenty-one assertions
in test/eio and one in test/stalwart that matched `State` now match the
typed variant naming the capability. test/eio/test_capability.ml covers
MOVE on rev1 and rev2, QRESYNC not enabled and the `enable` result.
`test/api/check.sh` passes. Build and runtest are clean, 15 suites and
218 test cases plus the new executable.

Step 3. Move `lib/sync/spool.ml{,i}` into a private library stanza
(`(library (name imap_sync_spool) (package imap))`) that `imap.sync` and
`test/io` both link. Delete the copy_files rules.

Done: `imap_sync_spool` (module `Spool`) and `imap_store_database` (module
`Database`) are private libraries of package `imap`, declared beside their
sources in lib/sync/dune and lib/store/dune. `imap.sync` and test/io link
the first, `imap.store` and test/store/database link the second, each with
`-open` so the sources are unchanged, and both copy_files rules are gone.
The libraries stay wrapped so their units cannot clash with a user's
`Spool` or `Database`. A probe stanza linking only `imap.sync` and
`imap.store` fails on bare `Spool` and `Database`, while the qualified
`Imap_sync_spool.Spool` and `Imap_store_database.Database` resolve through
implicit transitive dependencies. The spool and Maildir hash loops of the
reconcile.ml:41 finding stay separate, since step 4 makes `maildir` a
package that cannot link a private `imap` library. Build and runtest are
clean, 15 suites and 216 test cases.

Step 4. Create `bleeding/maildir/` with its own `dune-project`, package
`maildir`, library `maildir`, module `Maildir`, public submodule
`Maildir.Dotlock`. Dependencies: eio, eio.unix, unix, digestif, mail-flag,
plus optint only if the implementation needs it. Remove the
`internal_date` field, the `?internal_date` argument and
`upload_internal_date`; `imap.sync` gets a private adapter between mtime and
`Imap.Internal_date`. Move `paged_inventory`, `with_inventory_pages`,
`inventory_count`, `inventory_find`, `inventory_page` and the SQLite staging
into a private `Local_inventory` module in `imap.sync`. Maildir keeps `scan`,
a bounded fold over entries, and `with_unchanged_occurrence`. Replace
`with_writer_lock` with `with_writer : t -> (writer -> 'a) -> 'a` and make
`append`, `set_flags`, `remove` and `recover` take the writer. `open_dir` and
`scan` return `(_, error) result` over a typed error. Delete `lib/maildir/`
and `imap.maildir`; update every consumer and test. Move `test/maildir` and
`test/dotlock` under `bleeding/maildir/test/`.

Done: package `maildir` (library `maildir`, module `Maildir`) lives at
`bleeding/maildir/` with its own `dune-project` and depends on eio,
cstruct, optint, digestif and mail-flag. `Maildir.Dotlock` and
`Maildir.Keywords` are public, and the package refers to no `Imap`
module. `occurrence` keeps `mtime` and loses `internal_date`, `append`
and `check_append` take `?mtime`, and the `.r<32hex>` alias is gone.
`with_writer : t -> (writer -> 'a) -> 'a` replaces `with_writer_lock`.
`append`, `check_append`, `set_flags`, `remove` and `recover` take the
writer, `of_writer` recovers the handle, and a writer used after its
callback raises `Writer_expired`. `open_dir`, `scan`, `fold`, `find`,
`append`, `check_append` and `set_flags` return results over one `error`
type with `pp_error`, and the Keywords functions return the same type. An
unusable lease file, or a message directory found replaced at fsync, raises
`Eio.Io` with `Maildir.Unusable_file`. An occurrence whose flags can no
longer be read is stale. `fold` walks entries in directory order under
the metadata lock. In `imap.sync`, the private library `imap_sync_local`
holds `Local_date`, the mtime and INTERNALDATE conversions, and
`Local_inventory`, the paged inventory with the `?inventory` checks around
`with_unchanged_occurrence`, `sha256`, `open_message` and `append`. It
stages through `Maildir.fold` into `local-inventory-im-<32hex>.sqlite3` in
the sync spool directory, removes the file on every exit and offers
`recover` for leftovers. Since
`occurrence` stays private, a staged row keeps the marshalled observation
beside its ID, so the table is `occurrences(id PK, occurrence BLOB)`.
`Bridge.recover_local` runs `Maildir.recover` and `Local_inventory.recover`
under the lease, and the CLI calls it where it called `Maildir.recover`.
`verify_local_content`, `mark_local_retention`, `preview_deletions` and
`preview_sync` take `~spool_dir`, and the CLI passes its spool directory.
`Flags.reconcile_pair`, `Flags.recover_operation`, `Deletion.reconcile_pair`
and `Deletion.recover_operation` take `~writer` instead of `~maildir`.
`Bridge`, `Flags` and `Deletion` gain a `Maildir` error constructor, Bridge
lifts the nested ones to its own, and the CLI maps it to exit 7 as it
mapped the old `Failure`. A local INTERNALDATE outside the representable
range is `Invalid_operation` in Bridge where the scan used to fail.
Consumers edited: lib/sync bridge, flags, deletion, bin/imap_cli.ml,
test/bridge_faults, delete_sync, scale, cli, oracle, dovecot, stalwart.
Assertions changed: Maildir `Failure` expectations match the typed
constructor, the Unknown_letter and duplicate messages lose the
`Imap_maildir: ` prefix, Maildir recovery no longer names a staging file,
the leap-second case moved to `Local_date`, and the paged inventory, date
and staging cases moved to test/local. New cases cover the escaped
writer, the typed errors, the lease file check, Bridge surfacing a
malformed name as `Maildir` and startup recovery. Build and runtest are
clean for both packages, 16 suites and 226 test cases.

Step 5. Done: `Imap.Proto` is gone. `Imap.Uid`, `Uidvalidity`, `Modseq` and
`Seq` each have `of_int64`, `to_int64`, `to_string`, `equal`, `compare` and
`pp`, and `Uid` adds `succ` and `pred`, which are `None` at the range ends.
`Imap.Uid_set` gains `of_list`, `add`, `inter`, `diff`, `iter`, `fold` and
`to_list`, and `to_wire` raises `Invalid_argument` on the empty set.
`Selected` takes and returns `Imap.Uid.t` for every UID, UID list, UID
window, row record and ESORT MIN and MAX, and `Imap.Uid_set.t` for
`uid_fetch` and `uid_fetch_partial`, which refuse an empty set with `State`.
Response values enter `Uid.t` through `of_int64`, and a failure is
`Protocol`. `Error.Missing_uid` carries a `Uid.t`, and `Client.with_mailbox
?qresync` takes `Uidvalidity.t * Modseq.t`. `?unchangedsince` stays `int64`
because RFC 7162 allows 0 there, the live CONDSTORE tests send 0, and
`Modseq.t` rejects 0. Command encoders keep `set:string` because test/proto
drives them with raw and invalid sets. `Imap_store.stage_membership` takes a
`Uid.t list`. The CLI parses `--after-uid`, `--uidvalidity` and `--uid`
through the checked constructors, which removes its `invalid_arg` calls.
Engine, Bridge and Reconcile keep int64 window arithmetic, since an empty
mailbox has upper UID 0, and convert each window once. The 1,000-UID window
check is one helper in Selected, the COPYUID subset check is `Uid_set.diff`,
and Mirror and the engine compare MODSEQs with `Modseq.compare`.
`Uid_set.union` backs `add` and has a directed test. Tests changed only in
how they build and read identifiers, and test/proto gained a set-algebra
case. Build and runtest are clean, 16 suites and 227 test cases.

Step 6. Done: the vocabularies live in `Imap.Status_item`, `Mailbox_list`
(`selection` and `return`, whose constructors lose the `Return_` prefix),
`Sort` (`key`, `order`, `return`), `Thread` (`algorithm` with `Other`),
`Notify` (`filter`, `event`, `group`, `is_selected`) and `Metadata`
(`depth`), each with its `to_wire` and an `equal` where a caller compares.
`Capability.thread_algorithm` is `Thread.algorithm`, so `Other_algorithm`
is gone and `Selected.uid_thread` requires `Cap.Thread algorithm` without a
second mapping. `Command` keeps only encoders, and `uid_thread` accepts an
`Other` algorithm that is an atom. `Command.error` is `{ command; argument;
reason }` with `to_string` and `pp`, every encoder names the labelled
argument at fault, and Client and Selected raise `State (to_string e)`.
`Command.create`, `delete`, `subscribe` and `unsubscribe` take `~mailbox`,
as do the five Client mailbox mutations. `Mailbox_name.t` is private, and
`encode_rev1`, `decode_rev1` and `decode` are gone. `equal` compares `raw`
and `mode`, and `pp` prints the decoded name. `Client.list`, `lsub` and
`discovery.mailboxes` carry `Client.mailbox_entry = { name; info }`, with
the name decoded in the mode in effect when the row arrived.
`Response.thread` names its field `number`, and `Selected.uid_thread`
returns `Selected.thread` with `Uid.t` nodes. Consumers edited:
lib/sync/engine.ml, README.md, test/proto, eio (client, compress,
lifecycle, rejections, review_fixes, sort_thread), dovecot (test_dovecot,
body_memory), oracle and stalwart. bin needed no change. Tests changed only
in how they name vocabulary, label mailboxes and read LIST rows. test/proto
gained checks that an error names `CREATE mailbox`, `RENAME new_name` and
`UID STORE flags`, and the `Mailbox_name` round trip now goes through
`of_wire`, `equal` and `pp`. test/eio/test_client.ml gained
`test_decoded_names` for a modified UTF-7 row and a malformed one. Build and
runtest are clean, 16 suites and 227 test cases.

Step 7. Done: `Imap.Search` types the RFC 9051 search keys, the rev1
NEW, OLD and RECENT, RFC 7162 MODSEQ, RFC 8474 EMAILID and THREADID, the
RFC 5182 `$`, NOT, OR and a conjunction, with `Raw` for anything else.
`to_wire ~utf8` quotes every string, refuses a non-ASCII string without
UTF-8 and any control byte or invalid UTF-8, and validates a date, a size,
a nonempty UID set, a keyword flag and an object identifier. `Raw` keeps
the old checks and now also needs balanced parentheses and quotes, which
only the saved refinement checked before. `capabilities` names CONDSTORE,
SEARCHRES and OBJECTID, and `uidonly_safe` applies the old leading-token
guard to every nested `Raw`. `Imap.Fetch_item` has the twelve metadata
items and no body item, and `Command.uid_fetch_items` and
`uid_fetch_saved_items` encode them and check BINARY.SIZE sections. Every
Selected search takes `~criteria`, encoded with UTF-8 when the mailbox mode
is UTF-8 and gated through Session, with QRESYNC satisfying CONDSTORE.
`uid_search_page` takes `?before` ahead of the lease. `Selected.fetch` and
`fetch_range` replace `uid_fetch`, the six per-item fetchers and
`fetch_metadata_range`, and `uid_fetch_partial` and `uid_fetch_saved`
return the same `row`. One policy applies to all four. Rows come back in
request order for `fetch` and ascending otherwise, unsolicited UIDs are
ignored, rows for one UID merge with FLAGS and MODSEQ taking the last
value, a conflict in any other item is `Protocol`, and a row without a UID
is `Protocol` only when it carries an item other than FLAGS or MODSEQ. The
cap is 1,000 UIDs. `fetch_changes` and `fetch_changes_range` keep raw rows.
Consumers edited: lib/sync engine (scans, hydration), bridge (metadata and
APPEND inspection), flags, deletion and reconcile, which still treat a row
without FLAGS as absent, and bin needed no change. Tests edited:
test/proto, test/eio (binary, client, client_review, compress,
review_fixes, searchres, sort_thread), bridge_faults, dovecot, oracle and
stalwart. Hydration sizes each page of up to 100 UIDs with one FETCH. The bridge.ml:110 finding stays open, since dropping its
separate INTERNALDATE selection changes `Engine.archive_uid`. test/proto
gained search encoding, UTF-8, capability and UIDONLY cases and a fetch
item case, and test/eio/test_searchres gained a typed-criteria gate case.
Raw criterion strings became typed keys (ALL, UNSEEN, UID sets, SUBJECT,
HEADER) or `Raw` (sequence sets, RETURN (SAVE), the grammar escape).
Assertions changed by the policy: PREVIEW and BINARY.SIZE rows follow
request order, an unrequested BINARY.SIZE UID is ignored, an identical
repeated ENVELOPE or BINARY.SIZE merges while a differing one is
`Protocol`, and an OBJECTID row without THREADID reports it absent instead
of failing. The body-item refusals of `uid_fetch`, `uid_fetch_partial` and
`uid_fetch_saved` are now unrepresentable, so those cases keep only their
metadata assertions. The hydration fixtures answer one batched size FETCH.
README and IMAP-SPEC name the new calls. Build and runtest are clean, 16
suites and 231 test cases.

Step 8. Done: `Client.append t ~mailbox ?binary message` replaces
`append_flow`, `append_flow_receipt`, `append_binary_flow` and
`append_binary_flow_receipt`, and `append_many` replaces
`append_messages`. `append_message` takes `Mail_flag.Imap_flag.t` flags,
sent with `to_wire`. Both calls share one part builder and one APPENDUID
decoder, so a single APPEND whose APPENDUID names several UIDs is
`Uncertain` with "APPEND returned invalid UID correspondence" and closes
the connection as before. `Engine.append_journaled` and
`append_blob_journaled` take typed `?flags` and save them as the expected
flags without reparsing, and Bridge passes its durable flags directly.
Tests edited: test/eio client, binary_append, multiappend, literal_modes,
rejections, and test/dovecot, oracle and stalwart. Callers that discarded
the receipt use `Result.map ignore`. The MULTIAPPEND preflight case that
sent an invalid flag string, now unrepresentable, sends a 65,001-byte
keyword that fails the same syntax preflight. Every other assertion is
unchanged. README and IMAP-SPEC name the new calls. Build and runtest are
clean, 16 suites and 231 test cases.

Steps 5 to 9 are ordered so the tree builds after each. Step 9 groups: on
the lease Condstore, Qresync, Uidplus, Move, Binary, Searchres, Sort, Esort,
Thread, Partial, Preview, Objectid, Objectid_plus, Uidbatches, Messagelimit,
Notify; on the connection Acl, Quota, Metadata, Notify, Objectid_plus,
Multiappend, Compress. Each group has `require` returning a witness whose
operations take it instead of the bare lease or client. The base `Selected`
keeps search, fetch, store, copy, expunge, noop and idle.

Done: `Selected` keeps `info`, `select_updates`, `uid_search`,
`uid_search_range`, `fetch`, `fetch_range`, `fetch_to`, `uid_store_flags`
without `?unchangedsince` or a trailing unit, `uid_copy` and `noop`.
Fourteen lease submodules each have an abstract `t` and `require`, and a
witness is the lease itself, so it expires with the lease and an
operation on it after the lease is `State`. `Condstore` has
`uid_store_flags ~unchangedsince` and `fetch_changes_range`, `Qresync`
`fetch_changes`, `Uidplus` `uid_expunge`, `Move` `uid_move`, `Binary`
`fetch_binary_to`, `Searchres` the saved-search family with
`saved_search`, `Sort` `uid_sort`, `Esort` `uid_sort_extended`, `Thread`
`uid_thread` with the algorithm taken by `require`, `Partial`
`uid_search_partial` and `uid_fetch_partial`, `Messagelimit`
`uid_search_page`, `Uidbatches` `uid_batches`, `Notify` `notify_set` and
`notify_none`, and `Idle` `wait_for_change`. `Qresync.require` is
`Unsupported` without the capability and `Not_enabled` before ENABLE.
Gates that depend on arguments stay at the call: PARTIAL on
`uid_fetch_saved ?partial`, CONDSTORE on `uid_store_saved
?unchangedsince`, MOVE and UIDPLUS on the saved move and expunge, and
CONTEXT=SORT on a positional ESORT. `Client` keeps its lifecycle,
negotiation, discovery, `status`, `get_jmap_access`, mailbox mutations,
`with_mailbox`, `append`, `noop`, `logout` and `close`, and gains `Acl`,
`Quota` (QUOTASET checked in `set_quota`), `Metadata` (satisfied by
METADATA-SERVER, which still limits calls to the server scope),
`Notify`, `Multiappend`, `Compress.activate`, and the modes
`Objectid_plus` (`enable`, `pin_mailbox`, `create_mailbox`,
`rename_mailbox`, `status`) and `Uidonly` (`enable`). `append_many`
now needs the MULTIAPPEND witness even for one message. The step note's
Preview and Objectid groups were not made, since PREVIEW and OBJECTID are
fetch items already gated per call. The per-call gates the witnesses
replace are gone from the implementations. Consumers edited: lib/sync
engine (OBJECTID+ witness threaded through the scan, CHANGEDSINCE through
`Condstore.require`), deletion, flags and watch, README, IMAP-SPEC and
test/bridge_faults, dovecot (test_dovecot, body_memory), eio (binary,
capability, client, client_review, compress, lifecycle, literal_modes,
multiappend, rejections, review_fixes, searchres, sort_thread), oracle
(test_oracle, test_cross_protocol) and stalwart. bin needed no change.
Every assertion is kept, with each gate case now reached through
`require`. test/api/check.sh compiles a `Selected.Move` path, checks that
`Selected.uid_move` is unbound, and checks that a lease does not
typecheck as a `Move` witness. Build and runtest are clean, 16 suites and
231 test cases.

Step 10. Done: `Session.locked` returns `State "call inside with_mailbox on
the same connection"` without taking the mutex when the calling fiber
holds the session, recorded in an `Eio.Fiber` key that `with_mailbox`
binds around the callback through `Session.with_lease`. `with_mailbox`
now takes the session through `locked`, so a nested call is refused the
same way. Fibers forked inside the callback inherit the key, and a call
on another connection is unaffected. Selected operations use the lease's
own mutex and still serialize across fibers. The facade and README state
the typed outcome in place of the deadlock warning.
`test/eio/test_reentrancy.ml` covers a nested `with_mailbox` and a
`Client.noop` inside the callback, each refused with no bytes written,
and the connection stays usable. Without the guard the test ends in
`Eio_mock.Backend.Deadlock_detected`. Build and runtest are clean, 16
suites and 231 test cases plus the new executable.

### Step F notes

Each fix agent writes under its own heading only: what it fixed, what it
deliberately left for a later step, any interface it changed and the
consumers it touched, and the test evidence.

#### F: protocol

Fixed 63 findings in lib/protocol, one commit per module, and left 8
annotated in place. Every correctness fix has a directed test in
test/proto, test/mirror, test/policy or test/eio/test_session_limits.ml.

Interface changes. `Proto` gains `Uid_set.is_empty`, `equal`, `compare` and
`pp` on `Uid`, `Uidvalidity`, `Modseq` and `Uid_set`, and
`Uid_set.of_wire ?allow_star`, which reads `*` as 4294967295. `of_wire`
now rejects leading zeros and names the bad token. `Sync_policy.error` and
`Deleted_flag_requires_policy` are gone. `reconcile_flags` returns a
`flag_plan` with a new `deleted_held` field, holds only a `\Deleted` change
the endpoints disagree on, and merges every other flag. `Wire.feed` returns
events framed before an error and reports the error on the next call.
Documentation changed on `Wire`, `Mirror.initial` (`@raise`),
`Mirror.complete`, `Internal_date`, `Mailbox_name`, `Command` and
`Response` (`raw`, `Thread`, `parse_parts`).

Consumers edited outside lib/protocol. lib/eio/session.ml `read_event`
calls `Imap.Wire.feed t.wire ""` before each transport read so a deferred
wire error surfaces after the events framed before it. lib/sync/flags.ml
`plan_flags` maps `deleted_held` to `Deleted_flag_held`, keeping its old
whole-merge hold, and lib/sync/bridge.ml preview does the same with its
hold message. Wave 2 (sync) can now merge the other flags there.

Behaviour worth knowing for later steps. `Mirror.complete` anchors only on
an explicit HIGHESTMODSEQ, as `Imap_store.publish_stage` does, and
`changed` ignores `\Recent`. Command encoders accept `*` in `uid_expunge`,
emit CONDSTORE alongside QRESYNC, and reject quoted strings that are not
valid UTF-8, criteria ending in a literal marker and empty `Add`/`Remove`
ACL rights. Response keeps the FETCH line unaltered in `raw`, rejects
duplicate FETCH and STATUS items, missing or unterminated FETCH lists and
sequence numbers above int64, and accepts SEARCH and SORT `(MODSEQ n)`
suffixes, which it validates and drops because `Search` and `Sort` carry
no MODSEQ. THREAD chains of any length count as one nesting level.
`max_control_literal` now also bounds the combined size of all literals
retained from one response. ESEARCH and LANGUAGE literals are framed by
Wire and retained by `parse_parts`. `select_metadata` reports a tagged NO
or BAD with its code and text. Three existing assertions changed with the
decisions. UID EXPUNGE with `*` is now accepted. A THREAD chain of 101 is
now accepted, while nested depth 101 is still rejected. An ENVELOPE with an
unterminated quote is now rejected by `parse` itself, so the test accepts
rejection at either stage.

For step 5: `to_wire empty = ""` is kept because flags.ml:373,
deletion.ml:258 and selected.ml:684 compare against it. They can switch to
`Uid_set.is_empty`. `Proto.Seq` now has a caller in Response.

Evidence: `dune build --root . @bleeding/imap/all` clean and
`dune build --root . @bleeding/imap/runtest --force` clean, 15 suites and
182 test cases passing, up from 172.

#### F: eio

Commits, in order: 4a9221ede Deflate_flow errors, a16352a40 Client,
2371c0167 Session, 8f2542b4b Auth and Transport, eb09d48f4 Selected,
71286aa8c Pool, 7a27bc2f3 Deflate_flow allocation.

Fixed. Session sets its sent flag after each write returns. A failure before
the final CRLF keeps its kind and text, and one after it on a mutation is
`Uncertain` naming the cause. `protect` converts only I/O failures (Eio.Io,
Unix_error, End_of_file, TLS alerts and failures) and re-raises anything else
with its backtrace after closing. Only BODY[...] and BINARY[...] literals of a
FETCH reach `on_literal`. PREVIEW, ENVELOPE, BODYSTRUCTURE and unsolicited
LIST, STATUS or METADATA literals reach `parse_parts`. The PREVIEW limit uses
the parsed length. A tagged IDLE rejection leaves the session open. One
budgeted `next` helper replaces the five read loops, and `read_event` is
untouched. A local APPEND source failure is `State`. Client keeps the
callback outcome when UNSELECT fails, removes its switch hook in `close`,
resets `selected` on every exit, gates every ENABLE through one predicate,
merges ENABLED results, gates STATUS items, sends CAPABILITY once on PREAUTH
and checks STATUS pins after APPEND syntax. Selected accepts effective
IMAP4rev2 for MOVE, UID EXPUNGE, SEARCHRES and IDLE, refuses body items in
`uid_fetch` and `uid_fetch_partial`, accepts a quoted BODY[] in `fetch_to`,
reports a UID-less BINARY row as `Protocol`, keeps only rows with FLAGS in
`fetch_changes_range`, reports a failing sink as `State`, never leaves a page
open at UID 1, requires correlated ESEARCH and sorts SEARCH results, checks
CONDSTORE for MODSEQ metadata and the COPYUID source set. Auth validates fixed
credentials at construction and replaces an invalid or failing provider with
`Auth.Invalid_credentials`, which Client reports as `State "invalid
credentials"` before any secret is sent. Transport derives peer names only for
TLS. Pool refuses allocation after its switch is released. Deflate_flow codec
failures are `Eio.Io` carrying decompress's diagnostic.

Deflate_flow allocation, measured with `Gc.allocated_bytes` around one write:
1 MiB of random lowercase text 44,192,440 bytes before and 33,396,480 after;
940,108 bytes of repeated IMAP command text 9,202,344 before and 828,480
after; a 40-byte write 532,704 before and 532,552 after. One `Manual` LZ77
state now spans the caller's buffers in 64 KiB slices and the window lives in
`t`. The remainder is per-symbol allocation inside decompress and the per-write
LZ77 hash arrays, which the upstream API cannot reuse.

Interface changes. `Error.pp` and `Error.to_string`, with `Client.pp_error`
and `error_to_string` defined through them. Session gains `has`,
`revision_two`, `mailbox_mode`, `mailbox_wire` and `io_failure`, loses
`authentication_rejected` and `?collect_literals`, and `flow` is immutable.
Auth gains `Invalid_credentials`, drops `resolve_token`, and
`cram_md5_response t` validates before returning the challenge function.
`Deflate_flow.Deflate` extends `Eio.Exn.err`. The facade gains documentation
for Auth constructors, STATUS gating, APPEND outcomes, the UNSELECT outcome,
the Selected refusals and gates, and Pool after release, and no signature
changes. `unix` is a direct dependency of `imap_eio_core`. Client calls now
re-raise non-I/O exceptions, including a `with_mailbox` callback's, instead
of returning `Transport`. No consumer outside lib/eio needed a change.

Tests. New directed regressions are in test/eio/test_review_fixes.ml; the
switch-hook and pool tests were confirmed to fail without their fixes. Tests
whose assertions encoded replaced behaviour were changed to the new contract:
PREAUTH fixtures in test_client, test_client_review and test_lifecycle expect
one CAPABILITY; test_client_review expects the callback outcome after a
failed UNSELECT and a tagged empty ESEARCH; test_session_limits,
test_binary_append and test_multiappend expect `Limit` or `State` before the
final CRLF; test_binary expects a foreign LIST literal to stay out of the body
and a UID-less BINARY row to be `Protocol`; test_client expects sorted SEARCH
pages; test_deflate_flow matches `Deflate` and adds a buffer-list round trip.
Both `@bleeding/imap/all` and `@bleeding/imap/runtest --force` are clean.

Cross-module findings resolved here but not ticked above: exception
relabelling, lost callback result, and the session.ml and deflate_flow.ml
entries of error payload loss.

#### F: store

Commits 88693549a, e710b45c5, 8fae853c6, d26a71a5e, 20c80d652, a219531ee,
8e311c54f, f470eaf93, 0bf6184bd on the step F store branch.

Fixed. Database: `bind` checks the parameter count, statements are reset
and cleared on every path, `check` carries SQLite's `errmsg`, a nested
`transaction` or `locked` raises `Invalid_argument` through a per-fiber
`Eio.Fiber` key. Schema: primary keys and UNIQUE constraints are validated,
the reserved-name guard escapes `_`, one `current_version`, the new
`sync_conflicts_open_id` partial index joins the unversioned group.
Record_codec: shared cursor read, stale checks, SHA-256 validator, flag
grouping and `Scope_mismatch`. Journal: `equal_durable` everywhere, stale
repairs are `Stale_revision`, one `verify_repair`, batched page reads,
`resolve_open_conflicts` uses `changes()`. Intents: NULL legacy fields read
as `""`, UID without UIDVALIDITY rejected, confirmation keeps the stored
UIDVALIDITY. Blobs: finalisers no longer mask the body exception, Unix
directory errors become `Eio.Io`, one reachability statement, fsync after
the digest check, one temp-name retry. Imap_store: seeding checks the
epoch, OBJECTID+ name mismatch is `Conflict`, publish paths share the
cursor upsert and epoch replacement. A CONDSTORE publish without an
explicit HIGHESTMODSEQ anchors `None`, the same rule `Mirror.complete`
now follows, since a largest-row fallback yields a false regression after
the highest message is expunged.

Deviation. The decision that a tombstone can never be replaced by
`put_pair` breaks bridge.ml:539 and :1048, which renew an absence tombstone
after the side vanishes again and escalate `Local_absence` to `Retention`;
bridge_faults cases 35 and 36 failed under it. `put_pair` instead forbids
clearing and allows replacement only by the same or a more permanent reason
(absence, then `Expunge_receipt` or `Retention`, then `Explicit_delete`),
which closes the laundering path the finding describes.
`object_identity` follows the Imap_store decision (a `Conflict` outcome),
not the Record_codec one (raise `Scope_mismatch`), since both name the same
check. The temp-name retry has no directed test: names cannot be forced to
collide without an injection point.

Interface changes. `Imap_store` gains `exception Scope_mismatch`,
`forget_epochs` and `Blob.attach ?verify`, and `object_identity` returns
`` [ `Bound of object_identity | `Unbound | `Conflict ] ``. In the private
modules `Database.check` and `bind` take the handle, `locked`,
`rows_prepared` and `changes` are new, and `Record_codec` drops
`dec_phase`, `dec_mode` and `decode_cursor` for the shared helpers. Consumers edited: bin/imap_cli.ml (match `Scope_mismatch`,
`object_identity` outcome), lib/sync/engine.ml (`load_cursor` at the APPEND
path, `object_identity` outcome, `attach ~verify:false` after `put`),
test/bridge_faults, test/stalwart and test/store/test_store.ml for the
`object_identity` type; test_store.ml "stale pair cannot settle" now
expects `Stale_revision` as decided.

Left. `publish`, `load` and the test-only list readers (step 11), the
orphan collector in the CLI (step 12), the receipt epoch column (v14),
`sync_pairs_scope` (migration), full-epoch rewrites per publish, column
types and foreign keys in `validate_schema`.

Evidence. New directed tests: test/store/database (private Database via
copy_files), test_store_outcomes, test_journal_outcomes, and additions to
test_schema_guards and test_intent_validation. `dune build
@bleeding/imap/all` and `dune build @bleeding/imap/runtest --force` are
clean.

#### F: maildir

Branch `worktree-agent-a8b4ff22560c66080`: Keywords, Dotlock and
Imap_maildir commits, then a commit restoring rename for publication.

Fixed. `Stale_occurrence` is exported and documented on `open_message`,
`sha256`, `set_flags` and `remove`. A supplied `?id` is checked inside the
publication lock against `new/<id>`, every `cur` name that parses to the ID
and, when given, the paged view. Entries named with a leading dot and entries that are not
regular files are skipped in scan, staging and `find`. One
`stat ~follow:false` per entry replaces three to four, and an entry gone
before that stat counts as vanished. An epoch INTERNALDATE works. Cleanup
unlinks can no longer replace the body's exception. `find` probes
`new/<id>` and prefix-matches `cur`, stat-ing only matches, so it no longer
fails on unrelated malformed entries. Staging stores flags in one text
column, so a page is one query. `sha256` feeds the bigstring. The keywords
map is cached per handle and keyed by the file's inode, size, mtime and
ctime. A flagless append takes one lock and reads no keywords. Keywords owns
the letter table in both directions, tolerates blank lines and a missing
final newline, names the offending value in every message and adds `equal`.
Occurrence flags are now in `Imap_flag.durable` order. Dotlock runs on Eio,
raises `Lost` for a removed or replaced lock, checks without writing after
the callback, survives any stat failure on release, takes the refresh mutex
on release and unlinks the lock when `fstat` fails after creation.

Fsyncs removed: the `tmp` directory after writing a message, after
publishing it, after installing `dovecot-keywords`, after removing the
staging database and in `recover`. Each only made the removal or presence
of a `tmp` name durable, and `recover` handles a surviving name. Kept: the
message and keywords files before publication, the target directory after
each rename or unlink, and the parent after `mkdir`. Publication and flag
changes keep `Eio.Path.rename`, because Maildir names are unique and a
duplicate identity is rejected under the metadata lock before the rename.

Remaining `Unix`: `lockf` for the writer lease, `utimes`, directory
fsync (`openfile`, `fstat`, `fsync`, `close`), `/dev/urandom` in
`random_id` because `reserve_id` takes no environment, and `getpid` and
`gethostname` for the lock body. Each carries a comment naming the missing
Eio operation. The blocking calls run in a systhread and map `Unix_error` to
`Eio.Io` with context.

Interface changes. `Imap_maildir` gains `Stale_occurrence` and
`Metadata_lock_lost` (`= Dotlock.Lost`) and loses the dead `inventory`
function and type. `Dotlock.with_lock` takes an `Eio.Path.t` and raises
`Lost`. `Keywords` gains `fail`, `max_size`, `empty` and `equal`, `letters`
takes `?passed` and returns the sorted string with system letters, and
`flags` takes `~file`. No consumer outside `test/maildir` used the removed
values, and no consumer file changed.

Left. The writer-lease check on mutations, the `with_writer` capability,
typed results, prefix renames and the Dovecot lock body, as annotated
above. With a paged view, a supplied-ID append now also reads the `cur`
listing once.

Evidence. `dune build @bleeding/imap/all` and
`dune build @bleeding/imap/runtest --force` are clean. `test_maildir` has
21 cases, 9 of them new. Against the previous implementation the
supplied-ID, ignored-entry, find, epoch and cleanup cases fail.
`test_dotlock` adds deleted-lock, post-callback no-write and release stat
failure cases.

#### F: sync

Commits c894239e5 Spool, bda44d29d Watch, 0b58aaf34 lock exceptions,
c8945a2a6 Engine, bd958e03b Flags, cd90f4053 Deletion, e5a4c796c Bridge,
e1603535f Deletion doc.

Fixed. Spool keeps the callback exception when removal fails. Watch
reselects after every IDLE wakeup and scans only when UIDVALIDITY, UIDNEXT
or HIGHESTMODSEQ differ from the cursor, or at renewal, and caps
`idle_renew_seconds` at 1740. A watch connection without IDLE is reported
through `Idle_failed` instead of being held while it sleeps. Engine has `Uidvalidity_changed` at every
epoch site and defers a first OBJECTID+ binding to the scan after an epoch
change instead of failing every scan. `hydrate_once` takes `?after_uid`,
skips a body over either budget into `skipped`, and returns `last_uid`.
`audit_cache_once` skips a blob over the budget. Both return committed
counts with `more=true` after a concurrent publication. Flags merges every
flag except a held `\Deleted`, rejects a STORE refused before dispatch,
records a reason and a `Flag_conflict` for an uncertain STORE or a failed
verification read, never marks a local-only write sent before its
concurrent-change checks, reads PERMANENTFLAGS per RFC 9051, and takes the
held inventory. Deletion journals and hashes outside the lease, holds
MODIFIED, a longer body, a stale occurrence and a legacy pair without
content evidence, and reports a concurrent expunge as `Stale_inventory`.
Bridge holds date, content, CONDSTORE, PERMANENTFLAGS, concurrent-change
and tombstoned-but-present pairs, rejects a Maildir-unstorable remote copy
in the journal before it is sent, narrows `Writer_busy` to the lease, and
matches epoch errors by constructor.

Already fixed by wave 1. The lost-result half of deletion.ml:146 by the
eio `Client` change, and the full-scan cost behind bridge.ml:280 by the
maildir `find` rewrite, so the fresh absence probes that remain in Bridge
and Deletion are one `cur` listing each.

Left for later steps. bin/imap_cli.ml still calls `hydrate_once` without
`after_uid`, so a skipped UID keeps `more=true` there until step 12.

Interface changes. `Engine`: `Uidvalidity_changed`, exported
`validate_scope`, `hydrate_once ?after_uid`, `last_uid` and `skipped` in
`hydration_receipt`, `skipped` in `cache_audit_receipt`,
`fetch_uid_digest ~uidvalidity`. `Flags`: `No_pending_operation`, no
`Deleted_flag_held`, `plan_flags` returns `decision`,
`validate_permanent_flags ~defined`, `reconcile_pair ?inventory` returns
`reconciled`, `recover_operation ?inventory ... ()`. `Imap_maildir`:
`Writer_lock_busy` is its own exception, new `Metadata_lock_busy` and
`check_append`. `Imap.Sync_policy.deletion_hold` gains
`Missing_content_evidence`. Consumers edited outside lib/sync:
bin/imap_cli.ml (the `No_pending_operation` match and the
`no-content-evidence` hold label), lib/maildir, lib/protocol/sync_policy,
test/dovecot/test_dovecot.ml (`recover_operation ()`).

Assertions changed to the new contract. test_flag_sync expects a
`decision` record, a held `\Deleted` with the other flags merged, and a
missing PERMANENTFLAGS to permit every flag. test_maildir expects
`Metadata_lock_busy` from the Dovecot lock. test_bridge_faults expects a
first binding after an epoch change to publish unbound and bind on the next
scan, a wrong-date reappearance to be a held pair, and the offline scrub to
skip a tombstoned local absence.

Evidence. New directed tests in test/io and test/bridge_faults cover each
correctness fix except the journal-outside-lease move, which has no
injection point for a journal failure. `dune build @bleeding/imap/all` and
`dune build @bleeding/imap/runtest --force` are clean, 15 suites and 216
test cases.

#### F: cli

### 0.R Phase 2 findings

Filled in by the review batches. Each finding is `file:line`, one sentence,
severity in `[]`. Fixes applied in step F are ticked here.

#### lib/protocol/response.ml

- [x] response.ml:1470 [high] `fetch` receives `String.concat " " rest` from `split_words`, so runs of spaces inside quoted strings collapse in every FETCH row, altering `raw`, `preview`, ENVELOPE and BODYSTRUCTURE strings; probed with `PREVIEW "a    b"` giving `a b`. Pass the raw suffix unchanged.
- [x] response.ml:397 [high] the tokenizer scans to end of line for `}` on every `{` token, quadratic on hostile input; probed 0.88 s at 80 KB, minutes at the 1 MiB control default. Bound the search to the digit run.
- [x] response.ml:1603 [high] `parse_parts` copies the whole buffer on every `Literal_start` and re-tokenizes it twice per FETCH; probed 0.89 s for 4000 empty literals.
- [x] response.ml:1559 [high] no aggregate bound on retained control literals; a METADATA or LIST response with many 16 MiB literals grows the buffer without limit.
- [x] response.ml:1491 [high] `* SEARCH 2 5 (MODSEQ 917)` and the SORT equivalent are rejected; RFC 7162 adds the MODSEQ suffix to both.
- [x] response.ml:1346 [medium] a THREAD chain longer than 100 elements fails the whole response because chains recurse against the depth cap; build chains iteratively.
- [x] response.ml:1448 [medium] `(EARLIER)` is matched case-sensitively.
- [x] response.ml:1455 [medium] a sequence number above int64 turns FETCH, EXISTS and EXPUNGE lines into `Ok (Other raw)` instead of an error, because `parse_i64` returns `None` before the range check at :1468.
- [x] response.ml:495 [medium] `* 1 FETCH garbage` parses as a FETCH with every field `None`; `seek` returns `[]` without a paren and trailing tokens are ignored.
- [x] response.ml:533 [medium] duplicate FLAGS, RFC822.SIZE, INTERNALDATE, MODSEQ, EMAILID and THREADID items take the last value silently, while UID at :513 and PREVIEW at :561 reject duplicates; STATUS at :1136 and MAILBOXID at :1120 have the same gap.
- [x] response.ml:1071 [medium] ESEARCH skips one token for an unknown return item, so a parenthesised extension value fails the parse.
- [x] response.ml:1691 [low] `select_metadata` on a tagged NO or BAD discards the server code and text.
- [x] response.ml:1623 [low] a later `failure :=` overwrites the first cause.
- [x] response.ml:496 [dead] the empty-atom arm; `atom` never emits one.
- [x] response.ml:1440 [dead] the `value` arms at :1440, :1453 and :1461 duplicate the wildcard arm and their result is discarded by the second dispatch; :1485 `assert false` is unreachable.
- [x] response.ml:1139 [dead] non-negative guards at :328, :331, :522, :713, :721, :1077, :1139, :1274 and :1546 can never fail since `parse_i64` and `Lit` never yield negatives.
- [x] response.ml:279 [redundant] the literal `4_294_967_295L` range check is hand-coded at sixteen sites where `Proto.Uid.of_int64` and siblings already exist.
- [x] response.ml:456 [redundant] `valid_flag` is inlined again at :352 and :538; the FETCH `seek` is copied at :703, :741, :886, :914 and :1577; OBJECTID pair collection at :893 and :1100; `prefix` at :224 is `String.starts_with`; `valid_preview` at :460 reimplements `String.get_utf_8_uchar`.
- [ ] response.ml:960 [redundant] the special-use list and `selectable` duplicate `Mail_flag.Mailbox_attr`, with the caveat that `of_string` also accepts names without a backslash and maps `\Spam` to Junk. (left: `Mail_flag.Mailbox_attr.of_string` also accepts names without a backslash, maps `\Spam` to Junk and adds `\Inbox`, `\Scheduled` and `\Memos`, so the semantics differ)
- [x] response.ml:1452 [redundant] SEARCH words are parsed three times; `response_code` runs twice per status line via :375 and :1431.
- [x] response.ml:196 [comment] doc comment on `type thread` duplicates the interface; delete. Comments at :716, :1331 and :1406 earn their place.
- [x] response.mli:1 [drift] the header says literal payloads are never retained in `Fetch`, but PREVIEW, ENVELOPE and BODYSTRUCTURE literals are inlined into `raw` up to 256 KiB each, as :119 and :152 say.
- [x] response.mli:81 [drift] `raw` is not wire text: it drops the leading `* N `, collapses spaces, replaces retained literals with quoted strings and keeps `{n}` markers for streamed ones.
- Facts for later steps: ranges are validated inline, not through `Proto`; flags are validated with `Imap_flag.of_wire` and then discarded for the string. `Proto.Seq` is unused. `Capability` and `Enabled` are space-split words with no atom validation, case normalisation or deduplication, and a `[CAPABILITY ...]` code becomes `Other_code`. `parse` takes one physical line and cannot handle a literal-bearing LIST, STATUS or METADATA line. `raw` costs 7 to 11 MB per 100,000 metadata rows and every extractor re-tokenizes it.

#### lib/eio/session.ml

- [x] session.ml:180 [high] `written := true` precedes the write at :180, :285, :423 and :569, so a 64 KiB syntax `Limit` raised by `write` at :58 before any byte leaves is reported as sent; with `~mutation:true` the handler at :244 relabels it `Uncertain` and closes the session. A UID STORE or MOVE over a large set therefore loses the connection with an unknown outcome although nothing was sent.
- [x] session.ml:363 [high] `append_many` maps every failure to a generic `Uncertain` because `!written` is always true, dropping `Protocol "server BYE"`, `Limit`, the missing-continuation error and the short-source `State` error, all of which have a known outcome. `command_result` at :244 loses the same information.
- [x] session.ml:98 [high] with `on_literal` set, every `Literal_chunk` of every response in the command goes to the body sink and none reaches `parse_parts`, so an ENVELOPE or PREVIEW literal in the same FETCH, or a literal in an unsolicited LIST or STATUS, lands in the caller's sink and parses as an empty string.
- [x] session.ml:86 [medium] `on_literal_start` runs before the PREVIEW limit check at :91, and `on_literal` streams chunks before `parse_active` at :188 or a tagged NO can reject the response, leaving the sink with a partial or foreign payload. A partial body before a tagged failure is inherent to streaming and is documented as provisional.
- [x] session.ml:460 [medium] `idle_once` closes the session on a tagged NO or BAD, unlike every other rejection path at :240, :359, :520 and :575.
- [ ] session.ml:460 [medium] cancelling IDLE closes the session instead of sending DONE, so a timeout cannot bound `wait_for_change` without losing the connection; and any untagged line at :441 and :456 counts as a change, including `* OK Still here`. (left for step 14)
- [x] session.ml:469 [medium] `protect` relabels every non-`Session.Failure` exception as `Transport`, including `Stdlib.Failure`, `Invalid_argument`, `Out_of_memory` and `Stack_overflow`, and loses identity and backtrace; the local `Failure` at :33 shadows the stdlib one.
- [x] session.ml:89 [low] the PREVIEW 1024-byte check rebuilds the marker as `{%Ld}` while `Wire.literal_suffix` at wire.ml:71 accepts leading zeros, so `{0010}` bypasses it and `parse_parts` at response.ml:1604 misses it too; memory stays bounded by `max_metadata`.
- [x] session.ml:471 [confirmed] reentrancy deadlocks: `with_mailbox` holds the mutex for the callback at client.ml:634 and every other entry point relocks through `locked`; Eio mutexes have no owner tracking, so the second lock parks forever until cancellation, which then closes the session at client.ml:686. Plan step 10. (step 10: an `Eio.Fiber` key lists the sessions a fiber leases, and `locked` returns `State` for one of them)
- [x] session.ml:177 [dead] `written` and `sent` at :177, :283, :358, :402 and :531 are always true when read; the `| _ -> raise ex` arm at :365 is unreachable; `mutable` on `flow` at :14 is never used; `?collect_literals` in session.mli:41 has no external caller.
- [ ] session.ml:108 [redundant] response-kind detection duplicates response.ml:1597; the PREVIEW limit at :87 duplicates response.ml:1607; the read, size, limit, parse, BYE loop skeleton is written five times at :182, :286, :325, :375 and :405; `authentication_rejected` at :533 is redone by client.ml:132. (left: the read, size, limit, parse and BYE loop is one helper and the authentication redaction lives only in Client; kind detection and the early PREVIEW check stay because Response exposes no helper and the check bounds the read before parse_parts)
- Facts for later steps: tag counter, close-on-desync, COMPRESS boundary, APPEND literal handshake, IDLE DONE ordering, cancellation re-raise and mutex release are all clean. Session record writers: `capabilities` by Client only, uppercased latest CAPABILITY; `enabled` by Client, appended without dedup at :70 and :80; `selected` by Client at :658, :681, :699 and not reset by `close`, so a closed session can keep a stale `Some`; `generation` bumped by `Session.close` and Client, checked by `Selected.check`; `saved_search_nonce` by Session only; `readonly` never reset; `uidbatches_last_mailbox` by Selected only, compared by string so INBOX case variants escape; `wire` also by Client after STARTTLS. Optimisation and comments are clean.

#### lib/eio/selected.ml

- [x] selected.ml:83 [high] SEARCHRES at :83 and :91, MOVE at :805, UIDPLUS at :838 and IDLE at :858 are gated on their literal tokens, so an IMAP4rev2-only server is refused although RFC 9051 folds all four into the base protocol; `require_binary` at :978 already accepts rev2.
- [x] selected.ml:387 [high] `uid_fetch` and `uid_fetch_partial` accept `BODY[]`, `BODY[TEXT]`, `BODY[HEADER]` and `BINARY[]`, buffering up to 16 MiB of literal and discarding it, and the non-PEEK forms set Seen without the writable check or `~mutation:true`.
- [x] selected.ml:1097 [high] `fetch_to` keeps only rows with literals, so a body sent as a quoted string such as `BODY[] ""` is reported `Missing_uid`; `fetch_binary_to` handles the same case via `Inline` at :1044.
- [x] selected.ml:1031 [medium] a BINARY row with no UID closes the session but returns `Missing_uid`, which the interface at :145 presents as leaving the connection usable; this path should be `Protocol`.
- [x] selected.ml:910 [medium] `fetch_changes_range` keeps any row with a UID, so a later unsolicited row without FLAGS or MODSEQ overwrites the complete row; `fetch_metadata_range` guards on `Some uid, Some flags` at :654.
- [x] selected.ml:997 [medium] a sink write failure inside `stream_fetch` closes the connection and reports `Transport`, indistinguishable from a network failure.
- [x] selected.ml:522 [medium] the fetch helpers disagree on duplicates and unrequested UIDs: previews overwrite duplicates and ignore unrequested UIDs at :522, object IDs ignore unrequested UIDs at :560 and :608, while `fetch_attribute` at :476 and binary sizes at :1082 fail on them. (step 7: one row policy for every fetch)
- [x] selected.ml:347 [low] `uid_search_page` with `last_uid = 1` returns `complete = false` and `resume_before = None`, a state the interface does not describe, so a direct caller cannot tell done from stuck.
- [x] selected.ml:143 [low] `search_uids` accepts an ESEARCH with no tag despite the interface promise of a matching one, and its untagged SEARCH arm returns server order with duplicates while the ESEARCH arm at :154 returns sorted distinct UIDs.
- [x] selected.ml:634 [low] `fetch_metadata_range ~modseq:true` requests MODSEQ without the CONDSTORE check that `uid_fetch_saved` makes at :421, so a server BAD surfaces as `Rejected`.
- [x] selected.ml:763 [low] the COPYUID source set is never checked against the requested set.
- [x] selected.ml:452 [dead] the `> 50` test cannot fire after the Hashtbl check at :448; the `supports_limit` conjuncts at :661, :666, :917 and :922 are redundant since `accept_partial:false` never yields `partial = Some`; the SEARCHRES recheck at :83 cannot fail; `bytes = 0L` at :1102 is implied.
- [x] selected.ml:476 [redundant] the six `uid_fetch_<x>s` functions repeat UID-list validation, comma join, `Map.Make(Int64)` fold with `List.mem`, and projection; only the UID-list policy, result order, duplicate policy and unrequested-UID policy vary. Plan step 7. (step 7: `Selected.fetch` and `fetch_range`)
- [x] selected.ml:642 [redundant] `fetch_metadata_range` and `fetch_changes_range` at :897 run near-identical MESSAGELIMIT loops; the prefix test is written three ways at :322, :357 and :639.
- [x] selected.ml:679 [redundant] the `List.mem cap` then `raise (State "X unavailable")` pattern appears about twenty times and `has` is defined only at :679; the encoder unwrap about thirty times; `Fetch row | Uidfetch row` extraction twelve times; the tagged-tag match six times; the correlated-ESEARCH filter four times. Plan step 2 and step 9. (the capability idiom is `Session.require` and `require_enabled` since step 2. The witness submodules are left for step 9. `syntax`, fetch-row, completion-tag and correlated-ESEARCH helpers now replace the other copies. Step 9 added the witness submodules)
- [x] selected.ml:43 [redundant] `uid < 1L || uid > 4_294_967_295L` is written nine times at :43, :327, :453, :507, :546, :594, :1004, :1060 and :1089 although `Proto.Uid.of_int64` exists; the 1000-UID window check three times at :352, :631 and :888. Plan step 5. (left for step 5)
- [x] selected.ml:748 [redundant] the rev2 predicate is duplicated in `mailbox_wire` at :748, `require_binary` at :979 and `Client.revision_two`; `Selected.mailbox_wire` duplicates `Client.mailbox_wire` except for the error prefix.
- [x] selected.ml:442 [comment] restates the code; delete. At :38 keep the RFC 5267 sentence and delete "SEARCH retains its existing expansion."
- [x] selected.mli:258 [drift] "require their advertised extensions" is accurate to the code but conflicts with RFC 9051 for MOVE and UIDPLUS.
- Facts for later steps: lease checks, saved-search handles, the 100,000 expansion bound, streamed-body ordering, MESSAGELIMIT loops, COPYUID pairing and missing-response handling are clean. `uid_fetch` returns `fetch.raw` per row, including unsolicited rows not filtered by UID. The UIDONLY guard at :71 checks only whether the first token is made of `0-9,:*`, so `(1:5)`, `NOT 1:5` and `OR 1 2` pass. Capability tokens tested here: SEARCHRES, any `SORT` prefix, ESORT, CONTEXT=SORT, THREAD=ORDEREDSUBJECT, THREAD=REFERENCES, PARTIAL, `MESSAGELIMIT=` prefix, CONDSTORE, QRESYNC, PREVIEW, OBJECTID, IMAP4REV2, IMAP4REV1, BINARY, MOVE, UIDPLUS, IDLE, UIDBATCHES, NOTIFY; enabled: UIDONLY, OBJECTID+, QRESYNC, IMAP4REV2, UTF8=ACCEPT. A missing capability is always `State "<CAP> unavailable"`; missing enabled modes are `State "OBJECTID+ has not been enabled"` and `State "QRESYNC not enabled"`; missing SELECT identity is `Protocol`. `uid_search_range`, `fetch_metadata_range` and `fetch_changes_range` fall back silently without MESSAGELIMIT.

#### lib/sync/bridge.ml

- [x] bridge.ml:717 [high] `verify_pair_date`, called at :780 and :784, returns `Error (Date_diverged _)` when a paired local mtime differs from the pair date, stopping the whole cycle on every run; `preview_sync` at :1252 treats the same condition as a hold and so does the deletion path at :886. A single `touch` blocks the mailbox.
- [x] bridge.ml:819 [high] any `Flags.reconcile_pair` error other than `Deleted_flag_held` or `Pending_operation` stops the cycle, including `Content_mismatch`, which flags.ml:168 raises after opening a `Content_conflict`; the interface at :169 says content conflicts are holds. `Conditional_store_unavailable` and `Permanent_flag_unavailable` also abort while the deletion path holds on `Unsupported` at :913.
- [x] bridge.ml:152 [high] a Local_append is marked `Sent` before `Imap_maildir.append` runs the checks at imap_maildir.ml:484 and :498 that fail identically on every retry, and `reject_unsent_copy` at :421 only rejects `Prepared`, so one such message leaves the operation pending forever and every later cycle returns `Pending_operations`.
- [x] bridge.ml:941 [high] the `Writer_lock_busy` handler at :941, :1017, :1055, :1157, :1309, :1384 and :1446 wraps the whole cycle, and `Writer_lock_busy = Dotlock.Busy` at imap_maildir.ml:17 is also raised by the Dovecot uidlist lock inside scan, find, append and set_flags, so mid-transfer contention is reported as a busy writer lease and, inside `append` after `mark_sent`, produces the stuck operation above; the interface at :83 says Maildir exceptions propagate.
- [x] bridge.ml:964 [medium] `verify_local_content` matches `Some local_id, Some digest, Some length` before checking `local_tombstone`, so every deliberately deleted or retained local copy is counted as missing and reported on every run.
- [x] bridge.ml:777 [medium] a tombstoned pair with both endpoints present gets neither flag sync nor a hold count, so the receipt reports convergence while `preview_sync` at :1273 reports a hold.
- [x] bridge.ml:330 [medium] the legacy intent check compares `expected_flags` with structural equality while :1421 and :1479 use `same_flags`; on disagreement the receipt becomes `None` and the operation stays pending with no diagnostic.
- [x] bridge.ml:564 [low] mapping to `Uidvalidity_changed` depends on the exact text of Engine's message; the same condition from `archive_uid` and `fetch_uid_digest` at engine.ml:565 and :664 arrives as `Sync (Invalid_scope _)` and from `remote_metadata` at :83 as `Client (State "message epoch changed")`.
- [x] bridge.ml:833 [low] a conflict whose pair is missing is reported as `Stale_revision`.
- [x] bridge.ml:379 [low] a missing APPENDUID target is reported as `Source_vanished`, whose message says "vanished before archival".
- [x] bridge.ml:337 [dead] `uid` bound and unused; :244 the Prepared-on-Error branch is unreachable given engine.ml:512; :241 and :1445 `assert false` can be restructured away; :886 duplicates deletion.ml:301; :1542 `length < 0L` is loop-invariant. The `None` branches for `internal_date` at :293, :297 and :359 become live when the field moves in step 4. (the `internal_date` `None` branches left for step 4)
- [ ] bridge.ml:478 [redundant] the local content hash check repeats at :478, :836, :975 and flags.ml:119; the local date check at :485, :697, :1243 and flags.ml:288; verify-observe-commit at :159, :285 and :1362; the local-to-remote verify at :211 and :366; operation-against-intent at :326, :1415 and :1472; evidence validation at :1020, :1313 and :1388 with differing error constructors; `Missing_uid` mapping at :144 and :1359; CAS wrappers about twelve times; the page-of-1000 loop ten times. Plan step 11. (left for step 11: pair evidence helpers)
- [ ] bridge.ml:1070 [redundant] `deletion_preview_of` reimplements the plan in deletion.ml:298 including the tombstone checks at deletion.ml:111; export a pure `Deletion.plan` and use it from both. (left for step 11: Plan module)
- [ ] bridge.ml:110 [redundant] `remote_date` fetches live FLAGS and discards them, then the copy uses snapshot flags at :148, wasting a round trip. (left for step 7: one `Selected.fetch` can return the date with the archived body)
- [x] bridge.ml:280 [optimisation] `Imap_maildir.find` does a full scan at imap_maildir.ml:244 and is called per active operation at :280, :343, :424 and :1333, and per pair from flags.ml:114 while the paged inventory is already held; at 100,000 messages and 100 flag changes that is about ten million stats per cycle. Pass the inventory through. (`find` no longer scans after the maildir fixes, and Flags and Deletion now take the held inventory)
- [x] bridge.ml:75 [comment] restates the function; delete. Keep :108.
- [x] bridge.mli:169 [drift] content conflicts and date mismatches are documented as holds but abort; `preview_*` raise `Invalid_argument` on a negative `min_absence_scans` while `copy_once` returns `Invalid_configuration`; `inspect_append_candidates` rejects `max_uids > 10000` undocumented, skips the raw-name check `Reconcile.inspect_append` performs, and reports the UID span as `inspected_uids`; `repair_local_append` at :1375 skips the post-write flag check the copy path does at :166; `preview_deletions` at :1143 reports stale-epoch pairs with neither side present.
- Facts for later steps: budget, hold counters, journal order, spool removal, cancellation and Stale_revision handling are clean. `Reconcile.inspect_append` is used only by test/oracle/test_oracle.ml:266 and shares no code with the Bridge inspection. Both previews call `deletion_preview_of`. Ctx design from the threading: `{client; store; maildir; scope; mailbox; spool_dir; next_id}` plus a per-cycle `{cursor; uidvalidity; inventory}`. Paged inventory uses: `with_inventory_pages` at :596, :953, :1026, :1124, :1181; `inventory_find` at :460, :699, :790, :839, :967, :1037, :1134, :1231; `inventory_page` at :747, :1211; `inventory_count` at :640, :1193; `?inventory` at thirteen sites; passed to Deletion at :605 and :900. `internal_date` uses: field at :293, :297, :359, :365; `upload_internal_date` at :175, :488, :703, :1246; `append ~internal_date` at :158 and :1374.

#### lib/eio/client.ml

- [x] client.ml:695 [high] when the callback returns `Ok v` and the following UNSELECT fails, `with_mailbox` returns `Error` and drops `v` although every mutation completed; pool.ml:31 then closes the connection and a retrying caller replays a MOVE, STORE or EXPUNGE. Close the connection and still return the outcome.
- [x] client.ml:181 [high] each connection registers an `Eio.Switch.on_release` hook that is never removed, so under `Pool` reconnect churn every closed `Session.t` with its 64 KiB input buffer stays alive until the pool switch ends; use `on_release_cancellable` and remove it in `close`.
- [x] client.ml:634 [confirmed] a Client call from inside `with_mailbox`, including nested `with_mailbox`, `noop` or `logout`, deadlocks with no detection; the `selected` guards at :219, :239 and :259 run inside the lock and cannot catch it. Plan step 10. (step 10: `with_mailbox` takes the session through `Session.locked` and runs the callback under `Session.with_lease`, so these calls return `State` before any byte is sent)
- [x] client.ml:216 [medium] `enable_uidonly` and `enable_objectid_plus` require a literal ENABLE token at :216 and :236 while `enable_revision`, `enable_utf8` and `enable_qresync` at :48, :63 and :73 send ENABLE without checking; RFC 9051 folds ENABLE into rev2, so a rev2-only server advertising UIDONLY is refused. ENABLE is available when advertised or when IMAP4rev2 is advertised, since RFC 9051 makes it a base command; requiring effective rev2 would stop ENABLE IMAP4rev2 itself on a rev1+rev2 server.
- [x] client.ml:219 [medium] after a callback exception at :686 or a failed UNSELECT at :695 the session is closed but `selected` stays `Some`, so `enable_uidonly`, `enable_objectid_plus` and `pin_mailbox_objectid` report a "before selecting a mailbox" `State` instead of `Closed`.
- [x] client.ml:55 [low] `enable_revision` overwrites `enabled` instead of merging; correct only because it runs first on the empty list at :178.
- [x] client.ml:367 [low] `status` and `list_extended` gate only the `Objectid` item; `Highestmodseq`, `Mailboxid`, `Size`, `Deleted` and `Deleted_storage` are sent without checking CONDSTORE, OBJECTID, STATUS=SIZE, rev2 or QUOTA. DELETED also accepts IMAP4rev2, which RFC 9051 includes in STATUS.
- [x] client.ml:93 [dead] the LOGINDISABLED check in `login` is preceded by the same check in `authenticate` at :111; the `require` error branch at :742 and the range test at :831 and :835 are unreachable because response.ml:287 already bounds APPENDUID and the set passes `Uid_set.of_wire`. The range test is gone; the result conversions stay because they are the only way to obtain typed values.
- [x] client.ml:51 [redundant] ENABLED extraction appears five times at :51, :66, :76, :224 and :244; the three optional enables at :47, :62 and :72 and the two required enables at :214 and :234 differ only in name; the effective-rev2 test at :692 bypasses `revision_two`; syntax unwrapping is inlined at :96, :277, :288, :577, :603, :656 and :737 while `command_syntax` at :405 exists; `one_response` at :409 is rewritten in `namespace`, `status_locked` and `get_jmap_access`; the OBJECTID+ enabled check repeats at :257, :335, :368, :582 and :649; the pin lookup at :266, :637 and :710; `canonical` at :345 duplicates `same_mailbox` at :24; `begins` at :26 duplicates `String.starts_with`; the mechanism name is computed twice at :117 and :126; `connect` and `of_flow` handlers at :189 and :196 are identical; :702 is `Result.join`. (the OBJECTID+ enabled check is `Session.require_enabled` since step 2, and every other listed duplicate is factored)
- [x] client.ml:763 [redundant] `append_flow` and `append_binary_flow` are one-line wrappers over `append_receipt ~binary`; `append_messages` at :793 duplicates receipt decoding and Uncertain handling from `append_receipt`. Plan step 8. (step 8: `append` and `append_many` share one part builder and one APPENDUID decoder)
- [x] client.ml:149 [optimisation] a PREAUTH connection sends CAPABILITY twice at :149 and :177; `append_receipt` runs the pinned STATUS at :730 before validating syntax at :735.
- [ ] client.mli:8 [drift] `connect` silently ENABLEs IMAP4rev2, UTF8=ACCEPT and QRESYNC at :178, which changes `mailbox_mode` and replaces EXPUNGE with VANISHED, while the interface calls `enable_uidonly` and `enable_objectid_plus` the explicit modes; `of_flow` is always treated as insecure at :150; `capabilities` and `enabled` return uppercased tokens; `with_mailbox` closes on UIDNOTSTICKY at :667 and a failed UNSELECT replaces the callback result. (left for step 15, except the UNSELECT sentence, which is fixed)
- Facts for later steps: capability comparison is consistently case-insensitive by uppercasing on receipt at :41, :53, :68, :78, :226, :246; ENABLE results are always recorded; STARTTLS ordering, credential redaction and APPEND uncertainty are clean; comments are clean. Without `?auth`, a non-PREAUTH greeting fails `State "authentication required"`. Capability tokens tested here: IMAP4REV2, IMAP4REV1, UTF8=ACCEPT, QRESYNC, CONDSTORE, LOGINDISABLED, AUTH=PLAIN, AUTH=CRAM-MD5, AUTH=OAUTHBEARER, SASL-IR, STARTTLS, UIDONLY, ENABLE, OBJECTID+, NAMESPACE, LIST-EXTENDED, SPECIAL-USE, LIST-STATUS, JMAPACCESS, ACL, QUOTA and the `QUOTA=RES-` prefix, QUOTASET, METADATA, METADATA-SERVER, NOTIFY, UNSELECT, BINARY, LITERAL-, LITERAL+, MULTIAPPEND, `MESSAGELIMIT=` and `SAVELIMIT=` prefixes, COMPRESS=DEFLATE via Session. Never tested in Client: UIDPLUS, MOVE, IDLE, OBJECTID, STATUS=SIZE, ID. Missing-capability errors are always `State` with the strings listed in the review transcript, of the shape "<CAP> unavailable", "server does not advertise AUTH=<M>", "<X> and ENABLE must both be advertised", "binary APPEND requires BINARY capability", "MULTIAPPEND capability unavailable"; not-enabled errors are `State "<X> not enabled"` and the two STATUS OBJECTID variants.

#### lib/maildir/imap_maildir.ml

- [x] imap_maildir.ml:18 [high] `Stale_occurrence` is raised by `open_message` at :545, `sha256` at :562, `set_flags` at :569, `remove` at :598 and `with_unchanged_occurrence` at :468 but is not exported, so callers can only catch it with a wildcard. Plan step 4 typed errors.
- [x] imap_maildir.ml:490 [high] the duplicate-ID check for a supplied `?id` runs via `find` outside the metadata lock, and the in-lock check at :533 tests only the exact target name, so `id:2,S` in cur plus a flagless append of `id` to new succeeds and every later scan fails with "duplicate occurrence identity".
- [x] imap_maildir.ml:229 [high] `id_of_filename` at :229 and :400 accepts dotfiles, so `.DS_Store` in new or cur becomes a message occurrence; Maildir readers including Dovecot skip names starting with a dot.
- [x] imap_maildir.ml:523 [medium] an INTERNALDATE of exactly the Unix epoch always fails because `Unix.utimes p 0.0 0.0` sets both times to now and the check at :524 then raises.
- [x] imap_maildir.ml:535 [medium] `rename` at :535 and :591 overwrites an existing target; the `kind target <> Not_found` checks at :533 and :589 only protect against writers honouring the uidlist lock. Use link plus unlink or `RENAME_NOREPLACE`. (rename kept: names are unique and a duplicate identity is rejected under the lock before the rename)
- [x] imap_maildir.ml:215 [medium] a rename by an external MUA between the lstat at :215 and the stat at :218 raises `Eio.Io Not_found` and aborts the whole scan, while a disappearance before :215 is tolerated.
- [x] imap_maildir.ml:233 [medium] a symlink or subdirectory with a valid-looking name fails at :216 while one with an unparsable name is skipped silently at :234.
- [x] imap_maildir.ml:483 [confirmed] no mutation path checks the writer lease: `append`, `set_flags`, `remove`, `recover`, `ensure_keywords`, `with_inventory_pages` and `open_dir` all skip it; `writer_locks` is read only by `with_writer_lock`. Plan step 4. (step 4: mutations take the `writer`; staging and `open_dir` write no messages)
- [x] imap_maildir.ml:65 [low] `open_dir` raises `Eio.Io` for a non-native path where the interface says `Failure`.
- [x] imap_maildir.ml:257 [low] the "expired Maildir inventory" message lacks the module prefix the others carry.
- [x] imap_maildir.ml:505 [low] `Fun.protect ~finally` unlink at :505 and :178 can replace the original exception with `Finally_raised`.
- [x] imap_maildir.ml:246 [dead] `inventory` is `scan` with a constant `complete = true`; `random_occurrence_id` at :120 exists only to be aliased; `internal_date` is always `Some` at :222, so :413, :274, :445, :447 and :476 are unreachable; `.r<32hex>` alias stripping at :133 has no writer in the tree; `one_current` at :436 never consults the staged row; `assert false` at :315, :338 and :355 cannot be reached. (the `internal_date` `None` branches and the `.r<32hex>` alias are annotated and left for step 4)
- [x] imap_maildir.ml:224 [redundant] `scan` and staging at :388 duplicate the refresh counter and unrecognised-name check and differ only in sink and walk; `iter_directory_batched` at :281 reimplements `Eio.Path.with_dir_entries`, which also returns the kind and saves a stat; the letter table appears at :152 and :194; `index_sub name ":2,"` runs at :131, :144 and :571 and :582 re-parses `filename` output; `index_sub` at :125 allocates a `String.sub` per position; the Hashtbl duplicate check at :238 is redundant with the sort at :242; `ensure_keywords` runs at :502 and :529; the mtime conversion at :200 and :477.
- [x] imap_maildir.ml:244 [optimisation] `find` scans the whole Maildir under the Dovecot lock and is called about twenty times per message from bridge.ml and deletion.ml, so a sync of N messages costs O(N^2) stats and blocks Dovecot; beyond about 10,000 messages it dominates. Locate an ID by testing `new/<id>` and prefix-matching cur names.
- [x] imap_maildir.ml:215 [optimisation] three to four stats per entry at :215, :216, :218 and :234 where one `stat ~follow:false` suffices; at 1,000,000 messages that is 3,000,000 syscalls. `scan` at that size peaks at roughly 0.6 to 1 GB. The paged inventory issues N+1 queries per page at :305. `with_unchanged_occurrence` plus `sha256` re-read dovecot-keywords three to four times per message. Fsyncs at :527, :373 and :186 are unneeded, only :527 matters at scale. `Cstruct.to_string` at :559 copies each 64 KiB chunk where `feed_bigstring` would not. The metadata lock is taken for a flagless append at :502. (scan memory at 1,000,000 messages left for step 4, which replaces `scan` with a bounded fold)
- [x] imap_maildir.mli:47 [drift] `scan` also raises for non-regular entries at :216 and out-of-range mtimes at :203.
- Facts for step 4: `Imap.` is used at :11, :204, :322, :412, :446, :481 and :500, all `Internal_date`, so switching `internal_date` to Unix seconds removes the dependency. `Sqlite3` is used only by the paged inventory at :19, :250, :265, :305, :327, :340 and :365. The inventory database is `tmp/.inventory-<32hex>.sqlite3` with `journal_mode=OFF`, `synchronous=OFF`, `cache_size=-2048`, tables `occurrences(id PK, filename, location 0|1, length, mtime REAL, internal_date TEXT nullable, inode, ctime REAL)` and `flags(id, ord, wire, PK(id, ord))`, staged in one BEGIN/COMMIT; `recover` at :604 knows the filename pattern. Every `Failure` message and its site is listed in the review transcript, prefixed "Imap_maildir: ", plus the Keywords and Dotlock messages; `Invalid_argument` at :257, :261, :342, :343, :484, :489; untyped `Unix_error`, `SqliteError`, `Eio.Io`, `End_of_file` and `Finally_raised` also escape. `inventory.complete` is never false. `threads` is not needed in dune; `unix` is. Filename grammar, keyword-map ordering, mtime ordering, directory fsyncs, `same_occurrence` coverage, `reserve_id` entropy and cancellation handling are clean; comments are clean.

#### lib/store/imap_store.ml

- [x] engine.ml:503 [medium] `append_journaled` calls `Imap_store.load` only to read `cursor.frontier` and `cursor.uidvalidity`, so the production APPEND path materialises the whole destination snapshot per APPEND; use `load_cursor`.
- [x] imap_store.ml:239 [low] `seed_stage_from_published` checks stored revision and scope but not `uidvalidity`, unlike `snapshot_page` at :145 and `snapshot_contains_uid` at :191, so a cursor rebuilt with the current revision and an older epoch seeds rows from a quarantined epoch.
- [x] imap_store.ml:411 [low] `publish_stage` sets `anchor = None` in Condstore mode when no explicit HIGHESTMODSEQ arrives, while `Mirror.complete` at mirror.ml:136 falls back to the largest observed MODSEQ, so the two publish paths persist different anchors.
- [x] imap_store.ml:69 [low] `load` at :69, `publish_stage` at :425 and `decode_cursor` at record_codec.ml:131 discard the `Mirror.error` payload for a fixed string.
- [x] imap_store.ml:92 [low] a stored raw_name or encoding mismatch raises `Failure` in `object_identity` but returns `Conflict` in `observe_object_identity` at :107.
- [x] imap_store.ml:274 [low] with `preserve_newer` the `(None, None)` MODSEQ case reports "incremental row lacks MODSEQ" although the seeded row is at fault.
- [x] imap_store.ml:210 [dead] `action.id = ""` is unreachable since `Mirror.action` is private; the duplicate-cursor and duplicate-stage arms at :71, :133, :144, :190, :207 and :343 are unreachable on primary-key lookups; `load_unlocked` at :28 has one caller.
- [ ] imap_store.ml:337 [dead] `publish` has one caller, `Engine.run_once` at engine.ml:337, whose only caller is test/oracle/test_oracle.ml:183; `load` has callers at engine.ml:281 (run_once) and :503 (should be `load_cursor`). After those two changes both survive only through a test-only path. Plan step 11. (left for step 11)
- [x] imap_store.ml:29 [redundant] the cursor read plus `decode_cursor` appears at :29, :130, :141 and :187 and again as blob_store.ml:116 `current_cursor_unlocked`; the stale test at :145 and :191 duplicates blob_store.ml:122; the stale-header check at :239, :333 and :402 disagrees on the missing-row case; the mailbox upsert SQL at :348 and :427; epoch replacement at :363 and :436; row grouping at :46 and :160; stage header checks at :230 and :393; the action-versus-cursor precondition at :210, :222 and :386; intent types restated at :10. (intent types restated at :10 left for step 1)
- [x] imap_store.ml:271 [optimisation] `stage_rows` prepares a SELECT per row under `preserve_newer` and a DELETE per row always, about 2,000,000 prepares at 1,000,000 rows; hoist into `with_stmt`. Both publish paths delete and reinsert the whole epoch even for incremental scans, so a 1,000,000-row mailbox writes over 2,000,000 rows per sync. `load` at 1,000,000 rows peaks near 0.5 GB and matters from about 50,000. The `count(*)` at :443 re-scans the stage where `changes()` suffices. Indexes are all present. (left: the full-epoch rewrite per publish needs an incremental publish design; `load` memory goes with step 11)
- [x] imap_store.mli:54 [drift] `snapshot_page` and `snapshot_contains_uid` promise a scope check yielding `Stale_revision` but a stored-scope mismatch raises `Failure` via record_codec.ml:124 and a mismatched cursor argument raises `Invalid_argument` at :138 and :184; `object_identity` raises `Failure` at :94; `stage_rows` raises `Invalid_argument` on missing MODSEQ; `begin_stage` raises `SqliteError` on a duplicate stage; `publish_stage` raises `Invalid_argument` at :399, :401 and :415. None is documented.
- Facts for steps 1 and 11: `Sync` and `Blob` are module aliases at :454 and the intent values are aliases at :448 with types restated at :10, so docs can live in the facade only. `open_path` failures are `Failure` with the messages listed in the review transcript, plus `SqliteError` and `Invalid_argument` for the blob directory. Schema: `validate_schema` accepts 8..13, `open_path` migrates 0..13 and writes 13; migration steps 0 to 13 are at schema.ml:137 to :286; read gates at schema.ml:45, :62 and :71, imap_store.ml:85, sync_journal.ml:92, :116, :555 and :569; record_codec.ml:127 carries a cursor format version 1 unrelated to the schema. The two APPEND models are bridged only by a shared ID in bridge.ml:195, :243, :326, :409 and :1412 and by Blob_store orphan checks at blob_store.ml:255. Comments are clean.

#### lib/store/sync_journal.ml

- [x] sync_journal.ml:814 [high] the DELETE repair guards at :814 and :850 compare stored `desired_flags` structurally with `Some (List.sort_uniq compare pair.common_flags)`, but flags are stored in caller order at :516 and read back in that order at :584, so any unsorted list or a DELETE prepared with `desired_flags = None` returns `Invalid_operation` forever. Use `Imap_flag.equal_durable`.
- [x] sync_journal.ml:497 [high] flag sets at :497, :695, :814 and :850 are normalised with the case-insensitive `Imap_flag.compare` and then compared with structural equality, which disagrees with `Imap_flag.equal` for keywords whose case changed, so `commit_operation_with_pair` raises "pair contradicts operation evidence".
- [x] sync_journal.ml:772 [medium] `settle_flag_operation` at :772, `reject_unchanged_delete_operation` at :808 and `attest_targeted_expunge` at :844 return `Invalid_operation` for a stale pair because `current = pair` is a guard conjunct; the interface documents `Stale_revision`.
- [x] sync_journal.ml:255 [medium] `put_pair` forbids clearing a tombstone but allows replacing one, so a `Retention` or `Explicit_delete` tombstone can be rewritten to `Local_absence` and then cleared by `reactivate_local`. (replacement allowed only by the same or a more permanent reason, see F: store)
- [x] sync_journal.ml:241 [medium] a scope change in `put_pair` returns `Stale_revision` where every other immutability violation raises `Invalid_argument`, so a retrying caller loops.
- [x] sync_journal.ml:138 [medium] `note_presence` does not check the pair has an occurrence on the named side; `Remote` on a local-only pair reports a misleading generation error or raises the stdlib `Option.get` exception at :140.
- [x] sync_journal.ml:719 [low] `commit_operation_with_pair` returns `Stale_revision` for a paired operation called with `expected_pair_revision = None`, a caller error that never succeeds on retry.
- [x] sync_journal.ml:869 [low] `attest_targeted_expunge` uses `""` as an overflow sentinel and returns `Invalid_operation` with no reason once the 4096-byte evidence bound is hit.
- [x] sync_journal.ml:45 [low] validation errors name `put_pair` when raised via :754 and :787 from settle and commit.
- [x] sync_journal.ml:797 [dead] duplicates the wildcard at :798; `assert false` at :741 is unreachable given :736; :327 and :640 behind `LIMIT 1`; :566 since `sqlite_master` names are unique.
- [x] sync_journal.ml:799 [redundant] `reject_unchanged_delete_operation` and `attest_targeted_expunge` at :835 are identical through the active-op check, about thirty lines; `settle_flag_operation` at :763 shares eight guard conjuncts; the precondition read appears five times at :548, :722, :778, :820 and :856; the active-state list seven times; the scope-mismatch check at :183, :199 and :215; the SHA-256 validator at :74 and :452; flag insert loops at :283, :516 and :526 and decoders at :97, :541 and :584; the list variants `pairs`, `open_conflicts` and `active_operations` are called only from tests; `put_pair_unlocked` builds the same columns for INSERT at :269 and UPDATE at :274; `operation_source_mtime` probes `sqlite_master` at :555 where a version check would do; the pair is re-read at :237 after the caller read it. Plan step 11. (test-only `pairs`, `open_conflicts` and `active_operations` left for step 11)
- [x] sync_journal.ml:194 [optimisation] `pairs_page` selects ids then issues two statements per pair via `find_pair_unlocked`, so a 1000-page costs 2001 prepares and a 100,000-message scan about 200,000; `decode_operation` at :584 has the same N+1 shape; `open_conflicts_page` at :366 has no index on id restricted by scope and resolved conflicts are never deleted, so paging is O(n^2/limit). Add a partial index `sync_conflicts(id) WHERE resolved = 0`.
- [x] sync_journal.mli:46 [drift] `last_presence_generation` promises presence after an earlier absence but `note_presence` requires no tombstone; `local_source_mtime` at :121 is documented for an unpaired APPEND but the code never checks `pair_id = None`; the three repair docs at :175, :186 and :195 promise `Stale_revision`; `reject_unchanged_delete_operation` does not document exact blob equality, non-None sorted flags; the scope immutability at :29 is enforced as `Stale_revision`; the tombstone immutability at :53 does not hold through `put_pair`.
- Facts for later steps: every CAS runs inside one `BEGIN IMMEDIATE`; pages are ordered and limit-checked; statements are finalised; flag storage round-trips spelling and order. The interface paragraph at :3 attaches to `tombstone_reason`. Creation-input design: `pair.revision` duplicates `expected_revision` and is stored as `revision + 1`; on update `scope.raw_name`, `encoding` and `mailbox_id` are not written; `conflict.resolved` is hard-coded and `pair_revision` is only a CAS token, and `ensure_open_conflict` ignores `id` when one is open; `operation.state` must be `Prepared` and the three receipt fields `None`. This module never touches the intent tables; unifying the two APPEND models is a schema migration since `spool_ref`, `message_id`, `pre_send_frontier` and `expected_internal_date` have no operation columns and the state sets differ. Version gates: v10 at :92, v11 at :569, v13 at :116, v9 implicitly at :555.

#### lib/sync/flags.ml

- [x] flags.ml:370 [high] every `Error` from `uid_store_flags` at :370 and :380 goes to `mark_ambiguous` without a reason, including the pre-dispatch `State` errors "mailbox is read-only" and "CONDSTORE unavailable" from selected.ml:710 and tagged `Rejected`, all of which prove nothing was applied; imap_store.mli:314 forbids that, the bridge APPEND path at bridge.ml:239 rejects on `Rejected`, and recovery then opens a `Flag_conflict` only an operator settle can clear.
- [x] flags.ml:362 [high] `mark_sent` runs unconditionally, so when `remote_needed = false` a concurrent remote change at :386 or local change at :394 creates a pending `Flag_conflict` although no STORE or `set_flags` ran; an ordinary concurrent Seen in a local MUA wedges the pair until settle.
- [x] flags.ml:71 [medium] a missing PERMANENTFLAGS is treated as `Diverged` where RFC 9051 §6.3.2 says the client should assume all flags are permanent, so every remote flag write against such a server fails; undocumented at flags.mli:37.
- [x] flags.ml:82 [medium] with the `\*` wildcard, adding a keyword listed in FLAGS but absent from PERMANENTFLAGS is allowed, which RFC 9051 §7.1 forbids; `\*` licenses only new keywords.
- [x] flags.ml:380 [medium] an uncertain STORE at :380 or a failed post-STORE read at :384 and :407 leaves the operation Sent or Ambiguous with no `Flag_conflict` row, contradicting flags.mli:58.
- [x] flags.ml:281 [low] non-`Client` Engine errors including `Stale_revision` are flattened into `Diverged` text where `Stale_pair` is meant.
- [x] flags.ml:191 [low] a pair in the wrong scope is `Stale_pair` in `recover_operation` and `Missing_pair` in `settle_operation` at :268.
- [x] flags.ml:287 [low] `settle_operation` reports a local content mismatch as `Diverged` while `reconcile_pair` at :171 uses `Content_mismatch`, so imap_cli.ml:1099 cannot distinguish them.
- [x] flags.ml:330 [low] `active_operation_for_pair` is not filtered by kind, so `pp_error` prints "flag operation is pending" for a Delete, Copy or Append.
- [x] flags.ml:200 [dead] `Prepared -> assert false` is unreachable given :183.
- [ ] flags.ml:207 [redundant] nested-result flattening at :207, :236, :295 and :419; the binding check at :194 and :272; `local_content_matches` at :119 duplicates bridge.ml:478, :836 and :975 but without `with_unchanged_occurrence`, so a concurrent change raises instead of returning false; the date check at :288 duplicates three Bridge sites; `current.revision <> pair.revision` at :110 is implied; the `current_pair` reads at :317 and :246 repeat the CAS the calls perform; `Selected.info` is fetched twice at :97 and :128; `flags merged` at :152 re-normalises durable values. (partly fixed: one `with_selected`, one `Selected.info`, no repeated revision or `current_pair` reads, no re-normalising, and `local_content_matches` now uses `with_unchanged_occurrence`. The binding, content and date checks shared with Bridge and Deletion are left for step 11)
- [x] flags.ml:332 [optimisation] `Imap_maildir.find` full scans at :332, :389 and :399 in `reconcile_pair` while bridge.ml:803 holds the inventory, so reconcile is O(pairs times messages); :332 can use `inventory_find`, :389 `with_unchanged_occurrence ?inventory`, :399 the occurrence returned by `set_flags` at :398 which is ignored. `recover_operation` scans at :202 and :228. The body is hashed three times per reconcile at :333, :390 and :400. When remote, local and merged agree but differ from base, the code still journals, re-fetches twice and rehashes just to advance the baseline.
- [x] flags.mli:3 [drift] says the caller serialises writers, but `settle_operation` takes the lease itself at :260.
- Facts for later steps: journal order, MODIFIED handling, preimage re-verification, `equal_durable` use, `Stale_revision` propagation, Recent stripping and cancellation are clean; comments are clean. Only `settle_operation` acquires the writer lease; `reconcile_pair` and `recover_operation` assume it and their only production caller is `Bridge.copy_once_unlocked` under bridge.ml:935. `plan_flags` and `validate_permanent_flags` depend only on `Imap.Sync_policy`, `Mail_flag.Imap_flag` and the local error type, so they belong in `Imap.Sync_policy` with a small error type of their own. All twelve error constructors are produced; the `Diverged` strings are listed in the review transcript. `occurrence.internal_date` is never read here; `upload_internal_date` is used once at :291.

#### lib/sync/engine.ml

- [x] engine.ml:702 [high] one message with `size > max_body_bytes` returns `Error Limit` and `pages None` at :734 always restarts from the lowest missing UID, so no later UID is ever hydrated and imap_cli.ml:630 exits 6 on every sync; `hydrate_once` has no `after_uid`.
- [x] engine.ml:704 [high] when `max_total_bytes < size <= max_body_bytes` the call returns `hydrated = 0, more = true` for the same head UID forever, so the CLI exits 2 indefinitely.
- [x] engine.ml:629 [medium] a blob with `length > max_total_bytes` makes every audit call return `checked = 0, more = true, last_uid = after`, so the operator can never page past it.
- [x] engine.ml:110 [medium] an unbound mailbox whose epoch changed while OBJECTID+ is offered fails every scan with the first-bind error, and publication is exactly what would move the epoch; no API clears it and the string bypasses the bridge's epoch match at bridge.ml:564.
- [ ] engine.ml:156 [low] the in-memory path at :156 and :212 accepts rows without MODSEQ when `use_modseq` is true and publishes with `nomodseq = false`, while `run_once_staged` rejects the same at :447; test-only path. (left for step 11: `run_once` removed)
- [x] engine.ml:452 [low] the `try` at :452 and :463 covers the recursive call, so `fetch` and `inventory` are not tail-recursive and any later `Invalid_argument` becomes `Incomplete`.
- [x] engine.ml:640 [low] a `Stale_revision` after some detaches at :640 or attaches at :680 drops the committed counts.
- [x] engine.ml:554 [low] `fetch_uid_digest` at :741 checks SELECT UIDVALIDITY against the published cursor epoch, not the receipt epoch it exists to verify; it has no `uidvalidity` parameter and bridge.ml:211 and :374 call it before any receipt-epoch check.
- [ ] engine.ml:143 [dead] `run_once`, `scan`, `scan_qresync`, `Uids` and `Uid_set`, about 200 lines at :2 and :143 to :339, have one caller at test/oracle/test_oracle.ml:183; production uses `run_once_staged` at bridge.ml:564 and watch.ml:58. Delete together with the oracle case, engine.mli:3 to :13 and :34 to :42, and then `Imap_store.publish`, `Imap_store.load` and the `Mirror.complete` and `publish` path. Plan step 11. (left for step 11: `run_once` removed)
- [ ] engine.ml:413 [dead] the `fetch_windows` limit cannot trigger given :379 and :397; :449 and the `None -> Ok` arms at :168 and :248 are unreachable since selected.ml:654 drops such rows; :180 and :260 since `uid_search_range` rejects out-of-range UIDs; the `Uid_set.mem` dedup at :185 and :265; `rec` on `more_after` at :666. (partly fixed: the `fetch_windows` guard, the staged `None` arm and `rec` removed. The `run_once` arms are left for step 11)
- [ ] engine.ml:145 [redundant] the window count is written four times at :145, :198, :377 and :395; the FETCH loop three times at :151, :240 and :411; the SEARCH loop three times at :173, :253 and :457; STATUS OBJECTID plus identity comparison at :61 and :87; MODSEQ validation at :40 and :134; flag parsing at :496 and :126; `hydrate_once` at :661 and :715 reimplements `with_fetched_uid` at :563 and `archive_uid` at :576 and opens the spool twice; `guard_bound_mailbox` at :74 repeats `prepare_object_identity`; `verify_mutation_destination` at :80 repeats the STATUS check `Client.append_flow_receipt` already does. (partly fixed: `hydrate_once` opens its spool once. The rest is left for step 11)
- [x] engine.ml:633 [redundant] `Blob.verify store blob || Blob.verify store blob` rehashes a failing blob twice, up to 1 GiB of extra reads.
- [x] engine.ml:687 [optimisation] `hydrate_once` issues one RFC822.SIZE FETCH per UID where the page of 100 could be batched. (step 7: one `Selected.fetch` per page)
- [ ] engine.mli:3 [drift] the preamble describes only the test-only `run_once` including QRESYNC; production uses CONDSTORE CHANGEDSINCE and never QRESYNC. `append_journaled` at :73 omits that a saved binding requires OBJECTID+ already enabled or fails at :84. `archive_uid` at :96 does not remove a pre-existing spool. The `max_messages <= 10000` bound at :600 and :651 is undocumented. `Blob.attach` raises `Invalid_argument` at :579 and :728 when the UID left the snapshot. (partly fixed: `append_journaled`, `archive_uid`, the 10,000 bound and the attach failure are documented. The `run_once` preamble is left for step 11)
- Facts for later steps: windows, SEARCH ordering, anchor source, delta skipping, stage discard, spool scoping, budgets and cancellation are clean; comments are clean. `guard_bound_mailbox` callers: deletion.ml:444, :521, :608, flags.ml:278, bridge.ml:1340, :1496, engine.ml:553, :656. `fetch_uid_digest` callers: bridge.ml:211, :374. The epoch string is exactly `Invalid_scope "mailbox UIDVALIDITY changed"` at :365, :565 and :664, and bridge.ml:564 matches that literal; :111 differs. Engine constructs no `State` string; bridge.ml:83 constructs `State "message epoch changed"`.

#### lib/sync/deletion.ml

- [x] deletion.ml:146 [high] journal writes and spool I/O run inside the lease callback at :146, :207, :536 and :619, so `Session.protect` turns their `Invalid_argument` or `Eio.Io` into `Client (Transport _)` and closes the connection, and a commit at :288 or :658 followed by a failed UNSELECT discards `Ok (Deleted pair)` although the tombstone is committed.
- [x] deletion.ml:422 [high] the module doc at deletion.mli:3 tells callers to hold the writer lease, but the three repairs at :422, :493 and :580 take it themselves and `with_writer_lock` is not reentrant per imap_maildir.ml:27, so a caller following the doc always fails.
- [x] deletion.ml:254 [medium] a conditional STORE that fails with MODIFIED returns `Diverged` and aborts the whole Bridge pass at bridge.ml:919, while the same race one read earlier returns `Held Survivor_changed` at :291; the operation is already rejected at :255 so `Held` is safe.
- [x] deletion.ml:107 [medium] an external rename between `find` and use makes the unexported `Stale_occurrence` escape from `sha256` at :107 and :472 and from `remove` at :177, leaving the operation `Sent` with no unlink; `with_unchanged_occurrence` returns `Changed` for exactly this.
- [x] deletion.ml:141 [medium] a concurrent expunge surfaces as `Client Missing_uid` while the equivalent absence at :210 gives `Stale_inventory`; a longer body trips `max_bytes` as `Client Limit` while a shorter one gives `Held Survivor_changed`.
- [x] deletion.ml:259 [low] `mark_ambiguous` at :259, :262 and :274 is called without `?reason`, so nothing durable records the cause.
- [x] deletion.ml:324 [low] `survivor_unchanged` means "a digest and length are recorded", so a legacy pair without content evidence is held as `Survivor_changed`.
- [x] deletion.ml:363 [low] a scope mismatch is `Stale_pair` in `recover_operation` and `Missing_pair` in the repairs at :431, :504 and :591.
- [x] deletion.ml:152 [dead] `~cursor` is never read in `delete_local` or `delete_remote` at :152 and :192; :154 is unreachable given :330; :130, :132 and :137 since `fetch_metadata_range` returns at most one matching row; :333 and :340 after `bound`; :347 is always true; `assert false` and `_ -> false` at :375, :386, :404 and :410; `with_inventory_pages` plus `inventory_find` at :494 and :581 builds a disk-staged inventory for one lookup that `find` on the same line already does; the open-conflict hold at :299 is unreachable from Bridge because bridge.ml:885 filters it first, so delete Bridge's copy.
- [ ] deletion.ml:418 [redundant] about half of the 247 repair lines duplicate each other or `reconcile_pair`: evidence validation at :418, :487, :569 and flags.ml:256 and three Bridge sites; operation lookup at :423, :495, :582; pair lookup at :427, :500, :587, :358; the six-line identity conjunction at :434, :507, :594, :366; guard error mapping at :444, :521, :608, flags.ml:278; `load_cursor` plus `published_presence` at :453, :527, :614; stable two-read verification at :208, :538, :620; final re-check at :557, :634; post-EXPUNGE commit at :277, :646; post-unlink commit at :177, :472. Six helpers cover them: `valid_evidence`, `load_pending`, `guard`, `verify_stable_remote`, `finish_expunge`, `finish_unlink`. Plan step 11. (partly fixed: local `printable`, `guard`, `pending_delete`, `operation_pair`, `same_identity` and `remote_evidence` helpers. The Repair module is left for step 11)
- [ ] deletion.ml:44 [redundant] `decode_flags` is identical to the unexported `Flags.decode` at flags.ml:60; `remote_metadata` at :121 nearly duplicates `Flags.remote` at flags.ml:124; the PERMANENTFLAGS check at :216 reimplements the exported `Flags.validate_permanent_flags`. (partly fixed: the PERMANENTFLAGS check uses `Flags.validate_permanent_flags`. The flag decoder and remote read are left for step 11)
- [ ] deletion.ml:299 [redundant] bridge.ml:1070 copies :299 to :331 including the tombstone gate; export a pure `plan ~policy ~min_absence_scans ~current_generation ~last_presence ~remote_present ~local_present pair` folding in the `Unverified_absence` gate. Plan step 11. (left for step 11: Plan module)
- [x] deletion.ml:102 [optimisation] `Imap_maildir.find` full scans run twice per local delete at :102 and :178, once per remote delete at :202 and once per recovery at :388, up to 300 per Bridge pass at the default budget, mattering from about 10,000 messages; reuse the occurrence from `inventory_find` at :307 with `with_unchanged_occurrence ~inventory` and `sha256 ~inventory`. (the local delete reuses the staged occurrence, and the fresh absence probes that remain cost one `cur` listing each)
- [x] deletion.ml:468 [comment] the first sentence restates :422; keep only the crash-recovery sentence.
- [x] deletion.mli:29 [drift] says non-regressing MODSEQ but :39 requires strictly greater unless Deleted was set; :46 says the grace period applies to a local delete but sync_policy holds both directions; :67 says Sent and Ambiguous are committed but :376 also commits Observed and :381 requires both sides absent plus the tombstone; the remote-delete preconditions that `spool_dir` is a directory at :200, Deleted is a PERMANENTFLAG at :224 and MODSEQ is nonzero at :214 all return `Unsupported`, which Bridge records as a per-pair hold, so a misconfigured spool directory is held pair by pair.
- Facts for later steps: journal ordering, two-read survivor verification, tombstone-after-absence, `Stale_revision`, spool cleanup and cancellation are clean. `expunge_preflight` is called at :267 and otherwise only from tests. Of the nine `plan_disappearance_with_grace` booleans at :318, `paired`, `remote_complete` and `local_complete` are literals and `local_complete` is justified only by the view's type. `inventory_find` at :307, :379, :517, :604; `with_inventory_pages` at :494, :581; `inventory_page` and `inventory_count` unused. `internal_date` is never read directly; `check_local` never compares the survivor date although `Flags.settle_operation` does.

#### lib/eio/auth.ml, error.ml, transport.ml

- [x] auth.ml:15 [medium] invalid credentials pass construction and only fail inside `resolve_password`, `plain_response` and `resolve_token` at :31, :44 and :58, where client.ml:142 turns the `Invalid_argument` into `Transport "authentication exchange failed"`; a refresher exception gets the same label.
- [x] auth.ml:93 [medium] the CRAM-MD5 username whitespace check runs after `AUTHENTICATE CRAM-MD5` has been sent at session.ml:490, so a local config error closes a working connection and surfaces as `Transport`; PLAIN and OAUTHBEARER build their response before sending.
- [x] transport.ml:180 [medium] the TLS peer name is derived from `host` for every mode, so `Plain` endpoints reject non-LDH hosts such as `imap_test` or scoped IPv6 literals via `Domain_name.host_exn` although `tls_config` is `None`.
- [x] auth.ml:58 [low] the bearer token charset accepts `=` anywhere; RFC 6750 allows it only as trailing padding.
- [x] auth.ml:78 [dead] `resolve_token` has no caller outside auth.ml; drop it from the interface. `Transport.host` and `port` are unused internally but public; keep. transport.ml:204 is a defensive branch; keep.
- [x] auth.ml:15 [redundant] `password` is `refreshing (fun () -> password)` and `bearer` is `refreshing_bearer (fun () -> token)` with duplicated checks at :15 and :31; the flow type is spelled out five times at transport.ml:157, :178, :198, :209 and :248; session.ml:52 and deflate_flow.ml:40 wrap a `close` that already runs under `Cancel.protect`.
- [x] transport.ml:221 [comment] the STARTTLS ownership sentence sits above `check_open`; move next to `upgrade` or delete. auth.ml:82 stays.
- [ ] auth.mli:10 [drift] defaults are `Auto` and `false`, undocumented; `Invalid_argument` for an empty, control or non-UTF-8 username and for `Oauthbearer` on `password` is undocumented. transport.mli:6 defaults are 993 for `Implicit`, 143 otherwise; trust defaults to `Ca_certs.system_authenticator ()` loaded eagerly in `v`, raising `Failure` if the store is missing; `v` raises `Invalid_argument` for an empty host, bad port or unparseable name. None documented. Plan step 15. (left for step 15: Transport.v defaults and exceptions; the Auth constructor defaults and Invalid_argument cases are documented)
- [ ] auth.mli:26 [drift] doc comments at auth.mli:26, :28 and transport.mli:23 sit between two `val`s with no blank line, the facade uses the same pattern throughout, and odoc may attach them to the wrong item; confirm with `dune build @doc` in step 13. (left for step 13)
- Facts for later steps: CRAM-MD5, PLAIN and OAUTHBEARER wire formats, refresher call count, `close` idempotency, `upgrade` failure handling, `compress_deflate` guards, `read` End_of_file consistency and `connect` cleanup are clean. Callers of hidden values: `resolve_password` at client.ml:95 and auth.ml:64, :95; `cram_md5_response` at session.ml:499; `plain_response` and `oauthbearer_response` at client.ml:127. The `@ portable` on the authenticator is required by vendor/tls/lib/config.mli:83.

#### lib/protocol/proto.ml, wire.ml, mailbox_name.ml

- [x] wire.ml:72 [high] a literal length that overflows int64 falls through to `None`, so the line is framed as a complete response and the literal bytes are parsed as control lines; probed with `{99999999999999999999}`. Reject as "literal exceeds limit".
- [x] wire.ml:91 [high] `err` drops every event already framed in the same chunk, so a `* BYE` before a bad byte is lost; probed with `* OK hi\r\nbad\n`.
- [x] wire.ml:41 [medium] the `data_response` allowlist omits ESEARCH (RFC 4731 tag is a string) and LANGUAGE (RFC 5255 astring), whose grammar allows a literal; probed with `* ESEARCH (TAG {2}`.
- [x] proto.ml:75 [medium] `to_wire empty` returns `""`, which is not a valid sequence set and which `of_wire` rejects; callers at flags.ml:373, deletion.ml:258 and selected.ml:684 test emptiness by string comparison because there is no `is_empty`. Plan step 5. (left for step 5: `Uid_set.is_empty` added, `to_wire empty = ""` kept for the three callers)
- [x] mailbox_name.ml:179 [low] `Utf8` mode `decode` and `encode` at :179 and :184 accept NUL, CR, LF and C0 controls that `Rev1` rejects; `Command.quote` catches it later.
- [x] proto.ml:51 [low] `of_wire` accepts leading zeros and its endpoint errors omit the offending token.
- [x] wire.ml:97 [dead] the `remaining = 0L` branch and the `take = 0` branch at :102 are unreachable; mailbox_name.ml:90 `s = ""` is unreachable; `Proto.Seq` has zero callers in lib, bin and test; `Uid_set.union` has zero callers; `encode_rev1` and `decode_rev1` are called only by test/proto/test_proto.ml:458; `decode ~mode` has no external caller beyond `of_wire`, which only test_oracle.ml:93 and :121 call; mailbox_name.ml:4 `fail` aliases `Error`. (partly fixed: both Wire branches, `s = ""` and `fail` removed; `Proto.Seq` is now used by Response range checks; left for step 5: `Uid_set.union`; left for step 6: `encode_rev1`, `decode_rev1`, `decode ~mode`)
- [x] wire.ml:27 [redundant] `starts` and `has_prefix_ci` duplicate `String.starts_with` and the latter re-uppercases; the backward digit scan at :56 and :81; mailbox_name.ml:5 `add_utf8` is `Buffer.add_utf_8_uchar`; :20 `decode_utf8` duplicates a `String.get_utf_8_uchar` loop; :179 and :184 are `String.is_valid_utf_8`. Keep the hand-rolled base64 since it enforces strict padding and the protocol library has no base64 dependency.
- [ ] proto.ml:72 [optimisation] `mem` is a linear scan; matters only when a MODIFIED set has thousands of intervals, which current callers never produce. wire.ml:40 copies each untagged line twice, about 2 MiB per 1 MiB line, linear. (partly fixed: `data_response` compares prefixes in place and no longer uppercases the line; left: `mem` stays linear, no caller builds large sets)
- [x] wire.ml:62 [comment] the second sentence restates the code; delete. Keep :36, :60 and :61.
- [x] proto.mli:33 [drift] `of_intervals` swaps reversed pairs, sorts and merges overlapping and adjacent intervals, and `of_wire` normalises the same way; undocumented. wire.mli:15 defaults are 1,048,576 and 1,073,741,824; `create` raises `Invalid_argument` below 16 or negative; errors are sticky across later `feed` and `finish` calls; undocumented.
- Facts for step 5: 87 `Uid.to_int64` sites and 65 `Uidvalidity` or `Modseq.to_int64` sites; comparisons after unwrapping at mirror.ml:113, :128, engine.ml:236, watch.ml:16, :22, bridge.ml:82, :145, :1360, flags.ml:129, deletion.ml:123. No caller builds `Mailbox_name.t` directly. `Modseq` rejecting 0 is correct for received values. Proto normalisation, `union`, `cardinality`, Wire zero and split-CRLF handling, and the modified UTF-7 edge cases are clean.

#### lib/store/blob_store.ml, operation_intent.ml

- [x] blob_store.ml:280 [high] the directory sync in `Fun.protect ~finally` at :280 has no `try`, so an fsync failure or a cancellation replaces the body's exception with `Finally_raised`; `iter_directory` at :224 has the same shape for `closedir`.
- [x] blob_store.ml:250 [high] `referenced` counts any `blob_refs` row regardless of epoch while `publish` at imap_store.ml:377 and :441 deletes stale refs only for the new epoch, so after a UIDVALIDITY reset every old blob stays live forever.
- [ ] blob_store.ml:271 [high] nothing in lib or bin runs the orphan collector; the only callers of all four functions are test/store/test_store.ml:269 and test_blob_gc.ml:46, so crash temp files and detached blobs accumulate without bound. Wire `reap_orphans_iter` into the CLI at startup under the writer lease. (left for step 12)
- [x] blob_store.ml:116 [medium] `missing_page`, `referenced_page` and `detach_if_matches` raise `Failure` via record_codec.ml:24 when the stored raw_name, encoding or mailbox_id changed, where the interface at :41, :51 and :59 promises `Stale_revision`; imap_store.ml:340 treats the same change as stale.
- [x] operation_intent.ml:117 [medium] `confirm_intent` overwrites the pre-send `uidvalidity` that engine.ml:512 stores and reconcile.ml:69 reads as `journal_uidvalidity`; a receipt from another epoch replaces it and `None` nulls it. Latent since both callers pass `Some`. (receipt versus pre-send epoch left for the v14 migration, step 11)
- [x] operation_intent.ml:90 [medium] a NULL in the nullable `message_id`, `digest` or `spool_ref` columns raises `Failure "expected TEXT"` at :90, :142 and :165, so one legacy row breaks `pending_intents` for the scope; the interface at :36 promises legacy rows stay readable. Use `nullable_text`.
- [x] operation_intent.ml:32 [low] `prepare_intent` accepts `uid = Some _` with `uidvalidity = None`, which `confirm_intent` at :112 rejects.
- [x] blob_store.ml:33 [low] a temp-name collision across PID namespaces makes `put` fail on EEXIST instead of retrying.
- [x] blob_store.ml:21 [dead] `valid_hash` in `filename` and at :84 is unreachable since `blob` is private and every constructor validates; `count < 0L` at :94; `ignore (dec_state ...)` at operation_intent.ml:121; `orphan_candidates` and `reap_orphans` at :271 and :287 once the tests use the iterators. (`orphan_candidates` and `reap_orphans` left for step 12)
- [x] blob_store.ml:116 [redundant] the 14-column cursor read plus `decode_cursor` repeats imap_store.ml:29, :130, :141 and :187, and `checked_cursor` at :122 repeats imap_store.ml:146 and :192; the SHA-256 hex check is at blob_store.ml:16, operation_intent.ml:42, sync_journal.ml:74 and :452; `checked_page_args` at :127 duplicates imap_store.ml:138; blob row decode at :110 and :173; intent row decode at operation_intent.ml:128 and :150 with two column orders.
- [x] blob_store.ml:260 [optimisation] the per-name reference check prepares a statement per name, about 30 to 50 seconds at 1,000,000 blobs and seconds from 100,000; keep one prepared statement or check each 256-name batch in one query. `attach` at :199 re-reads and re-hashes every freshly written message after `put` at engine.ml:578 and :726, doubling fetch I/O. `Cstruct.to_string` at :64 and :96 copies each chunk. `put` fsyncs at :66 before the digest comparison.
- [x] blob_store.mli:8 [drift] `put` raises `Invalid_argument` for a negative length or malformed digest at :39; `attach` raises `Invalid_argument` when `verify` fails at :199; reusing an intent ID raises `SqliteError`; `sync_directory` raises `Unix_error`, not `Eio.Io`. None documented.
- Facts for later steps: the temp-file sequence, prefix disjointness, `verify` under concurrent `put`, epoch check in `attach`, batch correctness, the intent state table and digest validation are clean; comments are clean. Indexes are all present. The `intents` table has 18 columns and `sync_operations` 26; the column mapping and the three intent-only fields `message_id`, `spool_ref` and `pre_send_frontier` are in the review transcript. `intents.uidvalidity`/`uid` conflate pre-send epoch and receipt.

#### lib/eio/deflate_flow.ml, pool.ml

- [x] deflate_flow.ml:93 [optimisation, medium] every 64 KiB chunk on every write allocates a fresh `Lz77.state` (about 0.5 MiB of `prev` and `head`), a 64 KiB window and two copies of the input at :93 and :106; a 100 MiB APPEND allocates about 1.1 GiB and a 40-byte command line about 0.58 MiB. Use a `Manual` Lz77 source on the buffer to remove the copies and probe hoisting the window into `t`. Only `Fixed` blocks are emitted, costing ratio.
- [x] pool.ml:25 [low] `closed` is checked once before `Eio.Pool.use`, so a fiber that waits for a slot while the switch releases calls `connect ~sw` on a finished switch and gets `Transport "Invalid_argument(...)"` where `Closed` is meant; check `closed` in `alloc`.
- [x] pool.ml:35 [low] `Client.close` before `raise ex` can clobber the backtrace; deflate_flow.ml:41 does it correctly with `raise_with_backtrace`.
- [x] deflate_flow.ml:75 [low] `Malformed _` discards decompress's diagnostic.
- [x] deflate_flow.ml:33 [low] `close` takes neither direction mutex, so a read-side codec failure can close `raw` while a writer is inside `Eio.Flow.write`; the interface is silent about `close` racing an operation.
- [x] deflate_flow.ml:65 [dead] `count = 0` is unreachable since `single_read` asserts progress; the non-empty-queue check at :91 is unreachable while every batch ends in EOB. Keep :99 and :74.
- [x] deflate_flow.ml:42 [redundant] `Cancel.protect` around `close` double-wraps :36.
- [x] deflate_flow.mli:127 [drift] cancellation while blocked on the mutex leaves the flow open, which is correct; the doc should say cancellation during an operation closes it.
- Facts for later steps: the sync-flush claim holds, each write ends in an empty stored block; pool accounting, waiter fairness, `validate` on checkout, cancellation and the 16 MiB bound are clean; comments are clean.

#### bin/imap_cli.ml, main.ml

- [ ] imap_cli.ml:1081 [high] every `Bridge.Invalid_operation` at :1081 and :1144 is reported as exit 9 "not found in this scope" and its message dropped, although bridge.ml:1331 returns it for "reserved Maildir occurrence already exists; run sync" and bridge.ml:1466 for "UIDNEXT regressed".
- [ ] imap_cli.ml:761 [high] `repair-appenduid` never checks that `--maildir` exists, so `open_dir` creates a new Maildir at a mistyped path and the writer lease locks the wrong inode.
- [ ] imap_cli.ml:666 [medium] every `Sync`, `Flag_sync`, `Delete_sync` and `Client` error becomes exit 6 with a fixed message at :666, :556 and :642, although `Uidvalidity_changed`, `Modified`, `Stale_pair`, `Identity_changed`, `Stale_revision` and `Invalid_scope` are conflict or configuration outcomes that :591, :1003 and :677 map to 4 or 5; sync prints no `pp_error` detail.
- [ ] imap_cli.ml:429 [medium] a connect or authentication error is discarded although `Imap_eio.Error` already sanitises it; and `Auth.password` at :426 never passes `~allow_insecure_transport`, so `--tls plain` with `--auth plain`, `login` or `auto` without CRAM-MD5 always fails despite the usage text.
- [ ] imap_cli.ml:705 [medium] an unknown operation ID gives 9 in inspect and settle-flags, 3 in repair-appenduid at :770 and 4 in the three delete repairs at :1003 and :1049.
- [ ] imap_cli.ml:1110 [medium] :1110 and :442 match on library error strings "no pending FLAGS operation in this scope" and "Imap_store: stored mailbox scope differs from requested scope"; a wording change silently falls through to 4 or 7.
- [ ] imap_cli.ml:1176 [low] any `Invalid_argument` becomes exit 5 "configuration", including library programming errors.
- [ ] imap_cli.ml:1179 [low] `run` reads the password variable for every command on the exception path, contradicting imap_cli.mli:1.
- [ ] imap_cli.ml:772 [low] `Error _ -> 4` discards the bridge error; :125 and :255 validate `IMAP_PORT`, `IMAP_TLS` and `IMAP_AUTH` for offline commands; :1147, :1003, :1049, :1083, :1114 and :1150 print post-authentication `pp_error` including server text without `redact`; :744 `capped` is true at exactly `max_inspect` while :827, :892 and :978 use strict greater; inspect has no DB pre-check and :536 and :565 follow symlinks while the repairs reject them; main.ml:2 treats `-h` anywhere in argv as help even as an option value; :207 and :193 do not name the offending token; :624 returns 4 on a hold before checking `more`, so one persistent hold under Preserve stops every run after its first cycle.
- [ ] imap_cli.ml:574 [dead] `invalid_arg` at :574, :751 and :753 is unreachable once the receipt fields are typed; :208 binds `dst` unused.
- [ ] imap_cli.ml:238 [redundant] :238 duplicates `bounded_bytes` at :241; hydrate output at :546 and :630; the decision string at :866 and :960 where plan-sync loses the hold reason; preview counting at :850 and :922; the two paging loops at :717; `local_scope` loads the cursor and inspect loads it again at :687; the Maildir requirement at :289, :307 and :329; the operation-id rejection at :342 and :346; evidence validation at :304 and :325; `String.sub ... = "--"` is `starts_with`; `List.hd (List.rev page)` at :726 and :740; the lease-busy to 8 mapping eleven times. Plan step 12.
- [ ] imap_cli.ml:422 [comment] restates the `Fun.protect`; delete.
- [ ] bin/README.md:334 [drift] says `--max-inspect` defaults to 1000 for inspect-append-candidates but :140 defaults to 100; imap_cli.mli:59 says hydrate requires blob and spool directories but :541 creates them; :53 omits audit-cache, mark-local-retention and plan-* from the offline list and the two remote-delete repairs from the online list; usage :48 says `--encoding` is inspect only but six commands accept it; README:321 says the UTF-8 retry follows a different stored encoding but :442 retries on any stored-scope difference and a pinned mismatch exits 7.
- Facts for step 12: `int_of_string_opt` everywhere, cancellation re-raised at :1175, client closed under `Fun.protect` plus `Cancel.protect` at :431, the cycle loop bounded. The three deletion booleans combine at :523 into `Propagate`, `Propagate_remote`, `Propagate_local` or `Preserve`, and mixing is rejected at :363. Grammar: long options only, `--opt value` only, subcommand at argv.(1), no positionals, last repeat wins, fourteen `IMAP_*` environment defaults, `IMAP_PASSWORD_ENV` names the password variable defaulting to `IMAP_PASSWORD`, a "seen" set tracks five options. The exit-code table by condition, the per-command boilerplate table, the config-field usage table and the list of flags accepted but ignored per command are in the review transcript; every field is read by some command. Offline `local_scope` at :435 tries the configured encoding, retries once in UTF-8 on the exact scope-mismatch `Failure`, and online commands take the mode from `Client.mailbox_mode` at :433.

#### lib/store/database.ml, schema.ml, record_codec.ml

- [x] database.ml:18 [high] `bind` never compares the value count with the parameter count, so fewer values leave trailing parameters NULL on first use and, on `run_prepared` reuse at :32 after `reset`, keep the previous row's values; `bind_parameter_count` and `clear_bindings` exist.
- [x] database.ml:9 [medium] `check` discards SQLite's error message, so a constraint failure is reported as "CONSTRAINT" with no table or constraint name; attach `errmsg`.
- [x] record_codec.ml:32 [medium] `decode_cursor` drops the seven distinct reasons `Mirror.restore` gives at mirror.ml:26; `dec_enc` at :14 fails without printing the value.
- [x] database.ml:53 [medium] a nested `transaction` hangs on the non-reentrant mutex instead of failing; the interface forbids nesting but nothing checks it.
- [x] schema.ml:4 [medium] `validate_schema` compares column names and order only, not types, NOT NULL, primary keys, foreign keys, CHECK or the UNIQUE at :284, although the upserts at imap_store.ml:263, :427 and sync_journal.ml:147 depend on those keys. (primary keys and UNIQUE checked; types, NOT NULL, foreign keys and CHECK left: no upsert depends on them)
- [x] database.ml:26 [low] `run_prepared` leaves the statement without `reset` on any failure path; harmless today since every caller lets the exception reach `with_stmt`.
- [x] schema.ml:134 [low] `NOT LIKE 'sqlite_%'` treats `_` as a wildcard, so a table named `sqliteX` escapes the unversioned-database guard; needs `ESCAPE`.
- [x] database.ml:42 [low] `check rc; assert false` raises `Assert_failure` if step ever returns `OK`; use `fail`.
- [x] record_codec.mli:10 [dead] `dec_phase` and `dec_mode` are used only inside `decode_cursor`; drop from the interface. `IF NOT EXISTS` in the v0 block at schema.ml:138 has no effect.
- [x] database.ml:19 [redundant] `run` repeats `run_prepared`; `PRAGMA user_version` is read at schema.ml:14, :102 and :131; the six comparisons at :16 are a range test and the literal 13 appears at :117, :133 and :292; index `sync_pairs_scope` at :215 is a prefix of `sync_pairs_scope_id` at :297 and needs a DROP INDEX migration to remove. (`sync_pairs_scope` left: dropping it needs a migration)
- [x] schema.ml:298 [comment] justifies the unversioned index group but sits between the third and fourth statement; move above :293. Keep :71 and :249.
- [x] schema.mli:7 [drift] says the switch releases the handle on failure but `initialize` at :93 closes it immediately; database.mli:13 sits between two vals without blank lines; database.mli:33 reads as if the mutex lock were cancellation-protected.
- Facts for later steps: migration is crash-safe in one `BEGIN IMMEDIATE` with the version bump; `open_readonly` is correct; `with_stmt` finalises once on every path; version gates are consistent with v13 writes except the `sqlite_master` probe at sync_journal.ml:555; `of_checked` cannot reject a legitimately written value. `Database.t` has no mutable fields; `db`, `mutex`, `blob_dir` and `schema_version` are read by the other store modules; `Database` is private so the one-letter helpers never leave the library. The full DDL by version 1 to 13 with columns is in the review transcript; 21 tables; five auxiliary indexes are created unversioned on every read-write open; no index is created twice and every index matches a query.

#### lib/maildir/keywords.ml, dotlock.ml

- [ ] keywords.ml:58 [high] `flags` raises on a filename letter with no mapping or outside `a-z` and `DFPRST`, and `scan` runs it per entry at imap_maildir.ml:213, so one message renamed by another client with an unknown letter makes the whole mailbox unreadable; Dovecot tolerates unknown letters. (left: by decision an unknown letter still fails the scan so the syncer never reads an unreadable entry as absent, and the message now names the file and the letter)
- [x] keywords.ml:13 [medium] `parse` rejects a file without a trailing newline, and one blank, malformed or duplicate line at :18, :24, :26 and :28 fails `read_keywords` and blocks every read and `set_flags`; Dovecot skips bad lines. (malformed and duplicate lines still fail by decision)
- [ ] dotlock.ml:3 [medium] the lock body is `"%d %s\n"` while Dovecot writes and parses `pid:host`, so Dovecot cannot run its same-host dead-pid check on our lock and the "Dovecot-compatible" claim holds only partly. Verify against Dovecot's file-dotlock.c before changing. (left: no Dovecot source is in the tree, so the `pid:host` body could not be confirmed)
- [x] dotlock.ml:12 [low] a lock deleted by another party makes `refresh` raise a raw `Unix_error ENOENT` while a replaced lock raises `Failure`, two exceptions for one event.
- [x] dotlock.ml:59 [low] after `f` returns, `touch ()` rewrites a byte and re-checks identity, so a committed callback can still end in an exception; undocumented, and `check ()` alone would do.
- [x] dotlock.ml:42 [low] only `ENOENT` from `lstat` is caught in the release path, so `EACCES` or a racing unlink escapes as `Finally_raised` and hides the callback's result.
- [x] dotlock.ml:28 [low] release does not take the refresh mutex; the interface rule that the callback joins its fibers is what prevents a write to a closed fd, so keep that sentence prominent.
- [x] dotlock.ml:50 [low] a failed `fstat` after `owned_fd` is set leaves `acquired = None`, so the finally closes the fd and never unlinks the lock.
- [x] keywords.ml:6 [low] error messages at :6, :54, :62, :24 and :26 drop the offending flag, keyword, letter or line.
- [x] keywords.ml:2 [low] both modules prefix messages with "Imap_maildir: ", wrong once they live in the `maildir` package; a public `Dotlock` needs a named exception for lost ownership. Plan step 4. (step 4: typed errors replace the messages, `Maildir.Dotlock.Lost` is the named exception)
- [x] keywords.ml:63 [redundant] the `DFPRST` table appears at keywords.ml:63, imap_maildir.ml:152 and :192, and imap_maildir.ml:221 appends unsorted system flags to the sorted keywords; Keywords should own both directions. The 64 KiB limit at keywords.ml:37 and imap_maildir.ml:168; `fail` three times; the `ref` plus `String.iter` at :57 is `String.fold_left`; imap_maildir.ml:175 compares `Keywords.t` structurally.
- [x] keywords.ml:21 [format] lines over 80 columns at keywords.ml:21, dotlock.ml:41, :55 and dotlock.mli:13.
- [x] dotlock.mli:1 [drift] "Dovecot-compatible" is contradicted by the lock body; `refresh` raising `Failure` or `Unix_error` on lost ownership and the post-callback check are undocumented. keywords.mli has no value docs; one-sentence contracts for all six values are in the review transcript.
- Facts for step 4: `parse` rejects index above 25, non-canonical indexes and case-insensitive duplicate names; `add` reuses the lowest freed slot; only `Keyword` values are stored; `find` uses `Imap_flag.equal`; the lock is created with O_EXCL directly, so a crash leaves only the `.lock` itself; existing locks always raise `Busy` with the path; release skips unlink when dev or ino differ; acquire and release run under `Cancel.protect`. Dead code and optimisation are clean.

#### lib/protocol/internal_date.ml, sync_policy.ml, mirror.ml

- [x] sync_policy.ml:38 [high] if `\Deleted` differs from base on either side, `reconcile_flags` returns `Error Deleted_flag_requires_policy` for the whole message, so flags.ml:49 holds the entire merge and, since base only advances on persist, a server-side Deleted blocks Seen and Flagged sync for that message permanently; it also fires when both sides added Deleted identically. Hold only the Deleted flag and merge the rest.
- [x] mirror.ml:138 [medium] with no explicit HIGHESTMODSEQ, `complete` uses the largest row MODSEQ as the next anchor, which drops below the previous anchor once the highest message is expunged and yields a false `Modseq_regression` at :146; `publish_stage` at imap_store.ml:411 has no such fallback, so the two anchor rules disagree.
- [x] mirror.ml:20 [low] `initial` raises `Invalid_argument "Imap.Mirror.initial: empty scope"` while `restore` returns `Error (Invalid _)` for the same check at :29.
- [x] sync_policy.ml:76 [medium] with `last_present_generation = Some _` and `first_generation = None` the absence is never mature even at `min_scans = 0`, so a legacy tombstone whose message was ever seen present holds forever; the interface at :52 says it holds only when grace is enabled; no test covers it.
- [x] mirror.ml:135 [low] `nomodseq` on a Condstore action with `restart = Some Uidvalidity_changed` becomes `Some Nomodseq` and loses the original reason.
- [x] internal_date.ml:41 [low] `second <= 60` accepts a leap second at any minute; only 23:59:60 UTC is possible.
- [x] mirror.ml:125 [low] `covered_upper > upper_uid` is reported as `Incomplete_coverage`.
- [x] internal_date.ml:105 [low] `to_unix_seconds` of `01-Jan-0001 00:00:00 +0100` returns a value `of_unix_seconds` rejects; the interface promises no round trip.
- [ ] sync_policy.ml:47 [dead] the `match` choosing `value` always equals `representative`; `| _ -> assert false` at :105 is unreachable by restructuring; mirror.ml:209 `transition.more` is always false and never read. (partly fixed: the `value` match and `assert false` are gone; left for step 11: `transition.more`)
- [ ] mirror.ml:160 [redundant] `flags_equal` duplicates `Imap_flag.equal_durable` but keeps Recent so a Recent-only difference shows as changed; sync_policy.ml:15 `normalize` is `Imap_flag.durable`; :20 `overlay` is `Flags.union`; mirror.ml:32 `generation` and `revision` always carry one value; the anchor rule is implemented at mirror.ml:134 and imap_store.ml:411 and they disagree. Ptime is not warranted since it cannot hold `-0000` or second 60. (partly fixed: `equal_durable`, `Imap_flag.durable` and `Flags.union` replace the local copies, and the anchor rules now agree; left: `generation` and `revision` are persisted cursor fields; left for step 11: the duplicated anchor rule goes with `Mirror.complete`)
- [ ] mirror.ml:160 [optimisation] `publish` is O((n+m) log(n+m)) and about 150 MB per snapshot at 1,000,000 rows; irrelevant once `run_once` goes. Plan step 11. (left for step 11)
- [x] sync_policy.mli:27 [drift] "a changed Deleted is held" reads as one flag but the whole merge fails; mirror.mli:42 lacks `@raise`; sync_policy.mli:52 contradicts :76; internal_date.mli:9 promises invalid clock rejection.
- Facts for later steps: Internal_date calendar, zone, format, equality and range handling are clean under a 200,000-case probe; a total `compare` is derivable by count then leap-first tie-break. `Mirror.restore`, `plan`, `snapshot` and overflow guards are clean. `plan_disappearance` treats no combination as contradictory: both present or both absent gives `No_deletion`, then incomplete missing side, unpaired, survivor changed, retained local, then policy; the present side's completeness is never read; `Unverified_absence` is produced only by deletion.ml:331 and :338. `Mirror.complete` and `publish` are reached only via `Engine.run_once`. Comments are clean.

#### lib/sync/spool.ml, watch.ml, reconcile.ml

- [ ] reconcile.ml:145 [high] an unrelated message above `max_bytes` past the frontier makes every inspection fail with `Client (Limit _)` because the body is fetched before the length compare at :153; Bridge filters on RFC822.SIZE first. (left for step 11: module removed)
- [ ] reconcile.ml:54 [medium] `max_bytes` bounds each body, not the aggregate, so the defaults allow about 1000 downloads of 1 GiB; the interface reads as a total. (left for step 11: module removed)
- [x] watch.ml:80 [medium] any untagged response during IDLE, including a `* OK Still here` keepalive, triggers a full staged rescan and reconnect because :81 discards the list and the loop never re-runs `needs_rescan`.
- [ ] reconcile.ml:78 [medium] the saved OBJECTID+ binding is never checked before UIDs are inspected, unlike `run_once_staged` and Bridge. (left for step 11: module removed)
- [ ] reconcile.ml:145 [medium] the caller's single spool path is reused per candidate, so a leftover file raises `Eio.Io` out of `inspect_append` and closes the connection via the lease's exceptional exit. (left for step 11: module removed)
- [x] spool.ml:3 [low] a raising `unlink` in the finaliser replaces the original exception, including `Cancelled`, with `Finally_raised`.
- [x] watch.ml:28 [low] `idle_renew_seconds` above 1740 is accepted, breaking RFC 2177's 29-minute rule; the default of 1500 is compliant.
- [ ] reconcile.ml:54 [low] `max_bytes = 0L` is accepted although the message says positive; :123 uses plain `uid_search` with a hand-built range instead of `uid_search_range`, so a MESSAGELIMIT server fails the scan. (left for step 11: module removed)
- [ ] watch.ml:43 [dead] the `delay >= max /. 2.` branch duplicates `min`; :68 the non-IDLE branch runs only if a second connection advertises different capabilities and holds a connection while sleeping; spool.ml:17 the overflow guard needs 8 EiB; reconcile.ml:88 the wrapper is `Result.join`. (partly fixed: the watch branches and the spool guard removed. The Reconcile wrapper is left for step 11: module removed)
- [ ] reconcile.ml:1 [dead] `Reconcile.inspect_append` is called only by test/oracle/test_oracle.ml:266; Bridge's inspection is stricter and is what the CLI and other tests use. Remove Reconcile after moving the oracle case to Bridge, which also removes the six findings above. Plan step 11. (left for step 11: module removed)
- [ ] reconcile.ml:41 [redundant] `flags_equal` and `parse` at :41, bridge.ml:66 and engine.ml:126 are three wire-flag loops differing only in error constructor; :78 copies `Engine.validate_scope`; spool.ml:10 `hash_file` and imap_maildir.ml:548 `sha256` share the same 64 KiB loop and `Blob_store.put` repeats the exclusive temp-file pattern. (left for step 11: module removed, and the spool and Maildir hash loops for step 3)
- [x] watch.mli:29 [drift] defaults for `poll_seconds` (60) and `idle_renew_seconds` (1500) and the `Invalid_configuration` conditions are undocumented, and `on_publish` and `on_retry` exceptions propagate, which test_bridge_faults.ml:663 relies on.
- Facts for later steps: Spool exclusive create, non-removal of pre-existing paths, cancel protection and `hash_file` are clean. Watch backoff, timeouts, stage discard on cancel, `Closed` handling, `needs_rescan` coverage and stage-ID uniqueness are clean. Reconcile frontier and window arithmetic, spool removal per candidate, `flags_match` and `Epoch_changed` are clean. Comments are clean.

#### Cross-module findings

These are visible only across modules. Each names the step that absorbs it.

- [x] [error text coupling, step F] bridge.ml:564 matches the literal `Invalid_scope "mailbox UIDVALIDITY changed"` from engine.ml:365, :565 and :664; imap_cli.ml:1110 and :442 match the flags.ml and record_codec.ml message texts; bridge.ml:83 builds `State "message epoch changed"`. Replace each with a typed constructor: `Engine.Uidvalidity_changed`, `Flags.No_pending_operation`, and a typed store scope-mismatch outcome.
- [x] [exception relabelling, step F] session.ml:469 `protect` turns every non-`Session.Failure` exception into `Transport`, which client.ml:142 relies on for auth `Invalid_argument`, deletion.ml:146 suffers for journal writes inside the lease, and pool.ml:25 suffers for a finished switch. Fix once in Session: re-raise anything that is not an I/O failure. (Session by the eio fixes, and Deletion keeps journal and spool work outside the lease)
- [x] [lost callback result, step F] client.ml:695 drops a successful callback result when UNSELECT fails; deletion.ml:288 and every mutating `with_mailbox` caller then reports failure for a committed change. (Client by the eio fixes, and Deletion commits after the lease exits)
- [x] [full Maildir scans, step F then step 4] `Imap_maildir.find` at imap_maildir.ml:244 is a locked full scan called about twenty times per message across bridge.ml, flags.ml (three per pair) and deletion.ml (three to four per delete), so a sync of N messages is O(N^2) stats under the Dovecot lock. Make `find` locate by name without scanning and pass the paged inventory through every caller. (`find` by the maildir fixes, and Flags and Deletion take the held inventory)
- [x] [lease exceptions, step 4] `Writer_lock_busy = Dotlock.Busy` at imap_maildir.ml:17 conflates the application lease with the Dovecot metadata lock; bridge.ml wraps whole cycles in that handler at seven sites; the lease is non-reentrant yet deletion.ml:422, :493, :580 and flags.ml:260 take it themselves while their docs say the caller holds it. Distinct exceptions now, the `with_writer` capability in step 4. (`Metadata_lock_busy` is distinct and Bridge reports `Writer_busy` only for the lease. Step 4: `with_writer` grants a writer that `reconcile_pair` and `recover_operation` take, and the operator repairs take the lease themselves as documented)
- [ ] [pair evidence helpers, step 11] the local content hash check is at bridge.ml:478, :836, :975 and flags.ml:119; the local date check at bridge.ml:485, :697, :1243 and flags.ml:288; evidence validation at deletion.ml:418, :487, :569, flags.ml:256, bridge.ml:1020, :1313, :1388; operation-against-pair identity at deletion.ml:434, :507, :594, :366; guard error mapping at deletion.ml:444, :521, :608, flags.ml:278. One private `Pair_evidence` module in sync.
- [x] [flag equality, step F] structural equality after `sort_uniq compare` at sync_journal.ml:497, :695, :814, :850 and bridge.ml:330 disagrees with `Imap_flag.equal_durable` used everywhere else. Use `equal_durable` and consider an `Imap_flag.Set`. (journal by the store fixes, bridge.ml:330 by the sync fixes)
- [x] [hand-coded UID ranges, step 5] the literal `4_294_967_295L` check is at sixteen response.ml sites, nine selected.ml sites, command.ml:317 and imap_cli.ml:574, :751, :753 although `Proto.Uid.of_int64` exists.
- [ ] [store helpers, step F] the SHA-256 hex validator is at blob_store.ml:16, operation_intent.ml:42, sync_journal.ml:74 and :452; the cursor read plus decode at imap_store.ml:29, :130, :141, :187 and blob_store.ml:116; the stale check at imap_store.ml:239, :333, :402 and blob_store.ml:122 with three disagreeing missing-row cases. Move to Record_codec.
- [x] [capability idiom, step 2 and step 9] the `List.mem cap` then `State "X unavailable"` pattern is at about fifteen client.ml sites, twenty selected.ml sites and session.ml:256; the effective-rev2 predicate is written four ways at client.ml:57, :692, selected.ml:748, :979; `Capability` and `Enabled` are raw uppercase words with no dedup. (step 2 replaced the pattern with `Session.require` returning `Unsupported`, the predicates with `Session.has`, and the words with `Imap.Capability`. The extension witnesses are left for step 9. Step 9 added them)
- [ ] [two APPEND journals, decision in step 11] `intents` (18 columns) and `sync_operations` (26 columns) are bridged only by a shared ID at bridge.ml:195, :243, :326, :409, :1412; `intents.uidvalidity` conflates the pre-send epoch with the receipt epoch (operation_intent.ml:117). Unification is a schema v14 migration. Recommendation: keep both tables this round, add a separate receipt epoch column in v14, and record the unification as follow-up.
- [ ] [blob reclamation, step F and step 12] nothing in lib or bin runs the orphan collector (blob_store.ml:271), and `referenced` at blob_store.ml:250 counts refs from superseded epochs that `publish` retains at imap_store.ml:377, so no blob is ever reclaimed. Run `reap_orphans_iter` from the CLI at startup under the writer lease, and add an explicit epoch-drop operation so quarantined epochs can release their blobs.
- [ ] [finally clobbering, step F] `Fun.protect ~finally` with a raising finaliser at blob_store.ml:280, :224 and imap_maildir.ml:505, :178 replaces the original exception with `Finally_raised`.
- [x] [non-reentrant locks, step 10 and step F] `with_mailbox` deadlocks on a nested call (client.ml:634) and `Database.transaction` hangs on a nested call (database.ml:53); both need owner tracking through a fiber-local key or a flag on the record. (step F made a nested transaction `Invalid_argument`, and step 10 gave `with_mailbox` a fiber-local key)
- [ ] [error payload loss, step F] session.ml:244, :363, command.ml:263, response.ml:1623, :1691, record_codec.ml:32, imap_store.ml:69, deflate_flow.ml:75, database.ml:9, imap_cli.ml:429, :772 all replace a specific message with a fixed one.
- [ ] [test-only scan chain, step 11] `Engine.run_once` has one test caller, and through it `Imap_store.publish`, `Imap_store.load` (once engine.ml:503 uses `load_cursor`), `Mirror.complete` and `Mirror.publish` are test-only. Remove the chain and the oracle case with it, or keep `Mirror` planner functions if the JMAP sibling shares the design.
- [ ] [scale cluster, step F] N+1 queries in `pairs_page` (sync_journal.ml:194) and the paged inventory (imap_maildir.ml:305); a prepare per row in `stage_rows` (imap_store.ml:271) and per name in the blob collector (blob_store.ml:260); a full epoch rewrite on every publish (imap_store.ml:363, :436); a fresh Lz77 state per 64 KiB chunk (deflate_flow.ml:93). Each matters from about 10,000 to 100,000 items.

Rule for step F: apply correctness, dead code, local redundancy and comment fixes module by module. Leave any redundancy that a later step replaces wholesale (the six fetchers to step 7, the capability idiom to step 2 and 9, the Bridge and Deletion helper extraction to step 11, the identifier shapes to step 5, the CLI structure to step 12). Do not tick a finding a later step absorbs; annotate it with that step instead.

#### lib/protocol/command.ml

- [x] command.ml:397 [high] `uid_search` and the SORT, THREAD, PARTIAL and SAVE encoders accept a criterion ending in a literal marker such as `{5}` or `{5+}`, which desynchronises the session; only `uid_search_saved` is safe because it wraps the criterion in parentheses.
- [x] command.ml:317 [high] `valid_set` parses endpoints with `Int64.of_string_opt`, so `+1`, `0x10`, `1_0`, `0b11`, `0u5` and `0o7` pass and are sent verbatim; affects every `uid_fetch*`, `uid_store*`, `uid_copy` and `uid_move`.
- [x] command.ml:10 [medium] `quote` rejects only 0x00 to 0x1F and 0x7F, so 8-bit bytes reach quoted strings in `login`, mailbox arguments, ACL identifiers and metadata values; RFC 9051 quoted strings must be UTF-8.
- [x] command.ml:91 [low] leading zeros pass both `valid_set` and `Proto.Uid_set.of_wire` (proto.ml:91) and are emitted; `nz-number` forbids them.
- [x] command.ml:80 [low] `metadata_entry` accepts `/a//b`, `/a/` and 8-bit bytes; RFC 5464 §3.2 forbids all three.
- [x] command.ml:88 [low] METADATA MAXSIZE has no upper bound; it is a 32-bit number.
- [x] command.ml:107 [low] `setmetadata` duplicate check is case-sensitive while `setquota` at :70 uppercases first.
- [x] command.ml:263 [low] `finite_set`, the sequence-match check at :290 and the flag checks at :499 and :543 discard the underlying error text; `astring` never names the failing argument. (partly fixed: set, sequence-match and flag errors carry their cause; left for step 6: `astring` naming the argument belongs to a typed `Command.error`)
- [x] command.ml:277 [low] `~condstore:true` is dropped when `?qresync` is given; the interface at command.mli:73 does not say so.
- [x] command.ml:84 [dead] the `String.contains s '\000'` test is covered by `contains_control` at :83; delete.
- [x] command.ml:438 [dead] the empty-criterion check duplicates :398; delete.
- [x] command.ml:65 [dead] `quota_resource` aliases `atom` once; inline.
- [x] command.ml:314 [redundant] `valid_set` reimplements `Proto.Uid_set.of_wire` plus `*`; one validator with an `~allow_star` flag replaces both.
- [x] command.ml:34 [redundant] `bind` is `Result.bind`.
- [x] command.ml:97 [redundant] the astring-list loop appears at :97, :139 and :219 and the bare-or-parenthesised wrapper at :101, :144 and :224.
- [x] command.ml:241 [redundant] `list_status` is byte-identical to `list_extended ~patterns:[pattern] ~status:items`.
- [x] command.ml:402 [redundant] the criterion validator is run by building and discarding a SEARCH string at :402, :419, :440 and :481.
- [x] command.ml:70 [redundant] the sort-uniq duplicate idiom appears at :70, :107, :161, :212 and :460.
- [x] command.ml:498 [redundant] flag-list validation at :498 and :542, operation-to-prefix mapping at :52 and :502.
- [x] command.ml:13 [redundant] `atom` is a ref loop that is `String.for_all`, and duplicates the private `valid_atom` in mail-flag imap_flag.ml:52.
- [x] command.mli:153 [drift] `uid_expunge` rejects `*` while every other set encoder accepts it, and RFC 4315 permits it.
- [x] command.mli:68 [drift] nothing says mailbox arguments must already be wire-encoded; `Mailbox_name.encode` is never called here, callers encode first (selected.ml:752, imap_cli.ml:411).
- [x] command.mli:21 [drift] empty `rights` is accepted, emitting a bare `+` or `-`.
- Facts for later steps: 47 distinct error strings, listed in the review transcript, grouped by command family. All nine vocabulary types are to-wire only; `notify_filter` returns a result; `metadata_depth`, `sort_order`, `thread_algorithm`, `sort_return` are written inline; selected.ml:276 has a second `thread_algorithm` mapping to capability names. The FETCH whitelist `valid_items` at command.ml:326 has 19 names; selected.ml:417 keeps a narrower second list. Comments and optimisation are clean.


## Current baseline — do not reimplement

Five public libraries now separate protocol, Eio, Maildir, store and sync.
CRAM-MD5, verified TLS/STARTTLS, streamed bodies, standard Maildir flags,
Dovecot keywords, SQLite staging/journals and many modern extensions exist.
Custom Maildir metadata sidecars are no longer written.

The latest full release-check build passes. Recent store tests and 49 bridge
fault cases pass; all 29 live Dovecot cases passed after store/GC changes,
including shared Maildir metadata interchange and APPEND/EXPUNGE crash recovery.
These results do not prove every failure boundary or concurrent schedule.
A subsequent forced JMAP run passed 798 cases (43 unconfigured live skips).
Fresh Cyrus runs passed both IMAP oracle cases and all 45 separate JMAP oracle
cases with no live skips. The subsequently added cross-protocol case also passes; remaining fidelity
coverage is listed in §5.

## 1. Finish ownership and structural review

- [ ] Finish implementation/dead-code review against the accepted interface
  report; review shared `mail-flag` and `sqlite3-eio` changes with their callers.
- [ ] Introduce a scoped Maildir writer capability. Mutating operations must
  require a live lease; test escaped handles, competing writers, cancellation,
  stale observations and basename/inode replacement.
- [ ] Move disk-backed Maildir inventory responsibilities to the appropriate
  sync/storage layer without weakening bounded scanning or snapshot lifetimes.
- [ ] Review remaining large modules by purpose, especially mailbox publication,
  synchronization journals and CLI configuration. Split only where ownership
  and dependency clarity improve; avoid recreating tiny public libraries.
- [ ] Revalidate installed public interfaces and dependent applications after
  public API changes. Preserve shared IMAP/JMAP flag type identity.

## 2. Close durability and recovery gaps

- [ ] Enumerate every file fsync/rename/directory-fsync and SQLite transaction
  boundary for APPEND, local import, FLAGS, remote/local deletion and publication.
  Add subprocess crashes with old-or-new recovery assertions at each boundary.
- [ ] Cover disconnect/crash between command send, server mutation, tagged
  receipt, journal observation and pair commit. Ambiguous outcomes must remain
  held, never blindly replayed; duplicate candidates must remain distinct.
- [ ] Extend model-based randomized mutation/crash schedules and compare restart
  convergence with a fresh mailbox model, including UIDVALIDITY resets and CAS
  races. Existing directed cases are not the complete SPEC §14.2 matrix.
- [ ] Test concurrent schema initialization/migration and remaining structural
  corruption cases; preserve immediate cleanup on failed opens and atomic
  journal/pair/cursor transitions.
- [ ] Establish writer ownership for blob GC across relevant processes, or make
  the required offline/quiescent operational boundary enforceable. The bounded
  iterator exists; it does not itself prevent a writer adding references.

## 3. Establish Maildir interoperability and migration guarantees

- [ ] Test concurrent Dovecot/local keyword updates, flag renames, message
  replacement/deletion and INTERNALDATE preservation. Sequential shared-tree
  interoperability already passes; it is not adversarial concurrency coverage.
- [ ] Resolve safe unattended stale-dotlock recovery. Current behavior refuses
  existing locks and requires offline intervention. Do not restore the unsafe
  stat-then-unlink stale-lock reclamation race.
- [ ] Document the precise supported concurrency contract, including external
  tools that do not honor the application writer lease.
- [ ] Provide an explicit migration/repair path for old nonempty `.imap-flags`
  and `.imap-dates` trees. They are currently rejected. Preserve occurrence
  identity, dates and custom keywords, and handle Dovecot's 26 keyword slots
  without silent loss. Decide whether this is an offline tool or documented
  export/import procedure and test interrupted migration.

## 4. Complete resource and operational gates

- [ ] Complete large-body measurements for 1/10/100 MiB: normal GC, retained
  live heap, RSS, throughput and less-compressible input across plain, TLS,
  STARTTLS and compression. Distinguish sampled values from absolute peaks,
  managed heap from native buffers, and runtime overhead from payload buffering.
- [ ] Extend measurements to hashing/file sinks, blob hydration and other body
  transfer paths. The current probe covers streamed APPEND/FETCH only.
- [ ] Run current 100,001-occurrence and opt-in million-occurrence scale suites
  after inventory changes; record peak memory and journal paging behavior.
- [ ] Measure bounded blob GC at large scale; its new iterator uses 256-name
  batches and indexed reachability, while compatibility wrappers collect lists.
- [ ] Audit deadlines/progress timeouts, retry/backoff, pool/watcher shutdown,
  redacted diagnostics, profiles and metrics. Document ownership and test
  timeout/cancellation paths without treating uncertain sends as retryable.
- [ ] Complete whole-watcher overflow/backpressure/checkpoint tests. Typed
  NOTIFICATIONOVERFLOW and IDLE completion tests already exist.

## 5. Finish extension and independent-server acceptance

- [ ] Build a current per-extension matrix linking capability negotiation,
  enabled modes, fallback behavior, limits/errors, named scripted tests and live
  evidence. An exported function is not sufficient conformance evidence.
- [ ] Audit SPEC §14.1 fragmentation, malformed input, scalar boundaries,
  partial responses, literal failures and lifecycle coverage against actual
  tests. Fill gaps instead of treating existing suite labels as proof.
- [ ] Check core rev1/explicit rev2 plus QRESYNC/CONDSTORE, UIDPLUS/MOVE,
  SORT/THREAD/ESORT, SEARCHRES/PARTIAL, BINARY, COMPRESS, MULTIAPPEND,
  LITERAL-/LITERAL+, NOTIFY, UIDBATCHES, UIDONLY, MESSAGELIMIT/SAVELIMIT and
  OBJECTID/OBJECTID+ against that matrix. These are audit targets, not a list of
  entirely missing implementations. Keep draft OBJECTID+ explicitly gated.
- [x] Add and run a mandatory Cyrus test for JMAP import → IMAP exact body,
  IMAP APPEND → JMAP exact download, and bidirectional standard/custom keyword
  changes. The new `test_cross_protocol.ml` passes alongside both IMAP oracle
  cases; required mode fails when either endpoint is missing.
- [x] Cover mapping rejection for overlength keywords, Recent/unknown system
  flags and semantic collisions; live-test custom `seen` separately from
  system Seen, and Deleted invisibility in JMAP get/query with restoration.
- [ ] Extend cross-protocol coverage to the full synthetic MIME/duplicate
  corpus, mailbox counts for Deleted messages and server-specific unsupported
  flag behavior. Directed mapping tests do not close the complete fidelity
  matrix or implement a proxy conversion policy.
- [ ] Rerun pinned Stalwart baseline and separate OBJECTID+ fixtures after the
  protocol/identity changes. Rust source inspection is not interoperability.
- [ ] Retain current Dovecot CRAM-MD5, TLS, shared Maildir and crash tests after
  ownership changes; retain authentication failure/redaction coverage.

## 6. Release documentation and final validation

- [ ] Consolidate historical checkpoints into accurate current contracts and
  architecture documentation; remove stale sidecar and old-layout descriptions.
- [ ] Build API documentation cleanly and verify examples and recovery commands.
- [ ] Run required IMAP and JMAP suites, dependent application builds, mandatory
  live fixtures and the workspace release-check integration gate. Record actual
  reruns, cached results and skips separately.
- [ ] Audit every SPEC acceptance requirement against named current evidence
  before claiming production readiness. Keep unresolved ambiguities visible.

## Explicitly deferred or separate scope

- SCRAM and SASL security layers are deferred in the current SPEC. Keep that
  limitation visible; CRAM-MD5 does not encrypt subsequent traffic.
- A complete IMAP/JMAP proxy frontend is a separate product decision (M9).
  Reusable identity, flags, streaming and durable-state boundaries are required
  now; neither server frontend is implemented by these client libraries.
- Durable MULTIAPPEND batch journaling is not implied by the low-level API.
  Current durable sync dispatches individually journaled APPEND operations.

## Immediate handoff

Review the checkpoint before another ownership refactor. The normal-GC,
random-ASCII and STARTTLS memory-probe additions now pass all 18 live
size/transport combinations (36 APPEND/FETCH measurements). Evidence is in
`bleeding/imap/test/dovecot/body-memory-normal-results.csv`. Maximum sampled
additional RSS was 11,419,648 bytes; reserved major heap grew by at most
4,803,952 bytes. Normal mode does not measure live heap: these figures cannot
be compared directly to the 8 MiB live-payload target. Sampling, throughput,
truly incompressible binary input and other transfer paths remain open. The
fixture was removed after the run.
Most IMAP files remain untracked, so inspect the tree directly rather than only
`git diff`. No commit has been created for this checkpoint.
