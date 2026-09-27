## Unreleased

`fbbd86ad0` `Proto.Uid_set.of_wire` now rejects a non-canonical UID
set endpoint, such as a leading zero, and names the bad token.
`Uid`, `Uidvalidity`, `Modseq` and `Uid_set` gain `equal`, `compare`
and `pp`.

`edf906bf8` `Wire.feed` now returns the events framed before a
malformed line instead of discarding them, and reports the error on
the next call. An oversized literal length is rejected at the byte
count instead of later.

`d51956000` A UTF-8 mailbox name containing a control character is
now rejected instead of accepted.

`01df8ecd4` `Internal_date` accepts a leap second only at 23:59:60
UTC, and `to_unix_seconds` is bounded to the representable range
instead of overflowing.

`60a694758` `Sync_policy.reconcile_flags` merges every flag except a
`\Deleted` change the endpoints disagree on, held in the new
`deleted_held` field of `flag_plan`. A legacy absence tombstone
without a generation now matures at zero grace. `Sync_policy.error`
and `Deleted_flag_requires_policy` are gone.

`798179772` `Mirror.complete` anchors only on an explicit
HIGHESTMODSEQ instead of falling back to the highest row seen, and
`changed` ignores `\Recent`.

`630c1838a` Command encoders accept `*` in `uid_expunge`, always
emit CONDSTORE alongside QRESYNC, and reject a quoted string that is
not valid UTF-8, a search criterion ending in a literal marker, and
an empty ACL `Add` or `Remove` rights list.

`f2ce3ab83` `Response` keeps the FETCH line unaltered in `raw`,
rejects a duplicate FETCH or STATUS item, a missing or unterminated
FETCH list, and a sequence number above `int64`, and now accepts and
drops a SEARCH or SORT `(MODSEQ n)` suffix. A THREAD chain of any
length counts as one nesting level, so a 101-message chain is
accepted while 101 levels of nesting is still rejected. An ENVELOPE
with an unterminated quote is now rejected directly by `parse`.
`select_metadata` reports a tagged NO or BAD with its code and text.

`fb75572d3` `Database.bind` checks the bound parameter count, every
statement is reset and cleared on every exit path, `check` reports
SQLite's `errmsg`, and a transaction nested inside another or inside
`locked` now raises `Invalid_argument` instead of deadlocking.

`7d3bc3def` Schema validation checks primary keys and UNIQUE
constraints and escapes `_` in its reserved-name guard, so a table
merely resembling a reserved name is no longer rejected.

`5c57d8546` `Imap_store` gains `exception Scope_mismatch` and a
shared SHA-256 validator, stale check and flag grouping used by
every stage and publish path.

`585d773c4` A stale `Journal` repair now returns `Stale_revision`
consistently, durable flag sets compare as sets everywhere, and
journal pages are read in batches.

`2b7b150e6` A legacy intent's NULL `message_id`, `digest` or
`spool_ref` now reads as `""` instead of failing, a UID without a
UIDVALIDITY is rejected, and confirming an intent keeps its stored
UIDVALIDITY.

`7d7d01110` A blob finaliser failure no longer masks the body's own
exception, and a Unix directory error now raises `Eio.Io`.
`Imap_store` gains `forget_epochs`, to let a caller drop epochs
already quarantined, and `Blob.attach ?verify`.

`73db010ff` `Imap_store.object_identity` now returns
`` [ `Bound of object_identity | `Unbound | `Conflict ] `` instead
of raising on an OBJECTID+ name mismatch.

`494abf5d7` `imap_store.mli` and `operation_intent.mli` document
which error each function reports for a reused intent or stage ID.

`128571363` A staged CONDSTORE publish without an explicit
HIGHESTMODSEQ now anchors on `None` instead of the largest staged
row, matching `Mirror.complete`, so a publish after the highest
message is expunged no longer reports a false regression.

`74b2c0f89` A DEFLATE codec failure now raises `Eio.Io` carrying
decompress's diagnostic instead of an unlabelled exception.

`e5ad32c2f` `Client.with_mailbox` keeps the callback's outcome even
when the trailing UNSELECT fails, and `selected` is reset on every
exit path. A PREAUTH connection now sends CAPABILITY only once.

`e893401dc` A write failure after the final CRLF of a mutating
command is now `Uncertain` naming the cause, instead of an ambiguous
outcome. Only `BODY[...]` and `BINARY[...]` FETCH literals reach the
sink callback. PREVIEW, ENVELOPE, BODYSTRUCTURE and unsolicited
literals do not. A tagged IDLE rejection now leaves the session
open.

`6d44fc3a0` `Auth` validates fixed credentials when constructed, and
an invalid or failing provider now reports `Invalid_credentials`,
which `Client` surfaces as `State "invalid credentials"` before any
secret is sent. `Transport` derives a peer name only for a TLS
connection.

`f5dfad114` `Selected` now accepts effective IMAP4rev2, not only an
advertised token, for MOVE, UID EXPUNGE, SEARCHRES and IDLE. A
BINARY FETCH row without a UID is `Protocol`, a failing FETCH sink
is reported as `State`, and SEARCH results now come back sorted.

`2c2484889` `Pool.acquire` now refuses to allocate a connection
after its switch has been released instead of hanging.

`b8c6a17a3` `Deflate_flow` compresses each write with one LZ77 state
reused across 64 KiB slices of the caller's buffers instead of
allocating one per write. A 1 MiB write now allocates 33.4 MB
instead of 44.2 MB.

`c894239e5` `Spool` no longer loses a callback's exception when
removing its staged file also fails.

`bda44d29d` `Watch.run` reselects after every IDLE wakeup and scans
only when UIDVALIDITY, UIDNEXT or HIGHESTMODSEQ has changed, or at
renewal, and caps `idle_renew_seconds` at 1740. A connection without
IDLE now reports `Idle_failed` instead of holding while it polls.

`0b58aaf34` `Bridge`'s `Writer_busy` now names only the writer
lease. The Dovecot metadata lock reports Maildir's own
`Metadata_lock_busy`.

`c8945a2a6` `Engine.hydrate_once` takes `?after_uid` and skips a
body over either size budget into a new `skipped` count instead of
failing the pass, and `audit_cache_once` skips an oversized blob the
same way. Both report `more=true` after a concurrent publication.
An epoch change no longer fails every scan waiting for its first
OBJECTID+ binding.

`bd958e03b` `Flags.reconcile_pair` merges every flag except a held
`\Deleted` change, and an uncertain STORE or a failed verification
read now records a reason and a `Flag_conflict` instead of leaving
the operation ambiguous. PERMANENTFLAGS is read per RFC 9051.

`cd90f4053` `Deletion` now journals and hashes outside the mailbox
lease, and holds a MODIFIED response, a longer body, a stale
occurrence or a legacy pair without content evidence
(`Missing_content_evidence`) instead of deleting on partial
evidence. A concurrent expunge is reported as `Stale_inventory`.

`e5a4c796c` `Bridge.copy_once` now holds a pair whose date, content,
CONDSTORE state, PERMANENTFLAGS or a concurrent change cannot settle
instead of failing the whole cycle, and rejects a remote copy
Maildir cannot store before journalling it.

`e1603535f` `Deletion.reconcile_pair` documents that a negative
`min_absence_scans` raises `Invalid_argument`.

`d948dc830` `Imap_store.Sync` is renamed `Imap_store.Journal`
throughout.

`bdf4d9ff3` `Imap.Capability` types every advertised capability
token, comparing names case-insensitively per RFC 9051, with
`of_wire`, `to_wire`, a `Set`, and typed lookups such as
`messagelimit` and `auth_mechanisms`. `Response.Capability` and
`Enabled` now carry deduplicated typed lists.

`a21c8b091` Every missing-extension error is now `Error.Unsupported
cap` and every missing enabled mode `Error.Not_enabled cap`,
replacing a `State` text message. `Client` gains `has`, `is_enabled`
and the general RFC 5161 `enable`.

`97b0820c9` The CLI and `imap.sync` now open `Maildir` from the
separate `maildir` package instead of the bundled `Imap_maildir`.

`68af6a250` `Bridge`, `Flags` and `Deletion` gain a `Maildir of
Maildir.error` error case, and `verify_local_content`,
`mark_local_retention`, `preview_deletions` and `preview_sync` take
`~spool_dir`.

`a1ceb1900` `Imap.Proto` is gone. `Uid`, `Uidvalidity`, `Modseq`,
`Seq` and `Uid_set` are top-level modules, each with `of_int64`,
`to_int64`, `equal`, `compare` and `pp`, and `Uid` adds `succ` and
`pred`.

`7dbd4c0ca` `Selected` takes and returns `Uid.t` for every UID
argument and result, and `Uid_set.t` for `uid_fetch` and
`uid_fetch_partial`, which now refuse an empty set with `State`.
`Error.Missing_uid` carries a `Uid.t`.

`1db019cc4` `Command.error` is now a record `{ command; argument;
reason }` naming the argument at fault, and `create`, `delete`,
`subscribe` and `unsubscribe` take `~mailbox`. Search, sort, thread
and notify vocabularies move into their own typed modules
(`Status_item`, `Mailbox_list`, `Sort`, `Thread`, `Notify`,
`Metadata`).

`2415590b9` `Mailbox_name.t` is private, and `Client.list`, `lsub`
and `discovery.mailboxes` return a decoded `mailbox_entry = { name;
info }` instead of a raw string.

`67170b305` `Imap.Search` types the RFC 9051 search keys and
`Imap.Fetch_item` the FETCH metadata items, each with a checked
`to_wire`.

`5681e332f` `Selected.fetch` and `fetch_range` replace `uid_fetch`
and the six per-item fetchers. Rows for one UID merge, with FLAGS
and MODSEQ taking the last value and any other conflicting item
reported as `Protocol`.

`b03f5b918` `Client.append` replaces `append_flow`,
`append_flow_receipt`, `append_binary_flow` and
`append_binary_flow_receipt`, and `append_many` replaces
`append_messages`. Both take typed `Mail_flag.Imap_flag.t` flags.

`42eb28cab` Extension-only operations move into lease and client
witness submodules (`Condstore`, `Qresync`, `Uidplus`, `Move`,
`Acl`, `Quota` and others), each gated by its own `require`, in
place of a per-call capability check.

`286cc6993` A `Client` call made from inside `with_mailbox` on the
same connection now returns `State "call inside with_mailbox on the
same connection"` instead of deadlocking.

`92270326b` `Engine.run_once`, `scan` and `scan_qresync` are
removed. `run_once_staged` is renamed `scan_once`. `Reconcile` is
removed.

`fb96b9170` `Imap_sync.Error.t` replaces four separate module error
types with one flat type. Every online entry point takes `~ctx`, a
private `Ctx.t` built once by `Ctx.v`.

`533f506c0` Repairs move into `Repair` and previews into `Plan`.
`Deletion.plan` is now the pure deletion decision shared by
`Deletion.reconcile_pair` and `Plan`.

`9c9eb2334` `imap-sync` is rebuilt on cmdliner with one term per
command, and `--deletion-policy
preserve|propagate|propagate-remote|propagate-local` replaces the
three separate deletion flags.

`494effca3` `imap-sync` gains `gc`, which removes orphan blobs, and
`forget-epochs`, which drops quarantined epochs and prints
`epochs_dropped=N`. `sync` runs the orphan collector at startup and
prints its count when nonzero.

`e03ce0e85` Offline `imap-sync` commands load the stored cursor once
instead of once per operation.

`f5dcfb18b` bin/README.md documents every `imap-sync` command, its
exit codes and blob reclamation.

`668cfb783` `Imap` is the library's main module, aliasing all 21
protocol modules under one documented facade.

`39d8a46a7` The package gains odoc pages (`doc/index.mld`,
`client.mld`, `sync.mld`) built from compiled examples in
`test/examples`.

`62d068812` `imap.opam` lists `sqlite3`, `optint`, and `ptime` and
`jmap` as test-only dependencies. Several unused library
dependencies are dropped.

`796c75184` The README points at the odoc pages and examples.

`d5cc58200` `Imap_eio.Mailbox` wraps a `Selected.t` and reports the
strategy it chose for every operation, such as `` `Move ``,
`` `Copy_then_expunge `` or `` `Qresync ``, including on failure.

`b6957cba1` The client guide shows `Mailbox.move` reporting its
chosen strategy.

`cc38f155e` Every `lib/protocol` interface is fully documented under
the doc-style rules, stating every limit and default against the
implementation. No signature changed.

`1e8032fdd` Every `lib/sync` interface is fully documented. `Error`
now states, for each constructor, whether a retry, a later
reconciliation or an operator action follows.

`2b0b284d9` `imap_cli.mli` documents every record, field and exit
status, and bin/README.md is regrouped by task with every default
and range from the cmdliner terms.

`d22b216e3` The README is rewritten to the current layout, dropping
stale schema-version and future-work sections.

`028a9c7ed` `imap_eio.mli` states the session bounds directly: 64
KiB per command line, 10,000 untagged responses, 16 MiB per
response and 64 MiB per command.

`cdbd2e380` `imap_store.mli` states its durability contract: one
mutex, one transaction per write, WAL with `synchronous=FULL`, a 5 s
busy wait, and compare-and-swap publication with no replay.

`be84b858c` An uncertain STORE or UID EXPUNGE, or a failed
verification read after a flag STORE, now returns
`Pending_operations` so `sync` exits 3 instead of 6. `Bridge.copy_once`
and `Plan.preview_sync` take `?propagate_deleted` (default `false`),
and `--propagate-deleted-flag` sets it on `sync`, `plan-sync` and
`plan-deletions`. An unknown or mismatched operation ID is
`No_pending_operation` everywhere, and a bad budget or spool path is
`Invalid_configuration` everywhere.

`affaea851` `Response` now requires an astring for a STATUS mailbox
name, requires a positive ESEARCH MODSEQ, and applies the same 1 MiB
and balanced-quote checks to `fetch_objectid` as to other FETCH
extractors.

`99117f194` `Uidbatches.uid_batches` records a mailbox only after a
tagged OK, per connection, so a rejected request no longer blocks a
later one for the same mailbox. `Sort.uid_sort` and
`Thread.uid_thread` report more than 100,000 results as `Limit`
instead of `Protocol`.

`b6b45b59f` `Journal.note_presence` returns `Stale_revision` instead
of raising when the published generation has moved on.
`confirm_intent ~uid:None` keeps the stored UID, and `publish_stage`
checks staleness before stage coverage.

`132c6c04a` `test/bench` adds six benchmark executables (`bench_wire`,
`bench_uid_set`, `bench_encode`, `bench_session`, `bench_maildir`,
`bench_store`) that `@all` builds. `runtest` does not run them.

`5974c418b` `Uid.t`, `Uidvalidity.t` and `Seq.t` are declared
`immediate`.

`bae4359fc` `Uid_set.t` holds its interval bounds in an `iarray` and
bisects in `mem`, cutting a 100,000-entry membership benchmark from
436 ms to 6.4 ms.

`2e13a486a` Every `lib/protocol` interface is `@@ portable`.

`2c75fa627` `Error`, the `Auth` constructors and accessors, and
`Client.pp_error` and `error_to_string` are `@ portable`.

`824f35726` `Wire.feed` frames lines without a boxed `int64` offset
or a per-keyword closure, cutting its 100,000-row benchmark from
178.5 ms and 688 MB to roughly 105 ms and 275 MB allocated.

`93cc5f9ad` A response with a single literal-free line now parses
directly from that line instead of splitting it first.

`450684c38` The Session read loop detects a FETCH response from its
first three words instead of splitting the whole line.

`ed4ac2d2e` `Search.to_wire` encodes dates and sizes without
`Printf`, cutting its allocation from 424 MB to 363 MB over 100,000
encoded criteria.

The store has one schema at SQLite `user_version` 1 and no migrations.
`open_path` and `open_readonly` reject a database at another version or
whose tables or indexes differ from it, and `sync_pairs_scope` is gone.

`Journal.pairs`, `open_conflicts` and `active_operations` and
`Blob.orphan_candidates` and `reap_orphans` are gone. Use the paged
readers and the orphan iterators.

The APPEND intents are gone. `Journal.operation` carries an APPEND's
message ID, spool reference, pre-send UID frontier and INTERNALDATE, and
`Engine.append_journaled` sends an operation the caller prepared, moving
it to `Sent`, `Observed`, `Ambiguous` or `Rejected`. `inspect` prints
those fields.

Each flag list is one text column of wire spellings on its row, and the
single-valued operation side tables are columns of `sync_operations`, so
the schema has 10 tables instead of 19 and reads join no flag rows.
