# Operational IMAP sync command

`imap-sync audit-cache` checks stored body blobs without connecting to IMAP.
It rehashes a bounded page of references from the published inventory and
removes an exact SQLite reference if its file is missing or corrupt. It leaves
mailbox membership, Maildir occurrences and blob files unchanged. Run
`hydrate` or `sync --hydrate-bodies` afterward to fetch invalidated bodies.

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- audit-cache \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite \
  --blob-dir /var/lib/imap-sync/inbox.blobs \
  --max-transfers 100 --max-total-bytes 1073741824
```

The result prints `cache_checked`, `invalidated`, `bytes`, `last_uid`, `more`
and `revision`. If `more=true`, repeat with both `--after-uid LAST_UID` and
`--expected-revision REVISION`; a changed published revision is refused so
the continuation cannot skip a new snapshot. Exit 2 means another page
remains. If `last_uid=0` and `more=true`, raise the byte budget enough to fit
the next blob. Audit is an explicit operation; regular sync does not rehash
every cached body.

`imap-sync hydrate` fills missing local message bodies from an existing
published SQLite mailbox inventory. It does not need a Maildir and does not
change message flags. Each invocation is bounded by `--max-transfers`,
`--max-body-bytes` and `--max-total-bytes`; it exits 2 when another pass is
needed. Set the total byte budget large enough for the next message, or a
pass can correctly make no progress. For example, after a successful scan:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- hydrate \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite \
  --blob-dir /var/lib/imap-sync/inbox.blobs \
  --spool-dir /var/lib/imap-sync/inbox.spool \
  --max-transfers 100 --max-body-bytes 1073741824 \
  --max-total-bytes 1073741824
```

The password comes from `IMAP_PASSWORD` by default. The command verifies
the published mailbox identity and UIDVALIDITY, preflights RFC822.SIZE, and
attaches each exact body only after a completed FETCH and synced blob write.
It leaves already attached bodies in place if a later message fails.
Add `--hydrate-bodies` to `imap-sync sync` to run the same bounded hydration
pass after a bridge cycle finishes without held work or open conflicts. The
`--max-transfers` count applies separately to bridge transfers and hydration;
`--max-body-bytes` and `--max-total-bytes` bound that hydration pass. If bodies
remain, sync exits 2 so a scheduler can invoke it again. Hydration does not
run when the bridge itself still has more transfers or a conflict to resolve.

`imap-sync` drives one bounded IMAP↔Maildir bridge cycle at a time. It uses
`imap.store` for the SQLite journal and `imap.maildir` for exact message files.
The command defaults to preserving messages that disappear on one side. Set
`--propagate-deletions` only for a mailbox where that policy is intended.
For one-way propagation, use `--propagate-remote-deletions` to remove a
local survivor after verified remote disappearance, or
`--propagate-local-deletions` to remove a remote survivor after verified local
disappearance. The two directional flags together have the same effect as
`--propagate-deletions`; combining either with the latter is rejected.
For propagation, `--min-absence-scans N` waits for N additional complete
remote scan generations after an absence is first recorded before deleting
the survivor. For example, `--min-absence-scans 1` holds deletion in the
first complete scan that observes the absence and permits it after the next
complete scan confirms it. The default is 0, so existing opt-in propagation
acts on the first verified absence. Pass the same option to `plan-deletions`
or `plan-sync` to preview the grace hold. A legacy local absence tombstone
without a recorded generation stays held when N is positive; a new complete
scan does not replace that original evidence, so inspect or repair that pair
explicitly.
If a complete scan observes the paired side present again, the syncer records
that observation in SQLite. A later disappearance starts a fresh grace
period; the offline plans hold it until a new complete absence scan publishes
its first-generation evidence. This also applies to legacy tombstones once
an intervening presence has been observed.
An exact local restoration, including the paired INTERNALDATE, retires the
local absence tombstone so flag sync resumes. Altered bytes or date keep a
durable conflict and block deletion until the original occurrence is restored.
Local cache eviction must be marked explicitly before enabling propagation
from local to remote:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- mark-local-retention \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --pair-id pair-123 --evidence 'local cache expired'
```

The command requires the paired local occurrence to be absent, an active
remote binding, and no pending operation for that pair. It records a durable
retention tombstone under the Maildir writer lease. Subsequent sync cycles
hold remote deletion for that pair even under `--propagate-deletions`.
It does not perform server I/O. Record retention before a sync cycle with
local-to-remote deletion propagation runs; an already completed remote
deletion cannot be reversed by this marker.

To review one-sided pairs before enabling propagation, run the offline
candidate planner with the intended direction:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- plan-deletions \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --propagate-local-deletions --max-inspect 100
```

The planner reads the latest complete *published* remote inventory and stages
a fresh Maildir inventory under the writer lease. It pages all established
pairs, prints up to `--max-inspect` one-sided pairs, and reports complete
candidate, hold, and pending counts. It makes no IMAP connection or sync
journal changes. A candidate means the next sync would *attempt* deletion
under this policy; the syncer must still refresh the remote inventory and
verify live survivor identity, content, flags, and required server
capabilities before acting. If no
complete published remote inventory exists, the planner refuses to infer
absence. Run a preserving sync cycle to publish one first.

`plan-sync` extends the same offline view to copy candidates and paired
three-way flag changes, including the default `\Deleted` hold:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- plan-sync \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --propagate-remote-deletions --max-inspect 100
```

It pages the published remote inventory, a fresh disk-staged Maildir
inventory, and the pair table under the Maildir writer lease. The command
prints bounded event detail and complete counts for copies, flag changes,
deletions, policy holds and pending work. A saved `content` conflict appears
as a pair hold and suppresses its FLAGS candidate. A populated mailbox on both sides
without existing pairs yields only a bootstrap hold unless
`--allow-bootstrap-duplicates` is supplied, matching `sync`. This plan
does not connect to IMAP or change the journal; current server capabilities,
message bodies, and concurrent changes are checked by the later live sync.

Set credentials through an environment variable; the command has no password
argument and never prints the password or server authentication text:

```sh
export IMAP_PASSWORD='your secret'
opam exec -- dune exec bleeding/imap/bin/main.exe -- sync \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite \
  --blob-dir /var/lib/imap-sync/inbox.blobs \
  --maildir /home/alice/Maildir \
  --spool-dir /var/lib/imap-sync/inbox.spool \
  --max-transfers 100 --max-cycles 10
```

`--auth auto` is the default and negotiates a server-advertised mechanism.
`--auth cram-md5` requests CRAM-MD5 explicitly. Implicit TLS is the default;
`--tls starttls` requires STARTTLS. `--tls plain` should be used only for a
trusted local fixture. CRAM-MD5 authenticates the password exchange but does
not encrypt subsequent mail traffic.

The database parent directory must exist. The command creates the blob and
spool directories with owner-only permissions. The Maildir root's parent must
exist. Use a stable `--endpoint`, `--account`, `--mailbox-key` (defaults to the
mailbox name), database path, and Maildir path across runs. A new database
with messages on both sides stops before copying unless
`--allow-bootstrap-duplicates` is explicitly set. Inspect the mailboxes first:
the bridge never pairs messages merely because their bytes match.

Each invocation is bounded by `--max-transfers` and `--max-cycles`. Exit 0
means this invocation reached a converged pass. Exit 2 means more bounded
work remains; call it again. Exit 3 means pending journal work needs operator
inspection, 4 is a conflict or unsafe bootstrap, 5 is configuration, 6 is an
IMAP/protocol failure, 7 is local storage failure, and 8 is a busy Maildir
writer lease. A nonzero exit never authorizes automatically replaying a
possibly-sent APPEND or delete. An external scheduler can invoke the command
again with its own backoff; there is no endless in-process retry loop.
Exit 4 also reports a held flag or deletion policy decision; the command
prints up to 100 affected pair IDs so that a quiet no-op is not mistaken for
convergence.
If a remote UID vanishes after inventory but before body archival, sync leaves
no local transfer journal and rescans within `--max-cycles`. When the cycle
budget is exhausted, it exits 2 so the scheduler can retry.
The same bounded rescan applies when a staged local Maildir occurrence changes
before its bytes are archived; no APPEND intent is created for those bytes.
With the default preserve policy, a one-sided disappearance creates a durable
`deletion_hold` conflict and a nonzero hold count; repeated scans retain the
same conflict ID. Resolving the disappearance or explicitly enabling
propagation clears it after a complete scan.
For a held `\\Deleted` flag, a durable `policy` conflict retains its pair ID
and evidence across restarts. Repeated scans refresh the same conflict;
removing the held change or tombstoning the pair resolves it after the next
complete scan.
A sent FLAGS operation whose saved target cannot be verified leaves a pending
journal entry (exit 3) and a durable `flags` conflict describing the mismatch.
Its ID stays stable across repeated recovery attempts. A later verified FLAGS
pair commit resolves that conflict in the same SQLite transaction; recovery
never replays an uncertain remote STORE.
If an operator has manually aligned the two endpoints to the same flags,
`settle-flags` can adopt that state as the new common baseline:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  settle-flags \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --operation-id op-456 --evidence 'operator aligned both endpoints'
```

The command sends no STORE and changes no Maildir flags. It requires a
sent, ambiguous or observed FLAGS intent, a matching pair revision and
UIDVALIDITY, the paired local body and date, identical endpoint flags and a
stable remote MODSEQ across two reads. A saved OBJECTID+ binding must still
match the configured mailbox. Under the Maildir writer lease it atomically
rejects the superseded intent, advances the paired common flags and resolves
the flag conflict. If verification fails, the intent stays pending.

An uncertain remote DELETE whose UID is still present can be rejected only
after verifying that the target has its original paired bytes and flags:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  reject-remote-delete \
  --host mail.example.org --tls implicit --user alice \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --spool-dir /var/lib/imap-sync/inbox.spool \
  --operation-id op-789 --evidence 'target still has original bytes and flags'
```

This command requires a sent or ambiguous paired DELETE, the saved pair
revision, a complete published inventory containing the UID, a complete
Maildir scan proving the local occurrence absent, and a matching saved
mailbox identity. It reads the remote body into a spool capped at the saved
paired length,
compares its exact SHA-256, length and flags, and requires a stable MODSEQ
across reads. It changes only the journal state to `rejected`; it sends no
STORE or EXPUNGE. An already expunged or modified target stays pending for
normal inventory recovery or further operator investigation. A later sync
with deletion propagation explicitly enabled may prepare a new DELETE.

If the same pending UID instead has exactly its original flags plus
`\Deleted`, an operator can explicitly finish that targeted deletion:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  finish-remote-delete \
  --host mail.example.org --tls implicit --user alice \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --spool-dir /var/lib/imap-sync/inbox.spool \
  --operation-id op-789 --evidence 'reviewed exact marked UID for expunge'
```

The command requires UIDPLUS and CONDSTORE, the original pair revision,
published UID membership, local absence, a matching saved mailbox binding,
exact bytes, and stable MODSEQ and flags across two reads. It durably records
the operator's evidence and marks the operation ambiguous **before** sending
`UID EXPUNGE` for that UID alone. It verifies UID absence before committing.
If the result is lost, the operation stays pending; a later complete scan
may commit absence without replaying EXPUNGE. It never issues mailbox-wide
EXPUNGE. Remote edits can still occur between the final FETCH and EXPUNGE;
review the target immediately before running this command.

The read-only journal view uses the same endpoint/account/mailbox identity:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- inspect \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --max-inspect 100
```

Inspection pages active operations and unresolved conflicts, printing at most
`--max-inspect` of each. `content` conflicts identify paired Maildir
occurrences whose bytes differ
from the saved digest. No FLAGS operation is sent for such a mismatch. After
restoring the original bytes, a complete sync scan verifies and clears the
hold, including when the flags already agree. A live sync reports the pair ID
and exits with conflict status 4 when it first finds this mismatch.

To check paired local bodies even when no flags have changed, run the offline
scrub. It hashes pairs in pages under the Maildir writer lease, records or
clears durable `content` conflicts, and does not connect to IMAP:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- verify-local \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --max-inspect 100
```

The summary counts checked bodies, mismatches, restored conflicts, absent
local occurrences and legacy pairs without saved content evidence. The
command exits 4 if any mismatch, absence or unverified pair remains; it does
not treat a missing local file as a byte mismatch or delete either endpoint.
A changed body that reappears under a paired Maildir ID opens a durable
`content` conflict. Its later absence keeps that conflict open and blocks
deletion, including in offline plans; restore the original paired bytes and
run a complete sync or `verify-local` to clear it.

When present, the cursor header includes the saved OBJECTID+ account and
mailbox IDs for rename/replacement diagnosis. Active
rows include the source UIDVALIDITY/UID,
Maildir occurrence ID, any attested destination UID, and immutable body
digest/length to help identify an uncertain mutation. For paired operations
it also shows the saved and current pair revisions, tombstone evidence, target
flags, and any saved local flag preimage. A missing preimage appears as `?`,
distinct from a known empty list. It does not connect to IMAP or modify
journal rows.

To inspect one operation, including a committed or rejected row, add
`--operation-id ID` to `inspect`. This uses a direct journal lookup and does
not page the active operations or conflicts. It prints the same operation
identity and context as the list view. Exit 3 means the operation is active
(`prepared`, `sent`, `ambiguous`, or `observed`); exit 0 means it is terminal
(`committed` or `rejected`); exit 9 means the ID does not exist in the
requested mailbox scope. IDs in another scope are reported as absent. The
targeted view prints the current cursor first, as the list view does.
SQLite read-only WAL access may create `-wal` or `-shm` sidecars if they are
absent; it does not migrate or create the main database. Local commands first
resolve the stored scope in rev1 and, if the store reports a different
encoding, retry UTF-8. `--encoding rev1|utf8` pins the expected encoding and
makes a mismatch an error.

When an APPEND lost its tagged receipt, an operator may recover the exact
APPENDUID from a trusted server log or protocol trace.
`inspect --operation-id ID` shows the saved reason for a pending APPEND.
A tagged success without APPENDUID stays ambiguous; a source blob rejected
before any APPEND intent was prepared is recorded as an unsent, rejected
operation and can be attempted afresh after repairing the local storage.

Before reviewing the log, a bounded read-only candidate scan can narrow the
UID range. It checks the saved pre-send frontier, current UIDVALIDITY, flags,
exact body length and digest. It refuses a range larger than `--max-inspect`
(default 1000, maximum 10000) or an aggregate body read larger than
`--max-candidate-bytes` (default 1 GiB) instead of truncating it:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  inspect-append-candidates \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite \
  --spool-dir /var/lib/imap-sync/inbox.sqlite.spool \
  --operation-id op-123 --max-inspect 1000
```

The scan does not change the journal. Multiple clients can append identical
messages, so even one matching UID does not prove which APPEND produced it.
When the scope has a saved OBJECTID+ binding, inspection first verifies that
the configured mailbox name still resolves to that account/mailbox identity.
Use independent APPENDUID evidence before attesting a UID.

Record that attribution explicitly with `repair-appenduid`:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- repair-appenduid \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --operation-id op-123 --uidvalidity 42 --uid 9001 \
  --evidence 'server audit record 2026-09-26T12:00:00Z'
```

This is an operator attestation of UID attribution. It records a receipt
under the writer lease but does not commit a pair. The next `sync` checks the
complete UID inventory, exact body bytes, length and flags before committing.
Matching message bytes alone are insufficient evidence to invoke the repair.

If `inspect --operation-id` shows a `local_delete` in `sent` or `ambiguous`
and its Maildir file still exists, an operator can explicitly finish that
unlink after checking the account and message. The command connects to the
server, so use the same scope, endpoint, authentication, and verified TLS
settings as `sync`:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  repair-local-delete \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --operation-id op-456 --evidence 'operator reviewed server audit and local ID'
```

The command holds the Maildir writer lease, verifies that the journal is still
pending for the saved pair revision and UIDVALIDITY, that the current complete
remote inventory and a read-only live `UID FETCH` both show the UID absent,
and that the local occurrence still has the saved body digest, length, and
flags. It then unlinks that exact file and atomically commits the local
tombstone and journal entry. Any failed pre-unlink check leaves the file and
journal untouched. A crash after unlink leaves the existing pending journal
for complete-inventory recovery. The command never sends a remote delete
and is never run automatically.

If `inspect --operation-id` shows a `local_append` in `sent` or
`ambiguous` but its reserved Maildir ID is absent, an operator can finish
the copy after checking the source UID and account:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  repair-local-append \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite \
  --blob-dir /var/lib/imap-sync/inbox.blobs \
  --spool-dir /var/lib/imap-sync/inbox.spool \
  --maildir /home/alice/Maildir \
  --operation-id op-789 --evidence 'operator verified source UID and account'
```

The repair requires the saved UID in the complete published inventory and
the same UIDVALIDITY. It fetches the live source flags, INTERNALDATE and
body, and checks the body against the journaled length and SHA-256 digest.
When the journal has a saved source date, repair also requires the live
server date to represent the same instant.
Under the Maildir writer lease it publishes the reserved ID with the server
date, then commits the pair and operation. It refuses an existing reserved
file, changed flags, changed bytes, missing UID, or changed epoch. If the
process exits after publishing the file, the next regular `sync` reconciles
that file; do not invoke repair again. This action is never automatic.

The OCaml `Imap_cli.config` record is private: use `Imap_cli.parse` to construct
validated configurations; its fields remain readable. Online commands share a
scoped connection helper that closes the client on callback exit, including
cancellation. Credentials are resolved only when an online command runs.
