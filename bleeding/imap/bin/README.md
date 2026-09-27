# imap-sync

`imap-sync` keeps one IMAP mailbox and one Maildir in step through a SQLite
journal. Each invocation does bounded work and exits, so a scheduler can
invoke it again with its own backoff. `imap-sync --help` lists the commands,
and `imap-sync COMMAND --help` lists the options that command accepts.

## Command line

Every command is `imap-sync COMMAND [OPTION]...`. Options are long options,
written `--name value` or `--name=value`. A command rejects an option it does
not use, an unknown option and a repeated option. Each option below falls
back to the environment variable named beside it.

| Group | Options | Commands |
|---|---|---|
| Mailbox scope | `--endpoint ID` (`IMAP_ENDPOINT`), `--account ID` (`IMAP_ACCOUNT`), `--mailbox NAME` (`IMAP_MAILBOX`), `--mailbox-key ID` (`IMAP_MAILBOX_KEY`, default the mailbox name), `--db PATH` (`IMAP_DB`) | all but `gc`, which takes only `--db` |
| Stored encoding | `--encoding rev1\|utf8` | the offline commands `audit-cache`, `inspect`, `repair-appenduid`, `mark-local-retention`, `plan-deletions`, `plan-sync`, `verify-local`, `forget-epochs` |
| Connection | `--host HOST` (`IMAP_HOST`), `--port N` (`IMAP_PORT`, default 993 for implicit TLS and 143 otherwise), `--tls implicit\|starttls\|plain` (`IMAP_TLS`, default `implicit`), `--user USER` (`IMAP_USER`), `--password-env NAME` (`IMAP_PASSWORD_ENV`, default `IMAP_PASSWORD`), `--auth auto\|cram-md5\|plain\|login` (`IMAP_AUTH`, default `auto`) | the online commands `sync`, `hydrate`, `inspect-append-candidates`, `repair-local-delete`, `repair-local-append`, `settle-flags`, `reject-remote-delete`, `finish-remote-delete` |
| Paths | `--blob-dir PATH` (`IMAP_BLOB_DIR`, default `DB.blobs`), `--spool-dir PATH` (`IMAP_SPOOL_DIR`, default `DB.spool`), `--maildir PATH` (`IMAP_MAILDIR`) | each command that reads or writes that directory |

The per-command options are these, with their defaults.

| Command | Options |
|---|---|
| `sync` | `--max-transfers N` (100), `--max-cycles N` (1), `--min-absence-scans N` (0), `--deletion-policy POLICY` (`preserve`), `--allow-bootstrap-duplicates`, `--propagate-deleted-flag`, `--hydrate-bodies`, `--max-body-bytes N` and `--max-total-bytes N` (1 GiB, with `--hydrate-bodies` only) |
| `hydrate` | `--max-transfers N` (100), `--max-body-bytes N` (1 GiB), `--max-total-bytes N` (1 GiB) |
| `audit-cache` | `--max-transfers N` (100), `--max-total-bytes N` (1 GiB), `--after-uid N` with `--expected-revision N` |
| `inspect` | `--max-inspect N` (100), `--operation-id ID` |
| `inspect-append-candidates` | `--operation-id ID`, `--max-inspect N` (100), `--max-candidate-bytes N` (1 GiB) |
| `repair-appenduid` | `--operation-id ID`, `--uidvalidity N`, `--uid N`, `--evidence TEXT` |
| `repair-local-delete`, `repair-local-append`, `settle-flags`, `reject-remote-delete`, `finish-remote-delete` | `--operation-id ID`, `--evidence TEXT` |
| `mark-local-retention` | `--pair-id ID`, `--evidence TEXT` |
| `plan-deletions` | `--max-inspect N` (100), `--min-absence-scans N` (0), `--deletion-policy POLICY` (`preserve`), `--propagate-deleted-flag` |
| `plan-sync` | the `plan-deletions` options and `--allow-bootstrap-duplicates` |
| `verify-local` | `--max-inspect N` (100) |
| `gc` | `--db PATH`, `--blob-dir PATH`, `--maildir PATH` (optional) |
| `forget-epochs` | none beyond the mailbox scope |

`--max-transfers` and `--max-inspect` accept 1 to 10000, `--max-cycles` 1
to 100000, `--min-absence-scans` 0 to 100000, and a byte count 1 to 2^40.
`--deletion-policy` is one of `preserve`, `propagate`, `propagate-remote`
and `propagate-local`. Evidence is 1 to 1024 printable bytes that are not
all spaces. `--max-inspect` caps what is printed, and the command reports
`capped=true` only when more items exist than it shows.

An offline command opens the stored scope with `rev1` mailbox name
encoding and, when the stored scope differs in any way, retries once in
UTF-8. A difference in UTF-8 as well exits 5. `--encoding` pins the
encoding, and a stored scope that differs from it exits 5.

The password is never an option. A command that connects reads it from the
variable `--password-env` names when it runs, and an offline command never
reads it. No output includes the password. An error printed after the
password was read has it replaced by `[REDACTED]`.

## Exit status

| Status | Meaning |
|---|---|
| 0 | The command converged, or a targeted operation is terminal. |
| 2 | Bounded work remains. Run the command again. |
| 3 | Pending journal work needs operator inspection. |
| 4 | A conflict, a held flag or deletion decision, or an unsafe state such as a changed pair, a changed UIDVALIDITY or an unpaired bootstrap. |
| 5 | Invalid configuration, including a command line error, a missing database or directory, a missing password and a stored scope that differs from the requested one. |
| 6 | An IMAP connection, authentication or protocol failure. |
| 7 | A local filesystem, Maildir or SQLite failure. |
| 8 | The Maildir writer lease, the Maildir metadata lock or the database lock is busy. |
| 9 | The targeted operation or pair is not in the requested mailbox scope. |

A nonzero exit never authorizes replaying a possibly sent APPEND or
deletion. A command that takes an operation or pair ID exits 9 for an ID
outside the scope before it attempts the repair.

## Syncing

`imap-sync sync` runs bounded IMAP and Maildir cycles. Set the password in
the environment, never on the command line:

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

`--auth auto` negotiates a mechanism the server advertises, and
`--auth cram-md5` requests CRAM-MD5. `--tls starttls` requires STARTTLS.
`--tls plain` sends everything in clear text and permits every mechanism
over it, including PLAIN and LOGIN. Use it only for a trusted local
fixture. CRAM-MD5 protects the password exchange but not the mail traffic
that follows.

The parent directory of the database and of the Maildir root must exist.
`sync` creates the blob and spool directories with owner-only permissions.
Keep `--endpoint`, `--account`, `--mailbox-key`, the database path and the
Maildir path stable across runs. At startup `sync` removes the temporary
files an interrupted run left in the Maildir and the spool, and removes
orphan blobs as `gc` does. It holds the database lock for its whole run.

Each cycle publishes a complete remote inventory, stages a complete Maildir
inventory, settles the operations earlier runs left pending, and then copies
unpaired messages both ways, reconciles the flags of each pair and applies
the deletion policy. The bridge never pairs messages because their bytes
match. A new database with messages on both sides stops before copying,
with status 4, unless `--allow-bootstrap-duplicates` is set. Inspect both
mailboxes first.

A cycle prints its counts, and the pair IDs of up to 100 held pairs on
standard error. A held flag or deletion decision does not stop the cycles.
When the cycles end with no work remaining, a held decision or an open
conflict exits 4, so a quiet no-op is not mistaken for convergence.
Remaining work exits 2 even while a decision is held. A remote UID that
vanishes before its body is archived, or a Maildir occurrence that changes
before its bytes are archived, leaves no journal entry and is rescanned in
the next cycle. When `--max-cycles` is used up, the command exits 2.

### Deletions

`--deletion-policy preserve`, the default, keeps a message that disappears
from one side. A one-sided disappearance then opens a durable
`deletion_hold` conflict and counts as a hold. Repeated scans keep the same
conflict ID, and restoring the message or enabling propagation clears it
after a complete scan. `propagate` deletes the survivor on either side.
`propagate-remote` deletes a local survivor after a verified remote
disappearance, and `propagate-local` deletes a remote survivor after a
verified local disappearance. Enable propagation only for a mailbox where
that policy is intended.

`--min-absence-scans N` waits for N further complete scan generations after
an absence is first recorded before it deletes the survivor. With
`--min-absence-scans 1`, the first complete scan that observes the absence
holds the deletion, and the next complete scan permits it. With the
default of 0, propagation acts on the first verified absence. Pass the same
option to `plan-deletions` or `plan-sync` to preview the hold. An absence
tombstone without a recorded generation stays held while N is positive,
since a new scan does not replace that evidence. Inspect or repair such a
pair explicitly.

When a complete scan sees the absent side present again, the syncer
records that observation, and a later disappearance starts a fresh grace
period. An exact local restoration, with the paired bytes and
INTERNALDATE, retires the local absence tombstone so flag sync resumes.
Altered bytes or an altered date keep a durable conflict that blocks
deletion until the original occurrence is restored.

Mark a local cache eviction before enabling propagation from local to
remote:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- mark-local-retention \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --pair-id pair-123 --evidence 'local cache expired'
```

`mark-local-retention` requires the paired local occurrence to be absent, a
live remote binding and no pending operation for the pair. It records a
durable retention tombstone under the Maildir writer lease and makes no
IMAP connection. Later cycles hold remote deletion for that pair even under
`--deletion-policy propagate`. A remote deletion that already completed
cannot be reversed by this marker.

### Flags

Paired flags reconcile three ways against the last common flags. A remote
write uses a conditional STORE and so needs CONDSTORE and a message MODSEQ.
A pair whose write the server cannot make is held. A changed `\Deleted` is
held while the other flags merge, and a durable `policy` conflict keeps its
pair ID and evidence across restarts. Removing the held change or
tombstoning the pair resolves it after the next complete scan.
`--propagate-deleted-flag` on `sync`, `plan-sync` and `plan-deletions`
merges a changed `\Deleted` like any other flag instead. It sets the flag
only and never expunges.

A sent FLAGS operation whose target cannot be verified stays pending, which
exits 3, with a durable `flags` conflict describing the mismatch. Its ID
stays stable across attempts. A later verified commit resolves the conflict
in the same SQLite transaction, and recovery never replays an uncertain
STORE.

A paired Maildir body that differs from its saved digest opens a durable
`content` conflict, and no FLAGS operation is sent for it. The cycle holds
the pair and exits 4. After the original bytes are restored, a complete
cycle verifies them and clears the conflict.

## Bodies

`imap-sync hydrate` fills missing message bodies from the published SQLite
inventory. It needs no Maildir and changes no flags. It verifies the saved
mailbox identity and UIDVALIDITY, preflights RFC822.SIZE, and attaches each
exact body only after a completed FETCH and a synced blob write. A failure
leaves the bodies already attached in place. `hydrate` holds the database
lock described under blob reclamation.

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

`--max-transfers`, `--max-body-bytes` and `--max-total-bytes` bound each
invocation, which exits 2 when bodies remain. A body larger than either
byte budget is skipped and counted in `skipped=N`, and the first 100 such
UIDs go to standard error. A skipped body does not make the status 2, and a
pass that only skipped bodies continues after them, so they never stall
later UIDs. Raise the budgets to hydrate them.

`sync --hydrate-bodies` runs the same bounded hydration after a cycle that
ends without more work, held decisions or open conflicts. `--max-transfers`
applies to the cycles and to hydration separately, and `--max-body-bytes`
and `--max-total-bytes` bound the hydration. When bodies remain, `sync`
exits 2.

`imap-sync audit-cache` rehashes stored bodies without connecting to IMAP:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- audit-cache \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite \
  --blob-dir /var/lib/imap-sync/inbox.blobs \
  --max-transfers 100 --max-total-bytes 1073741824
```

It rehashes a bounded page of the blobs the published inventory references,
and detaches a reference whose file is missing or corrupt. It changes no
mailbox membership, Maildir occurrence or blob file. It prints
`cache_checked`, `invalidated`, `bytes`, `last_uid`, `more` and `revision`,
and exits 2 when another page remains. Continue with both
`--after-uid LAST_UID` and `--expected-revision REVISION`. A changed
published revision is refused, so a continuation cannot skip a new
snapshot. When `last_uid=0` and `more=true`, raise the byte budget to fit
the next blob. Run `hydrate` or `sync --hydrate-bodies` afterwards to refill
detached bodies. A regular cycle does not rehash cached bodies.

## Offline plans and checks

These commands read the database and the Maildir and make no IMAP
connection.

`plan-deletions` lists the one-sided pairs and the decision a cycle would
take under a policy:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- plan-deletions \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --deletion-policy propagate-local --max-inspect 100
```

It reads the latest complete published remote inventory, stages a fresh
Maildir inventory under the writer lease and pages every pair. It prints up
to `--max-inspect` one-sided pairs and the complete candidate, hold and
pending counts. A candidate means the next cycle would attempt the
deletion, after it refreshes the remote inventory and verifies the
survivor's identity, content and flags and the server's capabilities.
Without a complete published remote inventory the planner exits 5 rather
than infer absence. Run a preserving `sync` to publish one first.

`plan-sync` extends the same view to copies, three-way flag changes and the
`\Deleted` hold:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- plan-sync \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --deletion-policy propagate-remote --max-inspect 100
```

It prints bounded event detail and complete counts of copies, flag changes,
deletions, holds and pending work. A saved `content` conflict appears as a
pair hold and suppresses the flag change of its pair. Messages on both
sides without any pair yield only a bootstrap hold unless
`--allow-bootstrap-duplicates` is given, as for `sync`. A pending
operation stops both plans.

`verify-local` rehashes paired Maildir bodies even when no flags changed:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- verify-local \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --max-inspect 100
```

It pages the pairs under the Maildir writer lease, and opens or clears
durable `content` conflicts. Its summary counts checked bodies, mismatches,
restored conflicts, absent local occurrences and pairs without saved
content evidence. It exits 4 if a mismatch, an absence or an unverified
pair remains. It never treats a missing file as a byte mismatch and never
deletes either side. A changed body that reappears under a paired Maildir
ID opens a `content` conflict, which its later absence keeps open, so it
blocks deletion in the plans too. Restore the paired bytes and run a
complete `sync` or `verify-local` to clear it.

`inspect` prints the journal through a read-only SQLite connection:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- inspect \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --max-inspect 100
```

It prints the cursor, with the saved OBJECTID+ account and mailbox IDs
when a binding exists, then pages the active operations and open conflicts
and prints at most `--max-inspect` of each. An operation line shows the
source UIDVALIDITY and UID, the Maildir occurrence ID, any attested
destination UID and the body digest and length. For a paired operation it
also shows the saved and current pair revisions, the tombstones, the target
flags and any saved local flag preimage. A missing preimage prints as `?`,
distinct from a known empty list. The status is 4 with open conflicts, 3
with active operations and 0 otherwise.

`--operation-id ID` prints one operation, including a committed or rejected
one, after the cursor. It exits 3 for an active operation (`prepared`,
`sent`, `ambiguous` or `observed`), 0 for a terminal one (`committed` or
`rejected`) and 9 for an ID outside the scope. `inspect` never creates or
changes the database, although SQLite may create `-wal` and `-shm`
sidecars that are absent.

## Operator repairs

A repair settles one operation that a cycle holds. It never runs
automatically, takes the Maildir writer lease, and saves the operator's
`--evidence` in the journal. An online repair uses the same scope,
endpoint, credentials and TLS settings as `sync`, and first checks a saved
OBJECTID+ binding against the configured mailbox name.

### Flags

`settle-flags` adopts flags an operator aligned on both endpoints as the
new common flags:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  settle-flags \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --operation-id op-456 --evidence 'operator aligned both endpoints'
```

It sends no STORE and changes no Maildir flags. It requires a sent,
ambiguous or observed FLAGS operation, the saved pair revision and
UIDVALIDITY, the paired local body and date, equal flags on both endpoints
and a stable remote MODSEQ across two reads. It then rejects the superseded
operation, advances the common flags and resolves the flag conflict in one
transaction. A failed check leaves the operation pending.

### Remote deletions

`reject-remote-delete` rejects a pending remote deletion whose target is
unchanged:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  reject-remote-delete \
  --host mail.example.org --tls implicit --user alice \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --spool-dir /var/lib/imap-sync/inbox.spool \
  --operation-id op-789 --evidence 'target still has original bytes and flags'
```

It requires a sent or ambiguous paired deletion, the saved pair revision,
the UID in the complete published inventory, a recorded local absence that
still holds, and CONDSTORE. It reads the remote body into a spool capped at
the paired length, compares its SHA-256, length and flags, and requires a
stable MODSEQ across two reads. The spool directory must exist. It changes
only the journal state to `rejected` and sends no STORE or EXPUNGE. A
target already expunged or modified stays pending for a later inventory or
further investigation. A later `sync` with propagation enabled may prepare
a new deletion.

`finish-remote-delete` completes a pending remote deletion whose UID has
exactly its original flags plus `\Deleted`:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  finish-remote-delete \
  --host mail.example.org --tls implicit --user alice \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --spool-dir /var/lib/imap-sync/inbox.spool \
  --operation-id op-789 --evidence 'reviewed exact marked UID for expunge'
```

It checks what `reject-remote-delete` checks and requires UIDPLUS. It saves
the evidence and marks the operation ambiguous before it sends a `UID
EXPUNGE` of that UID alone, and it verifies the UID absent before it
commits. When the result is lost, the operation stays pending, and a later
complete scan may commit the absence without another EXPUNGE. It never
sends a mailbox-wide EXPUNGE. A remote edit between the final FETCH and the
EXPUNGE cannot be excluded, so review the target immediately before.

### Uploads

An APPEND that lost its tagged receipt stays ambiguous. `inspect
--operation-id ID` shows its saved reason. A source blob rejected before
any APPEND intent was saved is recorded as a rejected operation and is
attempted afresh after the local storage is repaired.

`inspect-append-candidates` narrows the UID range before an operator
reviews a server log:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  inspect-append-candidates \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite \
  --spool-dir /var/lib/imap-sync/inbox.spool \
  --operation-id op-123 --max-inspect 1000
```

It compares the UIDs above the saved pre-send frontier with the saved
UIDVALIDITY, flags, exact length and digest, and changes nothing. It
refuses a range wider than `--max-inspect` or a body total larger than
`--max-candidate-bytes` rather than truncate it. The spool directory must
exist. Several clients can append identical messages, so even one matching
UID does not prove which APPEND produced it.

`repair-appenduid` attests an APPENDUID recovered from a trusted server log
or protocol trace:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- repair-appenduid \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --operation-id op-123 --uidvalidity 42 --uid 9001 \
  --evidence 'server audit record 2026-09-26T12:00:00Z'
```

It records the receipt without connecting to IMAP and does not commit a
pair. The next `sync` checks the UID in a complete inventory and its exact
bytes, length and flags before it commits. Matching bytes alone are not
evidence for this repair.

### Local copies

`repair-local-delete` finishes a pending local deletion whose Maildir file
still exists:

```sh
IMAP_PASSWORD='...' opam exec -- dune exec bleeding/imap/bin/main.exe -- \
  repair-local-delete \
  --host mail.example.org --tls implicit --user alice --auth cram-md5 \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite --maildir /home/alice/Maildir \
  --operation-id op-456 --evidence 'operator reviewed server audit and local ID'
```

It requires a sent or ambiguous `local_delete` at the saved pair revision
and UIDVALIDITY, the UID absent from the complete published inventory and
from a live read-only `UID FETCH`, and the local occurrence with the saved
digest, length and flags. It then unlinks that exact file and commits the
local tombstone and the operation together. A failed check leaves the file
and the journal untouched. A crash after the unlink leaves the operation
pending for the next complete inventory. It never sends a remote deletion.

`repair-local-append` finishes a pending remote-to-Maildir copy whose
reserved Maildir ID is absent:

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

It requires the saved UID in the complete published inventory of the same
UIDVALIDITY. It fetches the live flags, INTERNALDATE and body, checks the
body against the saved length and digest and, when a source date was
saved, the date against it. It publishes the reserved ID with the server
date and commits the pair and the operation together. It refuses an
existing reserved file, changed flags, changed bytes, a missing UID and a
changed epoch. The blob and spool directories must exist. If the process
exits after publishing the file, the next `sync` reconciles it, so do not
run the repair again.

## Blob reclamation

A blob stays on disk until nothing references it. A superseded UIDVALIDITY
epoch keeps its snapshot rows, and so its blobs, until it is dropped:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- forget-epochs \
  --endpoint personal-dovecot --account alice --mailbox INBOX \
  --db /var/lib/imap-sync/inbox.sqlite
```

`forget-epochs` deletes the snapshot rows and blob references of every
epoch of the scope other than the current cursor's, and prints
`epochs_dropped=N`. It exits 4 when no epoch is published yet or the cursor
changes while it runs. It removes no file.

`gc` removes the blob files that no snapshot or pending operation
references, and the temporary files of an interrupted write, then prints
`orphans_removed=N`:

```sh
opam exec -- dune exec bleeding/imap/bin/main.exe -- gc \
  --db /var/lib/imap-sync/inbox.sqlite \
  --blob-dir /var/lib/imap-sync/inbox.blobs \
  --maildir /home/alice/Maildir
```

A blob written but not yet referenced is indistinguishable from an orphan,
so every writer of the blob directory must be stopped while `gc` runs,
including one in another process. `gc` holds a lock file at `DB.lock` and,
with `--maildir`, the Maildir writer lease. `sync` and `hydrate` hold
`DB.lock` for their whole run, so a concurrent one exits 8 rather than race
the collector. A program that writes blobs through the library without
these locks must not run beside `gc`. The blob directory must belong to
this database alone, since `gc` consults only its references.

## OCaml interface

`Imap_cli.cmd` is the command tree, and each command parses into an
`Imap_cli.job` whose private records hold only validated options.
`Imap_cli.eval` parses an argument vector against an environment lookup and
runs the job, and `main.exe` and the tests call it. An online command closes
its client on every exit, including cancellation.
