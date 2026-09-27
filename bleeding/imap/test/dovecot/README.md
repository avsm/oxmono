# Dovecot CRAM-MD5 oracle

Structured rejection tests verify AUTHENTICATIONFAILED, ALREADYEXISTS,
NONEXISTENT and TRYCREATE codes through the public Eio error interface, and
confirm that mailbox commands remain usable after ordinary rejections.

This fixture uses the [official Dovecot CE image](https://doc.dovecot.org/2.4.0/installation/docker.html), pinned to the locally verified 2.4.5 image digest. It enables `AUTH=CRAM-MD5` using a test-only `{PLAIN}` passdb secret. The container publishes cleartext IMAP and implicit TLS on loopback ports. `up.sh` generates a disposable CA and an IP-address SAN certificate for `127.0.0.1`, plus an unrelated CA for negative tests. It mounts only the server certificate and key into Dovecot, exports the CA paths for the OCaml test, and `down.sh` removes the certificates after the container stops. The plaintext tests retain their explicit transport selection; a separate test exercises CRAM-MD5 and LOGIN over implicit TLS and required STARTTLS with chain and hostname validation, verifies rejection of an untrusted CA and a wrong hostname, and refuses LOGIN on plaintext. Production transport retains normal certificate validation.

The live suite checks successful and rejected CRAM-MD5 authentication, CONDSTORE metadata, `UNCHANGEDSINCE` conflict and retry, MOVE/COPYUID, UID EXPUNGE, QRESYNC `VANISHED`, an IDLE wakeup from a second client connection, and two durable publications through the IDLE watch supervisor. It also imports and uploads exact message occurrences through the durable bridge under CRAM-MD5, then merges independent local and remote flags with conditional UID STORE. A subprocess crash test sends a real APPEND, saves its APPENDUID only as external witness evidence, and exits without saving a database receipt. The restarted bridge holds the operation without replay; operator attestation then verifies the remote occurrence and commits the pair. Each mutation test creates and removes its own synthetic mailboxes.
An additional MIME fixture appends a multipart message and verifies the typed
ENVELOPE and BODYSTRUCTURE projections against Dovecot.
The BINARY fixture fetches quoted-printable text and base64-encoded octets
containing NUL and high-bit bytes. It checks decoded partial offsets and size,
an empty read beyond the section's end, preservation of the unseen flag, and
an unchanged raw BODY fetch of the original transfer-encoded message.
The binary APPEND fixture uploads an octet-stream MIME message containing NUL
and high-bit bytes, then checks its APPENDUID, decoded payload, flags and
INTERNALDATE. It permits the server to transform the stored transfer encoding.
The pool fixture uses two concurrent CRAM-MD5 connections for discovery and
checks that closing a borrowed client does not prevent later reuse.
The hydration fixture publishes two synthetic messages, confirms a too-small
aggregate budget fetches neither body, then attaches one exact SHA-256 blob
per bounded pass. It checks a final no-op pass and provisional spool cleanup.
The first pass goes through the `imap-sync hydrate` command over CRAM-MD5 and
checks its exit-2 continuation signal.
After all blobs are present, a preserving `sync --hydrate-bodies` cycle
imports both Maildir occurrences. The fixture then evicts their SQLite cache
references and repeats sync: with no new Maildir transfers, the hydration
hook reattaches both exact bodies and leaves the two occurrences unchanged.
It also overwrites one cache file without changing its SQLite reference,
audits bounded UID pages, checks a stale revision is refused, and confirms
both the audit API and offline CLI invalidate the corrupt reference before
CRAM-MD5 hydration restores the original digest.

The FLAGS recovery fixture reopens a SQLite journal after a remote-only STORE, verifies the saved pair revision and local body identity, completes the local flag rename, and commits the pair without sending another STORE. It then changes local flags independently and verifies that recovery holds the divergent operation without overwriting that edit.
It also invokes `imap-sync settle-flags` over CRAM-MD5 after manually aligning
the endpoints, verifying that disagreement is refused and the old intent and
conflict are closed only after the flags match.

The deletion crash fixture journals `Sent`, performs conditional `UID STORE`
and targeted `UID EXPUNGE` against Dovecot, then exits before recording the
result in SQLite. A fresh complete scan commits only the target's inventory
tombstone; the unrelated UID survives and no mutation is replayed.
The deletion grace fixture removes a paired Maildir occurrence and sets one
additional complete scan as the minimum absence age. It confirms that the
first scan holds the remote survivor, a restored occurrence resets the
absence clock, and a second disappearance must wait through a fresh complete
scan before targeted deletion. It also restores altered bytes first, proving
the resulting content conflict survives another disappearance and clears only
when the exact paired bytes return. The exact restoration also retires the
local tombstone and resumes conditional flag sync for a new `\Seen` change.

The local deletion repair fixture first records a complete remote inventory
tombstone, then leaves a `Local_delete` operation in `Sent` with its Maildir
file still present. Repair refuses a foreign scope and altered local flags
without changing the journal or file. After the original flags are restored,
read-only UID absence and exact local identity checks allow a single explicit
unlink and atomic journal/tombstone commit.

```sh
eval "$(bleeding/imap/test/dovecot/up.sh)"
IMAP_DOVECOT_REQUIRED=1 opam exec --switch=5.2.0+ox -- \
  dune runtest bleeding/imap/test/dovecot
bleeding/imap/test/dovecot/down.sh
```

`up.sh` accepts a unique container name; `IMAP_DOVECOT_PORT=0` and `IMAP_DOVECOT_TLS_PORT=0` ask Docker for ephemeral ports. Both scripts refuse to reuse or remove an unrelated container. Credentials and certificates are disposable test values and must not be used elsewhere. A container reused after its two-day test certificate expires should be removed and recreated.

The SORT/THREAD fixture creates a root with two replies and an independent
message, removes an earlier occurrence so UIDs differ from sequence numbers,
and verifies ascending/reverse subject order, stable ties, REFERENCES branches,
filtered dummy parents and empty results through the typed APIs.

The same sorted mailbox checks ESORT summaries (COUNT plus sort-order MIN/MAX),
ordered ALL results, and empty results. Sorted positional pages are covered by
scripted CONTEXT=SORT transcripts; they require that separate capability.


For direct filesystem interoperability, start a fresh fixture with
`IMAP_DOVECOT_SHARED=1 ./up.sh imap-shared` and load its exported environment.
This mode bind-mounts a disposable Maildir tree and runs the container master
as root so Dovecot can use the invoking host UID/GID for mail processes.
The script discovers INBOX through `doveadm mailbox path` and exports
`IMAP_DOVECOT_SHARED_MAILDIR`; it does not assume a mailbox directory layout.
Only the generated fixture tree is shared, and `down.sh` removes it.

The shared-filesystem test publishes a message through `Imap_maildir`, verifies
Dovecot reads its exact bytes, system flags, keyword and INTERNALDATE, changes
flags and adds a keyword through IMAP, and verifies the local reader sees those
changes. A further local rename preserves both programs' keyword mappings and
is visible through IMAP. Targeted expunge removes the same local occurrence.
Without shared mode this additional case is skipped. Test environment variables
are explicit Dune dependencies so switching fixtures invalidates cached results.
Use `--force` when repeating a test against an unchanged fixture environment.


The MULTIAPPEND test sends two message streams in one atomic command and checks
that the ordered APPENDUID receipt maps back to the exact input bodies, separate
flag sets and supplied INTERNALDATE.


The authentication fixture also polls with Client.NOOP and selected-lease NOOP,
then requires the server's BYE and tagged LOGOUT completion before checking that
the transport is closed.


## Large-body memory probe

`body_memory.exe` is an opt-in live probe. It generates the body incrementally,
streams APPEND and FETCH, and verifies matching byte counts and SHA-256 digests.
Run each case in a fresh process against a disposable fixture:

```sh
bleeding/imap/test/dovecot/up.sh imap-body-memory > /tmp/imap-body-memory.env
. /tmp/imap-body-memory.env
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  bleeding/imap/test/dovecot/body_memory.exe
for transport in plain tls deflate tls-deflate; do
  for size in 1 10 100; do
    _build/default/bleeding/imap/test/dovecot/body_memory.exe "$transport" "$size"
  done
done
bleeding/imap/test/dovecot/down.sh imap-body-memory
```

The current probe accepts `IMAP_MEMORY_GC=retained|normal` (default retained)
and `IMAP_MEMORY_PAYLOAD=repeat|entropy` (default repeat). It also accepts
`starttls` and `starttls-deflate` transport modes. The entropy payload uses
seeded random printable ASCII with CRLF line wrapping; it is less compressible,
not a claim of fully incompressible binary data.

Current CSV fields are GC mode, payload, transport, message MiB, operation,
baseline live OCaml bytes, additional sampled live bytes, baseline RSS bytes,
additional sampled RSS bytes, baseline reserved major-heap bytes and additional
sampled reserved major-heap bytes. Normal mode emits -1 for additional live
bytes because it does not force collections during streaming. Both modes force
one collection at each operation's baseline. RSS is read from
`/proc/self/status` on Linux and reported as zero when unavailable.

The probe samples every MiB at source/sink callbacks and once at completion.
Retained mode samples RSS before forcing a full major collection for live-word
counts and rejects an additional sampled live heap of 8 MiB or more. Normal
mode observes RSS and reserved major heap without collecting at sample points.
Backing buffers outside the OCaml heap are reflected in RSS, not necessarily
in live managed words. Neither mode captures absolute peaks between callbacks.

At the original review checkpoint, the normal-GC, entropy and STARTTLS
extensions had built but had not been run. The existing results below apply only to the earlier
retained-GC, repeated-payload probe and use its original seven-column format.
The checked-in `body-memory-results.csv` records the 2026-09-27 run against the
pinned Dovecot 2.4.5 fixture, with OCaml 5.2.0+ox and release-check. All 12
combinations passed. Largest sampled increases were 80,408 live managed bytes
and 6,025,216 RSS bytes. The fixture was removed after the run.


Subsequent validation ran `IMAP_MEMORY_GC=normal IMAP_MEMORY_PAYLOAD=entropy`
for 1/10/100 MiB over all six transports (including STARTTLS with/without
compression). All 18 fresh-process cases passed exact digest/length checks.
`body-memory-normal-results.csv` records 36 operation measurements in the
current format. Maximum additional sampled RSS was 11,419,648 bytes and
reserved major-heap growth was 4,803,952 bytes. Neither is a live-payload
measurement; normal mode deliberately emits -1 for that unavailable quantity.
The disposable fixture was removed. Throughput, absolute peaks and truly
incompressible binary payload coverage remain open.
