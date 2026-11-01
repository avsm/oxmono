# IMAP acceptance requirements

These are release requirements, not a report that the current implementation
passes them. The [production gates](PRODUCTION-GATES.md) track remaining
coverage. Public contracts live in the [README](README.md) and `.mli` files.
The original design and dated implementation checkpoints remain in Git history.

## Offline protocol and Eio tests

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

## Mirror and sync correctness tests

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

## Live integration suites

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
- UIDBATCHES/UIDONLY/PARTIAL in separately gated suites when enabled.
- Large-message transfer into a hashing file sink/source with bounded memory.

Use Cyrus as the mandatory shipping-server oracle. Run the client against the
pinned Stalwart v0.15.5 fixture for independent baseline interoperability and
the separate v0.16.23 fixture for live OBJECTID+ coverage. Rust
source inspection is not evidence that the OCaml client interoperates with
Stalwart. RFC requirements take precedence over an isolated server quirk.

## Synthetic interoperability corpus

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

## Resource bounds

Measure 1, 10 and 100 MiB APPEND/FETCH bodies through plain, TLS, STARTTLS and
compressed transports, including incompressible input and file/hash sinks.
A 100 MiB body must not require proportionally sized live managed buffering.
The target is less than 8 MiB additional live payload buffering with default
settings, excluding deliberately retained metadata and TLS runtime overhead.
Record baseline, peak managed memory, native/RSS memory and throughput
separately. Sampled RSS and reserved heap are not peak live-payload measurements.

Run the 100,001-occurrence regression and an opt-in million-occurrence release
case. Measure inventory, publication, journal paging, blob collection and
restart behavior. Preserve bounded transactions and streaming bodies.

## Scope

SCRAM and SASL security layers remain deferred. CRAM-MD5 does not encrypt
subsequent traffic. Durable MULTIAPPEND batching requires its own journal and
recovery design. The low-level API does not provide that workflow.

A complete IMAP-to-JMAP or JMAP-to-IMAP server proxy is a separate product.
The client, Maildir synchronizer and cross-protocol tests do not constitute a
proxy frontend. Such a frontend must specify stable identity, representable
flags, streaming, mutation receipts and ambiguous-outcome recovery.
