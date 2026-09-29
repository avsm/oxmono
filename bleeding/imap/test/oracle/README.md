# Live Cyrus oracle

This directory holds opt-in end-to-end tests of `imap.eio` against the shared
Cyrus IMAP/JMAP test server. Ordinary `dune runtest` skips them when
`IMAP_ORACLE_HOST` is unset. Set `IMAP_ORACLE_REQUIRED=1` in CI to make a
missing fixture a failure. Once configured, connection and assertion failures
always fail the suite.

Start an isolated fixture without disturbing an existing `jmap-oracle`:

```sh
name="imap-oracle-$$"
export JMAP_ORACLE_HTTP_PORT=0 JMAP_ORACLE_LMTP_PORT=0
export JMAP_ORACLE_MGMT_PORT=0 JMAP_ORACLE_IMAP_PORT=0
export JMAP_ORACLE_CHECK_IMAP=1
eval "$(bleeding/jmap/scripts/oracle-up.sh "$name")"
# Run: dune build --force @bleeding/imap/test/oracle/runtest
bleeding/jmap/scripts/oracle-down.sh "$name"
```

The startup script binds HTTP, LMTP, management and IMAP to `127.0.0.1` only.
A zero host port lets Docker choose a free port; `oracle-up.sh` and
`oracle-env.sh` report the actual mappings. The scripts label containers they
create. Reusing or removing an unrelated container with the same name is
refused. If `jmap-oracle` already exists without the label, choose a unique
fixture name as above.

| Variable | Default | Purpose |
| --- | --- | --- |
| `IMAP_ORACLE_HOST` | unset | Explicitly enables live tests |
| `IMAP_ORACLE_PORT` | `18143` | Published IMAP listener |
| `IMAP_ORACLE_TLS` | `plain-test` | Test-only plaintext mode |
| `IMAP_ORACLE_USER` | `user1` | Synthetic account |
| `IMAP_ORACLE_PASSWORD` | `x` | Synthetic password |
| `IMAP_ORACLE_REQUIRED` | unset | Fail if live tests cannot run |
| `JMAP_ORACLE_URL` | unset | Required by `test_cross_protocol` |
| `JMAP_ORACLE_LMTP` | `127.0.0.1:18024` | Exported for separate JMAP suites; unused by this IMAP suite |

The corpus uses fixed RFC 5322 bytes and a unique Unicode mailbox name per run.
The suite also checks NAMESPACE and extended LIST where Cyrus advertises them,
SQLite-staged publication, retained blob references, and exact-body evidence
for an uncertain APPEND journal entry. It also rejects accidental bootstrap
between two populated endpoints, imports four remote messages into Maildir,
uploads one local occurrence, checks durable pair identities and verifies a
stable second pass. It merges independent remote and local flag additions,
checks the default `\\Deleted` hold, and exercises recovery of a locally published copy whose
journal was left in `Sent`, and of a remote APPEND with a persisted UIDPLUS
receipt before the pair commit. It also proves that a sent APPEND with no
attributable receipt stays pending and is not replayed, even when matching
bytes appear on the server. APPEND and FETCH are compared byte for byte.
Separate local removal and targeted UID EXPUNGE checks prove that a complete
scan records tombstones without recreating the missing occurrence.
The second live test requires RFC 8474 OBJECTID, verifies a selected
MAILBOXID and fetches typed EMAILID/THREADID metadata for an appended UID.
`test_cross_protocol` uses the existing JMAP harness (copied only into the test
build) to import a message and read its exact bytes through IMAP, then APPEND a
second message and download its exact bytes through JMAP. It checks unseen
preservation by BODY.PEEK, JMAP keyword changes observed through IMAP SEARCH,
and IMAP STORE changes observed through JMAP Email/get, in an isolated mailbox.
Required mode rejects missing JMAP or IMAP endpoints. Offline mapping cases cover overlength keywords, Recent, unknown system flags
and colliding names. The live case distinguishes custom `seen` from system
Seen, requires Deleted messages to be invisible through JMAP get/query, then
checks keyword-preserving restoration after removal of Deleted. Mailbox counts
and the broader MIME/duplicate corpus remain open. Neither IMAP
oracle executable currently uses LMTP. The server's
fake authentication accepts arbitrary
passwords, so it does not test authentication rejection or TLS verification.
