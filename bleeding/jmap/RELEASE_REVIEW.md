# First-release review — 2026-09-05

The RFC 8620/8621 API is sufficiently complete for an initial release after the
corrections below. Publishing is still blocked by unversioned dependencies and
an unverified clean opam installation. Do not describe the package as a complete
implementation of every JMAP extension. RFC 9404 blob methods and a persistent
mail store can follow separately.

This review started at `576f4e2`. It used the requested `review-ocaml`,
`ocaml-docs` and documentation-style skills: interfaces and Dune ownership first,
then implementations, regressions and live server behavior. Those fixes and the
refreshed examples are committed in `723a652`. The latest code-only follow-up
is recorded in [BUGS.md](BUGS.md). The earlier audits remain useful
history; this document records the additional findings and current release gates.

## Scope and structure

Reviewed protocol and mail types/codecs, the typed Chain builder, all public
interfaces, Eio authentication/transport/client/push/sync/profiles/CLI modules,
both command-line programs, Mosaic, examples, documentation, tests and packaging.

| Component | Responsibility and dependencies |
| --- | --- |
| `jmap` | Protocol/mail codecs and `Jmap.Chain`; Jsont, Httpz, JSON Pointer, mail flags and time types |
| `jmap.eio` | Auth, Transport, Client, Push, Sync, Profile and Cli; depends on `jmap`, Eio and Fetch/Httpz |
| `jmap.top` | Toplevel printers over `jmap` |
| `jmap`, `jmapq` executables | Commands over `jmap.eio`; Zulip-specific queries live in `jmapq` |
| `jmap-mosaic` | Model, View, Io and Login; depends on `jmap.eio` and Mosaic/Matrix |

The library dependency direction is sound. Libraries have interfaces and
explicit wrapper exports; no dependency-cycle or warning-suppression issue
requires a release-time restructure. Wire records remain exposed intentionally;
smart constructors and encoding checks enforce invariants where needed. Large
Email and Client implementations merit later internal splits, without changing
the public module layout merely for size.

## Findings corrected before release

P1 means a build failure, lost progress, or misleading outcome with substantial
user impact. P2 means incorrect behavior on a supported path. Related failures
are grouped so the reason for each change remains visible.

| Priority | Finding and correction | Evidence |
| --- | --- | --- |
| P1 | **Dependency API drift prevented compilation.** Client used a removed URI-template resolver; response wrappers omitted Fetch's required close callback. Migrate to expand-then-resolve using the original template and session base; preserve public absolute display templates and forward response close/sensitivity. | `eio/client.ml`, `eio/push.ml`; client and push regressions; all library/example builds |
| P1 | **Push completion could strand a consumer.** Full bounded queues could prevent an End item from being delivered, including inside Fetch's SSE subscription. Add `Push.next`, which drains events then observes completion, preserves events in completion races, and returns End on subsequent reads. Use it in public examples. | `test/eio/test_eio.ml`: full outer/inner queues, close, fatal completion |
| P1 | **Polling did not bound connection lifetime under backpressure.** Start the deadline before headers and close the underlying response with an independent timer, including while the producer waits for queue space. This matters for Cyrus's account lock. | Delayed-header, full-queue and response-close regressions; live push suite |
| P1 | **Successful submission could be reported without checking filing.** Mosaic now checks the implicit same-call-ID `Email/set` response from `onSuccessUpdateEmail`, in combined and sequential send paths. Missing/failed filing becomes `Sent_with_warning`; a confirmed submission is not turned into an ordinary send failure. | `mosaic/test/test_io_release.ml`; combined and one-call-limit paths |
| P1 | **Later batch errors hid completed mutations.** `jmap delete` and `jmapq zulip-timeout` retain confirmed earlier outcomes, stop on the failed exchange, and distinguish the uncertain batch from unattempted IDs. | Four CLI scenarios against a temporary HTTP mock, including partial deletion and marking |
| P2 | **Refreshing a session reset concurrency accounting.** Resize admission limits in place, preserving active requests; wake waiters and re-read endpoints, credentials and limits after admission. | `test/eio/test_client_release.ml`; active/queued refresh, moved endpoint and lowered call limit |
| P2 | **Sync could silently skip data.** Reject nonempty query pages whose positions differ from the requested position, and reject a changes interval whose oldState does not match the state requested. Preserve the first parallel failure. Apply the paging check to `jmapq` too. | `test/eio/test_sync_release.ml`; CLI forward-jump scenario |
| P2 | **The watch example could abandon a partial drain.** Continue immediately when `has_more` survives a fuel-limited drain. Stop visibly on `cannotCalculateChanges`, and explain resynchronisation rather than replacing the baseline with the announced state. | Built example; corrected walkthrough and Sync contract |
| P2 | **An import token had the wrong phantom type.** Low-level `Email.Import.args` now carries an Email creation token, matching its response and Chain's token. | Mail test: reuse token across import encoding and response lookup |
| P2 | **Typed mail APIs could emit invalid values.** Validate custom header names and permitted parsed forms, including direct variant construction; reject false `mailboxIds` memberships and duplicate mailbox replacement entries. | Mail tests: invalid smart/direct header properties, membership values and duplicate patches |
| P2 | **PatchObject rejected a valid object key.** A `-` path segment is legal for an object member. Remove the unconditional array interpretation; actual target arrays remain the server's concern. | `test/method/test_method.ml`: mailbox and keyword `-` keys |
| P2 | **Authentication validation and file behavior disagreed with the contract.** Validate printable-ASCII Basic credentials across settings and lazy sources; reject nonregular files without the ordinary FIFO-open hang; use nonblocking native open, preserve opened-file checks, invalidate in-flight cached reads by generation, and escape paths in printers. | `test/eio/test_auth.ml`: control/non-ASCII input, config errors, lazy denial, FIFO rejection, redaction |
| P2 | **Caller exceptions were relabelled as transport errors.** `Client.with_get` now preserves the callback's original exception identity and backtrace. | Client regression using a caller-raised Eio I/O exception |
| P2 | **Opening an unread result raced with marking it seen.** Fetch and accept the body before issuing the seen update; removing the row from the Unread search no longer discards the opened message. After a send warning, reload the source rather than falsely marking it answered locally. | Mosaic model regressions |
| P2 | **Mosaic invented a sender when Identity had no address.** Select a usable returned identity or report why replying is unavailable. Never manufacture an `@example.com` production address. | Mosaic IO regression: unusable first identity, usable later identity |
| P2 | **Verbose output corrupted JSON output; bracketed topics were misparsed.** Send diagnostics to stderr and treat the final bracket suffix as the server name, preserving earlier brackets in a topic. | CLI `zulip-view --json --verbose` output parses; topic `topic [PR]` remains intact |
| P2 | **Test dependencies and the oracle launcher were incomplete.** Declare `bytesrw` for the optional `jsont.bytesrw` library and `mdx` for tutorial tests. Keep launcher stdout executable as exports, configure a usable upload limit, bind fixture ports to loopback, and forward `--allow-insecure` in the headless test. | Regenerated/linted opam; tutorial test; live fixture startup and upload checks |
| P2 | **Public documentation encouraged unsafe sync cursors.** A pushed state is the new state, not the baseline for `/changes`; the reconnect cursor may precede application acknowledgement. Correct Push/Sync docs, README and watch tutorial. Also correct address equality, unknown-member preservation, receivedAt and action/request-count claims. | Interface review; documentation build |

## Work still required for publication

1. **Release and constrain the dependency set.** This checkout depends on
   development Httpz, Fetch, `fetch-httpz`, `json-pointer` and `mail-flag` APIs.
   The configured opam repository supplies no published version for the complete
   boundary. Establish the first compatible releases, add real minimum versions,
   and run a clean `opam install . --with-test --with-doc` without sibling
   symlinks. Do the same for Mosaic/Matrix's required behavior if publishing
   `jmap-mosaic`. Do not invent lower bounds from development version strings.
   A direct `dune build -p jmap @install` in this switch fails on the uninstalled
   `mail-flag` library: the successful development build alone is not sufficient
   packaging evidence.
2. **Build the published documentation with the required toolchain.** A fresh
   `@doc` build succeeds on installed Dune 3.24.2 and odoc 3.2.1, but reports
   unresolved standard-library `Invalid_argument`/`Not_found` roots. These are
   the known toolchain limitation identified by `ocaml-docs`, not an excuse to
   remove exception hyperlinks. Use Dune's `odoc-v3-rules-3.21` branch and odoc's
   staging branch, declare `(documentation (depends ocaml))` there, and verify
   the published links. No other warning category appeared in the fresh build.

## Incomplete features and coverage: release decisions

| Gap | Decision for the first release |
| --- | --- |
| RFC 9404 `Blob/get` and `Blob/lookup` codecs/builders | Defer explicitly. These extend the initial RFC 8620/8621 scope. Raw invocation remains available for extensions. |
| Durable local sync store, restart acknowledgement, OAuth token acquisition | Application responsibilities. Document the cursor rules; Auth accepts refreshing credentials but does not implement an OAuth login service. |
| Cross-account `Email/copy` and `Blob/copy` live tests | Recommended next coverage. Codecs/builders have unit coverage. A second login is insufficient: configure a second account accessible to the same session and appropriate sharing rights. |
| Successful `EmailSubmission/queryChanges` on a live server | Needs another server. The new suite checks the advertised flag; this Cyrus reports `canCalculateChanges=false`. |
| Identity/set, PushSubscription and VacationResponse live behavior | Codecs/builders exist; the current oracle does not implement these methods. Identity/set's test now accepts only the specific unsupported-method error, not arbitrary failures. Use a supporting server before claiming interoperability for them. |
| Mutation coverage for Thread/changes and Mailbox/queryChanges | New live tests cover current-state round trips; add explicit mutations for stronger coverage. Identity/changes already has a live test. |
| Fuzzing the Email codec; IP-SAN TLS fixtures; multi-domain misuse tests | Valuable additional hardening, not substitutes for the passing deterministic tests. The Eio client remains owned by its documented domain/switch context. |
| Mosaic pagination, full bodies, HTML, attachments, new-message composition, offline sync | Keep `jmap-mosaic` labelled a demo. It shows the newest 100 messages and limits each plain-text body value to 64 KiB. It needs a configured sending identity. Do not present it as a complete daily mail client. |

The coverage matrix in [doc/coverage.mld](doc/coverage.mld) now distinguishes
unimplemented extensions, unsupported oracle methods and missing tests. The
current fixture supports Mailbox/queryChanges; the earlier blanket claim that
Cyrus always refuses it was stale.

## Verification and practical limits

- **741 hermetic tests pass**, including mail, protocol, methods, Chain, Eio,
  profiles and Mosaic; all 36 configured-only oracle cases skip when their URL
  is unset. The mdx tutorial and all library, CLI, Mosaic and example targets
  build in the development workspace.
- **36 live oracle tests pass** against an isolated Cyrus container: smoke,
  email/import/parse/streaming, submission, sync, push and the new method suite.
  Capability-based omissions are described above; a passing suite does not
  imply that every optional method was exercised.
- **Four additional CLI scenarios pass** using a temporary loopback HTTP mock:
  partial deletion, verbose JSON/topic parsing, forward query jumps, and
  partial marking. These were manual release checks, not added test-framework
  dependencies.
- Opam files regenerate and lint successfully. A fresh documentation build
  succeeds with the warning limitation above. Changed OCaml sources are
  checked with ocamlformat 0.29.0.
- The release-profile install targets also build when the complete development
  dependency set is selected together:
  `dune build -p jmap,jmap-mosaic,json-pointer,mail-flag,httpz,fetch,fetch-httpz --promote-install-files=false @install`.
  Disabling install-file promotion keeps the sibling sources untouched. This
  verifies the local package set, not a clean opam installation.
- The additional Mosaic headless run built and exercised login, reading,
  keywords and filing, then stopped because Cyrus's fixed Identity has an empty
  email address. Identity/set is unsupported, so that fixture could not provide
  a sender. Automatic approval review separately rejected a proposed full
  headless run because it could send existing message content. It was not
  retried; mock IO tests cover send/filing outcomes and the live protocol
  submission suite uses synthetic test messages.

The compiler was OCaml 5.5.0. The development siblings were Httpz/Fetch
`ba20631af374f19145e2c4e64e76e99be4734248` **with local changes**,
JSON Pointer `8ccd0b477cfc172a8573b7f0349c61d68d2bdbd2`, and mail flags
`4a2492ec978c7191e9fe6599ccb2dacd40e07632`. Their symlinks and source trees
were pre-existing and were not modified by this review. This is why publishing
and testing an identifiable dependency set remains a release gate.

The Cyrus image was
`ghcr.io/cyrusimap/cyrus-docker-test-server:bookworm`, image ID
`sha256:00f1b99b243ae5d5d9902041b3bcb969fd3ae1569e681efa0c2c7dbd4fdb3522`.
Its original upload setting advertised zero bytes; with a configured 50 MiB
limit the previously rejected upload tests all pass. The client continues to
enforce the advertised limit. Cyrus's unqualified upload configuration uses
KiB, as documented in its [configuration reference](https://www.cyrusimap.org/imap/reference/manpages/configs/imapd.conf.html).
No production mailbox was used.

## Follow-up on operation diagnostics

The later audit in [BUGS.md](BUGS.md#io-error-context-audit--2026-09-05) adds JMAP
operation context before I/O failures become result values. Session, method,
blob, credential, profile and push operations retain their underlying error
details. Reraised I/O exceptions retain their raw backtraces; callback exceptions
and cancellation keep their existing behavior. New endpoint context names only
the origin, and request context excludes arguments and call IDs.

The audit also preserves failures while reading refused push responses,
registers the missing `Client.Jmap_client_error` exception printer, and retains
Eio context in session-observer logs. Default CLI/profile connections now return
transport-initialization I/O failures through `Client.connect_env`.

The affected suites pass 213 tests, and all examples, CLIs and both mdx tutorials
build and pass. The dependency/publication gates above remain separate from
these code fixes.
