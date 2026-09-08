# Security and reliability audit

See [the first-release review](RELEASE_REVIEW.md) for the subsequent whole-project
review, additional fixes, current test results and publication prerequisites.

Disposition of the 2026-09-04 audit plus the 2026-09-05 follow-up audit.
The threat model is an untrusted JMAP server on the network, plus accidental
local misuse. “Fixed” means fixed in this worktree, including the sibling
dependencies linked into it. Accepted limitations and release-time dependency
work are recorded separately.

## Fixed

### JSON nesting is bounded without another JSON decoder

Jsont 0.3.0 has no decoder depth option or structural-event hook, and its
array/object decoder recurses with the input nesting. The shared `Httpz.Json`
adapter therefore taps the byte stream through Jsonm's
incremental lexer and counts only array/object start and end lexemes. Jsont
still performs the definitive syntax and typed decode; there is no local JSON
parser or value builder.

The default maximum depth is 128. `Fetch.Json.v`, `Fetch.Json.lines`, and the
corresponding Proffer codecs expose `max_depth`; zero permits scalars only and
a negative limit is rejected at codec construction. `Jmap_eio.Codec.decode`
and `Jmap_top.decode_string` use the same adapter and expose the same bound.
The byte-size limit remains independent.

This covers session, method, upload, problem-detail, and push JSON received by
the Eio client. A later `Jsont.Json.decode'` operates on the already-bounded
tree. Regression coverage includes the exact limit, an over-limit document,
brackets inside strings, one-byte reader slices, scalar depth zero, malformed
JSON, an over-deep HTTP 200 response, and an over-deep push event.

Fetch and Proffer previously had near-identical Jsont media adapters. They now
expose aliases of the one `Httpz.Json` implementation instead.

### TLS distinguishes DNS names from IP literals

The TLS wrapper is the shared `Httpz_tls.system` provider, which
`Fetch_httpz.std` installs by default. Httpz classifies the canonical
URL host, verifies DNS names as DNS subjectAltNames with SNI, and verifies IPv4
and IPv6 literals as IP subjectAltNames without DNS SNI. Missing hosts, invalid
names or addresses, and IPvFuture hosts fail closed as `Fetch.Tls_failure` at
the Fetch boundary. JMAP no longer owns RNG or trust-store initialization, host
classification, TLS configuration, or TLS flow construction, and its six
TLS-specific implementation dependencies were removed.

### Exchange and download stalls are bounded

`Cli.connect` and `Cli.create_client` now default to a 60-second timeout. The
lower-level `Client.connect` keeps its explicit no-deadline default for
embedding applications. Non-finite or negative timeouts, and negative body
limits, are rejected when the client is constructed.

Mosaic uses the same 60-second default for its session fetch and every later
exchange. `Jmap_mosaic.Io.create` exposes the timeout and transport for callers
and tests, and rejects a configured timeout when the transport carries no
clock.

An ordinary exchange retains one total deadline. A blob download uses that
deadline for its response head and then as an idle timeout for every body
read. A healthy transfer may take longer in total while bytes keep arriving;
a stalled body returns `Client.Timeout`. Documentation notes that a sink may
already contain a prefix and recommends temporary-file-plus-rename when
atomic output matters.

The reusable source wrapper is `Fetch.with_idle_timeout` in the sibling
dependency, rather than a JMAP-only flow implementation. It deliberately
offers no optimized copy path that could bypass the per-read deadline. Tests
cover a healthy long transfer, a mid-body stall, a stalled response head, and
a stalled diagnostic error body.

### Session-advertised endpoint trust can be bounded

The authenticated Session document is trusted by default to name JMAP's API,
upload, download, and event-source origins, as RFC 8620 requires.
`Transport.restrict ~under` provides an outer Fetch URL allowlist while
preserving the transport clocks. It applies to the initial Session fetch and
every advertised endpoint, independently of redirect trust and credential
scope. A regression verifies that an out-of-policy advertised origin is
denied before reaching the backend.

### Authentication and origin policy use Fetch

JMAP's authentication code no longer constructs Basic or Bearer headers,
performs Base64 encoding, or implements origin comparison. The sibling Fetch
dependency owns `Credential.basic`, `Credential.bearer`, lazy credential forms,
RFC 6750 token validation, the Basic user-id colon rule, header encoding, and
canonical same-origin checks.

Constant credentials fail at construction. Malformed file or refreshed
credentials deny the request with `Fetch.Denied` instead of escaping as
`Invalid_argument`. JMAP validates those values before memoizing them, while
Fetch retains the same check at the header boundary as defense in depth. The
runtime `uri` dependency was removed. `base64` remains a direct dependency only
because PushSubscription Web Push key validation must decode the supplied key
material.

### The JMAP JSON boundary enforces I-JSON

All network JSON now passes through `Proto.Json`. It rejects duplicate object
member names after escape decoding, invalid UTF-8, Unicode noncharacters,
non-finite numbers, excessive nesting, and numeric input outside binary64
magnitude. Generic JSON numbers remain allowed to be fractional; only fields
specified as JMAP `Int` or `UnsignedInt` use the stricter Int53 codecs. The
same validation is applied before encoding caller-supplied generic JSON.

### Session limits are enforced before or across requests

`Client` rejects encoded requests over `maxSizeRequest`, chains over
`maxCallsInRequest`, and known-size uploads over `maxSizeUpload` before network
access. An unknown-size upload is stopped as soon as it crosses the bound.
`Sync.get_all` batches by `maxObjectsInGet` and fails locally if a server
advertises zero for a non-empty get. Query paging reports changing query states
and non-advancing pages as errors instead of returning a partial success.
`Sync.pages` and `Sync.all_ids` also have finite request fuel, so an advancing
server that invents fresh ids forever cannot keep a walk alive indefinitely.
The verbose paging loop in `jmapq` applies the same request bound and rejects a
server that moves its reported position backwards.

The command-line clients and Mosaic now use sequential query/get and
change/get flows where result references are not required. Mosaic retains the
two-call send chain when it fits, and falls back to saving the draft and
submitting it in separate requests when `maxCallsInRequest` is one. Both
created records must be present in the set responses before the send is
reported as successful.

### Saved Mosaic credentials are private and bounded

Named profile files are accepted only when they are regular files with no
group or other permission bits, and reads are bounded at 64 KiB. Filesystem
errors are ignored for the optional profile picker, but Eio cancellation is no
longer swallowed by catch-all exception handlers. Profile replacement writes a
private temporary file beside the destination and atomically renames it, so a
failed write does not first destroy the saved credential it was replacing.

Profile and Mosaic login fields reject C0, DEL, and C1 control characters.
Terminal paste removes them before the values reach the form. Secrets remain
ordinary immutable OCaml strings; the public contracts state that the runtime
cannot guarantee `mlock` or erasure and leave core-dump and process-inspection
policy to the deployment.

### Connection profiles are shared by JMAP clients

`Jmap_eio.Profile` now owns the named XDG store, its bounded private-file
policy, validation, atomic replacement and the path from a selected name to an
authenticated `Client.t`. New clients can compose `Cli.profile_term` with
`Profile.connect_name` instead of copying Mosaic's login code. Mosaic writes
the shared `$XDG_CONFIG_HOME/jmap/profiles` store and reads no store of its
own.

This does not add a generic Fetch credential-source layer: a stored profile
also names a JMAP session resource, and `Auth` already centralizes JMAP's lazy
key-file and refreshing-token lifecycle. Fetch continues to own only the HTTP
credential/header and origin-scoping semantics. A non-JMAP consumer of the
same source lifecycle would be the evidence needed to move that smaller
abstraction down.

### JMAP URI templates use Httpz

`Proto.Template` retains only its string convenience functions as a
compatibility wrapper over `Httpz.Uri_template`; its former `Rfc6570` module
alias has been removed. Parsing, UTF-8 checks, percent-encoding, operators,
modifiers, and expansion no longer have a local JMAP implementation. The
sibling Httpz implementation gained explicit template-level classification
(`Httpz.Uri_template.level`) and reference resolution
(`Httpz.Uri_template.expand_resolve`) so relative JMAP endpoint templates can
be resolved without percent-encoding their braces.

`Proto.Template.expand` and `Proto.Blob.expand_download_url` return a result;
the raising forms they replaced have been removed. Both go through one
expansion, so a session-advertised template is treated the same way whichever
one a caller reaches for, and an expansion that is not an RFC 3986 URI
reference is an error in both.

`Client.connect` parses and resolves all advertised endpoints once before
installing credentials, and stores the template endpoints in their typed Httpz
form while retaining string compatibility accessors. It rejects non-Level-1
syntax, missing JMAP-required variables, malformed expansions, non-HTTP(S)
results, and required variables that can change the origin. Literal
percent-encoded braces stay literal. Uploads, downloads, and push URL
construction expand the stored values through Httpz and return typed failures
on the result-returning paths.

The shared resolver now chooses a collision-free expression placeholder and
restores all expressions in linear passes. This removes quadratic behavior for
adversarial literals that repeatedly resemble its private placeholder while
preserving expression order through relative-reference resolution.

`PushSubscription.create_args` also uses `Httpz.Uriz` rather than accepting
every string with an `https://` prefix; malformed authorities, ports, and
hostless values are rejected.

### Capability objects are validated in their proper scope

Session decoding requires the mandatory `urn:ietf:params:jmap:core`
capability and all members of its server-wide limits object. Known capability
values must be JSON objects, and malformed values fail the Session decode
instead of later becoming indistinguishable from absence.

Mail and submission now have separate session- and account-scoped codecs. The
session values must be empty objects. Account mail requires all six RFC 8621
members, including its nullable fields, and enforces the specified minima for
`maxMailboxesPerEmail` and `maxSizeMailboxName`. Account submission requires
both of its members. Vacation-response capability objects must be empty in
either scope. Unknown capabilities, and core at account scope where no schema
is defined, remain opaque.

### JMAP wildcard results have an explicit budget

The sibling `json-pointer` package now accepts `?max_results` on
`Json_pointer.Jmap.get`, `get_result`, `find`, `path`, and `path_list`.
One budget is shared by nested wildcards and counts every final value, including
repeated traversal of a physically shared OCaml subtree. A negative bound is
rejected, and exceeding it is a structured Jsont error. Tests cover exact and
exceeded limits, nested wildcards, and a manually shared DAG. This library
builds result references but leaves their resolution to the server, so it uses
no budget of its own.

### Push failures are observable

Only `Eio.Cancel.Cancelled` is treated as orderly subscription teardown. Any
other pump exception settles the subscription result as an error. The
one-shot listener and the ordinary client also translate a raw
`Unix.Unix_error` from an early platform/socket failure into their typed
transport errors instead of leaking it from a result-returning API.

### Diagnostics share Httpz's control-byte sanitizer

Protocol errors, client errors, chain parse errors, email addresses,
email-header values, keywords, and push events render server-controlled
control bytes visibly while preserving printable UTF-8. The implementation
delegates to `Httpz.Media.sanitize_diagnostic`, so the C0/DEL/C1 policy is not
duplicated in JMAP. Raw values in decoded records are deliberately unchanged;
escaping belongs at a diagnostic output boundary.

The CLI applies the same boundary to custom mailbox roles and manually rendered
set-error type names. `Sync` escapes custom roles in its diagnostics. A
regression sends a terminal clear-screen sequence in a custom role.

### Push time and error handling fail closed

Polling subscriptions reject a finite interval that cannot be represented by
the monotonic clock. An unexpected timestamp-addition overflow expires the
connection immediately instead of replacing its deadline with the maximum
timestamp. Oversized non-success response bodies now produce a stable bounded
diagnostic instead of an empty string.

A failing session-change observer remains isolated from the refresh and from
later observers, but its control-safe exception description is written to the
Eio trace. Cancellation still propagates.

### Shared cookies retain Fetch's URL matching contract

The default transport shares one in-memory cookie jar between the session's
endpoints. Fetch applies host or explicit domain, path, and `Secure` matching to
each request. A regression sets a host-only cookie at a hostile download origin
and verifies that it does not reach the API origin. An explicit cookie `Domain`
continues to share with matching subdomains as RFC cookie behavior requires.

Persistent Fetch jars replace their destination atomically through a private
file. JMAP verifies the resulting file has no group or other permission bits
and documents that loading an existing file does not reject a permissive mode.
The caller remains responsible for the initial file and directory authority.

### Credential files and CLI defaults are hardened

Credential files must be regular, have no group or other permission bits, and
hold their secret on a first line no longer than 64 KiB. Both the Eio and native
paths inspect the opened handle, so a symlink is checked at its target and there
is no separate stat-then-open race. The CLI warns that `--api-key` is visible in
the process list and points to a private key file or environment variable.

### Misleading or missing contracts are corrected

- The suggested “refuse HTTPS” wrapper now says to raise `Tls_failure`;
  returning the raw connection would send plaintext.
- `Client.t` is documented as fiber-safe within one OCaml domain, not safe to
  share across domains. A default Fetch/httpz transport itself is
  domain-shareable; `Transport.of_fetch` inherits its backend's restrictions.
- Fetch's default retry policy is documented as idempotent-only, so JMAP POSTs
  and uploads are not retried after ambiguous failures.
- The package's OCaml floor is 5.5 and its Jsont floor is 0.3, matching the
  dependency APIs actually used.

## Dependency ownership sweep

| Concern | Owning dependency | JMAP disposition |
| --- | --- | --- |
| HTTP URL parsing, canonical origins, and scopes | `Fetch.Middleware.Url` | Delegated; the former local origin comparison is gone. |
| RFC 6570 parsing, relative resolution, and expansion | `Httpz.Uri_template` | Delegated; `Proto.Template` is only a compatibility/result wrapper. |
| Basic/Bearer validation and wire encoding | `Fetch.Credential` and typed Fetch headers | Delegated; Base64 is used directly only for PushSubscription Web Push key validation. |
| Streaming Jsont media and nesting checks | `Httpz.Json` using Jsonm lexemes | Shared by Fetch, Proffer, and JMAP convenience decoders. |
| Eio TLS, system trust, and DNS/IP peer verification | `Httpz_tls` and `Fetch_httpz.std` | Delegated; `Transport.v` installs it by default. |
| Per-read idle deadlines | `Fetch.with_idle_timeout` | The JMAP download policy composes the Fetch wrapper. |
| JSON Pointer and JMAP wildcard evaluation | `json-pointer` | The budget was added upstream and JMAP selects a finite default. |
| One-line control-byte sanitization | `Httpz.Media.sanitize_diagnostic` | All JMAP diagnostic printers delegate to it. |

## Clarified findings

### Int53 has no boundary off-by-one

This was a false positive. In the IEEE-754 binade ending at 2^53, adjacent
doubles are one integer apart, so 2^53−1 is exactly representable.
Existing and rerun tests accept ±(2^53−1) and reject ±2^53 for signed and
unsigned codecs.

### Wildcard evaluation is not intrinsically O(n^k) amplification

For an ordinary parsed JSON tree, nested width-n arrays with k wildcards
already contain or produce O(n^k) distinct nodes. Evaluation is proportional
to the input/output structure it visits. A manually shared OCaml DAG or a
caller accepting arbitrary pointers changes that resource model, which is why
the optional dependency-level budget was still added.

### Mosaic text does not emit terminal controls

The installed Matrix rendering path segments text into graphemes, skips
ASCII controls and other zero-width/control graphemes when drawing the grid,
and emits a blank for an empty glyph. Consequently mailbox names, subjects,
snippets, and bodies passed through Mosaic text widgets cannot inject raw
C0/ESC terminal sequences. Escaping those data fields at every call site
would alter legitimate display data and is unnecessary. Non-Mosaic diagnostic
printers remain protected as described above.

### `Chain`'s remaining `invalid_arg` sites are builder contracts

The literal pointer strings are owned by `Chain`; failure to parse one is a
library programming error. Other sites reject client-supplied builder values
or values a typed encoder cannot represent. Server response decoding returns
structured parse errors, so these exceptions should not be recast as remote
failures.

### POST retry remains conservative by design

Fetch does not assume a POST is replay-safe. Automatically retrying JMAP
method calls or uploads after an ambiguous failure can duplicate side effects.
Applications with stronger idempotency knowledge may install a custom retry
policy; the default remains unchanged.

## Remaining dependency and design work (updated 2026-09-05)

These are recorded rather than hidden behind another local implementation.

1. **Version the dependency boundary for release.** The symlinked development
   trees provide new Httpz URI-template, bounded-JSON, and Eio TLS APIs, Fetch
   credential/idle APIs, and bounded `json-pointer` APIs. No published release
   number identifies the complete set yet. Before publishing JMAP, release the
   siblings and constrain the opam dependencies to the first versions carrying
   these interfaces; inventing a minimum version now would be misleading.
   This also covers the Matrix/Mosaic terminal-sanitization contract and the
   Fetch URL-scheme and cookie-jar contracts. Pin `mosaic`/`matrix-eio` once the
   grid sanitization contract is versioned.
2. **Expand adversarial test generation.** Deterministic regressions cover the
   issues above. Property/fuzz tests for codecs and templates, a real IP-SAN TLS
   fixture, and multi-domain misuse/ownership tests remain useful additional
   coverage. The deterministic suite now includes custom-role terminal
   injection, advancing infinite paging, lazy invalid credentials, cross-origin
   cookies, and enormous push poll intervals.
3. **Build published documentation with the odoc-v3 Dune rules.** The installed
   Dune 3.24.2 `ordering` build completes `@doc`, but it cannot declare the
   standard-library documentation dependency and therefore warns on the
   existing `@raise Invalid_argument` and `@raise Not_found` references. The
   `ocaml-docs` workflow requires the `odoc-v3-rules-3.21` Dune branch, where
   `(documentation (depends ocaml))` supplies those roots. Use that toolchain
   for published docs rather than replacing structured raise tags with plain
   code text.

## Accepted limitations from the 2026-09-05 follow-up

- `Proto.Json.check_value` recursively checks caller-built `Jsont.json` trees.
  Network decode is iteratively depth-bounded before it builds such a tree. A
  caller can still construct an exceptionally deep value and overflow its own
  stack during encode; caller-built trees are trusted input.
- Secrets live in immutable OCaml strings and cannot be reliably locked or
  wiped. This is documented on `Auth` and `Profile`; deployments must control
  core dumps and process inspection.
- Permissive decoding of unknown roles and keywords is intentional. Encoding is
  strict, and terminal output sanitizes the decoded strings at its display
  boundary. Mosaic relies on Matrix's grid sanitization contract as described
  above.
- `download ?name ?accept` accepts trusted caller strings. Httpz validates UTF-8
  and percent-encodes reserved characters while expanding them; account and blob
  ids already use JMAP's restricted identifier grammar.

No remote code execution was found in the follow-up audit, and no confirmed
follow-up finding remains open apart from the explicitly deferred additional
adversarial test generation above.

## Follow-up pass 2026-09-05 (code only; pins/packaging out of scope)

Re-verified the fixed items above against the worktree. No RCE, credential
leak, or auth bypass found. The medium finding and four low hardening notes
below are now addressed. Pins and packaging remain outside this pass.

### M1 — Zulip subject parsing cost — fixed

`bin/jmapq.ml` compiled a regex on every server-controlled `email.subject`,
bounded only by `max_body`. The audit's quoted expression predates the
bracketed-topic fix in `723a652`; this pass does not establish its claimed
backtracking hang. The remaining regex work is replaced by the private
`bin/jmapq_subject.ml` parser, which uses a fixed number of linear scans.
The first `>` separates the channel, and the final `[server]` suffix leaves
earlier brackets in the topic intact. Empty fields and CR/LF subjects are
rejected. The now-unused direct `re` dependency is removed.

`test/cli/test_subject.ml` covers valid notifications, whitespace, UTF-8,
bracketed topics and malformed subjects, including three one-megabyte
nonmatching inputs and a long valid topic that must not be truncated.

### L1 — Shared permissions on existing profile directories — fixed

`Profile.load`, `list` and `save` now reject an existing store directory with
group or other permission bits and return a `Storage_error` explaining the
mode and how to correct it. They do not chmod caller-selected directories.
New stores remain `0700`, profile files remain `0600`, and listing a missing
store still returns an empty list. Mosaic ignores a rejected shared store.
The API docs, profile tutorial and examples describe the same contract.

Core and Mosaic regressions cover a pre-existing `0755` store, unchanged
permissions, rejected reads/listing and no new file written. The private-store
round trip and legacy migration cases continue to pass.

### L2 — Cookie persistence ownership and origin isolation — addressed

`eio/transport.mli` explicitly attributes atomic replacement through a `0600`
file to Fetch. Fetch accepts permissive modes when loading an existing file;
the caller owns that path's authority and initial permissions.

The cookie regression now has `evil.example` send both a host-only cookie and
an attempted `Domain=api.example` cookie. It checks that the hostile domain
cookie is rejected by the jar and that no cookie reaches the API origin.
This exercises Fetch middleware with a mock transport, alongside the existing
host-only selection and persisted-file mode checks.

### L3 — Media type data and terminal display — fixed

`eio/client.mli` documents Fetch's typed upload header validation and the
server-controlled nature of returned download media types. The blob and
streaming examples, including their README snippets, use `Cli.terminal_text`
before displaying returned upload or download media types. The streaming
example also sanitizes the caller-supplied media type on the same output line.

### L4 — Oracle LMTP environment diagnostics — fixed (test-only)

`Oracle_harness.lmtp` now uses `int_of_string_opt` and `Alcotest.failf` for
malformed or out-of-range ports and empty hosts, with the escaped configuration
value and `JMAP_ORACLE_LMTP` in the diagnostic. Ports must be in `1..65535`.
`test/oracle/config/test_config.ml` checks defaults, host-only configuration,
boundary ports, nonnumeric/empty/overflowing ports and empty hosts without
connecting to a server.

Verification: all affected Eio and Mosaic suites pass, along with the new
subject and oracle-configuration suites (197 tests across these suites).
All examples and both mdx tutorials build and pass. The documentation build
succeeds with the previously recorded unresolved Stdlib exception references;
changed OCaml files pass formatting checks and `git diff --check` is clean.

Non-findings this pass: `truncate_string`/`tail` sanitize-first and stay
bounds-checked on invalid UTF-8; `Cli.debug` carries no server strings;
`Sync.get_all` batch clamping, `Client` semaphore clamping, push `Held`
overflow-to-`now`, and `Proto.Template`/`Blob` deprecated raising forms
(unused for session templates) all behave fail-closed.

## I/O error-context audit — 2026-09-05

The latest five code findings above were rechecked and remain fixed. This pass
then followed operations through Auth, Profile, Transport, Client, Push, Sync,
CLI and Mosaic, including their exception printers and result conversions.

### Operation context was missing at JMAP boundaries — fixed

- `eio/client.ml` now identifies session fetches and refreshes, method requests,
  generic GETs, blob uploads and downloads when converting I/O failures to
  `Client.Transport`. Method requests include a bounded summary of method
  names, without arguments or call IDs. Source/sink failures retain their
  existing classification and the underlying I/O diagnostic.
- `eio/auth.ml` adds credential-renewal context to I/O exceptions raised by an
  `Auth.refreshing` callback. It uses `Eio.Exn.reraise_with_context` with the
  captured raw backtrace. Cancellation and non-I/O exceptions still propagate
  unchanged; no token is added to context.
- `eio/profile.ml` names loading, listing and saving in storage errors, including
  failures while inspecting the store. Connection failures retain the typed
  Fetch reason and add the selected, validated profile name.
- `eio/transport.ml` adds initialization context to propagated I/O failures.
  `Client.connect_env` converts them to the existing result type. CLI and
  default profile connections use that path too.
- `eio/push.ml` distinguishes opening, reading and closing a one-shot event
  stream, and identifies a fatal subscription failure after retry classification.
  Its callback exception sentinel remains outside I/O error conversion.

The shared `eio/error_context.ml` module is private. Returning an error uses
`Eio.Exn.add_context` before formatting; propagating an I/O exception uses
`reraise_with_context` and its original backtrace. New URL context contains only
the canonical origin, excluding user information, path, query and fragment.
Existing context supplied by Fetch or a caller is retained, so its owner remains
responsible for any sensitive values already present there. JMAP's public
diagnostic printers escape terminal controls.

### Push discarded refusal-body read errors — fixed

`Push.error_of_response` previously caught an I/O failure and returned
`Http_error (status, "")`. It now retains the HTTP status and stores the
body-read failure, its existing context and the JMAP operation in the diagnostic.

### Exception reporting hid existing diagnostics — fixed

`Client.Jmap_client_error` had no registered exception printer. Uncaught `_exn`
calls could therefore print the constructor with `_` in place of its diagnostic.
Its registered printer now uses the sanitized `Client.pp_error`, as Chain and
Sync already do for their exceptions. Session-change observer logging now uses
the Eio-aware diagnostic instead of `Printexc.to_string_default`, which skipped
Eio's registered context printer.

### Verified boundaries and limits

Sync, the CLIs and Mosaic already preserve Client's returned diagnostic when
reporting network failures. Optional legacy Mosaic login-file failures remain
ignored on their documented terms. Auth file reads already name the file and
preserve existing Eio context. The pure protocol/codecs do no I/O.

HTTP, JSON, JMAP and timeout result constructors retain their existing data and
classification. A `Client.Transport` result retains its typed Fetch reason and
formatted context, not a raw exception backtrace. Raw backtraces are preserved
where I/O exceptions are re-raised and where caller exceptions pass through
`with_get`, Push callbacks and Mosaic dispatch.

All affected Eio, Mosaic, subject and oracle-configuration suites pass (213
tests across these suites), along with both mdx tutorials and all example/CLI
builds. Regressions cover response-body failures at each client endpoint,
streaming source/sink failures, raw Unix errors, preservation of inner context
and typed reasons, a previously returned token staying out of refresh errors,
the original callback backtrace, push refusal diagnostics, profile open/rename
errors and cleanup, callback exception identity, and the registered exception
printer. No live server or production mailbox was needed for this pass.
