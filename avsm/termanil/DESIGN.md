# Termanil design

Termanil is a reading and triage workspace. An email, a contact and a task
remain distinct objects owned by JMAP, Sortal/CardDAV and Dooit respectively.
The terminal provides navigation between them without introducing another
contact store or task format.

## Libraries

| Public library | Responsibility | Dependencies |
|---|---|---|
| `termanil.model` | Domain records, identities, pure reducer, search normalization | Unicode, S-expression codecs |
| `termanil.mail` | Typed JMAP queries, conversations, flags and reply submission | Model, `jmap.eio` |
| `termanil.drafts` | Agent-editable Markdown replies, revision checks and send receipts | Model, Dooit file helpers |
| `termanil.fake_jmap` | Persistent embedded JMAP protocol server for the demo | Demo fixtures, Fetch transport, JMAP |
| `termanil.contacts` | Local vCards and read-only CardDAV metadata | Model, Sortal |
| `termanil.tasks` | Capture, query, revision-checked completion and sync | Model, Dooit |
| `termanil.backend` | XDG TOML configuration and scoped Eio execution | Backend libraries |
| `termanil.protocol` | Versioned process messages | Model, Sexplib |
| `termanil.ui` | Bonsai state, metadata and reply editor components | Model, Bonsai terminal |
| `termanil.demo` | Synthetic fixtures and isolated in-memory UI test backend | Model |

Each library has an explicit `.mli`. The binary chooses a backend and starts
the UI. Protocol records contain domain values, never a Fetch client, Eio
capability, authentication object or callback.

## Runtime boundary

The installed Bonsai/Async stack was compiled against the switch's Cstruct.
This monorepo's Eio and HTTP stack uses a patched Cstruct interface. Their
CMI hashes differ, so linking both stacks into one executable is invalid.

The Bonsai frontend and native worker therefore run in separate processes.
The frontend uses `Async.Process.run` with an argument list and stdin, with
no shell interpolation. A worker receives one request, loads the configuration,
performs one bounded operation, emits one response and exits. A request has
a 60-second Eio deadline. Dooit's durable journal handles interrupted writes.
The frontend waits for the current request before quitting.

Only the frontend and its UI tests allow overlapping Dune dependencies.
Those targets never depend on `termanil.backend` or Eio. The worker uses the
workspace networking stack. This keeps the incompatible Cstruct instances
in separate address spaces and avoids changes to the switch or vendored
networking packages.

The wire format is `(termanil/v4 PAYLOAD)`, encoded as a single S-expression
line. Requests and responses have explicit variants and typed fields.
Messages are bounded at 64 MiB. A version mismatch is an error. Tests
round-trip quoted strings, newlines and terminal controls through the codec.
No Marshal or command text crosses the boundary.

A persistent worker could later reuse JMAP sessions. Keep the protocol and
backend API unchanged when making that optimization. If the dependency
interfaces converge, the same backend API can instead run on an Eio worker
thread through `Effect.of_deferred_thunk`.

## State and effects

`Model.update model action` returns a model and at most one numbered request.
The Bonsai state machine schedules its effect and injects the result with
that request number. Stale numbers and mismatched result identities cannot
replace the current state. There is at most one operation in flight.

Selection and tabs remain usable during requests. A tab change queues a
refresh of the newly selected view. Writes carry the object's ID and the
displayed revision, not the selected row number. Refresh preserves selection
by identity when possible. Failed search keeps the old query paired with
the old results.

Rendering and keyboard translation are separate pure functions. The renderer
is given dimensions explicitly. It crops views to the terminal, wraps using
Unicode terminal widths, and substitutes control and bidi formatting
characters before displaying server or note content. Bracketed paste is routed to search or the focused reply editor.
Pasted command keys cannot dispatch operations. Multiline search paste cannot submit.

`Model.arrows_scroll` distinguishes help, reports and send review from item
navigation. Listing and detail views share the same arrow behavior. The reply
editor owns its cursor keys, and PageUp/PageDown scroll opened documents.
Returning to an older linked email anchors navigation to its conversation
representative in the current page.

The terminal driver shares one handler snapshot across each input burst.
`Input_queue` crosses focus and paste boundaries one per frame so subsequent
keys see the updated Bonsai state. Ordinary editor/search text is batched
(up to 4096 events per frame), preserving paste throughput. Tests send Enter,
`e`, text and Escape in one burst and assert that command letters stay text.
Tab and Shift-Tab are global boundaries, including inside the reply editor.
The model retains each tab's navigation position and retargets stored row
indices by identity when a response changes ordering. Entering reply focus
waits for an in-flight conversation read to supply the signature, so fast
typing cannot race the initial editor contents.

No loading, error or confirmation state blocks the Async scheduler.
Configuration, local files and remote responses never enter a Bonsai
computation except as explicit domain values.

## Identity and writes

Email references are `{ service; account; id }`. A source link must match the
selected profile's service binding and any explicitly configured account
before connection. Replies are checked for the expected account and IDs.
JMAP IDs are treated as opaque strings.

Mailbox search requests bounded pages. Email/get results are reordered to
match Email/query, and every requested ID must be accounted for as returned
or not found. Pagination uses the actual number of query IDs consumed,
including messages removed between query and get.

Read and star changes fetch a fresh Email state, then use a keyword-specific
PatchObject with `ifInState`. The adapter checks both method success and
per-object success. It does not replace the complete keyword map or
automatically replay a failed operation.

Contacts carry a local root and UID or a remote href. The worker converts
vCard properties to labelled metadata. The frontend never parses YAML or
vCard syntax. `Termanil_ui.Metadata` provides the reusable Bonsai component.
Original bytes remain in the cards and recovery archive. Unknown fields
remain searchable metadata. Local reads are bounded to 8 MiB per card and
report malformed neighbors separately. Reading cannot mutate CardDAV.

Sortal owns the one-time `carddav migrate` operation. Its editable store has
`cards/`, `store.json` and an immutable `recovery/` bundle. Migration verifies
original bytes and semantic field recovery before success. Termanil only
reads the active cards. Subsequent edits do not rewrite the archive.

Opening mail sets an explicit reader focus. Up/Down reads adjacent messages,
PageUp/PageDown scrolls, and `e` focuses the reply editor. In reply focus,
all ordinary keys go to `Bonsai_term_text_editor` with standard bindings and
buffered paste. Esc returns to reading. Tab changes views. A Bonsai `assoc` keys editor
instances by the complete email reference, retaining text, cursor and undo
history across navigation. The reducer mirrors draft text for quit handling.
Cursor updates run after display and only when the position or focus changes.
Ctrl-S saves to the native reply store using the editor's base revision. The
reducer tracks saved bodies separately from buffers, so edits made while a
save is in flight remain dirty. Workspace refresh updates clean buffers and
changes their editor generation. Dirty buffers retain their original base
revision, causing a subsequent conflicting save to fail without data loss.
Only unsaved edits trigger the quit confirmation.

Replies use YAML frontmatter plus a Markdown body. The source's complete
identity determines the filename. Saving replaces only the body, retaining
header bytes including unknown fields and comments. Queue receipts bind to
the complete file hash. External edits invalidate queue decisions. A store
lock serializes Termanil operations, and atomic writes check the prior bytes.
External writers should use the same revision-check discipline.

Send review captures exact draft revisions. The worker preflights every
revision, reply target, identity and mailbox before submitting. Email header
and body fields are immutable in JMAP, so editing remains local and sending
creates a new Email followed by EmailSubmission/set. See
[RFC 8621](https://www.rfc-editor.org/rfc/rfc8621.html), sections 4.6 and 7.5.
Submission applies the Drafts-to-Sent patch only on success. The local receipt
is marked uncertain before transmission, and records the remote Email id
before the submission call. A failure stops the batch. Receipts prevent
implicit retries, even if an agent edits an uncertain draft. Acceptance is
not a delivery guarantee. Outbox verification uses EmailSubmission/query/get
to resolve a receipt only when an accepted submission is found. An empty
query never permits an automatic retry.

The embedded fake server handles Session, Mailbox/get, Email/query/get/set,
Thread/get, Identity/get and EmailSubmission/query/get/set through Fetch.
It uses the same typed JMAP client and adapter as live mail. JSON state is
updated atomically under a file lock, before the response is returned.
Its advertised URLs are synthetic and never reached through a socket.
The demo also uses real local Dooit notes and the same reply library. Demo
configuration is constructed independently of live profiles and Dooit
configuration. `--demo-dir` selects a reproducible isolated workspace.

Task capture delegates to `Dooit.Link.capture`. The message source key derives
the initial UUID and prevents duplicate capture. Existing completed tasks
are returned without reopening them. Task completion delegates to
`Dooit.Store.apply` with the displayed byte revision and a fresh operation
UUID. Agent edits therefore participate in the same concurrency checks.

Dooit remains the only owner of WebDAV configuration, collection scope,
sync bases and recovery objects. Preview uses a read-only transport.
Applying sync requires Enter on the terminal's confirmation prompt, then
recomputes against current files. Unknown note metadata remains untouched.

## Tests and extension points

The frontend expect library uses `bonsai_term_test` and the installed
examples' handle pattern. Tests settle several Bonsai frames so chained
activation and completion effects finish before the next simulated key.
Initial golden output was reviewed for the expected behavior. Later
changes should be reviewed before promoting golden differences.

Tests cover narrow and wide layouts, resize, mail reading and search,
capture idempotency, completion and source navigation, contact metadata,
sync preview and confirmation, errors, paste isolation and Unicode wrapping.
Pure reducer tests cover delayed replies, busy writes, identity preservation,
empty selections, failed queries and queued tab changes.

The backend expect library links only the networking stack. Fetch mocks
verify JMAP request bodies, ordered pagination, read-only message access,
account/ID checks, conditional keyword patches, body truncation and
per-object failures. Temporary Dooit stores verify stale revisions and
capture of already-completed tasks. Cram tests execute the actual worker
and check protocol/configuration failures without credentials or network.

Add an operation by extending the model request and response types, adding
the adapter method with backend tests, and then introducing a reducer action
and terminal interaction. Keep third-party protocol objects behind the
adapters. Schema changes to wire records require a protocol version decision.

Attachment handling needs a bounded download
API and explicit destination. Contact writes need a shared reconciliation
model. None of these should be implemented as an unstructured shell command
inside the UI.

Workflow tests reconnect the native demo client between operations and check
flags, collapsed searches, thread hints, preserved reply headers, sent mailbox
membership and duplicate refusal. Reply-store tests simulate external edits
and ambiguous transport failure. Bonsai tests exercise task/email round trips,
collapsed history, queue cancellation and typing plus Ctrl-S in one input burst.
The desert palette uses explicit RGB attributes with text badges, so state
remains readable without relying only on colour.
