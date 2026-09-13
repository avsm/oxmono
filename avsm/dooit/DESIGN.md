# Dooit: a task manager made of notes

Design, 2026-09-12. Application name: `dooit` (renamed before release).
The first OCaml implementation is described in [README.md](README.md).
This document also records planned extensions; the README lists the commands
and capabilities currently implemented.

## Decisions

Each task is one Markdown file with YAML frontmatter. You can write the body
in your editor; agents use the same files through revision-checked commands.
A regular WebDAV collection holds these files, and each device has a local
working copy. Work and search continue offline. There is no export step.

Task identity is independent of its title and filename slug. Links are typed,
extensible records, with JMAP email as a first-class target. Tags are arbitrary
strings. A local index makes the notes searchable, but the files contain all
task content and can reconstruct that index.

Start with a native OCaml library and CLI. An email-client action and an agent
tool adapter call that library. A browser or phone interface can follow using
the same document format and reconciliation rules.

This chooses Markdown as the shared representation. A generic WebDAV server
can store it; integration with a calendar application's task UI would require
a separate adapter. Configure the file collection independently of the JMAP
mail account and Sortal's CardDAV address book.

## One task, one file

Example `notes/2d26ec67-139e-4cba-8bc3-6ec0f85b8735.md`:

```markdown
---
schema: dooit/v1
id: 2d26ec67-139e-4cba-8bc3-6ec0f85b8735
title: Reply about the WebDAV prototype
status: open
created_at: "2026-09-12T10:00:00Z"
due: "2026-09-18"
tags:
  - ocaml
  - "waiting on Alice"
  - work/webdav
links:
  - id: source-email
    rel: source
    type: jmap-email
    target:
      service: https://mail.example.net/jmap/session
      account_id: example-account
      email_id: example-email
    hints:
      thread_id: example-thread
      message_ids:
        - prototype@example.net
      subject: WebDAV prototype
  - id: project-page
    rel: related
    type: url
    target:
      uri: https://example.net/projects/webdav
---

Check how the server handles concurrent edits before replying.

- [ ] Try conditional writes from two clients.
- [ ] Send Alice the results.

## Working notes

The first test should include an interrupted upload.
```

The example identifiers and URLs are illustrative.

| Field | Contract |
| --- | --- |
| `schema` | Required format version. Unsupported major versions are retained but not rewritten. |
| `id` | Required immutable UUID. Normally random; default email capture uses the deterministic identity described below. |
| `title` | Required nonempty string. Renaming it does not move the resource. |
| `status` | `open`, `active`, `blocked`, `done`, or `cancelled`. |
| `created_at` | Required RFC 3339 instant, set at creation and immutable afterward. It does not decide merge winners. |
| `due` | Optional quoted `YYYY-MM-DD` date in v1. It is a date, not a midnight UTC reminder. |
| `tags` | List of nonempty strings, default empty. No predefined vocabulary or implied hierarchy. |
| `links` | List of link records, default empty. Each has a stable ID unique within the note. |
| `deleted_at` | Optional RFC 3339 instant marking a retained, hidden task. See deletion below. |

The body is unrestricted Markdown. Checkboxes are body text, not separate
synchronized tasks; promoting one to a task creates a new note and a link.
There is no second editable copy of the title in a required heading, and no
generated `updated_at` that would make every edit conflict.

Tag spelling, case, spacing, and order are preserved. Exact UTF-8 strings are
the membership identities; search additionally uses Unicode normalization
and case folding. Thus searching `OCaml` can find `ocaml` without rewriting
the tag. Slash and space characters have no special storage meaning. Exact
duplicate entries are diagnosed; commands never introduce them.

Read the full document, not just the known fields. Preserve unknown
frontmatter keys, nested extension values, link types, and untouched comments
and formatting. An edit to one field patches that field's source span and
reparses the result. An unchanged document round-trips byte for byte.
Reject duplicate YAML keys and unsupported YAML constructs such as aliases
for mutation, while retaining the original file for repair. V1 metadata uses
string-keyed mappings, lists, and JSON-compatible scalar values.

Namespaced extension keys, for example `org.example.review`, prevent accidental
collisions. New optional fields can extend v1; a change to existing meaning
requires a new version. Unknown status values are shown as unsupported and
held for review, never silently reset to `open`.

## Links, starting with JMAP email

Every link has `id`, `rel`, `type`, and a type-specific `target` mapping.
Optional `hints` hold display or recovery information. V1 understands:

| Type | Target | Typical relationship |
| --- | --- | --- |
| `jmap-email` | `service`, `account_id`, `email_id` | `source` |
| `url` | Absolute `uri` | `related` |
| `dooit` | `store_id` and task `id` | `related` or `depends-on` |

Relationships are extensible strings. External link types use namespaced
identifiers, for example `org.example.issue`. Unknown types remain editable
as raw metadata and visible in inspection output. A future Sortal contact
link can carry its store identity and Sortal ID without changing task schema.
Link relationships alone do not execute actions or change task status.

The JMAP identity key is the tuple
`(service, account_id, "Email", email_id)`. A message ID alone is insufficient:
JMAP IDs are scoped to an account and object type.
[RFC 8620, section 1.6.3](https://www.rfc-editor.org/rfc/rfc8620.html#section-1.6.3)

`service` is an agreed, credential-free JMAP session-resource URL used as the
identity namespace. Normalize scheme and host case and the default HTTPS
port when configuring it; preserve path/query semantics and opaque account
and email IDs exactly. Keep that namespace fixed across devices. Endpoint
aliases or a provider migration need an explicit mapping, not a guessed
equivalence. A local profile name is a credential binding, not an identity.
Never store a bearer token, signed download URL, or authenticated URL here.

Resolving a link connects through a local profile bound to that service,
discovers the current API endpoint, and calls `Email/get` for the stored
account and ID. JMAP email identity is independent of mailbox membership.
The optional `message_ids` list contains the email's RFC 5322 Message-ID
header values, not JMAP IDs; it supports a suggested recovery search if the
original object disappears.
[RFC 8621, section 4](https://www.rfc-editor.org/rfc/rfc8621.html#section-4)

Retain the task when mail is deleted or inaccessible. Show the cached subject
and a broken or unavailable link. Recovery searches can return several
candidates; relinking is explicit. `thread_id` is a hint, never a replacement
for the particular source message.

`dooit source show ID` can display the message through JMAP without depending
on a provider's browser URLs. `dooit source open ID` uses a configured mail
client or provider adapter; an optional browser URL is a convenience hint.
The file format does not invent a universal browser URL from opaque JMAP IDs.

## Capture from email

The shortest command, with a default store and JMAP profile configured, is:

```sh
dooit from-email EMAIL_ID --tag inbox
```

An explicit invocation is:

```sh
dooit from-email EMAIL_ID --root ~/bushel/dooit \
  --jmap-profile personal --account ACCOUNT_ID \
  --title "Reply about the prototype" --tag ocaml
```

Capture reads only the message metadata needed for the source link and a
default title. The email-client action can supply already-fetched metadata,
so saving the task also works offline. Mail bodies and attachments are not
copied automatically; an explicitly selected excerpt becomes ordinary note
text. Capture does not mark mail read, move it, or send anything.

The email reader's planned **Create task** action passes its selected service,
account, and email ID to the same operation. It returns the task ID and local
path immediately after a durable local write, with `pending sync` when offline.
If a task already exists, the action becomes **Open task**. A successful local
capture is not reported as an uploaded task until WebDAV confirms it.

Default capture means one task per source message within this store. Derive
the task UUID using UUIDv5 with the store UUID as namespace and the UTF-8
encoding of the compact JSON array
`["jmap-email", service, account_id, email_id]` as its name: no whitespace,
literal UTF-8 for non-ASCII characters, `\"` and `\\` for quote and backslash,
and lowercase `\u00xx` escapes for all U+0000–U+001F characters. Do not escape
`/` or normalize opaque IDs. Publish cross-client test vectors.
Also look up exact source links in existing notes: manually linked tasks and
intentional additional tasks must be visible to the capture action.

Repeated capture opens the existing task and does not replace its title,
notes, tags, or completion state. If several tasks already reference the
message, show those tasks. `--new` deliberately creates another task with a
random UUID and the same source link. Done or deleted tasks are shown with
explicit reopen or restore actions; capture does not revive them silently.

Two offline devices performing default capture choose the same filename.
Conditional creation prevents two remote resources. If their initial content
differs, retain both drafts as a creation conflict; there is no common
baseline from which to guess a merge. Choosing the same identity prevents a
duplicate task without discarding either draft.

## Store and local state

Connection settings live in an XDG TOML file resolved using `xdge`:

```toml
# ~/.config/dooit/config.toml
root = "~/bushel/dooit"

[webdav]
url = "https://dav.example.net/files/"
subdir = "personal/dooit"
username = "your-login"
password_file = "webdav.password"
```

The password file is relative to the TOML file, contains the app password on
one line, and must be private. An inline `password` is supported when the
config itself is private; the two forms are mutually exclusive. Requests and
credentials are confined to the configured subdirectory. Paths are resolved
without creating XDG directories during read-only commands. Optional `[jmap]`
settings select the shared JMAP profile, account and service identity.
Credentials never enter task documents or remote files.

```text
~/bushel/dooit/
  store.json                 # synchronized schema and immutable store UUID
  notes/
    <uuid>.md                # synchronized task documents, including soft deletes
  .dooit/                     # private local state; excluded from WebDAV uploads
    sync.json                # durable baselines and pending uploads
    operations.json          # local operation journal and idempotent results
    objects/<sha256>         # immutable raw revisions referenced by the journal
    conflicts/<uuid>/<run>/  # base.md, local.md, remote.md, resolution metadata
```

The initial implementation uses atomically replaced JSON journals referencing
immutable raw objects. Search builds a disposable SQLite FTS5 index in memory
on each invocation. A persistent index is a performance optimization; no
SQLite database is synchronized. Incremental discovery tokens are also a
future optimization; v1 uses complete PROPFIND listings.

The remote collection has only `store.json` and `notes/*.md` in v1. Initialize
`store.json` conditionally once; another device joins by reading it before
creating tasks. A different store UUID is a different store even at the same
URL. Never upload local configuration, credentials, SQLite files, or WALs.

Resource names are canonical lowercase UUIDs and must agree with frontmatter
IDs. Title changes do not rename resources. Duplicate IDs, mismatches,
unrecognized files, and temporary editor files are reported and retained,
not interpreted as task deletion. Ignore symlinks and reject escaping DAV
hrefs. A manually authored new task can be validated and assigned an identity
with `dooit adopt PATH`.

Bind sync state to the store UUID, destination URL, and authenticated remote
principal. Changing servers requires a preview and explicit baseline setup;
an old server's absence information must not imply deletion on a new one.
A fresh clone can reconstruct tasks from remote files. Losing local sync
state with divergent local files requires reconciliation without a baseline,
not an automatic overwrite.

The server holds current documents; it is not assumed to keep revision
history. Local revision objects retain the versions this client observed,
including conflicts. Back up the notes and durable state together. Garbage
collection must retain objects referenced by baselines, pending operations,
and unresolved conflicts. The search index alone is freely disposable.

## Editing by you and by agents

`dooit edit ID` opens a temporary working file in `$EDITOR`. On save it checks
the revision originally read, merges if safe, and commits through the same
store API as agent changes. Short commands cover status, tags, and links.

An agent reads `dooit show ID --json` and receives the structured fields, body,
and `revision`, a SHA-256 hash of the exact file bytes. Mutations carry that
expected revision and a caller-generated operation UUID:

```sh
dooit patch ID --if-revision SHA256 --operation-id UUID --json-file patch.json
```

The versioned patch API exposes operations such as `set_title`, `set_status`,
`add_tag`, `remove_tag`, `add_link`, `remove_link`, and `replace_body`.
Unknown fields are outside a targeted patch and must survive it. A stale
revision returns a structured conflict and current revision; it never
silently replaces the document. A retry with the same operation UUID and
payload returns the recorded result, even if its original revision is stale.
Reuse with different arguments is an error.

Use a per-store local lock for cooperating writers and transactional journal
entries for operation results. Fsync revision objects before referencing
them, then write a temporary note, fsync it, atomically replace the note, and
fsync its directory. Record enough before the replacement to recover after
a crash. Local operation idempotency belongs to this durable client state;
cross-device default email capture uses the deterministic identity above.
Release the local lock while waiting for an editor or a network request;
the recorded revisions and journal phases protect the later commit.

Directly editing `notes/*.md` is supported: scans detect byte changes and
invalid or partially saved documents stay local until valid. An ordinary
editor does not participate in the CLI's lock or revision protocol. A
check-then-rename cannot guarantee against an arbitrary concurrent writer;
use `dooit edit` or the patch API when editing during active synchronization.
Snapshot observed versions before replacement, recheck immediately before
committing, and report detected races. This is an explicit limit of editing
plain files, not a promise that all editors provide compare-and-swap.

Agents receive the same versioned API with capabilities for selected notes
and operations. Task text and linked email are content, not instructions
granting extra tool permissions. A future MCP adapter wraps this API rather
than implementing a second writer. No agent-specific metadata is required to
read or edit a note, and optional actor labels do not establish authority.

## Two-way WebDAV synchronization

Each note has three versions: last reconciled common base `B`, current local
bytes `L`, and fetched remote bytes `R` with the ETag from that same GET.
Modification times and device clocks never choose the winner.

| Situation | Result |
| --- | --- |
| `L = B` and `R = B` | Unchanged. |
| Only `L` changed | Conditional upload. |
| Only `R` changed | Install remotely edited content after a local revision check. |
| `L = R` | Record that version as the common base. |
| Both changed differently | Apply the merge rules below or retain a conflict. |
| No base and both sides exist differently | Creation or baseline conflict; do not guess which is newer. |
| New task on only one side, after complete discovery | Conditionally create it remotely or install it locally. Known removals use the deletion rules below. |

Independent scalar fields can merge. Concurrent changes to the same field
conflict unless their resulting values agree. When only one side changes a
tag list, take that list with its order intact. When both change membership,
merge the changes relative to `B`, so an unchanged peer does not resurrect a
removed tag. After filtering removed tags, preserve the ordering constraints
of both edited lists. Break ties between unrelated concurrent additions
lexically; incompatible order constraints produce a conflict. This preserves
deliberate placement of tags while allowing independent additions to converge.

Links merge by their stable link IDs. Different IDs can change independently;
an edit versus removal of the same link conflicts. Treat each link target
and its hints as one unit initially. Unknown top-level fields use the same
three-way rule, with nested unknown values treated as opaque units.

V1 treats the Markdown body as one field. Concurrent different body edits
produce a conflict, even if a later line-based merge could combine them.
Source comments and other formatting edits must also be accounted for: if
the patcher cannot preserve both changes unambiguously, retain a conflict.
A future diff3 body merger is an optimization, not a prerequisite for safe
two-way sync. Preserve all original bytes alongside every resolution.

Conflicts leave the working note intact and hold that task's upload. The
conflict directory contains all three versions and field-level explanations.
`dooit conflicts` lists them; `dooit resolve ID --file RESOLVED.md` validates a
chosen document and rechecks both local revision and remote ETag. Conflicts
do not block unrelated notes.

### Remote exchange and crash recovery

1. Scan the live local files and record their hashes. Fetch remote changes
   into staging. Prefer optional `sync-collection`; otherwise use complete
   depth-one `PROPFIND` listings of `notes/` and fetch changed resources.
   Drain truncated report pages; restart discovery if the token is rejected.
   Incomplete listings and per-resource errors never imply missing tasks.
   [RFC 6578](https://www.rfc-editor.org/rfc/rfc6578.html)
2. Persist fetched bytes and pending observations before advancing the
   discovery token. Keep the discovery cursor separate from each note's
   common base: a conflict may remain pending across many successful scans.
3. Build a pure reconciliation plan from `B/L/R`. Before each mutation,
   revalidate the local revision and journal the intended bytes, expected
   remote ETag, and operation phase durably.
4. Create with `If-None-Match: *`. Replace only with a strong `If-Match` ETag.
   A `412` causes a refetch and a new merge decision, with bounded retries.
   Missing or weak validators prevent replacement; they never cause an
   unconditional fallback.
   [RFC 9110, section 13.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-13.1)
5. Read back a successful write and retain the bytes and new ETag. Verify
   exact bytes before calling it synchronized. A transformed or concurrently
   changed response is an unresolved observation, not proof of data loss or
   permission to overwrite again. Retain the intended candidate.
   Strong ETags and write verification follow the WebDAV guidance in
   [RFC 4918, section 8.6](https://www.rfc-editor.org/rfc/rfc4918.html#section-8.6).
6. Install the reconciled local version only if its expected revision still
   matches, then commit the new common base and journal completion durably.
   If a local edit arrived during upload, retain the operation's original
   local version, accepted upload, and newer local bytes. Reconcile those
   branches using the original local version as their base before completing
   the local installation. Do not merely record the upload as the common
   base against an unreconciled local file: remote additions could otherwise
   be mistaken for local deletions on the next run. Keep the operation pending
   on conflict and refetch before any further conditional upload.

Writes use single-attempt transport calls. If the connection fails after a
PUT may have reached the server, keep its journal entry and GET before
retrying. Matching intended bytes can complete the operation; an unchanged
original ETag permits another conditional attempt. Anything else is held for
reconciliation with the intended candidate preserved. Crash recovery follows
the same path; it never blindly replays a PUT.

Require HTTPS, confine authenticated requests to the configured collection
and origin, bound responses, and reject unsafe paths. The initial endpoint
probe checks file storage and conditional-write behavior in a dedicated
test collection. Listing support alone is insufficient to enable uploads.
Do not point the two-way engine's working directory at a download-only mirror.

### Deletion

`dooit delete` adds `deleted_at` to the same document; it retains its identity,
body, and links on the server. Hidden tasks remain fetchable tombstones, so a
long-offline device does not mistake deletion for a never-uploaded task.
`dooit restore` explicitly clears that field.

Deletion concurrent with any substantive task edit is a conflict, even if
the modified metadata fields are otherwise independent. A missing file
locally or a hard deletion by another WebDAV client is reported as an
untracked removal, with the last known bytes retained. It neither erases the
other copy nor triggers automatic recreation. Convert the removal to an
explicit tombstone or restore it through resolution.

Do not garbage-collect remote tombstones in v1. Safe permanent deletion needs
a device-retirement and acknowledgement policy; a short retention timer
alone would let old devices resurrect tasks.

### Preview

```sh
dooit sync --root ~/bushel/dooit --dry-run --report /tmp/dooit-preview
dooit sync --root ~/bushel/dooit
```

Preview reads the actual note files and current remote state, then writes
only a new report directory containing a human-readable plan, JSON actions,
diffs, and recovery snapshots. It does not modify notes, baselines, tokens,
operation journals, indexes, or the server. A transport capability allows
GET, HEAD, OPTIONS, PROPFIND, and REPORT and rejects mutations. Applying later
always revalidates against current state; a saved report is not a write token.

## Search and interaction

Index title, Markdown body, exact tags, normalized tag search keys, status,
due date, link types and source identities. Cached email subjects may be
searched, but remote email bodies are not fetched during task search.
Use SQLite FTS5 locally, with dedicated tables for tag and link predicates.
Watch files as a latency improvement; rescan changes before answering a
command so a missed watcher event does not hide an edit.

```sh
dooit search "conditional writes"
dooit list --tag "waiting on Alice" --status blocked
dooit list --source jmap-email --tag ocaml
dooit for-email EMAIL_ID --service SERVICE_URL --account ACCOUNT_ID
dooit done ID
dooit tag add ID "next week"
dooit source show ID
dooit conflicts
```

Human output shows title, status, tags, due date, source, and sync state.
All reads also offer versioned JSON with task IDs and revisions. Search
defaults to tasks without `deleted_at`; done tasks remain searchable. Invalid
notes and conflicts have a visible diagnostic list, never silent omission.
Index recovery rebuilds from files; it does not require network access.

## OCaml implementation

Proposed components, with a single writer and reconciliation implementation:

| Component | Responsibility and existing code |
| --- | --- |
| `Dooit_doc` | Full-fidelity frontmatter/body representation, validation, raw revisions and minimal patches. Build on [frontmatter](../../bleeding/frontmatter/lib/frontmatter.mli), `yamlrw` and `Jsont`; extract useful source-span patching from [Sortal's YAML editor](../sortal/lib/carddav/yaml_edit.ml). `Frontmatter.to_string` alone does not promise comment or source-byte retention. |
| `Dooit_store` | File capabilities, local lock, revision checks, durable operation journal and recovery. |
| `Dooit_merge` | Pure base/local/remote reconciliation, with explicit conflicts and no network effects. |
| `Dooit_dav` | Conditional requests, bounded discovery and remote observations using [Fetch_dav](../../bleeding/fetch/dav/fetch_dav.mli). Its `Mirror` is remote-to-local and must not replace the two-way engine. Its `read_only` excludes REPORT, so construct the preview transport with the required explicit allowlist. |
| `Dooit_link` | Extensible typed targets, exact identity keys and resolver registry. |
| `Dooit_jmap` | Source lookup and capture using [Jmap_eio](../../bleeding/jmap/eio/jmap_eio.mli) and shared [JMAP profiles](../../bleeding/jmap/eio/profile.mli). Select an explicit account or the mail primary account after discovery; a profile does not select an account. |
| `Dooit_index` | Rebuildable local queries using [sqlite3](../../bleeding/sqlite3/lib/sqlite3.mli); [its build enables FTS5](../../bleeding/sqlite3/lib/config/discover.ml). Keep index state separate from durable reconciliation state. |
| `Dooit_cmd` | Cmdliner commands, structured output, editor integration and reports; native `dooit` binary under `avsm/dooit`. |

JMAP reads use POST requests carrying read methods; an HTTP GET-only policy
would break capture. Restrict allowed JMAP methods semantically, following
the existing [read-only email client](../crowthebot/lib/email_client.mli),
without depending on the whole Crowthebot application. Ordinary task sync
does not need JMAP credentials or resolve any source links.

## Delivery and acceptance

1. **Local notes and agent API.** Format codec, preservation, validation,
   create/edit/patch/status/tag/link commands, source-aware editor commits,
   durable revisions and rebuildable search.
2. **Email capture.** Shared JMAP profiles, source resolution, deterministic
   capture and a callable action for the email reader. Keep source links and
   normal task edits available when mail is unavailable.
3. **Two-way WebDAV.** Pure planner and dry mode first, then conditional
   writes, durable recovery, conflict resolution and retained tombstones.
4. **Convenience integrations.** Email-reader button, background sync and
   agent/MCP adapter over the tested library. Body diff3, reminders, recurrence,
   attachments and a browser interface are subsequent extensions.

Before real-server writes, exercise two independent clients against a test
DAV collection and a controllable HTTP fixture:

- Different tasks and different fields converge; same-field and body edits
  retain both versions. Tag removal survives an unchanged peer. Comment-only
  edits and unknown link types survive unrelated patches.
- A stale agent patch cannot overwrite a human save. Retrying an operation
  cannot apply it twice, including after a crash between file and journal writes.
- Concurrent default capture produces one task identity and retains differing
  drafts; `--new` creates a second. The same email ID in a different account
  or service is a different source. Deleted and done tasks are not revived.
- A mailbox move still resolves the source. Missing mail and ambiguous
  Message-ID recovery leave task text and link identities intact.
- A `412`, lost PUT response, server byte transformation, weak ETag, partial
  listing, truncated sync report, rejected token, and crash at each journal
  phase cannot cause an unconditional overwrite or advance an unresolved base.
- A local edit during an upload that includes remote changes retains both
  the new local edit and the remote additions on the following sync.
- An offline client observes a tombstone without recreating the task.
  Deletion versus editing conflicts. Hard removal needs explicit resolution.
- A preview leaves local files and durable state byte-identical and makes no
  HTTP mutation, including when the proposed plan contains uploads.

The first usable release ends with local editing, email capture, search, and
explicit two-way sync. It requires a configured WebDAV file collection; the
design does not assume that the existing Fastmail CardDAV endpoint accepts
Markdown files.
