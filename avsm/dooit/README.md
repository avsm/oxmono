# Dooit

An OCaml task manager with one Markdown file per task, freeform tags, JMAP
email links, and two-way synchronization to a WebDAV file collection. Humans
and agents use the same notes and revision-checked writer. No export step is
needed: commands always read the current files.

## Configure and start

From the monorepo root:

```sh
dune build avsm/dooit/bin/dooit_cli.exe
dune exec -- dooit config init --root ~/bushel/dooit
dune exec -- dooit config path
```

Edit the generated `~/.config/dooit/config.toml`:

```toml
root = "~/bushel/dooit"

[webdav]
url = "https://dav.example.net/files/"
subdir = "personal/dooit"
username = "your-login"
password_file = "webdav.password"

# Optional: an existing shared JMAP profile for your mail account.
[jmap]
profile = "personal"
account = "your-jmap-account-id"
service = "https://mail.example.net/jmap/session"
```

Put the WebDAV app password on one line in
`~/.config/dooit/webdav.password`, then `chmod 600` that file. Alternatively,
replace `password_file` with `password = "your-app-password"` in the TOML and
keep the config mode 0600. Configure exactly one form. Password files and
configs containing inline passwords must be regular files owned by you with
no group or other permissions. Credentials are omitted from `config show`,
reports and synchronized files.

Paths accept `~/`; relative paths are resolved against the config directory.
The WebDAV URL must use HTTPS and end in `/`. `subdir` is a decoded relative
directory path: spaces are encoded automatically; absolute paths and dot
segments are refused.

The example confines authenticated requests to
`https://dav.example.net/files/personal/dooit/`. Dooit can create that final
collection and its `notes/` child; its parent must already exist. It never
creates parent directories outside the configured scope or follows redirects.
Use a dedicated empty WebDAV **file** collection. A CardDAV address book URL
is not a place to upload Markdown.

Configuration uses `xdge`: `$XDG_CONFIG_HOME/dooit/config.toml`, defaulting
to `~/.config/dooit/config.toml`. Without a configured `root`, notes use the
`xdge` data directory, normally `~/.local/share/dooit`. `--config FILE` and
`--root DIR` override these choices. The library's application-specific XDG
overrides also apply. Read-only commands resolve paths without creating XDG
directories.

For a new store:

```sh
dune exec -- dooit init
dune exec -- dooit new "Test WebDAV writes" --tag ocaml --tag "next week"
dune exec -- dooit list
dune exec -- dooit sync --dry-run --report /tmp/dooit-preview
cat /tmp/dooit-preview/report.md
```

Preview writes only a new report directory, containing private note snapshots.
It does not create remote collections, upload or edit notes, or advance sync
state. Use a fresh report directory each time, or omit `--report` to print
the plan. Review items give a nonzero exit status after the plan is printed.
Apply with `dune exec -- dooit sync`.

On another machine, configure the same remote collection and an empty local
`root`, then run `dooit clone`. Cloning retrieves the existing store identity;
do not initialize a second independent store against the same collection.
Later `dooit sync` runs exchange edits in both directions.

The following examples use an installed `dooit`; `dune exec -- dooit …` works
identically from the monorepo root.

## Notes and search

```sh
dooit show ID
dooit edit ID
dooit tag add ID "waiting on Alice"
dooit tag remove ID "next week"
dooit status ID blocked
dooit done ID
dooit reopen ID
dooit search "conditional writes"
dooit list --tag ocaml --status open
dooit list --source jmap-email
```

Each file is `ROOT/notes/UUID.md` with YAML frontmatter and a freeform Markdown
body. Metadata includes `schema: dooit/v1`, immutable identity and creation
time, title, status, optional due date, tags and extensible links. Unknown
fields and untouched formatting survive edits to known fields. New notes
accept `--due YYYY-MM-DD` and `--body-file PATH`. `dooit adopt PATH` copies an
existing valid Dooit note into the store.

`dooit edit` runs `$EDITOR` on a temporary draft and checks the original
revision before committing. Failed or conflicting saves retain the draft and
print its path. Direct file editing works too; use the editor command when
sync or other writers are active, since arbitrary editors do not participate
in the revision protocol.

Search reads the live notes and builds a disposable SQLite FTS5 index in
memory. Tags remain exactly as typed; matching supports Unicode case folding.
`dooit delete ID` hides a task while retaining its content for synchronization;
`dooit restore ID` restores it. `list --include-deleted` includes retained
deletions. Removing files manually produces a review item.

## Mark an email as a task

Dooit reads shared JMAP profiles at `~/.config/jmap/profiles`, or its XDG
location. The WebDAV password and JMAP profile may belong to different services.

```sh
dooit from-email EMAIL_ID --tag inbox
dooit from-email EMAIL_ID --jmap-profile personal --account ACCOUNT_ID \
  --title "Reply about the prototype" --tag ocaml
dooit source show TASK_ID
```

Capture uses `Email/get` for source metadata. It does not mark mail read, move
it or send anything. `source show` explicitly fetches the linked email's plain
text, bounded to 256 KiB per part, with visible truncation markers. Missing
mail leaves the task intact.

An email reader or agent that already has the identity can capture offline:

```sh
dooit from-email EMAIL_ID --offline \
  --service https://mail.example.net/jmap/session --account ACCOUNT_ID \
  --title "Reply about the prototype" --tag inbox
dooit for-email EMAIL_ID \
  --service https://mail.example.net/jmap/session --account ACCOUNT_ID
```

`--body-file` supplies an optional selected excerpt. Source links store the
service, account and email IDs separately, plus subject, thread and Message-ID
hints when available. `jmap.service` binds the identity namespace to the local
profile; resolving through a mismatched binding is refused. Other links can
have type `url`, `dooit`, or a namespaced extension with arbitrary target data.

Repeated capture returns existing linked tasks, including done or deleted
tasks, without replacing their content or reopening them. `--new` deliberately
creates another task. Default capture on two devices chooses the same ID;
differing initial drafts become a creation conflict with both retained.

## Agent edits and retries

`dooit show ID --json` returns metadata, body and the SHA-256 `revision`.
Supply that revision with a versioned patch and a fresh operation UUID:

```json
{
  "schema": "dooit.patch/v1",
  "operations": [
    {"op": "set_status", "value": "blocked"},
    {"op": "add_tag", "value": "waiting on Alice"}
  ]
}
```

```sh
dooit patch ID --if-revision SHA256 --operation-id OPERATION_UUID \
  --json-file patch.json --json
dooit new "Follow up with Alice" --operation-id OPERATION_UUID --json
```

Operations are `set_title`, `set_status`, `set_due` (date or null),
`replace_body`, `add_tag`, `remove_tag`, `add_link` and `remove_link`.
Each carries a `value`; for `remove_link` it is the link ID. Remove and add a
link in one patch to replace its target.

Stale revisions fail. Repeating an operation UUID with the identical request
returns its original result without reapplying the edit, even after further
task changes. Different arguments with the same UUID are refused. Creation
supports the same mechanism. Operation history is local to that store copy;
email capture additionally has the cross-device identity rule.
`dooit recover` finishes interrupted local operations whose revisions still
match and reports any that need review.

## Synchronization and conflicts

Sync compares the common base with both current versions. Independent fields,
tag membership and separate links can merge. Different concurrent body edits,
same-field edits, deletion-versus-edit, and ambiguous formatting changes
produce conflicts. Neither clock time nor arrival order picks a winner.

Creates use `If-None-Match: *`. Replacements require a strong ETag and
`If-Match`. A `412` refetches and replans with bounded retries. Uncertain
uploads are journaled and read back before retrying; successful writes are
verified byte for byte. Local edits arriving during uploads are preserved.

```sh
dooit conflicts
dooit show ID --json
dooit resolve ID --if-revision CURRENT_SHA256 --file reviewed.md
```

Conflict versions and explanations live in `ROOT/.dooit/conflicts/ID/RUN/`;
`current` names the active run. Resolution fetches the current ETag and checks
the local revision again. Use `--if-revision absent` when restoring a manually
removed local file. A resolution containing `deleted_at` can convert a hard
removal to a retained deletion.

`.dooit/sync.json` holds baselines and pending uploads; `operations.json` holds
local operations. Immutable raw revisions live under `.dooit/objects/`.
Journals are replaced atomically; objects, notes and directories are fsynced.
This private state is never uploaded. Back it up with notes to retain local
history and unresolved revisions. The server holds current documents; server
revision history is not assumed.

## Status and checks

The first version implements the local CLI, agent patches, full-text search,
JMAP capture/source reading, and explicit two-way WebDAV sync. Discovery uses
complete `PROPFIND` scans. Background sync, a graphical email action, an MCP
adapter, incremental sync tokens, body diff3, recurrence and reminders remain
extensions in [DESIGN.md](DESIGN.md).

```sh
dune runtest avsm/dooit/test
dune runtest bleeding/xdge/test
```

Tests use synthetic notes, temporary directories, and a DAV fixture with
conditional writes and injected failures. No real account credentials or
contact exports are test inputs.
