# Dooit

An OCaml task manager with one Markdown file per task, freeform tags, JMAP
email links, and Git or WebDAV synchronization. Humans and agents use the same
notes and revision-checked writer. Commands read the current files.

## Start

For a new Git-backed store:

```sh
dooit init --root ~/bushel/dooit
dooit task add "Set up shared tasks" --tag inbox
dooit status
dooit sync
```

`init` creates an absent configuration, initializes the store and sets up the
selected backend. Git setup is local. It makes no commit or network request.
The default Git remote is `fork.recoil.org:git/dooit`, using SSH and branch
`main`. Create the empty bare repository on that server before the first sync,
and configure a Git author identity and SSH access.

For an existing remote, configure an absent local root and run `dooit clone`.
Clone preserves the remote store identity. Do not initialize an independent
store against an existing remote.

From the monorepo root, build with
`dune build avsm/dooit/bin/dooit_cli.exe` and use `dune exec -- dooit …` in
place of the installed command.

## Configuration

```sh
dooit config path
dooit config show
dooit config show --json
```

Configuration uses `$XDG_CONFIG_HOME/dooit/config.toml`, normally
`~/.config/dooit/config.toml`. Without a configured root, notes use
`$XDG_DATA_HOME/dooit`, normally `~/.local/share/dooit`. Common flags are
`--config FILE`, `--root DIR` and `--json`. Human output is the default.
Read-only commands do not create configuration or task directories.

A Git configuration is:

```toml
root = "~/bushel/dooit"
backend = "git"

[git]
remote = "fork.recoil.org:git/dooit"
branch = "main"

# Optional existing shared JMAP profile.
[jmap]
profile = "personal"
account = "your-jmap-account-id"
service = "https://mail.example.net/jmap/session"
```

Paths accept `~/`. Relative paths in TOML resolve against the configuration
directory. A command-line `--root` is relative to the invoking directory.

Backends are `git`, `webdav` and `local`. Without `backend`, a configuration
with only `[webdav]` selects WebDAV. Git is otherwise the default. When both
`[git]` and `[webdav]` are present, select `backend` explicitly. `local` keeps
all edits offline and does not fall back to Git or WebDAV.

## Tasks

```sh
dooit task add "Reply to Alice" --tag inbox --due 2026-12-01
dooit task list
dooit task list "conditional writes" --tag ocaml --status open
dooit task list --source jmap-email
dooit task show ID
dooit task edit ID
dooit task update ID --status blocked --add-tag "waiting on Alice"
dooit task update ID --remove-tag "next week" --clear-due
dooit task update ID --title "Revised title" --body-file body.md
dooit task done ID
dooit task update ID --status open
dooit task delete ID
dooit task restore ID
dooit task import existing-task.md
```

`task list [QUERY]` combines listing and full-text search. Filters apply before
results are returned. Unfinished tasks come before done and cancelled tasks,
including in search results. Search relevance is preserved within each group.
Due dates appear in task summaries. Tags retain their spelling and match with Unicode case
folding. Search builds a disposable SQLite FTS5 index in memory.

Each file is `ROOT/notes/UUID.md` with YAML frontmatter and a Markdown body.
Metadata includes `schema: dooit/v1`, immutable identity and creation time,
title, status, optional due date, tags and extensible links. Unknown fields and
untouched formatting survive edits. `task add` accepts `--body-file PATH`.
`task update` can change several fields in one operation. Use `--due YYYY-MM-DD`
or `--clear-due` to set or remove a due date.

`task edit` runs `$EDITOR` on a temporary draft and checks revisions before
saving. A failed or conflicting save retains the draft and prints its path.
Direct editing also works, but external editors do not participate in the
revision protocol. Keep them idle during synchronization.

`task delete` retains a tombstone for synchronization. `task list
--include-deleted` includes these tasks. Do not remove note files manually.
`task import` copies an existing valid Dooit note into the store.

## Synchronization

```sh
dooit status
dooit sync
dooit pull
dooit push
```

`status` reports the selected backend and task counts. For Git, it also shows
local repository state and ahead/behind counts relative to the last fetched
remote branch. It does not contact the remote.

`sync` selects the configured backend. For Git, it commits valid local task
changes, fetches, reconciles history and pushes. An empty remote receives the
first push. Remote-only changes fast-forward. Independent divergent changes
can merge. A conflict or invalid merge is refused, preserving the local commit
for explicit resolution. Dooit does not stash, reset or force-push.

`pull` and `push` are Git-only directional commands. Pull requires clean task
files and fast-forwards only. Push commits valid local changes and refuses a
remote branch that is ahead or has diverged. Use `sync` for ordinary two-way
synchronization. CLI task edits remain local until synchronization is requested.

Only `store.json` and `notes/UUID.md` belong in the Git repository. Private
`.dooit/` journals, cached revisions and conflicts stay local and are excluded
through `.git/info/exclude`. Back them up separately to retain retry history.
Different store identities, invalid notes and hard removals are refused.
External Git clients and editors must not modify the repository during sync.

### WebDAV

```toml
root = "~/bushel/dooit"
backend = "webdav"

[webdav]
url = "https://dav.example.net/files/"
subdir = "personal/dooit"
username = "your-login"
password_file = "webdav.password"
```

Put the app password on one line in `~/.config/dooit/webdav.password`, then
`chmod 600` that file. Alternatively use `password = "your-app-password"` in
a configuration owned by you with mode 0600. Configure exactly one form.
Credentials are omitted from `config show`, reports and synchronized files.

The URL must use HTTPS and end in `/`. `subdir` is a relative directory path.
Use a dedicated WebDAV file collection whose parent exists, not a CardDAV
address book. Requests stay within that collection and do not follow redirects.

```sh
dooit init
dooit maintenance preview --report /tmp/dooit-preview
cat /tmp/dooit-preview/report.md
dooit sync
```

WebDAV previews write snapshots only to a new report directory. Omit `--report`
to print the plan. Review items produce a nonzero exit status. On another
machine, configure the same collection and an empty root, then run
`dooit clone` to preserve its identity.

Sync compares the common base with both versions. Independent fields, tag
membership and separate links can merge. Concurrent body or same-field edits,
deletion-versus-edit and ambiguous formatting changes produce conflicts.
Neither clock time nor arrival order chooses a winner.

Creates use `If-None-Match: *`. Replacements require a strong ETag and
`If-Match`. A `412` refetches and replans with bounded retries. Uncertain
uploads are journaled and read back before retrying. Successful writes are
verified byte for byte. Local edits arriving during uploads are preserved.

## Email

Dooit uses shared JMAP profiles at `~/.config/jmap/profiles`, or its XDG
location. Task storage and JMAP credentials can belong to different services.

```sh
dooit email capture EMAIL_ID --tag inbox
dooit email capture EMAIL_ID --jmap-profile personal --account ACCOUNT_ID \
  --title "Reply about the prototype" --tag ocaml
dooit email show TASK_ID
```

Capture uses `Email/get` for source metadata. It does not mark mail read, move
it or send anything. `email show` fetches the linked email's plain text,
bounded to 256 KiB per part with truncation markers. Missing mail leaves the
task intact.

A caller with the source identity can capture offline:

```sh
dooit email capture EMAIL_ID --offline \
  --service https://mail.example.net/jmap/session --account ACCOUNT_ID \
  --title "Reply about the prototype" --tag inbox
dooit email find EMAIL_ID \
  --service https://mail.example.net/jmap/session --account ACCOUNT_ID
```

`--body-file` supplies an optional excerpt. Links store service, account and
email IDs separately, plus subject, thread and Message-ID hints when available.
`jmap.service` binds the namespace to the profile. A mismatched binding is
refused. Other links can have type `url`, `dooit` or a namespaced extension.

Repeated capture returns existing linked tasks, including done or deleted
tasks, without replacing or reopening them. `--new` deliberately creates
another task. Default capture on two devices chooses the same ID. Different
initial drafts become a creation conflict with both retained.

## Agent edits and retries

The `dooit.agent` library exposes `dooit_search`, `dooit_get`, `dooit_create`
and `dooit_update` as Agentkit tools. `Dooit_agent.create` takes an existing
store, Git settings, an Eio process manager and a response byte budget. Hosts
supply an authority check and a stable operation UUID per tool call. Reads are
local and paginated. Git writes pull first, check the revision against the
updated store, then commit and push under one store lock. Initialize and sync
the store once, or clone it, before enabling these agent writes.

A failed push reports the locally saved task ID and revision without claiming
remote success. Retrying the same host operation resumes pending sync without
applying the edit twice. Completed retries return their original result.
Do not repeat a failed creation with a new operation UUID. Inspect the existing
ID and resolve the synchronization failure. Pending retries refuse unrelated
local edits. `create_local` explicitly selects offline writes.

Crow discovers the configured store at startup and checks account and room
permissions around each invocation. Restart Crow after initial setup. Git
writes synchronize automatically. With `backend = "local"`, writes report
`sync=local_only`. Crow can read WebDAV-backed local tasks, but rejects writes
because its WebDAV agent adapter is not implemented. It never silently switches
backends, initializes a store or migrates memory tasks. Dooit holds task state.
Crow memory holds background context and references. Cron remains for scheduled
actions and reminders. Termanil reads the same configured local store.

For direct revision-checked CLI writes, obtain the revision with
`dooit task show ID --json`:

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
dooit task patch ID --if-revision SHA256 --operation-id OPERATION_UUID \
  --json-file patch.json --json
dooit task add "Follow up with Alice" --operation-id OPERATION_UUID --json
```

Patch operations are `set_title`, `set_status`, `set_due` (date or null),
`replace_body`, `add_tag`, `remove_tag`, `add_link` and `remove_link`. Each has
a `value`. For `remove_link`, this is the link ID.

Stale revisions fail. Reusing an operation UUID with identical arguments returns
its original result even after later task changes. Different arguments are
refused. The operation journal is local to that store copy. Email capture also
has the cross-device identity rule.

## Maintenance

```sh
dooit maintenance conflicts
dooit task show ID --json
dooit maintenance resolve ID --if-revision CURRENT_SHA256 --file reviewed.md
dooit maintenance recover
```

WebDAV conflict versions live in `ROOT/.dooit/conflicts/ID/RUN/`. Resolution
fetches the current ETag and checks the local revision again. Use
`--if-revision absent` to restore a manually removed local file. A resolution
containing `deleted_at` can turn a hard removal into a retained deletion.

`recover` finishes interrupted local operations whose revisions still match
and reports any needing review. `.dooit/sync.json` holds WebDAV baselines and
pending uploads. `operations.json` holds local operations. Immutable raw
revisions live in `.dooit/objects/`. Journals are replaced atomically and writes
are fsynced. Back up this private state with notes to retain retry history and
unresolved versions. It is not uploaded to the remote store.

## Tests

```sh
dune runtest avsm/dooit/test avsm/dooit/test_agent avsm/dooit/test_git
dune runtest bleeding/xdge/test
```

Tests use synthetic notes, temporary directories, local Git repositories and a
WebDAV fixture with conditional writes and injected failures. They do not use
real credentials or contact exports. Future extensions are in
[DESIGN.md](DESIGN.md).
