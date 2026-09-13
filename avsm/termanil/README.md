# Termanil

A Bonsai terminal workspace for JMAP mail, Sortal and CardDAV contacts, and
Dooit tasks. All application code is OCaml.

From the monorepo root:

```sh
dune exec -- termanil --demo
dune exec -- termanil --init-config
```

Demo mode starts an embedded fake JMAP server with nine synthetic messages in
six conversations, eight contacts, two linked tasks and a saved reply. It uses
the production JMAP adapter and remembers flags, accepted replies, tasks and
drafts across restarts. It reads no live configuration or credentials and makes
no network connections. State lives in `$XDG_DATA_HOME/termanil/demo`
(default `~/.local/share/termanil/demo`). For a fresh, isolated prototype:

```sh
dune exec -- termanil --demo-dir "$(mktemp -d)"
```

The desert-inspired palette uses charcoal, warm sand, sky blue and pale green.
Text badges distinguish tasks, drafts, queued replies and sent replies as well.

Edit the generated `~/.config/termanil/config.toml`, then run:

```sh
dune exec -- termanil
```

`--config FILE` selects another configuration. XDG paths are resolved by
`xdge`. Relative paths in TOML are relative to the configuration file. `~/`
expands to your home directory. Missing sections disable that integration.

## Configuration

```toml
[mail]
profile = "personal"
# account = "account-id"  # otherwise use the primary mail account
# service = "https://mail.example.net/jmap/session"
# identity = "identity-id"  # required if the account has several sending identities
# signature = "-- \nAnil"  # override identity signature, or "" to disable
# draft_root = "~/bushel/replies"  # default: $XDG_DATA_HOME/termanil/replies

[contacts]
vcard_root = "~/.local/share/sortal"

# Optional. These contacts appear alongside the local vCards.
[contacts.carddav]
url = "https://carddav.fastmail.com/"
username = "your-login"
password_file = "carddav.password"
# collection = "https://carddav.fastmail.com/path/to/addressbook/"

[dooit]
config = "~/.config/dooit/config.toml"
# root = "~/bushel/dooit"  # optional override of Dooit's configured root
capture_tags = ["inbox"]
```

Remove the `contacts.carddav` table if you only want local contacts.
CardDAV requires HTTPS. Its password file contains one app password line,
is owned by you, and has mode 0600. Credentials and redirects are confined
to the configured server origin. If address book discovery is ambiguous,
set `collection` to the full URL of the intended book.

Mail uses the shared JMAP profile store. Existing `jmap` and `jmap-mosaic`
profiles work unchanged. To create one from a private token file:

```sh
dune exec -- bleeding/jmap/examples/0-profiles/profiles.exe save personal \
  --url https://mail.example.net/jmap/session \
  --auth bearer --api-key-file ~/.config/jmap/token
```

See the [profile example](../../bleeding/jmap/examples/0-profiles/README.md)
for authentication and account setup. `mail.service` optionally fixes the
identity namespace used by Dooit email links. It defaults to the profile's
session URL. Keep it consistent with existing Dooit links.

Dooit reads its existing XDG TOML configuration, including the WebDAV app
password and configured subdirectory. See [Dooit setup](../dooit/README.md).
Capturing a task can initialize an empty local store. To use an existing
remote store on a new machine, run `dooit clone` first.

## Keys

| View | Key | Action |
|---|---|---|
| All, including reply editing | Tab / Shift-Tab | Next / previous tab, retaining selection, reader position and editor cursor |
| Outside the reply editor | `1` / `2` / `3` / `4` | Mail / contacts / Dooit / outbox |
| Lists and opened details | `j` / `k`, arrows | Select the previous or next item |
| All | Enter / Esc | Open / return |
| All | `/`, then Enter | Search |
| All | `r` / `?` | Refresh / help |
| All | `q` / Ctrl-C | Quit after the current request finishes |
| Opened mail | Up / Down, `k` / `j` | Read previous / next message |
| Opened mail | `e` | Focus the reply editor |
| Reply editor | Arrows / Enter | Move cursor / insert newline |
| Reply editor | Esc | Return to the reader |
| Reply editor | Ctrl-S | Save a durable reply, including an empty scaffold for an agent |
| Mail | `b` | Choose mailbox |
| Mail | `]` / `[` | Next / previous page |
| Mail | `u` / `f` | Toggle read flag / star |
| Mail | `a` | Archive the selected email, retaining other mailbox memberships |
| Opened mail | `H` | Toggle fuller headers, including Message-ID, Reply-To and received date |
| Mail | `t` / `g` | Capture or open the associated task / jump to its task |
| Opened mail | `h` | Expand or collapse conversation history |
| Reader | `Q` / `S` | Queue a saved reply / review and send it now |
| Outbox | Enter | Open the reply’s source email |
| Outbox | `Q` | Queue or unqueue the selected reply |
| Outbox | `S`, then Enter | Review and send all queued replies |
| Outbox | `r` | Reload draft files after agent or external edits |
| Outbox | `v` | Verify uncertain submissions without retrying |
| Mail | `p` | Find sender in contacts |
| Dooit | `d` | Complete selected task |
| Dooit | `o` | Read the first linked email |
| Dooit | `s` | Preview WebDAV sync without writes |
| Dooit | `S`, then Enter | Apply two-way WebDAV sync |
| Details | PageUp / PageDown | Scroll ten lines |

At 100 columns the client shows a list and detail pane together. Smaller
terminals switch between them. The minimum usable size is 30 by 7.
Arrows continue selecting tasks after following a task link. PageUp/PageDown
scroll the opened task or contact. Help and reports use arrows to scroll.
Bracketed paste inserts text into search or the focused reply editor.
Tab and Shift-Tab switch views even inside search, help and send review.
Switching cancels a pending search or review. Returning restores the tab's
selection and reader or editor focus. Pasted tabs remain text in the editor.
Dooit shows the most recently modified notes first, using local file times so
direct human and agent edits appear on refresh. Completion keeps the same
task selected as its position changes. A WebDAV download also updates the
local file time, so this ordering is local recency rather than remote edit history.

Reading mail does not change its read flag. Search uses JMAP filters in the
chosen mailbox and collapses results by conversation. Free text combines with
`from:`, `to:`, `cc:`, `subject:`, `body:`, `is:unread`, `is:read`,
`is:starred`, `has:attachment`, `before:YYYY-MM-DD` and `after:YYYY-MM-DD`.
Conditions combine with AND. Quote values containing spaces, for example
`subject:"garden plan" from:ada is:unread`. Dates are midnight UTC, with
inclusive `after` and exclusive `before` boundaries.
Text in older messages is searchable. Enter reads the selected match and
loads its conversation, with other messages collapsed. `h` expands their
bodies. Conversation reads retain the selected message and up to 49 recent
others, bounded by the server's object limit. The initial mailbox is Inbox when one
is advertised, otherwise all mail. Results are paginated in groups of up
to 50. Text body parts are limited to 256 KiB each, with truncation and encoding
problems shown explicitly. Plain text takes precedence. HTML-only messages
are converted to text with paragraphs and link targets, without fetching
images or other remote resources. The reader shows recipients, dates and
attachment names, media types and sizes. `H` reveals additional headers.
Attachment downloads and browser rendering are not implemented.

`a` archives the selected email with a conditional JMAP patch that removes
Inbox membership and adds Archive, preserving other mailboxes and flags.
Other emails in the conversation retain their memberships. The server must
advertise unique Inbox and Archive mailboxes. There is no deletion or automatic
retry after a failed write.

`t` uses the message's service, account and email ID. Capturing it again
returns the existing task, including when it is already completed.
The configured tags are applied only on first capture. Message bodies are
not copied into task notes. Completion uses the revision displayed by the
client. If an editor or agent changed it, refresh before trying again.

Point the contacts view at the native Sortal store:

```toml
[contacts]
vcard_root = "~/.local/share/sortal"
```

Refresh reads the live `cards/*.vcf` files and displays names, emails and
labelled metadata, including Sortal IDs, feeds and unknown fields. Contact
search includes metadata. No export is needed. `contacts.sortal_root` is
accepted as an alias for the native store path.

Contacts are read-only in Termanil. Equal names or email addresses remain
separate source identities. See the [CardDAV workflow](../sortal/spec/carddav-migration.md)
for native snapshots, dry runs and synchronization limits.

An opened email includes a Bonsai reply editor. `e` focuses it. Ordinary
keys insert text, arrows move the cursor, Enter inserts a newline and Ctrl-Z
undoes. Esc returns to the reader. Each message retains its cursor and undo
history across navigation and resize. Ctrl-S saves a durable Markdown reply.
Quitting only asks about unsaved edits.

New reply buffers use the configured JMAP sending identity's `textSignature`,
falling back to text extracted from `htmlSignature`. Set `mail.signature` to
override it, including an empty string to disable signatures. TOML multiline
strings work for longer signatures. The signature appears in the editor and
saved Markdown body before send review. It is inserted once for a new buffer,
never appended during sending or added to an existing saved draft. An untouched
signature template does not trigger an unsaved edit prompt. Identity lookup
errors leave mail readable and are reported when entering a new reply.

## Reply workflow and agents

1. Open an email. `t` captures a task, `g` opens its linked task, and `o` in
   Dooit returns to the email. A task badge follows the conversation using
   the thread hint stored in new Dooit links. Older tasks still match their
   exact source email.
2. `e` opens the editor. Write a reply and press Ctrl-S. You can also save an
   empty scaffold for an agent to fill in later.
3. Esc returns to the reader. `S` reviews and sends this saved reply now, or
   `Q` queues it for later. Sending always requires Enter on the review screen.
4. `4` opens the outbox. Review saved bodies and recipients, use `Q` to queue
   individual replies, then `S` to review the queued batch. PageUp/PageDown
   scroll the complete review. Esc cancels without sending.

Replies are files under `mail.draft_root`, defaulting to
`$XDG_DATA_HOME/termanil/replies` (`~/.local/share/termanil/replies`). The outbox
shows each path. An agent can edit the Markdown body below the YAML
frontmatter. Preserve the source, thread, subject and recipient fields. Unknown
frontmatter and comments survive saves. The files identify the JMAP service,
account and source email, so an agent can fetch the original conversation.
For example, the worker accepts a read-only request of this form:

```sh
printf '%s\n' '(termanil/v4(Conversation((service "https://mail.example.net/jmap/session")(account "account-id")(id "email-id"))))' | termanil-worker
```

Use the actual source fields from the reply file, and pass `--config FILE`
when needed. For synthetic messages, pass `--demo` or the same `--demo-dir DIR`.
A useful agent instruction is: “Read these source conversations, revise the
reply bodies, and leave them for me to review. Preserve the frontmatter.”

Return to the outbox and press `r` to load agent edits. A changed file loses
its queued status. Clean editor buffers reload, while unsaved typing is
retained. A stale editor save or send review is refused. If you have both
unsaved typing and an external edit, retain your text elsewhere before
quitting and reopening to load the file.

Queue decisions and send receipts are separate `*.receipt.json` files bound
to the Markdown byte revision. Do not edit or remove receipts to retry a send.
Sending preflights the entire batch, creates a JMAP draft Email, and submits
it with the configured identity. Reply-To takes precedence over From, and
Message-ID references preserve threading. Successful submissions move the
Email to Sent. If there are several sending identities, set `mail.identity`.
This is a plain-text reply to the sender, without attachments or reply-all.

An interrupted submission stops the batch and remains `uncertain`, with the
remote Email id recorded when available. It is never replayed automatically.
Press `v` in the outbox to look up the recorded remote Email id. A matching
accepted submission resolves the receipt to `sent`. If no acceptance is
found, it remains uncertain and needs investigation on the server. No retry
is made.
`sent` means the JMAP server accepted the submission, not confirmed delivery.
Drafts are local files until sending. They are not synchronized to the
server's Drafts mailbox during editing.

Task sync uses Dooit's existing conflict detection and recovery journal.
`s` creates no report files or sync state. `S` recomputes the plan against
current files and ETags. Conflicts remain visible in the result and retained
in Dooit's recovery directory. Refresh the task list after synchronization.

Attachment downloads, contact editing, background sync and offline live-mail
caching remain future additions. The persistent demo is isolated from live mail.

## Development

Run the keyboard regression harness without credentials or a live server:

```sh
dune runtest avsm/termanil/test/keyboard --force
```

It sends real `Bonsai_term.Event.Key_press` events through the application and
its input queue. Tests check selected email IDs, saved reply bodies and rendered
screens. They cover task/email round trips, arrows and `j`/`k`, scrolling,
collapsed conversation links, filtering, resize and rapid input across the
reply editor boundary. Add a sequence to
[`test/keyboard/test_keyboard.ml`](test/keyboard/test_keyboard.ml) when changing
navigation. Review failures before promoting expected output.

Run the complete suite and build with:

```sh
dune build @avsm/termanil/all
dune runtest avsm/termanil --force
dune exec -- termanil --check-worker
```

[DESIGN.md](DESIGN.md) describes library boundaries, identities, failure
handling and testing. The installed Bonsai terminal examples are the API
reference for the UI. The implementation follows their `Bonsai.state_machine`,
`View.With_handler` and `Bonsai_term_test.create_handle` patterns.

The Bonsai frontend starts `termanil-worker` as a sibling executable.
`dune exec` builds both automatically. Install both executables together.
`--worker PATH` overrides the sibling lookup for development.
