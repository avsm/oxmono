# Siftbot

Siftbot imports JMAP mail into a local SQLite corpus and reviews independent
area, triage, delivery and attention assignments. It never writes to JMAP.
The implementation status and remaining work are recorded in
[DESIGN.md](DESIGN.md).

## Build and demo

```sh
opam exec --switch=5.2.0+ox -- dune build @u/avsm/siftbot/all
opam exec --switch=5.2.0+ox -- dune runtest u/avsm/siftbot --force
opam exec --switch=5.2.0+ox -- dune exec siftbot -- review --demo
```

The demo persists under `$XDG_DATA_HOME/siftbot/synthetic-demo`, falling back
to `$HOME/.local/share/siftbot/synthetic-demo`. It contains teaching, research,
social mail, list discussion, an announcement, an automated reminder, spam,
an ambiguous collaboration, duplicate delivery and a suspicious message in
an otherwise legitimate conversation. Demo commands never load credentials
or contact a model provider.

The message list sits beside a wider reading pane. `j` and `k` select messages.
PageUp and PageDown scroll the message. Coloured labels show the saved
classification in a fixed column beside the subject and sender. `t` groups the
current matches by thread, with the most recently active threads first and
newest messages first within each thread. Counts refer to loaded matches
(up to 100), not the full conversation. Press `t` again for the original
search order. `e` shows the reasoning behind each label. `g` opens optional
topics in the reading pane, where `j` and `k` select a topic. Press `g` or
Escape to return to the message. Tab switches focus between panes. `/` searches
with SQLite FTS5 syntax. `r` refreshes. `a` previews approval of the selected
area, `n` renames it, and `x` archives it. `v` assigns the selected area to
the current message. Keys `1` through `4` preview relevant, social, spam and
uncertain triage. `u` previews undo. Enter commits after an explanation has
been entered. Escape cancels. `c` opens a model discussion in configured live
profiles. `p` opens connection profiles. `q` quits when no operation is pending.

Press `l` to correct attention or delivery. Choose a label, then `m` for this
message, `s` for this sender, or `l` for this mailing list. Review the exact
match, type an explanation, and press Enter to save. For a Zulip notification,
use `l`, `1`, `s` to mark mail from that sender as no reply / information.
Repeat with `l`, `6`, `s` to mark its delivery as automated.

Sender and List-ID rules apply immediately to existing and future mail in the
same account. They preserve model results and do not require classification to
run again. A message correction wins over a sender rule, which wins over a list
rule. `u` previews undo, including removal or replacement of a saved rule.
Rules use exact case-insensitive identities, not substrings or domains. A
no-reply label does not decide whether a notification needs action in Zulip.

Every manual message correction and saved sender/list rule also enters a
feedback store with its explanation, source context and previous predictions
(where available).
Superseded or undone corrections stop influencing classification. Existing
confirmed labels and rules are imported on first use. Untouched predictions
never become trusted feedback.

For each new email, Sift retrieves up to eight relevant corrections from the
whole store, using sender, List-ID and text similarity. If the profile has
`embeddingUrl`, `embeddingModel` and `embeddingDimensions` configured, it also
uses semantic similarity. Feedback embeddings are cached locally. Without an
embedding endpoint, sender/list/text retrieval still works. The message itself,
its thread and duplicate content are excluded from its examples.

Press `e` in review to see which corrections were supplied to the classifier
and why they were selected. This records prompt context, not proof that the
model followed a particular example. Adding feedback does not automatically
rerun completed classifications. To revisit existing mail selectively:

```sh
siftbot classify --profile mail-sixmonths --all --refresh-feedback
```

This scans the corpus but calls the classifier only where the relevant example
set changed. Explicit rules still override model predictions immediately.
No model weights are retrained. The evaluation retrieval mode also excludes
sources later than the held-out email.

## Live profile

Open the connection form with `siftbot setup --profile NAME`. A missing profile
opens setup automatically. The picker uses `j`/`k` to select, Enter to open,
`n` to create and `e` to edit. Tab or Enter advances through the form.
Shift-Tab or Up moves to the previous field. Down moves to the next field.
Left/Right, Home/End, Backspace and Delete edit within a field. Ctrl-U clears it.
The selected field stays visible in a small terminal. Enter on **Save profile**
saves. Escape cancels. Ctrl-C always quits, including during a request or paste.
Setup requests time out after ten seconds. Exiting stops outstanding helpers and
restores the terminal. `review --setup` also opens the same form.

Set the Siftbot profile name, shared JMAP profile name, HTTPS session URL,
bearer access token, optional account ID and optional chat model. The account
ID defaults to the primary mail account. Existing profile names are fixed.
Tokens are masked and never read back into the UI. A blank token keeps the
shared credential only if the URL is unchanged. Changing a shared JMAP login
also affects other applications using that login. Advanced JSON settings are
preserved when the form is saved.

Setup makes no network requests. Opening another profile starts a fresh review
screen for its separate corpus. Open setup again with `p`. Saving does not
import mail. Review can display already imported messages while sync is
incomplete. Run `sync` to populate the selected profile:

```sh
siftbot setup --profile personal
siftbot sync --profile personal --since 1y
```

For a fresh six-month corpus, choose a new profile name:

```sh
dune exec siftbot -- setup --profile mail-sixmonths
dune exec siftbot -- sync --profile mail-sixmonths --since 6m
dune exec siftbot -- discover --profile mail-sixmonths
dune exec siftbot -- review --profile mail-sixmonths
```

In setup, use `mail-sixmonths` as the Siftbot profile and a dedicated shared
JMAP profile such as `sift-readonly`. Supply a read-only mail bearer token.
Leave the chat model blank to start with local import, search and baseline
discovery. `sync` makes JMAP reads but no model requests. It runs until the
interval, thread context and change catch-up are complete. The terminal shows
phase progress, counts and elapsed time. Ctrl-C stops safely. Run the same
command to resume. `--no-progress` uses plain status lines. `--max-steps N`
explicitly limits one invocation. `learn` remains an alias for `sync`. If the
shared JMAP profile already
exists, use `siftbot init --profile mail-sixmonths --jmap-profile sift-readonly`
instead of creating credentials through setup.

To reuse an existing shared JMAP profile from the command line:

```sh
siftbot init --profile personal --jmap-profile personal
siftbot sync --profile personal --since 1y
siftbot inspect --profile personal
siftbot discover --profile personal --cutoff 2026-07-01T00:00:00Z
siftbot review --profile personal
siftbot evaluate --profile personal --cutoff 2026-07-01T00:00:00Z
```

Configuration is `$XDG_CONFIG_HOME/siftbot/NAME/profile.json`, falling back to
`$HOME/.config/siftbot/NAME/profile.json`. The database is
`$XDG_DATA_HOME/siftbot/NAME/corpus.sqlite3`. Data directories require mode
0700. Source bodies, transcripts and audit records belong to this private
profile. Credentials remain in `$XDG_CONFIG_HOME/jmap/profiles` (or the
corresponding HOME fallback), with files mode 0600 and directories mode 0700.
Review shows the latest 100 matching messages with their classification labels
and loads the selected body on demand. Bordered panes highlight the current
selection and show its position. An animated activity bar indicates pending
requests. Use `/` to search the full corpus and Ctrl-C to exit while loading.
Bodies above 256 KiB are shortened for display and marked truncated. Full
imported sources remain in the database. Terminal status logs contain identifiers
and outcomes.

The JSON configuration fields are:

| Field | Meaning |
| --- | --- |
| `name` | Profile name, matching its directory. |
| `jmapProfile` | Shared JMAP credential profile. |
| `account` | Explicit account ID, or null for its primary mail account. |
| `sortalRoots` | Read-only Sortal store roots used by discussion tools. |
| `bodyLimit` | Maximum bytes per JMAP body value, default 65536, maximum 1048576. |
| `maxSteps` | Classification and embedding batch budget, default 100. Sync has no default step limit. |
| `chatModel` | Model ID served by the configured chat endpoint, or null. |
| `chatBaseUrl` | OpenRouter-compatible API base URL. Omit for hosted OpenRouter. |
| `embeddingUrl` | OpenAI-compatible embeddings endpoint, or null. |
| `embeddingModel` | Embedding model ID, independent of chat. |
| `embeddingDimensions` | Expected vector width, 1 through 4096 when enabled. |

`sync --since` accepts `Nd`, `Nw`, `Nm`, `Ny` or an RFC3339 start timestamp.
Months and years use calendar arithmetic in UTC, clamping to the last valid day
when needed. Days and weeks use 24-hour days. The default is `1y`.
The first import persists both endpoints before listing. Restarting retains
that interval even if `--since` changes. Use a fresh Siftbot profile for a
different initial interval. The default discovery/evaluation cutoff is
three calendar months before its persisted end, with an explicit `--cutoff`
override. Import commits each source batch, hydration
queue update and cursor atomically. A changing query restarts its listing.
Expired change states reconcile existing sources before restarting. New mail
after the initial interval is accepted during change tracking. Thread context
outside the interval and truncated body values are marked explicitly.

The shared `Jmap_eio.Mail` capability confines mail requests to one discovered
account and the exact HTTPS API URL advertised by the authenticated session.
The API may use another origin or a query. Redirects to other endpoints are
rejected. Siftbot receives a reader without mutation methods.
The transport rejects mutation calls and mixed batches before transport even
if the server token grants broader permissions. Reading never changes flags.
Shared bearer credentials may be literal tokens, private-file references or
`sekrit:NAME` references. Setup retains an existing reference when the token
field is blank and the URL is unchanged. It never resolves one into the form.

## Classification, retrieval and discussion

Discovery currently uses a reproducible lexical baseline with duplicate
exclusion and sender/list volume caps. Its candidate definitions need review.
It does not claim semantic clustering. An area must be approved before it can
receive predictions. Header rules identify list and automated delivery but do
not infer spam from an unsubscribe header or broadcast from a list header.

Set `chatModel` to enable classification, discussion and overviews. For Sequoia:

```json
"chatBaseUrl": "http://sequoia:8000/v1",
"chatModel": "Qwen/Qwen3.8-27B-FP8"
```

Sequoia needs no API key. Omit `chatBaseUrl` for hosted OpenRouter and provide
`SIFTBOT_OPENROUTER_API_KEY`. That key is never inherited by custom endpoints.
`SIFTBOT_CHAT_API_KEY` explicitly supplies a key for the selected endpoint if
needed. Credentialed endpoints require HTTPS. Classification caches distinguish
custom endpoints as well as model IDs. Setup preserves `chatBaseUrl` when editing
other profile fields.

Run:

```sh
siftbot classify --profile personal
siftbot classify --profile personal --all
siftbot classify --profile personal --all --jobs 4
siftbot overview --profile personal --area AREA --maintain
siftbot watch --profile personal
```

`classify` processes up to `maxSteps` pending messages by default. `--all`
processes every pending local email found at startup. Both modes skip current
results. Repeating `--all` after syncing processes new or changed emails;
changing the model or approved classification context also refreshes results.
`classify` and `watch` use four concurrent model requests by default. Set
`--jobs N` (or `-j N`) to choose 1 to 32. Results are saved atomically as replies
finish. A failure cancels outstanding work; a later run resumes saved progress.

The progress bar shows completed count, percentage and elapsed time. Beneath it,
the latest saved labels and email subject replace the previous preview. The
final summary reports how many messages remain. `--no-progress` prints results
as plain lines. Ctrl-C preserves completed results; rerun to resume. The same
preview is shown during `watch`. Each worker loads one email body at a time.

Classification makes bounded, tool-free model requests with reasoning disabled.
It requests JSON Schema output, validates replies and retries malformed or
truncated output once. The model provider must support JSON Schema. It uses
only confirmed examples and records model, prompt and source revisions. It stores
suggestions separately from admin decisions. Human decisions take precedence.
Changing provider versions reuses their cached inference when the complete
input identity still matches. `watch` keeps syncing and checking for changes
every 30 seconds until interrupted. Each pass limits classification to
`maxSteps` messages. Model output cannot create rules, confirm labels or
capture tasks. The discussion tools can search, inspect source records, read
area definitions and resolve contacts at the message date.

Area overviews use Agentkit Memo trees over confirmed assignments. They cite
source ranges and mark missing summaries, truncated text and partial coverage.
They remain derived data. Read the source for exact claims.

Set all three embedding fields to enable `siftbot embed --profile personal`.
An optional `SIFTBOT_EMBEDDING_API_KEY` requires an HTTPS endpoint. Endpoints
without credentials may be local HTTP services. `embed --query TEXT` performs
a nearest-neighbour search. The library also exposes reciprocal-rank hybrid
retrieval. Provider batches are bounded and cancellable. Cache identity
includes endpoint/model, dimensions, normalization, input hash and source
revision. Source updates and deletions remove their vectors atomically.

The vector extension is the unmodified sqlite-vec v0.1.6 amalgamation, compiled
statically against the workspace SQLite. Filesystem vector readers are
excluded. See [the vendor record](../../../vendor/sqlite-vec/README.md).

## Evaluation and task capture

`evaluate` currently evaluates the lexical/header baseline, not the configured
chat model. It deduplicates before chronological splitting and freezes source
records, approved definitions, confirmed decisions, classifier version and
per-dimension reports. Test-period replies never enter the training corpus.
Accuracy compares complete label sets per reviewed target/dimension. Reports
show reviewed counts, abstentions, false spam and missed relevance at several
thresholds. With no separately reviewed test examples, denominators are zero.
This is not evidence for enabling automatic decisions.

Task capture requires an explicit preview and revision confirmation:

```sh
siftbot capture --profile personal \
  --service https://mail.example/jmap/session --account ACCOUNT --email-id EMAIL \
  --title 'Review proposal' --dooit-root /path/to/dooit
# Repeat the exact reviewed command with its printed --confirm-revision NUMBER.
```

Optional `--area` names an approved area. `--deadline` takes RFC3339 and is
stored as Dooit's due date in UTC, with the exact timestamp retained in the
note body. Reservations and receipts prevent repeat captures
across crashes. The initial deduplication scope is one conversation and area
per Dooit root. Multiple independent requests in the same conversation need a
future finer capture identity. Dooit remains the owner of task storage.

The attention library provides evidence-linked request/reply suggestions and
revision-aware local snoozes. These APIs are not yet exposed as a complete
attention workflow in the terminal.

## Boundaries and verification

`siftbot.model` and `siftbot.protocol` contain only values. The Bonsai executable
links neither Eio nor the backend. An Async subprocess sends a versioned,
4 MiB-bounded S-expression to `siftbot-worker`. The worker has a 60-second
operation deadline. Request IDs and database revisions reject stale results
and edits. Profile setup uses a separate `siftbot-config` helper and a
64 KiB-bounded, versioned stdin channel. Secrets never enter process arguments,
review-worker messages, corpus records or model input. Setup responses contain
only redacted metadata. Saving the shared JMAP login and local settings is two
atomic file replacements. If the second fails, the UI reports that the login
was saved and asks for a retry. Terminal source text is sanitized. Termanil's shared input queue
keeps pasted command letters from dispatching actions.

Tests use synthetic sources, mocked JMAP and model responses, temporary
SQLite and Dooit stores, and Bonsai expect interactions. They exercise
interruption, state expiry, account confinement, mixed mutation rejection,
source deletion, vector results, cache/provider changes, rollback, restart,
review conflicts, undo, date boundaries, future-context exclusion, tool
permissions, failed turns, input bursts and capture deduplication.

No live mailbox or real-model trial has been run. The remaining work includes
semantic discovery, richer evaluation breakdowns and a frozen model evaluation,
reviewed identity associations, useful-question selection, split/list/future
rule review, dedicated label filters and full attention/task UI integration.
Server label publication remains a separate later extension.
