# Siftbot

Siftbot imports JMAP mail into a local SQLite corpus and reviews independent
area, triage, delivery and attention assignments. It never writes to JMAP.
The implementation status and remaining work are recorded in
[../../SIFT.md](../../SIFT.md).

## Build and demo

```sh
opam exec --switch=5.2.0+ox -- dune build @avsm/siftbot/all
opam exec --switch=5.2.0+ox -- dune runtest avsm/siftbot --force
opam exec --switch=5.2.0+ox -- dune exec siftbot -- review --demo
```

The demo persists under `$XDG_DATA_HOME/siftbot/synthetic-demo`, falling back
to `$HOME/.local/share/siftbot/synthetic-demo`. It contains teaching, research,
social mail, list discussion, an announcement, an automated reminder, spam,
an ambiguous collaboration, duplicate delivery and a suspicious message in
an otherwise legitimate conversation. Demo commands never load credentials
or contact a model provider.

Tab selects a pane. `j` and `k` select an area in the left pane or a message
in the other panes. PageUp and PageDown scroll source evidence. `/` searches
with SQLite FTS5 syntax. `r` refreshes. `a` previews approval of the selected
area, `n` renames it, and `x` archives it. `v` assigns the selected area to
the current message. Keys `1` through `4` preview relevant, social, spam and
uncertain triage. `u` previews undo. Enter commits after an explanation has
been entered. Escape cancels. `c` opens a model discussion in configured live
profiles. `p` opens connection profiles. `q` quits when no operation is pending.

## Live profile

Open the connection form with `siftbot review --setup`. A missing profile
opens setup automatically. The picker uses `j`/`k` to select, Enter to open,
`n` to create and `e` to edit. Tab or Enter advances through the form.
Ctrl-U clears a field. Enter on **Save profile** saves. Escape cancels.

Set the Siftbot profile name, shared JMAP profile name, HTTPS session URL,
bearer access token, optional account ID and optional chat model. The account
ID defaults to the primary mail account. Existing profile names are fixed.
Tokens are masked and never read back into the UI. A blank token keeps the
shared credential only if the URL is unchanged. Changing a shared JMAP login
also affects other applications using that login. Advanced JSON settings are
preserved when the form is saved.

Setup makes no network requests. Opening another profile starts a fresh review
screen for its separate corpus. Open setup again with `p`. Saving does not
import mail. Run `learn` to populate the selected profile:

```sh
siftbot review --setup
siftbot learn --profile personal --since 1y
```

To reuse an existing shared JMAP profile from the command line:

```sh
siftbot init --profile personal --jmap-profile personal
siftbot learn --profile personal --since 1y
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
Terminal status logs contain identifiers and outcomes.

The JSON configuration fields are:

| Field | Meaning |
| --- | --- |
| `name` | Profile name, matching its directory. |
| `jmapProfile` | Shared JMAP credential profile. |
| `account` | Explicit account ID, or null for its primary mail account. |
| `sortalRoots` | Read-only Sortal store roots used by discussion tools. |
| `bodyLimit` | Maximum bytes per JMAP body value, default 65536, maximum 1048576. |
| `maxSteps` | Work budget per command, default 100. |
| `chatModel` | Explicit OpenRouter model ID, or null. |
| `embeddingUrl` | OpenAI-compatible embeddings endpoint, or null. |
| `embeddingModel` | Embedding model ID, independent of chat. |
| `embeddingDimensions` | Expected vector width, 1 through 4096 when enabled. |

`learn` resolves `1y` to 365 days and persists both endpoints before listing.
Restarting retains that interval. The default discovery/evaluation cutoff is
three calendar months before its persisted end, with an explicit `--cutoff`
override. Import commits each source batch, hydration
queue update and cursor atomically. A changing query restarts its listing.
Expired change states reconcile existing sources before restarting. New mail
after the initial interval is accepted during change tracking. Thread context
outside the interval and truncated body values are marked explicitly.

The shared `Jmap_eio.Mail` capability confines every request to one discovered
account and HTTPS origin. Siftbot receives a reader without mutation methods.
The transport rejects mutation calls and mixed batches before transport even
if the server token grants broader permissions. Reading never changes flags.

## Classification, retrieval and discussion

Discovery currently uses a reproducible lexical baseline with duplicate
exclusion and sender/list volume caps. Its candidate definitions need review.
It does not claim semantic clustering. An area must be approved before it can
receive predictions. Header rules identify list and automated delivery but do
not infer spam from an unsubscribe header or broadcast from a list header.

Set `chatModel` and provide `SIFTBOT_OPENROUTER_API_KEY` to enable:

```sh
siftbot classify --profile personal
siftbot overview --profile personal --area AREA --maintain
siftbot watch --profile personal
```

Classification runs bounded Agentkit turns, validates their output, uses only
confirmed examples, and records model, prompt and source revisions. It stores
suggestions separately from admin decisions. Human decisions take precedence.
Changing provider versions reuses their cached inference when the complete
input identity still matches. `watch` follows changes with separate sync and
classification budgets. Model output cannot create rules, confirm labels or
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
excluded. See [the vendor record](../../vendor/sqlite-vec/README.md).

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
