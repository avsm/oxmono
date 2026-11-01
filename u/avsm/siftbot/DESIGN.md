# Siftbot design and remaining work

The [README](README.md) is the command and configuration reference. This
document describes the enduring boundaries and work still to assess. Earlier
implementation plans and checkpoints remain in Git history.

## Authority and evidence

Sift imports mail through a restricted JMAP reader and stores classifications
locally in SQLite. Import and classification do not change server keywords,
mailbox membership or read state. Any future label publisher needs a separately
configured writer, explicit previews and durable mutation receipts.

Areas, triage, delivery and attention are independent dimensions. An automated
notification can be relevant and require action elsewhere without needing an
email reply. Areas require approval. Keep stable responsibilities distinct
from dated activities and preserve historical assignments when archiving.

Assignments retain their source revision, origin, explanation and evidence.
Manual message corrections override sender rules, which override List-ID rules.
Rules and corrections preserve model predictions and support undo. Untouched
predictions never become trusted examples. Scores are ranking signals, not
calibrated probabilities.

## Incremental processing

Persist the import interval, source revisions, hydration queue and checkpoints.
Interrupted work resumes without repeating committed batches. Reconcile changed
or deleted sources and invalidate derived work against the source revision.
Preserve duplicate deliveries as distinct sources even when their content is
used to detect duplicate evidence.

Classification stores completed work incrementally. Manual corrections and
saved rules enter the feedback store. Retrieval selects relevant active examples
by identity and text, optionally augmented with cached embeddings. Exclude the
target message, its thread and duplicate content from its examples. Undo and
supersession remove influence. `--refresh-feedback` revisits completed mail
where the selected example set changed. This supplies model context, not weight
training. Recorded prompt provenance is not proof the model obeyed it.

## Boundaries

- [Core interfaces](lib/core) own corpus state, import, rules, classification,
  feedback, retrieval and evaluation. The shared retrieval library supplies
  embedding and ranking primitives.
- Bonsai and Eio communicate through a bounded worker protocol. Keep blocking
  work and model calls out of UI rendering. Validate source revisions before
  committing a preview and preserve cancellation and stale-response handling.
- Email and retrieved text are untrusted evidence, never tool instructions.
  Agentkit provides model discussion and evidence tools. Suggestions are not
  committed decisions. Read-only Sortal resolution preserves ambiguous matches
  and dated affiliations. Reviewed identity associations need their own flow.
- Dooit is the task store. Capture is explicit, source-deduplicated and recorded
  with a receipt. Classification alone must not silently create tasks.
- Credentials remain in shared service configuration. Keep them out of model
  input and ordinary review-worker messages. Synthetic data is the default for
  development tests. Live accounts and providers require operator configuration.

## Remaining work

- Extend lexical/header discovery to semantic area discovery. Balance examples
  across conversations, participants and active days so newsletters do not
  dominate proposals.
- Add model-based frozen evaluation. Chronological splitting, reviewed-label
  scoring and `freeze_baseline` already exist, but evaluate the baseline.
  Freeze definitions, model/rule versions, inputs and feedback. Exclude future
  context and duplicate leakage, report abstentions and per-label errors, and
  assess revisions before adopting them.
- Add reviewed identity association and ambiguity resolution to the UI.
  Address resolution and historical affiliations already exist in `Identity`.
- Develop useful-question selection and emerging-topic/activity queues. Reuse
  the existing explanations, previews, scoped corrections and undo instead of
  recreating the earlier planned review UI.
- Complete attention and local-snooze terminal workflows around the existing
  attention inference and queue APIs. Distinguish replying by email from acting
  on an external notification.
- If server label publishing is wanted, design the writer independently of the
  classifier. Preview exact changes, preserve unrelated labels, record receipts
  and surface conflicts.

## Verification

Run `dune build --force @u/avsm/siftbot/runtest` from the repository root.
Relevant tests cover import/checkpoint races, read-only capabilities, rules and
undo, feedback retrieval/invalidation, classifier bounds and parallelism,
chronological evaluation, Bonsai interactions, worker limits and Dooit capture.
The CLI progress checks use a PTY and exercise plain output and interruption.

Assess model usefulness with reviewed chronological examples. A successful
synthetic test or historical live run does not establish classification quality
for another mailbox, provider or model revision.
