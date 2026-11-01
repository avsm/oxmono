(** Bounded model suggestions, separated from confirmed review decisions. *)

val schema : Store.t -> Jsont.json
(** [schema db] constrains output to one triage, delivery and attention entry,
    followed by optional entries using approved area IDs from [db]. Pass it to
    the model adapter before calling [run] with the same database. *)

type progress = { completed : int; total : int; remaining : int }
(** [total] is the selected batch size. [remaining] includes pending messages
    beyond that batch. Cached, current messages do not enter these counts. *)

val run :
  ?on_progress:(progress -> unit) ->
  ?on_classified:(subject:string -> Siftbot_model.assignment list -> unit) ->
  ?max_messages:int ->
  ?jobs:int ->
  ?feedback_provider:Embedding.provider ->
  ?refresh_feedback:bool ->
  Store.t ->
  complete:Agentkit.Chat.complete ->
  model:string ->
  int
(** [run db ~complete ~model ~max_messages] classifies changed inputs using
    approved definitions and relevant corrections from the entire feedback
    store. Optional [feedback_provider] adds cached semantic retrieval to
    sender, list and text matching. New corrections affect new mail without
    invalidating every completed classification. Results are validated before
    committing, cached by input/model/prompt versions, and audited. Returns the
    number of newly classified messages. Without [max_messages], processes all
    pending sources found at startup. Unchanged, current classifications are
    skipped. [jobs] bounds concurrent inference and loaded bodies, from 1 to 32
    (default 1). Saves and callbacks are serialized in completion order.
    [on_classified] receives each saved result. [on_progress] runs before
    inference and after each successfully saved result, never for a failed
    result. Invalid or truncated model replies are retried once before failing.
    Oversized email evidence is reduced to fit the encoded request budget and
    marked truncated. Stored source messages are preserved. Complete JSON in a
    single Markdown fence is accepted. An error cancels outstanding work. Saved
    results remain reusable. Changes outside this run raise [Store.Conflict]. *)
