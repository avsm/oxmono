(** Daily notes generated from bounded batches of persisted tool-use records. *)

type complete =
  Openrouter.Message.t list ->
  Openrouter.Tool.t list ->
  string option * Openrouter.Tool.call list

val generate :
  store:Store.t ->
  config:Config.t ->
  complete:complete ->
  day:string ->
  Store.daily_note
(** [generate ~store ~config ~complete ~day] summarizes a completed UTC day.
    Every record is visited, with bounded excerpts and incremental summaries.
    Existing notes with the same source watermark are reused. A failure leaves
    the previous note unchanged so a later run can retry. Model tool calls are
    rejected. The caller serializes generators for the same profile. *)

val render : Store.daily_note -> string
