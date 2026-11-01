(** Source-linked, bounded derived overviews. *)

val area : Store.t -> area_id:string -> string
(** [area db ~area_id] renders the approved area's current source range with
    explicit missing-summary and coverage markers. *)

val maintain :
  Store.t ->
  area_id:string ->
  model:string ->
  complete:Agentkit.Chat.complete ->
  max_merges:int ->
  int
(** [maintain db ~area_id ~model ~complete ~max_merges] updates a bounded Memo
    summary tree. Cache writes reject source or review changes during inference.
    Exact claims still require reading the original messages. *)
