type preview = {
  source : Siftbot_model.source;
  source_revision : string;
  title : string;
  deadline : string option;
  area : string option;
  thread : string;
}
(** Explicit Dooit capture with durable source deduplication. *)

val preview :
  Siftbot_model.message ->
  title:string ->
  ?deadline:string ->
  ?area:string ->
  unit ->
  preview

val capture :
  Store.t -> revision:int -> dooit_root:string -> preview -> string list
(** [capture db ~revision ~dooit_root preview] commits a reviewed capture. A
    durable reservation pins one task source per conversation and area. A crash
    between Dooit's commit and its receipt is safely retried through Dooit's
    deterministic source link. Failures are recorded and propagated. Deadlines
    set Dooit's UTC due date and retain their exact timestamp in the note body. *)
