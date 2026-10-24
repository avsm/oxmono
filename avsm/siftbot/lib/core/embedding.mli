(** Embedding inference is independent of chat completion. *)

type provider = {
  model : string;
  dimensions : int;
  max_batch : int;
  embed :
    task:Retrieval.Embedding.task ->
    check:(unit -> unit) -> string list -> float list list;
}

val index :
  Store.t -> provider -> check:(unit -> unit) -> max_batches:int -> int
(** [index db provider ~check ~max_batches] embeds bounded source passages,
    reusing vectors by model, dimensions, normalization and input hash. [check]
    propagates cancellation before and after each provider batch. Returns the
    number of newly indexed messages. *)

val search :
  Store.t ->
  provider ->
  check:(unit -> unit) ->
  string ->
  (Siftbot_model.message * float) list
(** [search db provider ~check query] performs a bounded nearest-neighbour
    query. *)

val hybrid :
  Store.t ->
  provider ->
  check:(unit -> unit) ->
  lexical:string ->
  semantic:string ->
  Siftbot_model.message list
(** [hybrid db provider ~check ~lexical ~semantic] combines FTS and vector
    ranks, retaining complete source identities. *)

val http :
  sw:Eio.Switch.t ->
  fetch:_ Fetch.t ->
  url:string ->
  ?api_key:string ->
  model:string ->
  dimensions:int ->
  unit ->
  provider
(** [http ~sw ~fetch ~url ~model ~dimensions ()] is an OpenAI-compatible
    embeddings endpoint. Responses and batch sizes are bounded, indices must
    match every input, and credentials require HTTPS. Redirects are refused. *)
