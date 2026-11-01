module Response = Bushel_knowledge_store.Response

type t

val create :
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  corpus:Bushel_knowledge.t ->
  ?provider:Retrieval.Embedding.provider ->
  ?refresh:(unit -> unit) ->
  max_bytes:int ->
  unit ->
  t

val of_store : Bushel_knowledge_store.t -> max_bytes:int -> t

type search = {
  query : string;
  mode : string;
  kind : string option;
  tag : string option;
  contact : string option;
  project : string option;
  since : string option;
  until : string option;
  offset : int;
}

type request =
  | Search of { bushel : bool; search : search }
  | Latest_posts of int
  | Resolve of { query : string; offset : int }
  | Contact of string
  | Read of { id : string; offset : int }
  | Context of { id : string option; url : string option; offset : int }

val call : t -> check:(unit -> unit) -> request -> (Response.t, string) result
(** [call t ~check request] reads one bounded result. It never indexes
    documents. [check] enforces authority and cancellation. *)

val request : string -> string -> request
(** [request name arguments] decodes an external tool call with strict schemas
    and a 4096-byte argument limit. Native callers construct [request] values. *)
