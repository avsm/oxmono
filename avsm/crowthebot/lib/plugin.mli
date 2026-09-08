(** Plugins receive only capabilities supplied by their owner. *)

type t = { name : string; description : string; run : query:string -> string }

val tool : t -> Openrouter.Tool.t

val invoke : t -> string -> string
(** [invoke plugin arguments] validates the JSON query argument and bounds tool
    results to 4096 bytes plus a truncation marker. *)

val clip : bytes:int -> string -> string
val blogroll_url : string

val blogroll : fetch:_ Fetch.t -> now:(unit -> float) -> t
(** [blogroll ~fetch ~now] restricts Fetch to GET under the fixed blogroll URL.
    It caches the bounded OPML document for one hour. It never fetches the
    listed feeds or follows OPML include outlines. [now] is monotonic seconds.
    Invoke a plugin instance serially. *)
