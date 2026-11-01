type task = [ `Query | `Document ]
(** Providers can distinguish document encoding from query instructions. *)

type provider = {
  model : string;
  dimensions : int;
  max_batch : int;
  embed : task:task -> check:(unit -> unit) -> string list -> float list list;
}
(** Corpus-independent embedding inference and rank fusion. *)

type 'a backend = {
  documents : unit -> 'a list;
  key : 'a -> string;
  revision : 'a -> string;
  passage : 'a -> string;
  index : model:string -> dimensions:int -> normalization:string -> string;
  current : index:string -> document:'a -> input_hash:string -> bool;
  put : index:string -> document:'a -> input_hash:string -> float list -> unit;
  nearest : index:string -> limit:int -> float list -> ('a * float) list;
  lexical : string -> 'a list;
}

val hash : string -> string
val clip : int -> string -> string

val checked : provider -> float list -> float list
(** [checked provider vector] L2-normalizes a nonzero vector. It rejects
    mismatched dimensions, nonfinite values, and magnitudes above [1e30].
    Tiny nonzero vectors are scaled to avoid underflow. *)

val index_name : 'a backend -> provider -> string

val index :
  ?on_progress:(int -> unit) ->
  'a backend -> provider -> check:(unit -> unit) -> max_batches:int -> int
(** [index backend provider ~check ~max_batches] embeds changed passages in
    bounded batches, inspecting documents only until the budget is filled.
    Each stored hash identifies the exact passage passed to inference.
    [check] runs around inference. The backend must reject writes whose
    document revision changed during inference. [on_progress] receives the
    cumulative number written after each fully validated and stored batch.
    It is not called when no passages are written. Callback exceptions
    propagate. *)

val search :
  'a backend -> provider -> check:(unit -> unit) -> string -> ('a * float) list

val fuse : key:('a -> string) -> limit:int -> 'a list list -> 'a list
(** [fuse ~key ~limit lists] combines reciprocal ranks. Each key votes at most
    once per input list. Equal scores are ordered by key. *)

val hybrid :
  'a backend ->
  provider ->
  check:(unit -> unit) ->
  lexical:string ->
  semantic:string ->
  'a list

val http :
  sw:Eio.Switch.t ->
  fetch:_ Fetch.t ->
  url:string ->
  ?api_key:string ->
  model:string ->
  dimensions:int ->
  unit ->
  provider
(** [http ...] uses an explicitly configured embeddings endpoint. Batches and
    responses are bounded. Redirects are refused and credentials require HTTPS. *)
