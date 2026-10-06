(** A derived summary tree over an ordered, authoritative source log.

    Sources are supplied oldest first. Their IDs must be unique and stable. Keys
    include IDs, revisions and text. Editing or erasing a source makes every
    dependent key unreachable. No operation changes the original log. *)

type source = { id : string; revision : string; text : string }
type t

type view = {
  key : string;
  first : string;
  last : string;
  count : int;
  text : string;
  summarized : bool;
  missing : bool;
}
(** A leaf, cached summary, or explicit pointer to an unsummarized range.
    Summary text is untrusted, lossy data. Use source reads for exact facts. *)

val create : source list -> t
(** [create sources] indexes [sources]. Empty or duplicate IDs raise
    [Invalid_argument]. Text must be valid UTF-8. *)

val keys : t -> string list
(** [keys t] are all current node keys, for validating cache writes. *)

val overview : t -> budget:int -> lookup:(string -> string option) -> view list
(** [overview t ~budget ~lookup] covers every source once with at most [budget]
    views. Newer ranges are split first. [budget] must be positive. Missing
    summaries yield explicit range pointers, without losing coverage. *)

val expand : t -> key:string -> lookup:(string -> string option) -> view list
(** [expand t ~key ~lookup] returns a node's two children, or its original
    source for a leaf. An obsolete or unknown [key] raises [Invalid_argument].
*)

val render : limit:int -> view list -> string
(** [render ~limit views] renders views within [limit] UTF-8 bytes, including
    range keys and truncation notices. [limit] must be at least 256. *)

val validate_summary : limit:int -> string -> unit
(** [validate_summary ~limit text] rejects empty, oversized or invalid UTF-8
    summaries with [Invalid_argument]. *)

val maintain :
  t ->
  max_merges:int ->
  limit:int ->
  lookup:(string -> string option) ->
  save:(key:string -> text:string -> unit) ->
  summarize:(limit:int -> string -> string) ->
  int
(** [maintain t ~max_merges ~limit ~lookup ~save ~summarize] fills at most
    [max_merges] absent summaries, largest ready ranges first and oldest first
    at each size. Small ranges use original sources. Larger ranges use their two
    cached children. Failures propagate. [save] must reject keys whose sources
    changed during inference. A rejected save may raise. Original text is never
    truncated for inference. Returns the number of summaries passed to [save].
*)

val instructions : words:int -> string
(** [instructions ~words] is a prompt for {!Summary.run} that asks for source
    IDs, attribution, uncertainty and corrections to be preserved. *)

val clip : int -> string -> string
(** [clip limit text] is at most [limit] UTF-8 bytes of [text], cut at a
    character boundary. [limit] must be nonnegative. [text] must be UTF-8. *)
