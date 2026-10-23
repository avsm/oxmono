type document = {
  id : string;
  revision : string;
  kind : string;
  title : string;
  text : string;
  url : string;
  date : string;
  facets : (string * string) list;
}
(** A derived corpus. Replacing its snapshot never modifies source files. *)

type chunk = { document : document; id : string; ordinal : int; text : string }

type filter = {
  kind : string option;
  since : string option;
  until : string option;
  facets : (string * string) list;
}

val all : filter

type t

exception Conflict

val open_db : sw:Eio.Switch.t -> _ Eio.Path.t -> t
val memory : sw:Eio.Switch.t -> unit -> t

val replace : t -> document list -> unit
(** [replace t documents] atomically reconciles the complete snapshot, removing
    obsolete chunks and vectors. Unchanged sources retain cached vectors. IDs
    must be nonempty and unique. *)

val chunks : document -> chunk list
(** [chunks document] partitions all text into passages of at most 6000 bytes.
    Valid UTF-8 is split at codepoint boundaries. Newlines are preferred.
    Malformed input is retained byte-for-byte, and chunking always advances. *)

val get : t -> string -> document option

val lexical : t -> filter -> string -> chunk list
(** [lexical t filter query] applies FTS5 syntax and filters before selecting up
    to 100 passages. An empty query browses by date. *)

val backend : t -> filter -> chunk Embedding.backend
(** [backend t filter] uses exact filtered L2 ranking over cached normalized
    vectors. Filtering precedes the result limit. Stale writes raise [Conflict]. *)

val find_url : t -> string -> document list
