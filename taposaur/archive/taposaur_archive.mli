(** Read-only, capability-confined access to an existing archive.
    Functions propagate Eio I/O errors and cancellation. Parse failures
    raise {!Parse_error}. Callbacks run synchronously and may raise. *)

type t
type paper
type error = { path : string; line : int option; message : string }
exception Parse_error of error
val error_to_string : error -> string

(** {1 Opening and locating} *)

val open_root : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_root ~sw root] confines access beneath [root], which contains
    [papers/] and [index/]. No write operation is exposed. *)
val find : t -> Taposaur.Uid.t -> paper option
(** [find t uid] uses the canonical path and requires [paper.json]. *)
val find_staged : t -> batch:string -> Taposaur.Uid.t -> paper option
(** [find_staged t ~batch uid] tries sharded then legacy flat paths in
    [staging/batch]. [batch] must be a single filename component. *)
val manifest : paper -> Taposaur.Manifest.t
(** [manifest paper] returns the snapshot decoded when [paper] was found.
    It performs no I/O. Find the paper again to refresh it. *)
val directory : paper -> string
(** [directory paper] is relative to the archive root. *)

(** {1 Bounded streaming} *)

val iter_jsonl :
  ?max_line:int -> decode:(string -> ('a, string) result) ->
  _ Eio.Path.t -> (int -> 'a -> unit) -> unit
(** [iter_jsonl ~decode path f] calls [f line value] in file order.
    Lines are numbered from one. Empty lines are invalid. The default
    buffer limit is 16 MiB, including the line terminator. A final line
    without a newline is parsed. Exceptions from [f] propagate unchanged. *)
val iter_events :
  ?max_line:int -> paper -> (int -> Taposaur.Event.t -> unit) -> unit
val iter_index :
  ?max_line:int -> t -> shard:string ->
  (int -> Taposaur.Index.t -> unit) -> unit
(** [iter_index t ~shard f] reads an index-relative JSONL filename. *)
val iter_shards : t -> (string -> unit) -> unit
(** [iter_shards t f] lists DOI and PMC sub-manifests without opening
    paper directories. Symlink directories are not traversed. *)
val shard_stat : t -> string -> int64 * float
(** [shard_stat t shard] is the file size and modification time. *)
val iter_papers : t -> (paper -> unit) -> unit
(** [iter_papers t f] recursively finds [paper.json] markers in [papers/].
    This explicit audit can be expensive. Nested papers are included. *)

(** {1 Artifacts and work lists} *)

val with_artifact :
  paper -> string -> (Eio.File.ro_ty Eio.Resource.t -> 'a) -> 'a
(** [with_artifact paper file f] opens a manifest-listed artifact inside
    the paper subtree and closes it when [f] returns. It refuses symlinks
    at the filename and cannot follow links out of the paper subtree. *)
val read_artifact : ?max_bytes:int -> paper -> string -> string
(** [read_artifact paper file] loads at most 16 MiB by default. *)
val iter_dois : _ Eio.Path.t -> (int -> Taposaur.Uid.t -> unit) -> unit
(** [iter_dois path f] skips blank and comment lines in a DOI work list. *)
