(** This module provides static-file serving descriptors for backend authors.

    The shipped backends do not interpret these descriptors directly. *)

(** [confine segs] is [segs] joined with ['/'] when every segment names something directly
    under a root, and [None] otherwise. A segment that is empty, ["."] or [".."], or that
    holds a slash, backslash, or NUL is refused. A backend must still resolve the result
    beneath a directory capability because lexical checks cannot detect symlink traversal. *)
val confine : string list -> string option @@ portable

(** A [t] is a directory label and optional cache policy. A backend resolves the label
    against its filesystem capability. *)
type t : immutable_data

(** [v ~root ()] is a static-file description rooted at [root], a name the backend
    resolves. A backend can use {!Mime.of_path} for each file's Content-Type and apply
    [cache] to its response. *)
val v : root:string -> ?cache:Cache_control.t -> unit -> t @@ portable

(** [root t] is the label [t] was built with. *)
val root : t -> string @@ portable

(** [cache t] is the policy [t] applies to each file, if any. *)
val cache : t -> Cache_control.t option @@ portable
