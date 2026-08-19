(** Serving a directory of files, described as data. *)

val confine : string list -> string option @@ portable
(** [confine segs] is [segs] joined with ['/'] when every segment names
    something directly under a root, and [None] otherwise. A segment that is
    empty, ["."] or [".."], or that holds a ['/'] or a NUL, is refused, so the
    result can never leave the subtree. A backend that resolves the result
    against a filesystem must still open it under a confining root, since
    [confine] cannot see symlinks. *)

type t : immutable_data
(** A served directory. It holds a label and a cache policy, not a filesystem
    handle, so a backend resolves [root] against its own capability. *)

val v : root:string -> ?cache:Cache_control.t -> unit -> t @@ portable
(** [v ~root ()] serves files under [root], a name the backend resolves. Each
    file's Content-Type comes from {!Mime.of_path} and its response carries
    [cache] when given. *)

val root : t -> string @@ portable
(** [root t] is the label [t] was built with. *)

val cache : t -> Cache_control.t option @@ portable
(** [cache t] is the policy [t] applies to each file, if any. *)
