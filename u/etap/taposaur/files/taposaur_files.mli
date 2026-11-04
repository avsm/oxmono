(** Durable local file primitives shared by archive and staging writers.

    Every writer beneath a paper tree uses the same fsync, atomic
    replacement and digest helpers, so an ordering fix applies to all of
    them. Paths must be on a local Unix filesystem. Functions propagate
    Eio I/O errors and cancellation. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] flushes [dir]'s entries, so a completed rename or
    creation inside it survives a crash. *)

val exists : _ Eio.Path.t -> bool
(** [exists path] is true for any entry at [path], without following a
    final symbolic link. *)

val load : ?max_bytes:int -> _ Eio.Path.t -> string
(** [load path] reads a regular file without following a final symbolic
    link. [max_bytes] defaults to 16 MiB and exceeding it raises
    [Eio.Buf_read.Buffer_limit_exceeded]. *)

val atomic : ?perm:int -> _ Eio.Path.t -> string -> string -> unit
(** [atomic dir name value] writes [value] to [name.tmp] inside [dir] with
    mode [perm] (default 0600), syncs it, renames it over [name] and syncs
    [dir]. A reader observes either the previous file or the complete new
    one. *)

val mode_of : _ Eio.Path.t -> string -> int -> int
(** [mode_of dir name default] is the permission bits of the regular file
    [name] in [dir], or [default] when absent or not a regular file, so a
    replacement keeps the mode readers already depend on. *)

val digest : _ Eio.Flow.source -> string * int
(** [digest flow] streams [flow] to its end and returns the lowercase
    SHA-256 hex digest and the byte count. *)

val digest_path : _ Eio.Path.t -> string * int
(** [digest_path path] is {!digest} of the regular file at [path], without
    following a final symbolic link. *)
