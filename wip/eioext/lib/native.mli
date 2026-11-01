(** Synchronous Unix compatibility for existing stores. These operations may
    block. Eio callers should prefer {!Path} or offload to a worker thread. *)

exception Error of string
exception Busy of string

val read_regular : max_bytes:int -> string -> string
(** [read_regular ~max_bytes path] reads at most [max_bytes] from a regular
    file. It rejects symlinks, special files and files replaced during open.
    Reads stay bounded even when another process extends the file. *)

val read_private : max_bytes:int -> string -> string
(** [read_private ~max_bytes path] also requires current-user ownership and no
    group or other permissions, checked on the opened descriptor. *)

val mkdir_private : string -> unit
(** [mkdir_private path] creates missing directories with mode 0700. Existing
    ancestors may be shared. An existing final path must be a directory, not a
    symlink. Use [require_private_dir] to check its ownership and mode. *)

val require_private_dir : string -> unit
(** [require_private_dir path] requires an owned directory with no group or
    other permissions and rejects a symlink. *)

val atomic_write :
  ?mode:int -> ?check:(unit -> unit) -> string -> string -> unit
(** [atomic_write ~check path data] publishes [data] using a same-directory
    temporary file and rename. It fsyncs the file and parent directory. [check]
    runs immediately before publication. [mode] defaults to 0600. *)

val create_private : string -> string -> unit
(** [create_private path data] durably publishes a mode-0600 file, refusing an
    existing path. It never truncates or follows an existing target. *)

val with_lock : ?wait:(unit -> unit) -> path:string -> (unit -> 'a) -> 'a
(** [with_lock ~path f] excludes other callers in this process and other
    processes for [f]. The private lock file is retained and must never be
    linked, renamed, unlinked or opened independently while locked. Its parent
    directory must be trusted. Contention raises [Busy path], unless [wait] is
    supplied, in which case it is called between nonblocking attempts. An Eio
    caller can supply a cancellable sleep. The callback must not recursively
    acquire the same lock. Descriptors are close-on-exec. *)
