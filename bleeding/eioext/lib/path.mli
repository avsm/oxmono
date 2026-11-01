(** Directory-capability filesystem operations for native Unix Eio backends. *)

exception Error of string

exception Busy of string
(** Filesystem-policy failures and lock contention. These are also raised by the
    synchronous [Eioext.Native] operations. Other I/O failures use [Eio.Io]. *)

val with_open_private :
  _ Eio.Path.t -> (Eio.File.ro_ty Eio.Resource.t -> 'a) -> 'a
(** [with_open_private path f] opens and validates an owned private regular file
    through its directory capability. [f] must bound its reads. *)

val read_private : max_bytes:int -> _ Eio.Path.t -> string
(** [read_private ~max_bytes path] reads through the supplied directory
    capability. The file must be owned, private and regular. Symlinks are
    refused, the opened descriptor is checked, and reads are bounded. *)

val require_private_dir : _ Eio.Path.t -> unit
(** [require_private_dir path] requires an owned directory with no group or
    other permissions and rejects a symlink. *)

val atomic_write : ?check:(unit -> unit) -> _ Eio.Path.t -> string -> unit
(** [atomic_write ~check path data] replaces [path] with a mode-0600 file and
    fsyncs the file and parent directory. The parent must be private. [check]
    runs before publication. A failed check or cancellation before publication
    preserves the old file. Rename and directory sync cannot be cancelled once
    started. Temporary files are removed on failure. *)

val create_private : _ Eio.Path.t -> string -> unit
(** [create_private path data] exclusively creates and fsyncs a private file.
    Creation is protected from cancellation once started. Failed writes remove
    the newly created file. A process crash may leave an incomplete file, which
    is never overwritten implicitly. *)

val with_lock : ?wait:(unit -> unit) -> _ Eio.Path.t -> (unit -> 'a) -> 'a
(** [with_lock path f] coordinates fibers and processes using an opened
    directory capability. Contention raises [Busy] unless [wait] is supplied.
    Waiting is cancellable when [wait] yields. The owned private lock file must
    not have hard links or be renamed, unlinked or opened independently while
    locked. Its parent directory must be trusted. The callback must not
    recursively acquire the same lock. *)
