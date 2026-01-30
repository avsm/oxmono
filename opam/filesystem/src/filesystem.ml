(** This library provides an interface for filesystem access. It is abstracted over the
    monad used for i/o operations. Implementations are provided as [Filesystem_async], for
    asynchronous i/o, and [Filesystem_core], for blocking i/o. *)

open! Core
open Filesystem_types

module type S = sig
  module IO : T1

  (** The currently running executable.

      OCaml semantics do not guarantee an absolute path here. *)
  val executable_name : File_path.t Lazy.t

  (** {2 File I/O Wrappers}

      These functions abstract over either [In_channel] and [Out_channel], or
      [Async.Reader] and [Async.Writer]. *)

  val read_file : File_path.t -> string IO.t

  val write_file
    :  ?perm:File_permissions.t (** default [u_rw] *)
    -> File_path.t
    -> contents:string
    -> unit IO.t

  val load_sexp : File_path.t -> Sexp.t IO.t
  val load_sexps : File_path.t -> Sexp.t list IO.t
  val load_as_sexp : File_path.t -> of_sexp:(Sexp.t -> 'a) -> 'a IO.t
  val load_as_sexps : File_path.t -> of_sexp:(Sexp.t -> 'a) -> 'a list IO.t
  val save_sexp : ?perm:File_permissions.t -> File_path.t -> Sexp.t -> unit IO.t
  val save_sexps : ?perm:File_permissions.t -> File_path.t -> Sexp.t list -> unit IO.t

  val save_as_sexp
    :  ?perm:File_permissions.t
    -> File_path.t
    -> 'a
    -> sexp_of:('a -> Sexp.t)
    -> unit IO.t

  val save_as_sexps
    :  ?perm:File_permissions.t
    -> File_path.t
    -> 'a list
    -> sexp_of:('a -> Sexp.t)
    -> unit IO.t

  (** {2 [Filename] Wrappers}

      These functions abstract over [Filename_unix]. *)

  val realpath
    :  File_path.t
    -> relative_to:File_path.Absolute.t
    -> File_path.Absolute.t IO.t

  val realpath_absolute : File_path.Absolute.t -> File_path.Absolute.t IO.t
  val realpath_relative_to_cwd : File_path.t -> File_path.Absolute.t IO.t

  (** {2 [Sys] Wrappers}

      These functions abstract over either [Core.Sys] or [Async.Sys]. *)

  val exists : ?follow_symlinks:bool -> File_path.t -> [ `Yes | `No | `Unknown ] IO.t
  val exists_exn : ?follow_symlinks:bool -> File_path.t -> bool IO.t

  val is_directory
    :  ?follow_symlinks:bool
    -> File_path.t
    -> [ `Yes | `No | `Unknown ] IO.t

  val is_directory_exn : ?follow_symlinks:bool -> File_path.t -> bool IO.t
  val is_file : ?follow_symlinks:bool -> File_path.t -> [ `Yes | `No | `Unknown ] IO.t
  val is_file_exn : ?follow_symlinks:bool -> File_path.t -> bool IO.t
  val is_symlink : File_path.t -> [ `Yes | `No | `Unknown ] IO.t
  val is_symlink_exn : File_path.t -> bool IO.t
  val ls_dir : File_path.t -> File_path.Part.t list IO.t

  (** {2 [Unix] Wrappers}

      These functions abstract over either [Core_unix] or [Async.Unix]. *)

  (** Please note that [rmdir] raises on a non-empty directory. If you want to delete a
      directory and all of its contents, see [rm]. *)
  val rmdir : File_path.t -> unit IO.t

  val chdir : File_path.t -> unit IO.t
  val getcwd : unit -> File_path.Absolute.t IO.t
  val unlink : File_path.t -> unit IO.t
  val rename : src:File_path.t -> dst:File_path.t -> unit IO.t

  val mkdir
    :  ?parents:bool (** default: [false] *)
    -> ?perm:File_permissions.t (** default [t_0755] *)
    -> File_path.t
    -> unit IO.t

  val stat : File_path.t -> File_stats.t IO.t
  val lstat : File_path.t -> File_stats.t IO.t
  val chmod : File_path.t -> perm:File_permissions.t -> unit IO.t

  (** Set the file's [access_time] and [modify_time] to [at]. *)
  val update_timestamps
    :  ?at:Time_ns.t (** default: [Time_ns.now ()] *)
    -> File_path.t
    -> unit IO.t

  (** Set the file's [access_time] and [modify_time] to the given values. *)
  val update_timestamps_separately
    :  File_path.t
    -> access_time:Time_ns.t
    -> modify_time:Time_ns.t
    -> unit IO.t

  (** {2 Command-line tool functionality}

      These functions provide similar functionality to command-line tools like [rm],
      either by mimicking them or by calling them directly. *)

  (** Removes files.

      If [~recursive:true] is supplied, also recursively removes directories.

      If [~force:false] is supplied, will raise if given nonexistent or write-protected
      files. Defaults to [true] because programmatically-called [rm] is not interactive,
      we expect it to just succeed silently. *)
  val rm
    :  ?force:bool (** default: [true] *)
    -> ?recursive:bool (** default: [false] *)
    -> File_path.t
    -> unit IO.t

  (** {2 Links, both hard and symbolic} *)

  (** Creates the given path as a hard link to [referring_to]. *)
  val hard_link : File_path.t -> referring_to:File_path.t -> unit IO.t

  (** Creates the given path as a symbolic link to [referring_to]. *)
  val symlink : File_path.t -> referring_to:File_path.t -> unit IO.t

  (** Reads the contents of a symbolic link. *)
  val readlink : File_path.t -> File_path.t IO.t

  (** As [symlink]. Raises if [referring_to] is not a valid path, but preserves
      non-canonical paths such as "multiple//and//trailing/". *)
  val symlink_raw : File_path.t -> referring_to:string -> unit IO.t

  (** As [readlink]. Preserves non-canonical paths such as "multiple//and//trailing/". *)
  val readlink_raw : File_path.t -> string IO.t

  (** {2 Current Directory Functions}

      These functions combine [File_path] and [getcwd]. *)

  (** Like [File_path.make_absolute ~under:(getcwd ())]. Avoids calling [getcwd] unless
      necessary. *)
  val make_absolute_under_cwd : File_path.t -> File_path.Absolute.t IO.t

  (** Like [File_path.make_relative ~if_under:(getcwd ())]. Avoids calling [getcwd] unless
      necessary. *)
  val make_relative_to_cwd : File_path.t -> File_path.Relative.t option IO.t

  (** Like [make_relative_to_cwd]. Raises instead of returning [None]. *)
  val make_relative_to_cwd_exn : File_path.t -> File_path.Relative.t IO.t

  (** Like [make_relative_to_cwd]. Returns the original path instead of [None]. *)
  val make_relative_to_cwd_if_possible : File_path.t -> File_path.t IO.t

  (** {2 Temporary Files and Directories} *)

  (** The system-determined default temporary directory.

      Has the same value as {!Core.Filename.temp_dir_name}. OCaml semantics do not
      guarantee an absolute path here. *)
  val default_temp_dir : File_path.t Lazy.t

  (** Creates a new directory with a unique name, [chdir]s to it, runs the given function,
      [chdir]s back, and then recursively deletes the temporary directory and its
      contents.

      If the directory is renamed or removed before the function ends, there is a race
      condition. Some other function or process may create a temporary directory or file
      by the same name, and this function may attempt to delete that. *)
  val within_temp_dir
    :  ?in_dir:File_path.t (** default [force default_temp_dir] *)
    -> ?on_cleanup_error:unit IO.t On_cleanup_error.t (** default [Raise] *)
    -> ?perm:File_permissions.t (** default [u_rwx] *)
    -> ?prefix:string (** default [""] *)
    -> ?suffix:string (** default [""] *)
    -> (unit -> 'a IO.t)
    -> 'a IO.t

  (** Creates a new directory with a unique name, runs the given function with the
      directory's path, and then recursively deletes the temporary directory and its
      contents.

      Has the same race condition as [within_temp_dir] if the path is renamed or removed
      before the function finishes. *)
  val with_temp_dir
    :  ?in_dir:File_path.t (** default [force default_temp_dir] *)
    -> ?on_cleanup_error:unit IO.t On_cleanup_error.t (** default [Raise] *)
    -> ?perm:File_permissions.t (** default [u_rwx] *)
    -> ?prefix:string (** default [""] *)
    -> ?suffix:string (** default [""] *)
    -> (File_path.Absolute.t -> 'a IO.t)
    -> 'a IO.t

  (** Creates a new file with a unique name, runs the given function with the file's path,
      and then deletes the temporary file.

      Has the same race condition as [within_temp_dir] if the path is renamed or removed
      before the function finishes. Overwriting the file atomically by renaming something
      else to it should not have the same race condition. *)
  val with_temp_file
    :  ?in_dir:File_path.t (** default [force default_temp_dir] *)
    -> ?on_cleanup_error:unit IO.t On_cleanup_error.t (** default [Raise] *)
    -> ?perm:File_permissions.t (** default [u_rw] *)
    -> ?prefix:string (** default [""] *)
    -> ?suffix:string (** default [""] *)
    -> (File_path.Absolute.t -> 'a IO.t)
    -> 'a IO.t

  (** Creates a new directory with a unique name. *)
  val create_temp_dir
    :  ?in_dir:File_path.t (** default [force default_temp_dir] *)
    -> ?perm:File_permissions.t (** default [u_rwx] *)
    -> ?prefix:string (** default [""] *)
    -> ?suffix:string (** default [""] *)
    -> unit
    -> File_path.Absolute.t IO.t

  (** Creates a new, empty file with a unique name. *)
  val create_temp_file
    :  ?in_dir:File_path.t (** default [force default_temp_dir] *)
    -> ?perm:File_permissions.t (** default [u_rw] *)
    -> ?prefix:string (** default [""] *)
    -> ?suffix:string (** default [""] *)
    -> unit
    -> File_path.Absolute.t IO.t

  (** {2 Advisory File Locks}

      The flock system call supports mutual exclusion. When an exclusive lock on a file is
      held, other attempts to lock it fail or block. When a shared lock on a file is held,
      other attempts at shared locks succeed and attempts at exclusive locks fail or
      block.

      No other access is controlled by these locks. For locks to affect access such as
      reading and writing, processes must cooperate with a locking protocol.

      As of Linux 2.6.12, flock-based locks do function over NFS. *)

  module Fd : T
  module Flock : T

  (** Locks the given file via the flock system call, returning when the lock has been
      acquired. If the file does not exist, this call raises.

      If the file cannot be locked immediately due to an existing lock, this call waits
      until the file can be locked. If this lock and existing locks are shared, the file
      can be locked immediately.

      If the same process calls [flock] twice on the same file you will end up with two
      competing locks. You have been warned! *)
  val flock
    :  ?shared:bool (** default: false, i.e. exclusive *)
    -> File_path.t
    -> Flock.t IO.t

  (** Like [flock] above. Uses an already-open file descriptor for locking. *)
  val flock_of_fd
    :  ?shared:bool (** default: false, i.e. exclusive *)
    -> Fd.t
    -> Flock.t IO.t

  (** Like [flock] above. If the file does not exist, creates it with the given
      permissions before locking. *)
  val flock_create
    :  ?perm:File_permissions.t (** default: [File_permissions.u_rw] *)
    -> ?shared:bool (** default: false, i.e. exclusive *)
    -> File_path.t
    -> Flock.t IO.t

  (** Like [flock] above. If the file cannot be locked immediately due to an existing
      lock, this call returns [None] instead of waiting. *)
  val try_flock
    :  ?shared:bool (** default: false, i.e. exclusive *)
    -> File_path.t
    -> Flock.t option IO.t

  (** Like [try_flock] above. Uses an already-open file descriptor for locking. *)
  val try_flock_of_fd
    :  ?shared:bool (** default: false, i.e. exclusive *)
    -> Fd.t
    -> Flock.t option IO.t

  (** Like [try_flock] above. If the file does not exist, creates it with the given
      permissions before locking. *)
  val try_flock_create
    :  ?perm:File_permissions.t (** default: [File_permissions.u_rw] *)
    -> ?shared:bool (** default: false, i.e. exclusive *)
    -> File_path.t
    -> Flock.t option IO.t

  (** Releases the given lock. These locks are automatically released on [exit] or [exec],
      so this only needs to be used if the lock needs to be released during the execution
      of the program. Does not close any file descriptors passed to [*_of_fd] creators. *)
  val funlock : Flock.t -> unit IO.t

  (** Produces the file descriptor underlying a lock. *)
  val flock_fd : Flock.t -> Fd.t

  (** Acquires a lock for the duration of [f]. Calls [flock] before [f], and [funlock]
      after [f] returns or raises. *)
  val with_flock
    :  ?shared:bool (** default: false, i.e. exclusive *)
    -> File_path.t
    -> f:(Flock.t -> 'a IO.t)
    -> 'a IO.t

  (** Like [with_flock]. Uses an already-open file descriptor for locking. *)
  val with_flock_of_fd
    :  ?shared:bool (** default: false, i.e. exclusive *)
    -> Fd.t
    -> f:(Flock.t -> 'a IO.t)
    -> 'a IO.t

  (** Like [with_flock] above. Calls [flock_create] instead of [flock]. *)
  val with_flock_create
    :  ?perm:File_permissions.t (** default: [File_permissions.u_rw] *)
    -> ?shared:bool (** default: false, i.e. exclusive *)
    -> File_path.t
    -> f:(Flock.t -> 'a IO.t)
    -> 'a IO.t
end
