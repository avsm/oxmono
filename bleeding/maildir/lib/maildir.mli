(** Dovecot-compatible Maildir storage.

    System flags and keyword letters live in message filenames. Keyword names
    live in [dovecot-keywords]. A message's modification time is its arrival
    date. Flag changes preserve message basenames and timestamps. Nonempty
    legacy [.imap-flags] or [.imap-dates] directories require offline
    migration.

    Mutations and inventory scans acquire [dovecot-uidlist.lock]. Contention
    fails immediately. Existing locks, including stale locks, are retained.
    Stale-lock recovery must occur offline with all Maildir users stopped,
    because automatic pathname-based reclamation cannot safely exclude other
    reclaimers. Mutations also require a {!writer}, the capability that
    {!with_writer} grants under an application lease serializing whole sync
    cycles. External writers must respect the Dovecot lock for complete
    inventories to establish absence.

    Format and policy failures are {!error} results. I/O failures raise
    [Eio.Io]. The exceptions below report concurrency conditions. *)

module Dotlock = Dotlock
module Keywords = Keywords

type t
(** The type of an open Maildir. *)

type writer
(** The type of the capability to change a Maildir, valid only inside the
    {!with_writer} callback that granted it. *)

type location = New | Cur

type occurrence = private {
  id : string;
  filename : string;
  location : location;
  length : int64;
  mtime : float;  (** [mtime] is the modification time in POSIX seconds. *)
  inode : int64;
  ctime : float;
  flags : Mail_flag.Imap_flag.t list;
}
(** The type of one observation of a message file. *)

type error = Maildir_error.t =
  | Legacy_metadata of string
      (** [Legacy_metadata name] is a nonempty legacy directory [name]. *)
  | Not_a_directory of string
      (** [Not_a_directory path] is a Maildir directory that is another kind
          of file. *)
  | Non_native_path of string
      (** [Non_native_path path] is a root without a native filename. *)
  | Malformed_filename of string
      (** [Malformed_filename name] is a message entry whose name is not a
          Maildir filename. *)
  | Duplicate_identity of string
      (** [Duplicate_identity id] is an identity published more than once,
          or already published when {!append} was asked to publish it. *)
  | Unknown_letter of { file : string; letter : char }
      (** [Unknown_letter {file; letter}] is a flag letter of entry [file]
          that is neither a system letter nor a mapped keyword letter. *)
  | Keyword_map of string
      (** [Keyword_map reason] is a [dovecot-keywords] file that cannot be
          read or written for [reason]. *)
  | Unsupported_flag of Mail_flag.Imap_flag.t
      (** [Unsupported_flag flag] is [\Recent] or a system flag Maildir
          cannot store. *)
  | Too_many_keywords of Mail_flag.Imap_flag.t
      (** [Too_many_keywords keyword] is a keyword with no free slot among
          the 26 keyword letters. *)
  | Unrepresentable_date of float
      (** [Unrepresentable_date mtime] is a modification time that is not
          finite or that the filesystem cannot store exactly. *)
  | Target_exists of string
      (** [Target_exists name] is a publication or rename target that
          already exists. *)
  | Vanished of string
      (** [Vanished name] is a file that disappeared right after it was
          published or renamed. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints a one-line description of [e]. *)

exception Writer_lock_busy of string
(** [Writer_lock_busy path] is raised by {!with_writer} when another process
    or handle holds the application writer lease on the Maildir at
    [path]. *)

exception Writer_expired
(** [Writer_expired] is raised when a {!writer} is used after the
    {!with_writer} callback that granted it has returned. *)

exception Metadata_lock_busy of string
(** [Metadata_lock_busy path] is raised by {!scan}, {!fold}, {!find},
    {!append}, {!set_flags} and {!remove} when the Dovecot metadata lock at
    [path] already exists. *)

exception Metadata_lock_lost of string
(** [Metadata_lock_lost path] is raised when the metadata lock at [path] was
    removed or replaced by another party while an operation held it. *)

exception Stale_occurrence
(** [Stale_occurrence] is raised when the file an occurrence names no longer
    matches that observation, including when its flags can no longer be
    read. *)

type Eio.Exn.err += Unusable_file of string
(** [Unusable_file reason] reports a file that Maildir cannot use as it
    requires, such as a writer lease that is not a singly linked regular
    file or a message directory replaced by another kind of file. *)

val open_dir : _ Eio.Path.t -> (t, error) result
(** [open_dir path] is the Maildir at [path]. Missing standard directories
    are created and synced. A path without a native filename, a
    non-directory path and nonempty legacy metadata are errors. *)

val with_writer : t -> (writer -> 'a) -> 'a
(** [with_writer t f] is [f w] under an exclusive application writer lease,
    where [w] is the capability to change [t]. The lease is released and
    [w] expires on return, exception or cancellation. The permanent
    [.imap-writer.lock] inode must not be removed. External Maildir programs
    do not acquire this lease. Do not fork while holding it.

    @raise Writer_lock_busy if another application writer holds the lease.
    @raise Eio.Io with {!Unusable_file} if the lease file is not a singly
    linked regular file. *)

val of_writer : writer -> t
(** [of_writer w] is the Maildir that [w] changes.

    @raise Writer_expired if [w] has expired. *)

val scan : t -> (occurrence list, error) result
(** [scan t] is the complete ID-ordered inventory. Entries whose names begin
    with a dot and entries that are not regular files are skipped. Duplicate
    IDs, unrecognised filenames, unknown flag letters and an unreadable
    keyword map are errors. *)

val fold : t -> init:'a -> f:('a -> occurrence -> 'a) -> ('a, error) result
(** [fold t ~init ~f] folds [f] over the occurrences of [t] in directory
    order, [new] before [cur], under the metadata lock. Memory is bounded by
    one directory batch and the accumulator. Entries are skipped as {!scan}
    skips them. Duplicate IDs are not detected. Unrecognised filenames,
    unknown flag letters and an unreadable keyword map are errors. Exceptions
    from [f] propagate. *)

val find : t -> id:string -> (occurrence option, error) result
(** [find t ~id] is the published occurrence with identity [id], if present.
    It tests [new/id] and lists the names in [cur], but inspects only entries
    whose names carry [id]. An [id] published twice is
    [Duplicate_identity]. *)

val reserve_id : unit -> string
(** [reserve_id ()] is a fresh occurrence identity. Persist it before
    publication when recovery must attribute a local write to its journal
    operation. *)

val with_unchanged_occurrence :
  t -> occurrence -> (unit -> 'a) -> ('a, [ `Changed ]) result
(** [with_unchanged_occurrence t occurrence f] is [Ok (f ())] when identity,
    filename, location, inode, length, timestamps and flags agree before and
    after [f]. Changed observations return [Error `Changed]. Other I/O errors
    and cancellation propagate. *)

val sha256 : t -> occurrence -> string
(** [sha256 t occurrence] is its lowercase SHA-256 digest. Content is
    streamed.

    @raise Stale_occurrence if the file no longer matches [occurrence] or
    holds more than [occurrence.length] bytes.
    @raise End_of_file if the file shrinks while it is read. *)

val append : writer -> ?id:string ->
  source:_ Eio.Flow.source -> length:int64 ->
  flags:Mail_flag.Imap_flag.t list -> ?mtime:float -> unit ->
  (occurrence, error) result
(** [append w ~source ~length ~flags ()] is the durably published occurrence.
    Exactly [length] bytes are consumed. [id] defaults to a fresh reserved
    ID. A supplied [id] already published in [new] or [cur] under any flags
    is [Duplicate_identity], checked before publication. [mtime] is the
    modification time in POSIX seconds and defaults to the time the file is
    written. A supplied [mtime] is set and verified before file sync and
    publication. Unsupported flags, keyword mappings requiring more than 26
    slots and an unrepresentable [mtime] are errors returned before the
    message is published. Keyword mappings are additive and durable before
    filenames reference them.

    @raise Invalid_argument if [length] is negative or [id] was not made by
    {!reserve_id}.
    @raise Writer_expired if [w] has expired. *)

val check_append : writer -> flags:Mail_flag.Imap_flag.t list ->
  ?mtime:float -> unit -> (unit, error) result
(** [check_append w ~flags ?mtime ()] is the error that {!append} with
    [flags] and [mtime] would return whatever the source: an unsupported
    flag, a keyword with no free slot in [dovecot-keywords], or an [mtime]
    that is not finite. It writes nothing. [Ok ()] does not guarantee that
    {!append} succeeds.

    @raise Writer_expired if [w] has expired. *)

val open_message : t -> sw:Eio.Switch.t -> occurrence ->
  Eio.File.ro_ty Eio.Resource.t
(** [open_message t ~sw occurrence] is its read-only file owned by [sw].

    @raise Stale_occurrence if the file no longer matches [occurrence]. *)

val set_flags : writer -> occurrence -> Mail_flag.Imap_flag.t list ->
  (occurrence, error) result
(** [set_flags w occurrence flags] is the updated occurrence in [cur]. The
    basename, modification time, Passed flag and filename extension fields
    are preserved. An unsupported flag set and an existing file at the new
    name are errors.

    @raise Stale_occurrence if the file no longer matches [occurrence].
    @raise Writer_expired if [w] has expired. *)

val remove : writer -> occurrence -> unit
(** [remove w occurrence] unlinks the exact occurrence and syncs its
    directory.

    @raise Stale_occurrence if the file no longer matches [occurrence].
    @raise Writer_expired if [w] has expired. *)

type recovery = { removed_temporary : string list }

val recover : writer -> recovery
(** [recover w] removes abandoned owned temporary files. Call it only at
    startup. Standard Dovecot metadata is retained.

    @raise Writer_expired if [w] has expired. *)
