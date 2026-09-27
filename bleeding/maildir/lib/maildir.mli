(** Maildir storage in the Dovecot layout.

    A Maildir holds one file per message in [new] or [cur]. System flags and
    keyword letters live in the filename after [:2,], and keyword names live
    in [dovecot-keywords], so Dovecot and other Maildir programs can share
    the directory. A message's modification time is its date. Flag changes
    keep the basename and the modification time. A nonempty legacy
    [.imap-flags] or [.imap-dates] directory needs offline migration.

    Scans, lookups and mutations take the Dovecot metadata lock
    [dovecot-uidlist.lock]. An existing lock, stale or not, fails the
    operation at once and is left in place. Remove a stale lock offline
    with every user of the Maildir stopped, since no pathname check can
    reclaim it safely against another reclaimer. Mutations also take a
    {!writer}, which {!with_writer} grants under an application lease that
    serializes whole sync cycles. A complete scan proves a message absent
    only if external writers honour the metadata lock.

    Format and policy failures are {!error} results. I/O failures raise
    [Eio.Io]. The exceptions report concurrency conditions. *)

module Dotlock = Dotlock
(** Scoped exclusive dotlocks. *)

module Keywords = Keywords
(** Keyword maps and filename flag letters. *)

(** {1 Maildirs and occurrences} *)

type t
(** The type for open Maildirs. *)

type writer
(** The type for the capability to change a Maildir. A writer is valid only
    inside the {!with_writer} callback that granted it. *)

type location =
  | New  (** The [new] directory, for messages without flags. *)
  | Cur  (** The [cur] directory. *)
(** The type for message directories. *)

type occurrence = private {
  id : string;  (** The identity, the filename up to [:2,]. *)
  filename : string;  (** The name within the directory [location]. *)
  location : location;
  length : int64;  (** The size in bytes. *)
  mtime : float;  (** The modification time in POSIX seconds. *)
  inode : int64;
  ctime : float;  (** The status change time in POSIX seconds. *)
  flags : Mail_flag.Imap_flag.t list;
      (** The flags the filename denotes, normalised by
          [Mail_flag.Imap_flag.durable]. *)
}
(** The type for observations of one message file. An operation that takes
    an occurrence first checks that the file still matches it. *)

(** {1 Errors} *)

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
(** The type for format and policy failures. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints a one-line description of [e] on [ppf]. *)

exception Writer_lock_busy of string
(** [Writer_lock_busy path] is raised by {!with_writer} when another writer
    holds the application lease on the Maildir at [path]. *)

exception Writer_expired
(** [Writer_expired] is raised when a {!writer} is used after the
    {!with_writer} callback that granted it has returned. *)

exception Metadata_lock_busy of string
(** [Metadata_lock_busy path] is raised by {!scan}, {!fold}, {!find},
    {!check_append}, {!append}, {!set_flags} and {!remove} when the Dovecot
    metadata lock at [path] already exists. It is the same exception as
    {!Dotlock.Busy}. *)

exception Metadata_lock_lost of string
(** [Metadata_lock_lost path] is raised by the same operations when another
    party removed or replaced the metadata lock at [path] while they held
    it. It is the same exception as {!Dotlock.Lost}. *)

exception Stale_occurrence
(** [Stale_occurrence] is raised when the file an occurrence names no longer
    matches that observation, including when its flags can no longer be
    read. *)

type Eio.Exn.err += Unusable_file of string
(** [Unusable_file reason] reports a file that Maildir cannot use as it
    requires, such as a writer lease that is not a singly linked regular
    file or a message directory replaced by another kind of file. *)

(** {1 Opening and writing access} *)

val open_dir : _ Eio.Path.t -> (t, error) result
(** [open_dir path] is the Maildir at [path]. The directory [path] and its
    [tmp], [new] and [cur] are created and synced when missing, but a
    missing parent of [path] is not. The error covers a [path] without a
    native filename, a directory that is another kind of file and nonempty
    legacy metadata. *)

val with_writer : t -> (writer -> 'a) -> 'a
(** [with_writer t f] is [f w] under an exclusive application lease on
    [t], where [w] is the capability to change [t]. The lease is released
    and [w] expires when [f] returns, raises or is cancelled. The lease
    lives on the permanent file [.imap-writer.lock], whose inode must never
    be removed. External Maildir programs do not take the
    lease. Do not fork while holding it.

    @raise Writer_lock_busy if another writer holds the lease, including a
    nested [with_writer] on the same Maildir in this process.
    @raise Eio.Io with {!Unusable_file} if the lease file is not a singly
    linked regular file. *)

val of_writer : writer -> t
(** [of_writer w] is the Maildir that [w] changes.

    @raise Writer_expired if [w] has expired. *)

(** {1 Reading} *)

val scan : t -> (occurrence list, error) result
(** [scan t] is the complete inventory of [t] in ascending [id] order.
    Entries whose names begin with a dot and entries that are not regular
    files are skipped. The error covers a repeated [id], an unrecognised
    filename, an unknown flag letter and an unreadable keyword map. *)

val fold : t -> init:'a -> f:('a -> occurrence -> 'a) -> ('a, error) result
(** [fold t ~init ~f] folds [f] over the occurrences of [t] in directory
    order, [new] before [cur], starting from [init], under the metadata
    lock. Memory is bounded by one directory batch and the accumulator.
    Entries are skipped as {!scan} skips them, and a repeated [id] is not
    detected. The error covers an unrecognised filename, an unknown flag
    letter and an unreadable keyword map. An exception from [f]
    propagates. *)

val find : t -> id:string -> (occurrence option, error) result
(** [find t ~id] is the published occurrence with identity [id], if any.
    It tests [new/id] and lists the names in [cur], but inspects only
    entries whose names carry [id]. An [id] published twice is
    [Duplicate_identity]. *)

val with_unchanged_occurrence :
  t -> occurrence -> (unit -> 'a) -> ('a, [ `Changed ]) result
(** [with_unchanged_occurrence t occurrence f] is [Ok (f ())] when the file
    matches [occurrence] before and after [f] in identity, filename,
    location, inode, length, both times and flags. Otherwise it is
    [Error `Changed]. When [f] raises [Stale_occurrence], [End_of_file] or
    a not-found [Eio.Io] after the file changed, the result is also
    [Error `Changed]. Any other exception, and cancellation, propagates.
    The metadata lock is not taken. *)

val open_message : t -> sw:Eio.Switch.t -> occurrence ->
  Eio.File.ro_ty Eio.Resource.t
(** [open_message t ~sw occurrence] is the file of [occurrence], open for
    reading and owned by [sw].

    @raise Stale_occurrence if the file no longer matches [occurrence]. *)

val sha256 : t -> occurrence -> string
(** [sha256 t occurrence] is the lowercase hexadecimal SHA-256 digest of
    the file of [occurrence]. The content is streamed.

    @raise Stale_occurrence if the file no longer matches [occurrence] or
    holds more than [occurrence.length] bytes.
    @raise End_of_file if the file shrinks while it is read. *)

(** {1 Writing} *)

val reserve_id : unit -> string
(** [reserve_id ()] is a fresh occurrence identity, [im-] followed by 32
    random lowercase hexadecimal digits. Persist it before {!append} when
    recovery must attribute a local write to its journal operation. *)

val append : writer -> ?id:string ->
  source:_ Eio.Flow.source -> length:int64 ->
  flags:Mail_flag.Imap_flag.t list -> ?mtime:float -> unit ->
  (occurrence, error) result
(** [append w ~id ~source ~length ~flags ~mtime ()] is the occurrence of a
    new message holding exactly [length] bytes read from [source] with
    [flags]. The message is synced to disk and then published, in [new]
    when [flags] is empty and in [cur] otherwise. [id] defaults to
    [reserve_id ()]. A supplied [id] already published in [new] or [cur]
    under any flags is [Duplicate_identity], checked under the metadata
    lock before publication. [mtime] is the modification time in POSIX
    seconds and defaults to the time the file is written. A supplied
    [mtime] is set and verified before publication. An unsupported flag, a
    keyword with no free slot and an unrepresentable [mtime] are errors
    returned before the message is published. New keyword mappings are
    written and synced before any filename refers to them.

    @raise Invalid_argument if [length] is negative or [id] does not have
    the form {!reserve_id} gives.
    @raise End_of_file if [source] ends before [length] bytes.
    @raise Writer_expired if [w] has expired. *)

val check_append : writer -> flags:Mail_flag.Imap_flag.t list ->
  ?mtime:float -> unit -> (unit, error) result
(** [check_append w ~flags ~mtime ()] is the error that {!append} with
    [flags] and [mtime] would return before reading its source. It covers
    an unsupported flag, a keyword with no free slot in [dovecot-keywords],
    an unreadable keyword map and an [mtime] that is not finite. When
    [flags] hold a keyword it reads the keyword map under the metadata
    lock, as {!append} does. It writes nothing, and [Ok ()] does not
    guarantee that {!append} succeeds, since another writer of
    [dovecot-keywords] can fill the free slots after the lock is released.

    @raise Metadata_lock_busy if [flags] hold a keyword and the metadata
    lock is held.
    @raise Metadata_lock_lost if the metadata lock is lost while held.
    @raise Writer_expired if [w] has expired. *)

val set_flags : writer -> occurrence -> Mail_flag.Imap_flag.t list ->
  (occurrence, error) result
(** [set_flags w occurrence flags] is [occurrence] with [flags], renamed
    into [cur]. The basename, modification time, Passed letter and the
    filename fields after the flag letters are kept. An occurrence already
    in [cur] under the resulting name is returned unchanged. The error
    covers an unsupported flag, a keyword with no free slot and an
    existing file at the new name.

    @raise Stale_occurrence if the file no longer matches [occurrence].
    @raise Writer_expired if [w] has expired. *)

val remove : writer -> occurrence -> unit
(** [remove w occurrence] unlinks the file of [occurrence] and syncs its
    directory.

    @raise Stale_occurrence if the file no longer matches [occurrence].
    @raise Writer_expired if [w] has expired. *)

(** {1 Recovery} *)

type recovery = { removed_temporary : string list }
(** The type for recovery reports. [removed_temporary] names the files
    removed from [tmp]. *)

val recover : writer -> recovery
(** [recover w] removes the temporary files an interrupted {!append} or
    keyword map update left in [tmp]. Call it only at startup. Other files,
    including standard Dovecot metadata, are left alone.

    @raise Writer_expired if [w] has expired. *)
