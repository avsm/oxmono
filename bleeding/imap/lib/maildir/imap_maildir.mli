(** Dovecot-compatible Maildir storage.

    System flags and keyword letters live in message filenames. Keyword names
    live in [dovecot-keywords]. Message modification times represent INTERNALDATE.
    Flag changes preserve message basenames and timestamps. Nonempty legacy
    [.imap-flags] or [.imap-dates] directories require offline migration.

    Mutations and inventory scans acquire [dovecot-uidlist.lock]. Contention
    fails immediately. Existing locks, including stale locks, are retained.
    Stale-lock recovery must occur offline with all Maildir users stopped;
    automatic pathname-based reclamation cannot safely exclude other reclaimers.
    The application writer lease additionally serializes
    whole sync cycles. External writers must respect the Dovecot lock for
    complete inventories to establish absence. *)

type t
type location = New | Cur
type occurrence = private {
  id : string;
  filename : string;
  location : location;
  length : int64;
  mtime : float;
  inode : int64;
  ctime : float;
  flags : Mail_flag.Imap_flag.t list;
  internal_date : Imap.Internal_date.t option;
}

exception Writer_lock_busy of string
(** [Writer_lock_busy path] is raised by {!with_writer_lock} when another
    process or handle holds the application writer lease on the Maildir at
    [path]. *)

exception Metadata_lock_busy of string
(** [Metadata_lock_busy path] is raised by {!scan}, {!find},
    {!with_inventory_pages}, {!append}, {!set_flags} and {!remove} when the
    Dovecot metadata lock at [path] already exists. *)

exception Metadata_lock_lost of string
(** [Metadata_lock_lost path] is raised when the metadata lock at [path] was
    removed or replaced by another party while an operation held it. *)

exception Stale_occurrence
(** [Stale_occurrence] is raised when the file an occurrence names no longer
    matches that observation. *)

val open_dir : _ Eio.Path.t -> t
(** [open_dir path] is the Maildir at [path]. Missing standard directories are
    created and synced. A path without a native filename raises [Eio.Io].
    Unsupported legacy metadata and a non-directory path raise [Failure]. *)

val with_writer_lock : t -> (unit -> 'a) -> 'a
(** [with_writer_lock t f] is [f ()] under an exclusive application writer
    lease. The lease is released on return, exception or cancellation. Its
    permanent [.imap-writer.lock] inode must not be removed. External Maildir
    programs do not acquire this application lease. Do not fork while holding
    it.

    @raise Writer_lock_busy if another application writer holds the lease. *)

val scan : t -> occurrence list
(** [scan t] is the complete ID-ordered inventory. Entries whose names begin
    with a dot and entries that are not regular files are skipped. Duplicate
    IDs, unrecognised filenames, unknown flag letters and modification times
    outside the supported range raise [Failure]. *)
val find : t -> id:string -> occurrence option
(** [find t ~id] is the published occurrence with identity [id], if present.
    It tests [new/id] and lists the names in [cur], but inspects only entries
    whose names carry [id]. An [id] published twice raises [Failure]. *)
val reserve_id : unit -> string
(** [reserve_id ()] is a fresh occurrence identity. Persist it before
    publication when recovery must attribute a local write to its journal
    operation. *)

type paged_inventory
type inventory_page = {
  occurrences : occurrence list;
  next_after : string option;
}
val with_inventory_pages : t -> (paged_inventory -> 'a) -> 'a
(** [with_inventory_pages t f] is [f view] for a complete disk-staged inventory.
    Memory is bounded by directory batches, the SQLite cache and requested pages.
    The metadata lock covers staging and is released before [f]. The view expires
    and its temporary files are removed on return, exception or cancellation.
    Concurrent changes can invalidate observations after staging. *)
val inventory_count : paged_inventory -> int64
(** [inventory_count view] is the number of staged occurrences. *)
val inventory_find : paged_inventory -> id:string -> occurrence option
(** [inventory_find view ~id] is the staged occurrence with identity [id]. *)
val inventory_page : paged_inventory -> ?after:string -> limit:int -> unit -> inventory_page
(** [inventory_page view ~after ~limit ()] is the next ID-ordered page.
    [after] defaults to the beginning. [limit] must be positive. *)
val with_unchanged_occurrence : ?inventory:paged_inventory ->
  t -> occurrence -> (unit -> 'a) -> ('a, [ `Changed ]) result
(** [with_unchanged_occurrence t occurrence f] is [Ok (f ())] when identity,
    filename, location, inode, length, timestamps and flags agree before and after [f].
    Changed observations return [Error `Changed]. Other I/O errors and
    cancellation propagate. [inventory] must be a live view owned by [t]. *)
val upload_internal_date : occurrence -> (Imap.Internal_date.t, string) result
(** [upload_internal_date occurrence] is its UTC whole-second modification time
    as an IMAP date. Unsupported timestamps return an error. *)
val sha256 : ?inventory:paged_inventory -> t -> occurrence -> string
(** [sha256 t occurrence] is its lowercase SHA-256 digest. Content is
    streamed.

    @raise Stale_occurrence if the file no longer matches [occurrence] or
    holds more than [occurrence.length] bytes.
    @raise End_of_file if the file shrinks while it is read. *)
val append : ?inventory:paged_inventory -> t -> ?id:string ->
  source:_ Eio.Flow.source -> length:int64 -> flags:Mail_flag.Imap_flag.t list ->
  ?internal_date:Imap.Internal_date.t -> unit -> occurrence
(** [append t ~source ~length ~flags ()] is the durably published occurrence.
    Exactly [length] bytes are consumed. [id] defaults to a fresh reserved ID.
    A supplied [id] already published in [new] or [cur] under any flags, or
    recorded in [inventory], raises [Failure] before publication.
    [internal_date] defaults to the new file's modification time. Supplied dates
    are set and verified before file sync and publication. Leap seconds and
    unrepresentable timestamps fail. Unknown system flags, Recent and keyword
    mappings requiring more than 26 slots fail before message publication.
    Keyword mappings are additive and durable before filenames reference them.
    [inventory], when supplied, must remain live under the writer lease. *)
val open_message : ?inventory:paged_inventory -> t -> sw:Eio.Switch.t -> occurrence ->
  Eio.File.ro_ty Eio.Resource.t
(** [open_message t ~sw occurrence] is its read-only file owned by [sw].

    @raise Stale_occurrence if the file no longer matches [occurrence]. *)
val set_flags : t -> occurrence -> Mail_flag.Imap_flag.t list -> occurrence
(** [set_flags t occurrence flags] is the updated occurrence in [cur].
    The basename, modification time, Passed flag and filename extension fields
    are preserved. An unsupported flag set or an existing file at the new
    name raises [Failure].

    @raise Stale_occurrence if the file no longer matches [occurrence]. *)
val remove : t -> occurrence -> unit
(** [remove t occurrence] unlinks the exact occurrence and syncs its
    directory.

    @raise Stale_occurrence if the file no longer matches [occurrence]. *)
type recovery = { removed_temporary : string list }
val recover : t -> recovery
(** [recover t] removes abandoned owned temporary files and inventory indexes.
    Call only at startup under exclusive application writer ownership. Standard
    Dovecot metadata is retained. *)
