@@ portable

(** Complete Maildir inventories staged in SQLite and read in pages.

    A view is staged in a file in the caller's spool directory, so memory is
    bounded by one directory batch, the SQLite cache and the requested pages.
    A function below that takes an optional view raises [Invalid_argument]
    before it calls [Maildir] unless the view is live and staged from the
    same Maildir handle. *)

type t
(** The type for staged inventory views. *)

type page = {
  occurrences : Maildir.occurrence list;
  next_after : string option;
}

val with_pages : spool_dir:_ Eio.Path.t -> Maildir.t -> (t -> 'a) ->
  ('a, Maildir.error) result @@ nonportable
(** [with_pages ~spool_dir maildir f] is [Ok (f view)] for a complete
    inventory of [maildir] staged in a new file in [spool_dir]. The Maildir
    metadata lock covers staging and is released before [f]. The view
    expires and its file is removed on return, exception or cancellation.
    Concurrent changes can invalidate observations after staging. A
    duplicate identity and the errors of {!Maildir.fold} are returned
    without calling [f]. *)

val count : t -> int64
(** [count view] is the number of staged occurrences. *)

val find : t -> id:string -> Maildir.occurrence option
(** [find view ~id] is the staged occurrence with identity [id]. *)

val page : t -> ?after:string -> limit:int -> unit -> page
(** [page view ~after ~limit ()] is the next ID-ordered page of at most
    [limit] occurrences. [after] defaults to the beginning. [limit] must be
    positive. *)

val with_unchanged_occurrence : ?inventory:t -> Maildir.t ->
  Maildir.occurrence -> (unit -> 'a) -> ('a, [ `Changed ]) result @@ nonportable
(** [with_unchanged_occurrence ?inventory maildir o f] is
    {!Maildir.with_unchanged_occurrence} after checking [inventory]. *)

val sha256 : ?inventory:t -> Maildir.t -> Maildir.occurrence -> string
  @@ nonportable
(** [sha256 ?inventory maildir o] is {!Maildir.sha256} after checking
    [inventory]. *)

val open_message : ?inventory:t -> Maildir.t -> sw:Eio.Switch.t ->
  Maildir.occurrence -> Eio.File.ro_ty Eio.Resource.t @@ nonportable
(** [open_message ?inventory maildir ~sw o] is {!Maildir.open_message} after
    checking [inventory]. *)

val append : ?inventory:t -> Maildir.writer -> ?id:string ->
  source:_ Eio.Flow.source -> length:int64 ->
  flags:Mail_flag.Imap_flag.t list -> ?mtime:float -> unit ->
  (Maildir.occurrence, Maildir.error) result @@ nonportable
(** [append ?inventory w ~source ~length ~flags ()] is {!Maildir.append}
    after checking [inventory] against the Maildir of [w]. A supplied [id]
    that [inventory] staged, or that an earlier [append] through [inventory]
    published, is [Duplicate_identity] before anything is written. *)

val recover : _ Eio.Path.t -> string list @@ nonportable
(** [recover spool_dir] removes staging files that an interrupted process
    left in [spool_dir] and is the list of their names. Call it only while
    no view is staged in [spool_dir]. *)
