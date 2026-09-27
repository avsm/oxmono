(** Content-addressed message files and their snapshot references,
    documented in [Imap_store.Blob]. *)

type t = Database.t

type blob = private { sha256 : string; length : int64 }

exception Digest_mismatch

val put : t -> source:_ Eio.Flow.source -> length:int64 ->
  ?expected_sha256:string -> unit -> blob
(** [put t ~source ~length ()] durably stores exactly [length] octets from
    [source] under their SHA-256 digest, raising [Digest_mismatch] when
    [expected_sha256] differs. *)

val verify : t -> blob -> bool
(** [verify t blob] holds when the file still has the length and digest of
    [blob]. *)

val open_in : t -> sw:Eio.Switch.t -> blob -> Eio.File.ro_ty Eio.Resource.t
val attach : ?verify:bool -> t -> scope:Imap.Mirror.scope ->
  uidvalidity:Imap.Proto.Uidvalidity.t -> uid:Imap.Proto.Uid.t ->
  blob -> unit
(** [attach ?verify t ~scope ~uidvalidity ~uid blob] references [blob] from a
    message of the current epoch, rehashing it first when [verify] is [true],
    the default. *)

val find : t -> scope:Imap.Mirror.scope ->
  uidvalidity:Imap.Proto.Uidvalidity.t -> uid:Imap.Proto.Uid.t ->
  blob option
val missing_page : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Proto.Uid.t ->
  limit:int -> unit ->
  [ `Uids of Imap.Proto.Uid.t list | `Stale_revision ]
val referenced_page : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Proto.Uid.t ->
  limit:int -> unit ->
  [ `Refs of (Imap.Proto.Uid.t * blob) list | `Stale_revision ]
val detach_if_matches : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> uid:Imap.Proto.Uid.t -> blob ->
  [ `Detached | `Unchanged | `Stale_revision ]
(** [detach_if_matches t ~scope ~cursor ~uid blob] removes the reference only
    while the cursor and the reference still match, and never unlinks a file. *)

val iter_orphan_candidates : t -> (string -> unit) -> unit
(** [iter_orphan_candidates t f] visits unreferenced blobs and temporary files,
    and requires every blob writer to be quiescent. *)

val reap_orphans_iter : t -> removed:(string -> unit) -> unit
(** [reap_orphans_iter t ~removed] unlinks every orphan candidate under the same
    quiescence rule as {!iter_orphan_candidates}. *)

val orphan_candidates : t -> string list
(** [orphan_candidates t] is the sorted list that {!iter_orphan_candidates}
    visits. *)

val reap_orphans : t -> string list
(** [reap_orphans t] is the sorted list of names that {!reap_orphans_iter}
    removed. *)
