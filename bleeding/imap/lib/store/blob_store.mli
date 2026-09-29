@@ portable

(** Content-addressed message files and their snapshot references,
    documented in [Imap_store.Blob]. *)

type t = Database.t

type blob = private { sha256 : string; length : int64 }

exception Digest_mismatch

val put : t -> source:_ Eio.Flow.source -> length:int64 ->
  ?expected_sha256:string -> unit -> blob @@ nonportable
(** [put t ~source ~length ()] durably stores exactly [length] octets from
    [source] under their SHA-256 digest, raising [Digest_mismatch] when
    [expected_sha256] differs. *)

val verify : t -> blob -> bool @@ nonportable
(** [verify t blob] holds when the file still has the length and digest of
    [blob]. *)

val open_in : t -> sw:Eio.Switch.t -> blob ->
  Eio.File.ro_ty Eio.Resource.t @@ nonportable
val attach : ?verify:bool -> t -> scope:Imap.Mirror.scope ->
  uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
  blob -> unit @@ nonportable
(** [attach ?verify t ~scope ~uidvalidity ~uid blob] references [blob] from a
    message of the current epoch, rehashing it first when [verify] is [true],
    the default. *)

val find : t -> scope:Imap.Mirror.scope ->
  uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
  blob option
val missing_page : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Uid.t ->
  limit:int -> unit ->
  [ `Uids of Imap.Uid.t list | `Stale_revision ]
val referenced_page : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Uid.t ->
  limit:int -> unit ->
  [ `Refs of (Imap.Uid.t * blob) list | `Stale_revision ]
val detach_if_matches : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> uid:Imap.Uid.t -> blob ->
  [ `Detached | `Unchanged | `Stale_revision ]
(** [detach_if_matches t ~scope ~cursor ~uid blob] removes the reference only
    while the cursor and the reference still match, and never unlinks a file. *)

val iter_orphan_candidates : t -> (string -> unit) -> unit @@ nonportable
(** [iter_orphan_candidates t f] visits unreferenced blobs and temporary files,
    and requires every blob writer to be quiescent. *)

val reap_orphans_iter : t -> removed:(string -> unit) -> unit @@ nonportable
(** [reap_orphans_iter t ~removed] unlinks every orphan candidate under the same
    quiescence rule as {!iter_orphan_candidates}. *)
