(** room_keys — the [/room_keys] endpoints.

    A backup has a {e version}, an opaque server-assigned string naming one
    generation with its own algorithm and public key, and only the newest
    version accepts writes. Nothing here needs the backup's private key.
    {!Backup} encrypts the keys client-side beforehand and decrypts what comes
    back.

    @see <https://spec.matrix.org/v1.11/client-server-api/#server-side-key-backups>
      Server-side key backups *)

(** {1 Backups and their contents} *)

type version_info = {
  version : string;  (** The opaque version identifier. *)
  algorithm : string;  (** For example {!Backup.backup_algorithm}. *)
  auth_data : Jsont.json;
      (** Algorithm-dependent. Under [m.megolm_backup.v1.curve25519-aes-sha2] it
          decodes with {!Backup.megolm_v1_auth_data_jsont}. *)
  count : int;  (** How many keys the backup holds. *)
  etag : string;  (** Changes whenever the backup's contents do. *)
}
(** The type for a backup version as the server describes it. *)

val equal_version_info : version_info -> version_info -> bool
(** [equal_version_info a b] is [true] when [a] and [b] have equal fields.
    Comparing the [etag] of a version fetched now with one fetched earlier is
    how a client notices that another device has written to the backup. *)

type key_backup_data = Backup.key_backup_data = {
  first_message_index : int;
  forwarded_count : int;
  is_verified : bool;
  session_data : Backup.encrypted_session_data;
}
(** The type for one backed-up Megolm session. It is {!Backup.key_backup_data},
    which documents the fields. *)

type sessions = Backup.sessions
(** The type for the sessions of one room. It is {!Backup.sessions}. *)

type rooms = Backup.rooms
(** The type for the backed-up sessions of several rooms. It is {!Backup.rooms}.
*)

type update_response = { etag : string; count : int }
(** The type for the result of any write to the backup. *)

(** {1 Versions} *)

val create_version :
  Client.t ->
  algorithm:string ->
  auth_data:Jsont.json ->
  (string, Error.t) result
(** [create_version t ~algorithm ~auth_data] creates a backup and returns its
    version, which becomes the current one. Keys in earlier versions are
    abandoned rather than migrated.

    Uses [POST /_matrix/client/v3/room_keys/version]. *)

val get_current_version : Client.t -> (version_info, Error.t) result
(** [get_current_version t] describes the latest backup version. It is
    [Error (Matrix_error { errcode = M_NOT_FOUND; _ })] when the user has no
    backup at all.

    Uses [GET /_matrix/client/v3/room_keys/version]. *)

val get_version : Client.t -> version:string -> (version_info, Error.t) result
(** [get_version t ~version] describes one specific backup version.

    Uses [GET /_matrix/client/v3/room_keys/version/{version}]. *)

val update_version :
  Client.t ->
  version:string ->
  algorithm:string ->
  auth_data:Jsont.json ->
  (unit, Error.t) result
(** [update_version t ~version ~algorithm ~auth_data] replaces a version's auth
    data, leaving its keys in place. The algorithm and public key must not
    change; the usual reason to call it is to add a signature over the public
    key from a newly verified device.

    Uses [PUT /_matrix/client/v3/room_keys/version/{version}]. *)

val delete_version : Client.t -> version:string -> (unit, Error.t) result
(** [delete_version t ~version] deletes a backup version
    {e and every key in it}. This cannot be undone.

    Uses [DELETE /_matrix/client/v3/room_keys/version/{version}]. *)

(** {1 Keys} *)

val put_keys :
  Client.t -> version:string -> rooms -> (update_response, Error.t) result
(** [put_keys t ~version rooms] stores keys for several rooms at once. [version]
    must be the current backup version, or the request fails with
    [M_WRONG_ROOM_KEYS_VERSION]. A key already in the backup is replaced only
    when the new one is better: verified over unverified, then fewer forwards,
    then a lower first message index.

    Uses [PUT /_matrix/client/v3/room_keys/keys?version=]. *)

val get_keys : Client.t -> version:string -> (rooms, Error.t) result
(** [get_keys t ~version] retrieves the whole backup, in one request. It is the
    empty list when the backup holds no keys.

    Uses [GET /_matrix/client/v3/room_keys/keys?version=]. *)

val delete_keys :
  Client.t -> version:string -> (update_response, Error.t) result
(** [delete_keys t ~version] deletes every key in a version, keeping the version
    itself.

    Uses [DELETE /_matrix/client/v3/room_keys/keys?version=]. *)

val put_room_keys :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  sessions ->
  (update_response, Error.t) result
(** [put_room_keys t ~version ~room_id sessions] stores the keys of one room.

    Uses [PUT /_matrix/client/v3/room_keys/keys/{roomId}?version=]. *)

val get_room_keys :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (sessions, Error.t) result
(** [get_room_keys t ~version ~room_id] retrieves the keys of one room.

    Uses [GET /_matrix/client/v3/room_keys/keys/{roomId}?version=]. *)

val delete_room_keys :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (update_response, Error.t) result
(** [delete_room_keys t ~version ~room_id] deletes the keys of one room.

    Uses [DELETE /_matrix/client/v3/room_keys/keys/{roomId}?version=]. *)

val put_session_key :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  key_backup_data ->
  (update_response, Error.t) result
(** [put_session_key t ~version ~room_id ~session_id key] stores a single
    session key.

    Uses [PUT /_matrix/client/v3/room_keys/keys/{roomId}/{sessionId}?version=].
*)

val get_session_key :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  (key_backup_data, Error.t) result
(** [get_session_key t ~version ~room_id ~session_id] retrieves a single session
    key.

    Uses [GET /_matrix/client/v3/room_keys/keys/{roomId}/{sessionId}?version=].
*)

val delete_session_key :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  (update_response, Error.t) result
(** [delete_session_key t ~version ~room_id ~session_id] deletes a single
    session key.

    Uses
    [DELETE /_matrix/client/v3/room_keys/keys/{roomId}/{sessionId}?version=]. *)
