(** room_keys — the [/room_keys] endpoints, raising instead of returning a
    result.

    These move ciphertext to and from the server. {!Backup} owns the keys that
    encrypt it. A backup has a version, an opaque server-assigned string naming
    one generation, and only the newest version accepts writes. Every function
    here raises [Eio.Io] carrying an {!Error.type-err} where its
    {!Matrix_client.Room_keys} counterpart returns an error. *)

(** {1 Backups and their contents} *)

type version_info = Matrix_client.Room_keys.version_info = {
  version : string;
  algorithm : string;
  auth_data : Jsont.json;
  count : int;
  etag : string;
}
(** The type for a backup version as the server describes it. It is
    {!Matrix_client.Room_keys.version_info}, which documents the fields. *)

val equal_version_info : version_info -> version_info -> bool
(** [equal_version_info a b] is [true] when [a] and [b] have equal fields.
    Comparing the [etag] of a version fetched now with one fetched earlier is
    how a client notices that another device has written to the backup. *)

type key_backup_data = Matrix_client.Room_keys.key_backup_data = {
  first_message_index : int;
  forwarded_count : int;
  is_verified : bool;
  session_data : Matrix_client.Backup.encrypted_session_data;
}
(** The type for one backed-up Megolm session. It is
    {!Matrix_client.Backup.key_backup_data}, which documents the fields. *)

type sessions = Matrix_client.Room_keys.sessions
(** The type for the sessions of one room, keyed by Megolm session identifier.
*)

type rooms = Matrix_client.Room_keys.rooms
(** The type for backed-up sessions keyed by room identifier. *)

type update_response = Matrix_client.Room_keys.update_response = {
  etag : string;
  count : int;
}
(** The type for the result of any write to the backup. It is
    {!Matrix_client.Room_keys.update_response}, which documents the fields. *)

(** {1 Versions} *)

val create_version :
  Client.t -> algorithm:string -> auth_data:Jsont.json -> string
(** [create_version c ~algorithm ~auth_data] is
    {!Matrix_client.Room_keys.create_version} with the result unwrapped, the new
    version, which becomes the current one. Keys in earlier versions are
    abandoned rather than migrated. *)

val get_current_version : Client.t -> version_info
(** [get_current_version c] is {!Matrix_client.Room_keys.get_current_version}
    with the result unwrapped. A user with no backup at all raises
    [M_NOT_FOUND]. *)

val get_version : Client.t -> version:string -> version_info
(** [get_version c ~version] is {!Matrix_client.Room_keys.get_version} with the
    result unwrapped. *)

val update_version :
  Client.t -> version:string -> algorithm:string -> auth_data:Jsont.json -> unit
(** [update_version c ~version ~algorithm ~auth_data] is
    {!Matrix_client.Room_keys.update_version} with the result unwrapped. The
    version's keys are left in place, and its algorithm and public key must not
    change. *)

val delete_version : Client.t -> version:string -> unit
(** [delete_version c ~version] is {!Matrix_client.Room_keys.delete_version}
    with the result unwrapped. It deletes the version {e and every key in it},
    and cannot be undone. *)

(** {1 Keys} *)

val put_keys : Client.t -> version:string -> rooms -> update_response
(** [put_keys c ~version rooms] is {!Matrix_client.Room_keys.put_keys} with the
    result unwrapped. [version] must be the current backup version. *)

val get_keys : Client.t -> version:string -> rooms
(** [get_keys c ~version] is {!Matrix_client.Room_keys.get_keys} with the result
    unwrapped, the whole backup in one request. *)

val delete_keys : Client.t -> version:string -> update_response
(** [delete_keys c ~version] is {!Matrix_client.Room_keys.delete_keys} with the
    result unwrapped. The version itself is kept. *)

val put_room_keys :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  sessions ->
  update_response
(** [put_room_keys c ~version ~room_id sessions] is
    {!Matrix_client.Room_keys.put_room_keys} with the result unwrapped. *)

val get_room_keys :
  Client.t -> version:string -> room_id:Matrix_proto.Id.Room_id.t -> sessions
(** [get_room_keys c ~version ~room_id] is
    {!Matrix_client.Room_keys.get_room_keys} with the result unwrapped. *)

val delete_room_keys :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  update_response
(** [delete_room_keys c ~version ~room_id] is
    {!Matrix_client.Room_keys.delete_room_keys} with the result unwrapped. *)

val put_session_key :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  key_backup_data ->
  update_response
(** [put_session_key c ~version ~room_id ~session_id key] is
    {!Matrix_client.Room_keys.put_session_key} with the result unwrapped. *)

val get_session_key :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  key_backup_data
(** [get_session_key c ~version ~room_id ~session_id] is
    {!Matrix_client.Room_keys.get_session_key} with the result unwrapped. *)

val delete_session_key :
  Client.t ->
  version:string ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  update_response
(** [delete_session_key c ~version ~room_id ~session_id] is
    {!Matrix_client.Room_keys.delete_session_key} with the result unwrapped. *)
