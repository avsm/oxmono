(** session — what a profile's files hold.

    A profile keeps its credentials and its cryptographic state as JSON files in
    one directory. Each module here is one record of that schema together with
    the codec that reads and writes it, so a caller can inspect or migrate a
    file without going through {!Profile_store}, which owns the directory.

    Ratchet state appears as an opaque string in every record below.
    {!Session_pickle} is what makes and reads one. *)

(** {1 [session.json]} *)

(** Which account, on which homeserver. *)
module Server : sig
  type t = { homeserver : Uriz.t; user_id : Matrix_proto.Id.User_id.t }
  (** The type for the account's home. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** What authenticates the account. *)
module Auth : sig
  type method_ =
    | Matrix
    | OAuth of { client_id : string }
        (** How the stored tokens were issued. [Matrix] sessions are invalidated
            by the Client-Server [/logout] endpoint. An [OAuth] session retains
            the dynamic [client_id] needed to revoke its tokens at the
            authorisation server. *)

  type t = {
    access_token : string;
    device_id : Matrix_proto.Id.Device_id.t;
    refresh_token : string option;
    access_token_expires_at : Ptime.t option;
        (** Absolute access-token expiry, when the issuing endpoint supplied
            one. Older profiles decode this as [None]. *)
    method_ : method_;
  }
  (** The credentials and logout provenance of one device. Older serialized
      values have neither the expiry nor provenance member and decode them as
      [None] and [Matrix], respectively. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** Where an incremental [/sync] resumes from. *)
module Sync_state : sig
  type t = {
    next_batch : string option;  (** The last sync token received. *)
    filter_id : string option;  (** From {!Sync.Filter.create}. *)
  }
  (** The type for a sync position. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** When the profile was made and last opened. *)
module Metadata : sig
  type t = {
    created_at : Ptime.t;
    last_used_at : Ptime.t;
    client_name : string;  (** The application that wrote the file. *)
  }
  (** The type for a profile's provenance. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** The contents of [session.json]. *)
module Session_file : sig
  type t = {
    server : Server.t;
    auth : Auth.t;
    sync : Sync_state.t;
    metadata : Metadata.t;
  }
  (** The type for a stored session. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** {1 [device.json]} *)

(** The device's long-term identity. *)
module Device_keys : sig
  type t = {
    ed25519_public : string;  (** Unpadded base64, as published. *)
    ed25519_private : string;
    curve25519_public : string;
    curve25519_private : string;
    uploaded_at : Ptime.t option;
        (** When the public halves were last accepted by [/keys/upload]. *)
    algorithms : string list;  (** Encryption algorithms the device supports. *)
  }
  (** The type for a device's key pair set. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** {1 [one_time_keys.json]} *)

(** One key of the one-time key pool. *)
module One_time_key : sig
  type t = {
    key_id : string;  (** The [signed_curve25519:AAAA] suffix. *)
    public : string;
    private_ : string;
    created_at : Ptime.t;
  }
  (** The type for a one-time key pair. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** The contents of [one_time_keys.json]. *)
module One_time_keys_file : sig
  type t = {
    target_count : int;  (** How many unclaimed keys to keep on the server. *)
    last_upload_at : Ptime.t option;
    next_key_id : int;  (** Counter the next [key_id] is derived from. *)
    keys : One_time_key.t list;
    fallback : One_time_key.t option;
        (** The fallback key, used when the pool is exhausted. *)
    previous_fallback : One_time_key.t option;
        (** The previous fallback key retained for in-flight pre-key messages.
        *)
    fallback_used : bool;
        (** The fallback key has been claimed, so a fresh one is due. *)
  }
  (** The type for the one-time key pool. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** {1 [olm_sessions.json]} *)

(** One Olm session with another device. *)
module Olm_session : sig
  type t = {
    their_identity_key : string;  (** The peer's Curve25519 key. *)
    session_id : string;
    pickle : string;
        (** Ratchet state, as {!Session_pickle.pickle_session} wrote it. *)
    created_at : Ptime.t;
    last_used_at : Ptime.t;  (** Most recent successful encrypt or decrypt. *)
  }
  (** The type for a stored Olm session. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** The contents of [olm_sessions.json]. *)
module Olm_sessions_file : sig
  type t = { sessions : Olm_session.t list }
  (** The type for every stored Olm session. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** {1 [megolm_inbound.json]} *)

(** One Megolm session received from another device. *)
module Megolm_inbound : sig
  type t = {
    room_id : Matrix_proto.Id.Room_id.t;
    session_id : string;
    sender_key : string;  (** Curve25519 key of the device that shared it. *)
    signing_key : string;  (** Ed25519 key that vouches for the sender. *)
    pickle : string;
    first_known_index : int;
        (** The earliest ratchet index this copy can decrypt. Messages before it
            are unreadable. *)
    created_at : Ptime.t;
  }
  (** The type for a stored inbound Megolm session. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** The contents of [megolm_inbound.json]. *)
module Megolm_inbound_file : sig
  type t = { sessions : Megolm_inbound.t list }
  (** The type for every stored inbound Megolm session. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** {1 [megolm_outbound.json]} *)

(** A device an outbound session's key has been sent to. *)
module Shared_with : sig
  type t = {
    user_id : Matrix_proto.Id.User_id.t;
    device_id : Matrix_proto.Id.Device_id.t;
    shared_at : Ptime.t;
  }
  (** The type for one recipient of a room key. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** One Megolm session this device sends with. *)
module Megolm_outbound : sig
  type t = {
    room_id : Matrix_proto.Id.Room_id.t;
    session_id : string;
    pickle : string;
    message_index : int;
    created_at : Ptime.t;
    message_count : int;
    max_age_ms : int64;
        (** With [message_count], decides when the session must be rotated. *)
    shared_with : Shared_with.t list;
  }
  (** The type for a stored outbound Megolm session. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end

(** The contents of [megolm_outbound.json]. *)
module Megolm_outbound_file : sig
  type t = { sessions : Megolm_outbound.t list }
  (** The type for every stored outbound Megolm session. *)

  val jsont : t Jsont.t
  (** Reads and writes {!t}. *)
end
