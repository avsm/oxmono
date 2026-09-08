(** keys — the endpoints that publish, fetch and claim a device's keys.

    A device publishes its identity keys once and its one-time keys as the
    server runs out of them, and fetches other users' keys to find the devices
    it must encrypt for. The key material itself is {!Crypto_key}; the ratchets
    that consume it are {!Olm}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#end-to-end-encryption>
      End-to-end encryption *)

(** {1 Keys on the wire} *)

type signatures =
  (Matrix_proto.Id.User_id.t
  * (Crypto_key.Key_id.t * Crypto_key.Signature.t) list)
  list
(** The type for the [signatures] member of a signed object, as signing user to
    the signature made with each of that user's keys. *)

type device_keys = {
  user_id : Matrix_proto.Id.User_id.t;
  device_id : Matrix_proto.Id.Device_id.t;
  algorithms : string list;  (** The encryption algorithms the device speaks. *)
  keys : (Crypto_key.Key_id.t * string) list;
      (** The device's public keys, unpadded base64, named
          [<algorithm>:<device id>]. *)
  signatures : signatures;
  dehydrated : bool option;
      (** [Some true] marks an MSC3814 dehydrated device. *)
  unsigned : Jsont.json option;
      (** Server-supplied extras, notably [device_display_name]. They fall
          outside the signature and are not authenticated. *)
}
(** The type for the identity of one device.

    It is the [device_keys] object [/keys/upload] takes and [/keys/query]
    returns. Only the device's own signature in [signatures] binds these keys to
    the device identifier, so a caller that reads one must check it.

    @see <https://spec.matrix.org/v1.11/client-server-api/#device-keys>
      Device keys *)

val device_keys_jsont : device_keys Jsont.t
(** [device_keys_jsont] is the JSON codec for {!type-device_keys}. *)

type one_time_key = {
  key : string;  (** The Curve25519 public key, unpadded base64. *)
  fallback : bool option;
      (** [Some true] marks a signed fallback key, and is part of its signature.
          Ordinary one-time keys leave this absent. *)
  signatures : signatures option;
      (** Absent for the [curve25519] algorithm, which Matrix does not use. *)
}
(** The type for a one-time or fallback key on the wire.

    A one-time key is handed out once and then forgotten by the server. A
    fallback key is handed out again whenever the pool is empty, so a session
    opened from one is only as fresh as the last time it was replaced.

    @see <https://spec.matrix.org/v1.11/client-server-api/#one-time-and-fallback-keys>
      One-time and fallback keys *)

val one_time_key_jsont : one_time_key Jsont.t
(** [one_time_key_jsont] is the JSON codec for {!one_time_key}. *)

val one_time_key_signing_json : ?fallback:bool -> string -> string
(** [one_time_key_signing_json key] is the canonical JSON [{"key":<key>}] that a
    one-time key's signature covers. For a fallback key, pass [~fallback:true]
    to include the signed fallback-key marker.
    {!Olm.Account.signed_one_time_keys} signs the ordinary form. *)

(** What a cross-signing key certifies. *)
type key_usage =
  | Master  (** ["master"], the root of the user's identity. *)
  | Self_signing  (** ["self_signing"], which signs the user's own devices. *)
  | User_signing  (** ["user_signing"], which signs other users' master keys. *)
  | Other of string  (** Any usage this library does not model. *)

val key_usage_to_string : key_usage -> string
(** [key_usage_to_string u] is the wire form of [u], such as ["master"]. *)

val key_usage_of_string : string -> key_usage
(** [key_usage_of_string s] is the usage [s] names. An unrecognised [s] is
    [Other s]. *)

type cross_signing_key = {
  user_id : Matrix_proto.Id.User_id.t;
  usage : key_usage list;
  keys : (Crypto_key.Key_id.t * string) list;
      (** Holds exactly one entry, named [ed25519:] followed by the unpadded
          base64 public key, whose value is that same key. *)
  signatures : signatures;
}
(** The type for a cross-signing key, either the master key or the self-signing
    or user-signing key it certifies. [signatures] is empty on a master key that
    no device has signed.

    @see <https://spec.matrix.org/v1.11/client-server-api/#cross-signing>
      Cross-signing *)

val signatures_jsont : signatures Jsont.t
(** [signatures_jsont] is the JSON codec for {!type-signatures}. *)

val key_id_map : 'a Jsont.t -> (Crypto_key.Key_id.t * 'a) list Jsont.t
(** [key_id_map v] reads an object whose member names are key identifiers and
    whose values are read with [v]. Bindings come out sorted by name however the
    sender ordered them. *)

val cross_signing_key_jsont : cross_signing_key Jsont.t
(** [cross_signing_key_jsont] is the JSON codec for {!cross_signing_key}, whose
    wire shape is the same in a [/keys/query] response and a
    [/keys/device_signing/upload] request. *)

type failures = (Matrix_proto.Id.Server_name.t * Jsont.json) list
(** The type for the remote servers a query could not reach, each with the error
    that server or the local one produced. The error is left as JSON because it
    is whatever the remote answered and follows no schema this library can rely
    on. *)

(** {1 Uploading keys} *)

type upload_keys_response = {
  one_time_key_counts : (string * int) list;
      (** Algorithm to the number of this device's keys the server holds
          unclaimed. *)
}
(** The type for a [/keys/upload] response. *)

val upload_keys :
  Client.t ->
  ?device_keys:device_keys ->
  ?one_time_keys:(Crypto_key.Key_id.t * one_time_key) list ->
  ?fallback_keys:(Crypto_key.Key_id.t * one_time_key) list ->
  unit ->
  (upload_keys_response, Error.t) result
(** [upload_keys client ()] publishes any of the three kinds of key. The
    one-time and fallback key identifiers name the algorithm, which is
    [signed_curve25519] for every key Matrix uses.

    [device_keys] is needed only once per device, and a second upload naming
    different identity keys is refused by the server. It defaults to absent, as
    do the two key lists. Uploading nothing at all is legal and is how a client
    asks for the current counts without publishing anything.

    The result says how many one-time keys remain unclaimed, which is what
    decides when to upload more.

    Uses [POST /_matrix/client/v3/keys/upload]. *)

(** {1 Querying keys} *)

type query_keys_response = {
  failures : failures;
  device_keys :
    (Matrix_proto.Id.User_id.t
    * (Matrix_proto.Id.Device_id.t * device_keys) list)
    list;
  master_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
  self_signing_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
  user_signing_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
      (** Populated for the querying user alone, since nobody else's
          user-signing key is disclosed. *)
}
(** The type for a [/keys/query] response. *)

val query_keys :
  Client.t ->
  ?timeout:int ->
  users:(Matrix_proto.Id.User_id.t * Matrix_proto.Id.Device_id.t list) list ->
  unit ->
  (query_keys_response, Error.t) result
(** [query_keys client ~users ()] fetches the device and cross-signing keys of
    [users]. An empty device list asks for every device of that user.

    [timeout] is how long, in milliseconds, the server may wait on other
    homeservers before giving up, and defaults to the server's own choice. The
    ones it gives up on appear in [failures] rather than failing the request.

    Uses [POST /_matrix/client/v3/keys/query]. *)

(** {1 Claiming keys} *)

type claim_keys_response = {
  failures : failures;
  one_time_keys :
    (Matrix_proto.Id.User_id.t
    * (Matrix_proto.Id.Device_id.t * (Crypto_key.Key_id.t * one_time_key) list)
      list)
    list;
      (** User to device to the one key claimed from it. *)
}
(** The type for a [/keys/claim] response. *)

val claim_keys :
  Client.t ->
  ?timeout:int ->
  keys:
    (Matrix_proto.Id.User_id.t * (Matrix_proto.Id.Device_id.t * string) list)
    list ->
  unit ->
  (claim_keys_response, Error.t) result
(** [claim_keys client ~keys ()] claims one key for each (device, algorithm)
    pair, which the server then never hands out again. The algorithm is
    [signed_curve25519].

    [timeout] is how long, in milliseconds, the server may wait on other
    homeservers, and defaults to the server's own choice.

    A device that has run out is absent from the result, unless it published a
    fallback key, which the server returns instead and may return again to
    somebody else.

    Uses [POST /_matrix/client/v3/keys/claim]. *)

(** {1 Key changes} *)

type key_changes_response = {
  changed : Matrix_proto.Id.User_id.t list;
      (** Users sharing an encrypted room with this client whose device list
          changed. *)
  left : Matrix_proto.Id.User_id.t list;
      (** Users this client no longer shares an encrypted room with. *)
}
(** The type for a [/keys/changes] response. *)

val get_key_changes :
  Client.t ->
  from:string ->
  until:string ->
  (key_changes_response, Error.t) result
(** [get_key_changes client ~from ~until] is the device-list changes between the
    sync tokens [from] and [until]. It answers what a sync response's
    [device_lists] answers, for a client catching up across a gap.

    Uses [GET /_matrix/client/v3/keys/changes]. *)

(** {1 Cross-signing} *)

val upload_signing_keys :
  Client.t ->
  ?master_key:cross_signing_key ->
  ?self_signing_key:cross_signing_key ->
  ?user_signing_key:cross_signing_key ->
  ?auth:Uiaa.auth_data ->
  unit ->
  (unit, Error.t) result
(** [upload_signing_keys client ()] publishes the user's cross-signing keys.
    Each key defaults to absent.

    The endpoint is protected by user-interactive authentication except when the
    request would not change what the server already holds, so the first upload
    of a fresh set of keys, and any exact re-upload of them, succeed with [auth]
    absent. Pass [auth] once a challenge is known, or use
    {!upload_signing_keys_uiaa} to answer one as it arrives.

    A self-signing or user-signing key must carry a signature by the master key,
    whether that master key is in this request or already on the server.

    Uses [POST /_matrix/client/v3/keys/device_signing/upload]. *)

val upload_signing_keys_uiaa :
  Client.t ->
  ?master_key:cross_signing_key ->
  ?self_signing_key:cross_signing_key ->
  ?user_signing_key:cross_signing_key ->
  auth_callback:(Uiaa.uiaa_response -> Uiaa.auth_data option) ->
  unit ->
  unit Uiaa.uiaa_result
(** [upload_signing_keys_uiaa client ~auth_callback ()] is
    {!upload_signing_keys} driven through {!Uiaa.with_uiaa}. When the server
    answers the unauthenticated attempt with a challenge, [auth_callback] is
    given it and may return the credentials to retry with. Each key defaults to
    absent.

    Uses [POST /_matrix/client/v3/keys/device_signing/upload]. *)

type upload_signatures_response = {
  failures : (Matrix_proto.Id.User_id.t * (string * Jsont.json) list) list;
      (** User to the key that was rejected and the error rejecting it. An
          invalid signature is reported as [M_INVALID_SIGNATURE]. The error is
          left as JSON because it is whatever the server answered. *)
}
(** The type for a [/keys/signatures/upload] response. *)

val upload_signatures :
  Client.t ->
  (Matrix_proto.Id.User_id.t * (string * Jsont.json) list) list ->
  (upload_signatures_response, Error.t) result
(** [upload_signatures client signatures] publishes cross-signing signatures,
    pairing each user with a list of (signed key, signed object). The signed key
    is a device identifier, or the unpadded base64 of a cross-signing key.

    Each signed object must reproduce the key it signs, whether device keys or a
    cross-signing key, differing only in its [signatures] member, which carries
    the signatures being added. Users with no signatures to publish are dropped.

    A signature can be rejected without the request failing, so a caller must
    read [failures] rather than the result alone.

    Uses [POST /_matrix/client/v3/keys/signatures/upload]. *)
