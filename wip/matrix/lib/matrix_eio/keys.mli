(** keys — the endpoints that publish, fetch and claim a device's keys, raising
    instead of returning a result.

    A device publishes its identity keys once and its one-time keys as the
    server runs out of them, and fetches other users' keys to find the devices
    it must encrypt for. Every function here raises [Eio.Io] carrying an
    {!Error.type-err} where its {!Matrix_client.Keys} counterpart returns an
    error. *)

(** {1 Keys on the wire} *)

type signatures = Matrix_client.Keys.signatures
(** The type for the [signatures] member of a signed object. It is
    {!Matrix_client.Keys.type-signatures}. *)

type device_keys = Matrix_client.Keys.device_keys
(** The type for the identity of one device. It is
    {!Matrix_client.Keys.type-device_keys}. *)

type one_time_key = Matrix_client.Keys.one_time_key
(** The type for a one-time or fallback key on the wire. It is
    {!Matrix_client.Keys.one_time_key}. *)

val one_time_key_signing_json : ?fallback:bool -> string -> string
(** [one_time_key_signing_json key] is the canonical JSON [{"key":<key>}] that a
    one-time key's signature covers.

    Raises [Invalid_argument] on an encoding failure, which a two-member object
    of plain strings should never suffer. *)

(** What a cross-signing key certifies. It is
    {!Matrix_client.Keys.type-key_usage}. *)
type key_usage = Matrix_client.Keys.key_usage =
  | Master
  | Self_signing
  | User_signing
  | Other of string

type cross_signing_key = Matrix_client.Keys.cross_signing_key = {
  user_id : Matrix_proto.Id.User_id.t;
  usage : key_usage list;
  keys : (Matrix_client.Crypto_key.Key_id.t * string) list;
  signatures : signatures;
}
(** The type for a cross-signing key, either the master key or the self-signing
    or user-signing key it certifies. It is
    {!Matrix_client.Keys.cross_signing_key}, which documents the fields. *)

(** {1 Uploading keys} *)

type upload_keys_response = Matrix_client.Keys.upload_keys_response = {
  one_time_key_counts : (string * int) list;
      (** Algorithm to the number of this device's keys the server holds
          unclaimed. *)
}
(** The type for a [/keys/upload] response. *)

val upload_keys :
  Client.t ->
  ?device_keys:device_keys ->
  ?one_time_keys:(Matrix_client.Crypto_key.Key_id.t * one_time_key) list ->
  ?fallback_keys:(Matrix_client.Crypto_key.Key_id.t * one_time_key) list ->
  unit ->
  upload_keys_response
(** [upload_keys c ()] is {!Matrix_client.Keys.upload_keys} with the result
    unwrapped. [device_keys] defaults to absent and is needed only once per
    device. [one_time_keys] and [fallback_keys] each default to the empty list.
*)

(** {1 Querying keys} *)

type query_keys_response = Matrix_client.Keys.query_keys_response = {
  failures : Matrix_client.Keys.failures;
  device_keys :
    (Matrix_proto.Id.User_id.t
    * (Matrix_proto.Id.Device_id.t * device_keys) list)
    list;
  master_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
  self_signing_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
  user_signing_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
}
(** The type for a [/keys/query] response. It is
    {!Matrix_client.Keys.query_keys_response}, which documents the fields. *)

val query_keys :
  Client.t ->
  ?timeout:int ->
  users:(Matrix_proto.Id.User_id.t * Matrix_proto.Id.Device_id.t list) list ->
  unit ->
  query_keys_response
(** [query_keys c ~users ()] is {!Matrix_client.Keys.query_keys} with the result
    unwrapped. An empty device list asks for every device of that user.
    [timeout] is how long, in milliseconds, the server may wait on other
    homeservers, and defaults to the server's own choice. *)

(** {1 Claiming keys} *)

type claim_keys_response = Matrix_client.Keys.claim_keys_response = {
  failures : Matrix_client.Keys.failures;
  one_time_keys :
    (Matrix_proto.Id.User_id.t
    * (Matrix_proto.Id.Device_id.t
      * (Matrix_client.Crypto_key.Key_id.t * one_time_key) list)
      list)
    list;
}
(** The type for a [/keys/claim] response. It is
    {!Matrix_client.Keys.claim_keys_response}, which documents the fields. *)

val claim_keys :
  Client.t ->
  ?timeout:int ->
  keys:
    (Matrix_proto.Id.User_id.t * (Matrix_proto.Id.Device_id.t * string) list)
    list ->
  unit ->
  claim_keys_response
(** [claim_keys c ~keys ()] is {!Matrix_client.Keys.claim_keys} with the result
    unwrapped. It claims one key per (device, algorithm) pair, which the server
    then never hands out again. [timeout] is how long, in milliseconds, the
    server may wait on other homeservers, and defaults to the server's own
    choice. *)

(** {1 Key changes} *)

type key_changes_response = Matrix_client.Keys.key_changes_response = {
  changed : Matrix_proto.Id.User_id.t list;
  left : Matrix_proto.Id.User_id.t list;
}
(** The type for a [/keys/changes] response. It is
    {!Matrix_client.Keys.key_changes_response}, which documents the fields. *)

val get_key_changes :
  Client.t -> from:string -> until:string -> key_changes_response
(** [get_key_changes c ~from ~until] is {!Matrix_client.Keys.get_key_changes}
    with the result unwrapped, the device-list changes between the sync tokens
    [from] and [until]. *)

(** {1 Cross-signing} *)

val upload_signing_keys :
  Client.t ->
  ?master_key:cross_signing_key ->
  ?self_signing_key:cross_signing_key ->
  ?user_signing_key:cross_signing_key ->
  ?auth:Matrix_client.Uiaa.auth_data ->
  unit ->
  unit
(** [upload_signing_keys c ()] is {!Matrix_client.Keys.upload_signing_keys} with
    the result unwrapped. Each key and [auth] default to absent. The endpoint is
    protected by user-interactive authentication except when the request adds
    nothing the server does not already hold, so a first upload succeeds without
    [auth] and an unanswered challenge raises. *)

val upload_signing_keys_uiaa :
  Client.t ->
  ?master_key:cross_signing_key ->
  ?self_signing_key:cross_signing_key ->
  ?user_signing_key:cross_signing_key ->
  auth_callback:
    (Matrix_client.Uiaa.uiaa_response -> Matrix_client.Uiaa.auth_data option) ->
  unit ->
  Matrix_client.Uiaa.uiaa_response option
(** [upload_signing_keys_uiaa c ~auth_callback ()] is
    {!Matrix_client.Keys.upload_signing_keys_uiaa} with the result unwrapped. It
    is [None] once the keys are uploaded, and [Some challenge] when
    authentication is still outstanding. Each key defaults to absent. A request
    that fails outright raises. *)

type upload_signatures_response =
      Matrix_client.Keys.upload_signatures_response = {
  failures : (Matrix_proto.Id.User_id.t * (string * Jsont.json) list) list;
}
(** The type for a [/keys/signatures/upload] response. It is
    {!Matrix_client.Keys.upload_signatures_response}, which documents the field.
*)

val upload_signatures :
  Client.t ->
  (Matrix_proto.Id.User_id.t * (string * Jsont.json) list) list ->
  upload_signatures_response
(** [upload_signatures c signatures] is {!Matrix_client.Keys.upload_signatures}
    with the result unwrapped. A signature can be rejected without the request
    failing, so a caller must read the result's [failures] rather than its
    success alone. *)
