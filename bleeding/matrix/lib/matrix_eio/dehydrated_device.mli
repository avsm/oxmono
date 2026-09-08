(** dehydrated_device — a device that receives room keys while the user is
    offline, raising instead of returning a result.

    A dehydrated device holds an Olm account and one-time keys on the
    homeserver, encrypted under a key the server cannot read, so that other
    users can keep sending it room keys while the user has no client running.
    The endpoints sit on the unstable prefix of MSC3814 and a server may not
    implement them at all. Every function here raises [Eio.Io] carrying an
    {!Error.type-err} where its {!Matrix_client.Dehydrated_device} counterpart
    returns an error. *)

type t = Matrix_client.Dehydrated_device.t = {
  device_id : Matrix_proto.Id.Device_id.t;
  device_data : Jsont.json;
}
(** The type for a dehydrated device as the server holds it. It is
    {!Matrix_client.Dehydrated_device.t}, which documents the fields. *)

val pickle_key_secret_name : string
(** The SSSS event type used for the MSC3814 pickle key. *)

module Pickle_key : sig
  type t
  type error = [ `Msg of string ]

  val generate : random:Matrix_client.Random.t -> t
  val of_base64 : string -> (t, error) result
  val to_base64 : t -> string
end

val is_key_stored : Matrix_client.Secrets.store -> bool
(** Reports presence of the pickle-key secret without decoding it. *)

val load_key : Matrix_client.Secrets.store -> Pickle_key.t option
(** Loads and validates the optional pickle key. *)

val cached_key : Encryption.t -> Pickle_key.t option
(** Reads and validates only the driver's locally cached pickle key. *)

val load_key_with_driver :
  ?create_if_missing:bool ->
  ?random:Matrix_client.Random.t ->
  Encryption.t ->
  Matrix_client.Secrets.store ->
  Pickle_key.t option
(** Checks the local cache first, then loads and persists a valid SSSS key. With
    [create_if_missing], a missing server value is replaced by a new key. *)

val reset_key :
  Matrix_client.Secrets.store -> random:Matrix_client.Random.t -> Pickle_key.t
(** Generates and persists a fresh pickle key before returning it. *)

val reset_key_with_driver :
  Encryption.t ->
  Matrix_client.Secrets.store ->
  random:Matrix_client.Random.t ->
  Pickle_key.t
(** Writes SSSS first, then caches and persists the fresh key locally. *)

val is_supported : Client.t -> bool
(** [is_supported c] probes MSC3814 and reports whether the endpoint exists. *)

type events = Matrix_client.Dehydrated_device.events = {
  next_batch : string option;
  events : Jsont.json list;
}
(** The type for a page of to-device events waiting for a dehydrated device. It
    is {!Matrix_client.Dehydrated_device.events}, which documents the fields. *)

type rehydrate_outcome = Matrix_client.Dehydrated_device.rehydrate_outcome = {
  device_id : Matrix_proto.Id.Device_id.t;
  room_keys_imported : int;
  to_device_events : int;
  delete_error : Matrix_client.Error.t option;
}
(** The result of rehydrating and draining a legacy MSC3814 V1 device. *)

val get : Client.t -> t
(** [get c] is {!Matrix_client.Dehydrated_device.get} with the result unwrapped.
    A user with no dehydrated device raises [M_NOT_FOUND]. *)

val get_if_present : Client.t -> t option
(** [get_if_present c] returns [None] for [M_NOT_FOUND] or [M_UNRECOGNIZED],
    otherwise unwrapping the single GET result. *)

val put :
  Client.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?initial_device_display_name:string ->
  device_data:Jsont.json ->
  ?device_keys:Jsont.json ->
  ?one_time_keys:(string * Jsont.json) list ->
  ?fallback_keys:(string * Jsont.json) list ->
  unit ->
  Matrix_proto.Id.Device_id.t
(** [put c ~device_id ~device_data ()] is {!Matrix_client.Dehydrated_device.put}
    with the result unwrapped. It replaces any previous dehydrated device. *)

val put_and_remember :
  Encryption.t ->
  Client.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?initial_device_display_name:string ->
  device_data:Jsont.json ->
  ?device_keys:Jsont.json ->
  ?one_time_keys:(string * Jsont.json) list ->
  ?fallback_keys:(string * Jsont.json) list ->
  unit ->
  Matrix_proto.Id.Device_id.t
(** Uploads a dehydrated device, then persists its returned id in the driver. *)

val create_and_upload :
  Encryption.t ->
  Client.t ->
  private_identity:Matrix_client.Cross_signing.private_identity ->
  pickle_key:Pickle_key.t ->
  ?initial_device_display_name:string ->
  random:Matrix_client.Random.t ->
  unit ->
  Matrix_proto.Id.Device_id.t
(** Creates and uploads an independent legacy MSC3814 V1 device, raising on HTTP
    or persistence failure. The account is not installed as the primary device.
*)

val rehydrate :
  Encryption.t ->
  Client.t ->
  pickle_key:Pickle_key.t ->
  random:Matrix_client.Random.t ->
  unit ->
  rehydrate_outcome option
(** Drains the legacy device into the primary machine and then deletes it,
    raising [Eio.Io] for setup, pagination, or import failures. A deletion
    failure is returned in [delete_error] after successful import. *)

val delete : Client.t -> unit
(** [delete c] is {!Matrix_client.Dehydrated_device.delete} with the result
    unwrapped. *)

val delete_if_present : Client.t -> unit
(** [delete_if_present c] treats [M_NOT_FOUND] and [M_UNRECOGNIZED] as an
    already-completed deletion. *)

val get_events :
  Client.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?from:string ->
  unit ->
  events
(** [get_events c ~device_id ()] is
    {!Matrix_client.Dehydrated_device.get_events} with the result unwrapped.
    [from] is a previous page's [next_batch] and defaults to absent, which
    starts at the oldest event held. *)

(** {1 Lifecycle manager} *)

module Manager : sig
  (** Lifecycle notifications, following the Rust SDK's dehydrated-device state
      stream. Counts in progress and completion are cumulative. *)
  type event =
    | Created of Matrix_proto.Id.Device_id.t
    | Uploaded of Matrix_proto.Id.Device_id.t
    | Deleted
    | Key_cached
    | Rehydration_started of Matrix_proto.Id.Device_id.t
    | Rehydration_progress of {
        room_keys_imported : int;
        to_device_events : int;
      }
    | Rehydration_completed of {
        device_id : Matrix_proto.Id.Device_id.t;
        room_keys_imported : int;
        to_device_events : int;
      }
    | Rehydration_error of string
    | Rotation_error of string

  type subscription
  (** An opaque callback registration. *)

  type t
  (** A manager bound to one Eio client, encryption driver, identity and source
      of randomness. All values and operations must stay in one Eio domain. *)

  val create :
    ?random:Matrix_client.Random.t ->
    encryption:Encryption.t ->
    client:Client.t ->
    private_identity:Matrix_client.Cross_signing.private_identity ->
    unit ->
    t
  (** [create ...] binds the manager. Randomness defaults to the client's
      configured source. *)

  val subscribe : t -> (event -> unit) -> subscription
  (** Registers a lifecycle callback. Callback exceptions are logged and
      isolated, except Eio cancellation, which propagates. *)

  val unsubscribe : t -> subscription -> unit
  (** Removes a callback. It is idempotent and safe from inside a callback. *)

  val start :
    ?only_if_key_cached:bool ->
    ?create_new_key:bool ->
    ?skip_rehydration:bool ->
    ?interval:float ->
    clock:float Eio.Time.clock_ty Eio.Std.r ->
    t ->
    store:Matrix_client.Secrets.store ->
    unit ->
    unit
  (** [start ~clock manager ~store ()] stops an earlier schedule, optionally
      rehydrates the current device, creates and uploads one immediately, then
      rotates every week by default. [interval] is intended for deterministic
      tests; the production default remains seven days. [only_if_key_cached]
      returns without touching Secret Storage when no local key is cached.
      Rehydration failures are reported and do not prevent replacement; when
      [create_new_key] is requested such a failure preserves the old key. *)

  val stop : t -> unit
  (** Stops scheduled rotation. The current server-side device remains. *)

  val delete : t -> unit
  (** Stops rotation and deletes the server-side device. [Deleted] is emitted
      only when the server confirms deletion, not for an absent device. *)
end
