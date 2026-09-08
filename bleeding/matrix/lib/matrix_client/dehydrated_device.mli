(** dehydrated_device — a device that receives room keys while the user is
    offline.

    A dehydrated device holds an Olm account and one-time keys on the
    homeserver, encrypted under a key the server cannot read, so that other
    users can keep sending it room keys while the user has no client running. A
    user has at most one, and uploading replaces it. The endpoints sit on an
    unstable prefix, [/_matrix/client/unstable/org.matrix.msc3814.v1], and the
    [device_data] is opaque, so this module only moves it.

    @see <https://github.com/matrix-org/matrix-spec-proposals/pull/3814>
      MSC3814: Dehydrated devices with SSSS *)

type t = {
  device_id : Matrix_proto.Id.Device_id.t;
  device_data : Jsont.json;
      (** The pickled Olm account and whatever the dehydration algorithm needs
          to unpickle it. *)
}
(** The type for a dehydrated device as the server holds it. *)

val pickle_key_secret_name : string
(** [pickle_key_secret_name] is the SSSS event type ["org.matrix.msc3814"]
    containing the dehydrated-device pickle key. *)

module Pickle_key : sig
  type t
  type error = [ `Msg of string ]

  val generate : random:Random.t -> t
  (** [generate ~random] draws exactly 32 random bytes. *)

  val of_base64 : string -> (t, error) result
  (** [of_base64 value] strictly decodes standard padded or unpadded Base64,
      requiring exactly 32 decoded bytes. *)

  val to_base64 : t -> string
  (** [to_base64 key] uses unpadded standard Base64. *)
end

val is_key_stored : Secrets.store -> (bool, Error.t) result
(** [is_key_stored store] reports whether the pickle-key secret is present; it
    does not validate or decode the value. *)

val load_key : Secrets.store -> (Pickle_key.t option, Error.t) result
(** [load_key store] loads and strictly validates the optional pickle key. *)

val cached_key : Encryption_driver.t -> (Pickle_key.t option, Error.t) result
(** [cached_key driver] reads and validates only the locally persisted pickle
    key, without making an SSSS request. *)

val load_key_with_driver :
  ?create_if_missing:bool ->
  ?random:Random.t ->
  Encryption_driver.t ->
  Secrets.store ->
  (Pickle_key.t option, Error.t) result
(** [load_key_with_driver driver store] checks the local cache first. If it is
    absent, it loads SSSS and persists a valid fetched key locally. With
    [create_if_missing], an absent server value is replaced by a newly-created
    SSSS key; [random] is required for that branch. *)

val reset_key :
  Secrets.store -> random:Random.t -> (Pickle_key.t, Error.t) result
(** [reset_key store ~random] generates a fresh key, stores it through SSSS, and
    returns it only after the server write succeeds. *)

val reset_key_with_driver :
  Encryption_driver.t ->
  Secrets.store ->
  random:Random.t ->
  (Pickle_key.t, Error.t) result
(** [reset_key_with_driver driver store ~random] writes SSSS first, then caches
    and persists locally. A local persistence failure is returned and the
    preceding server mutation is not rolled back. *)

val is_supported : Client.t -> (bool, Error.t) result
(** [is_supported client] probes the direct MSC3814 dehydrated-device endpoint.
    Success and [M_NOT_FOUND] mean the endpoint is supported; [M_UNRECOGNIZED]
    means it is not. Other errors are returned unchanged. *)

val get : Client.t -> (t, Error.t) result
(** [get client] is the user's dehydrated device. It is
    [Error (Matrix_error { errcode = M_NOT_FOUND; _ })] when there is none. Uses
    [GET /_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device]. *)

val get_if_present : Client.t -> (t option, Error.t) result
(** [get_if_present client] returns [Some device] for a successful GET and
    [None] when the server reports [M_NOT_FOUND] or [M_UNRECOGNIZED]. Other
    errors propagate unchanged. It makes exactly one request. *)

val put :
  Client.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?initial_device_display_name:string ->
  device_data:Jsont.json ->
  ?device_keys:Jsont.json ->
  ?one_time_keys:(string * Jsont.json) list ->
  ?fallback_keys:(string * Jsont.json) list ->
  unit ->
  (Matrix_proto.Id.Device_id.t, Error.t) result
(** [put client ~device_id ~device_data ()] uploads a dehydrated device,
    replacing any previous one, and is the device id the server recorded.

    [initial_device_display_name] is the name the device is listed under, and
    defaults to absent, which leaves the server to choose. [device_keys],
    [one_time_keys] and [fallback_keys] publish the device's keys in the shapes
    {!Keys.upload_keys} uses, so that other devices can claim keys for it; each
    defaults to absent, which is right only when re-uploading data for keys the
    server already holds. Uses
    [PUT /_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device]. *)

val put_and_remember :
  Encryption_driver.t ->
  Client.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?initial_device_display_name:string ->
  device_data:Jsont.json ->
  ?device_keys:Jsont.json ->
  ?one_time_keys:(string * Jsont.json) list ->
  ?fallback_keys:(string * Jsont.json) list ->
  unit ->
  (Matrix_proto.Id.Device_id.t, Error.t) result
(** [put_and_remember driver client ...] performs {!put}, then remembers and
    persists the returned device id. If persistence fails, the successful server
    upload is not rolled back and the error is returned. *)

val create_and_upload :
  Encryption_driver.t ->
  Client.t ->
  private_identity:Cross_signing.private_identity ->
  pickle_key:Pickle_key.t ->
  ?initial_device_display_name:string ->
  random:Random.t ->
  unit ->
  (Matrix_proto.Id.Device_id.t, Error.t) result
(** Creates and uploads an independent legacy MSC3814 V1 Olm account. The
    account is never installed as the primary device; the returned server device
    ID is remembered only after the PUT succeeds. *)

type create_event =
  | Created of Matrix_proto.Id.Device_id.t
  | Uploaded of Matrix_proto.Id.Device_id.t

val create_and_upload_with_callbacks :
  ?on_event:(create_event -> unit) ->
  Encryption_driver.t ->
  Client.t ->
  private_identity:Cross_signing.private_identity ->
  pickle_key:Pickle_key.t ->
  ?initial_device_display_name:string ->
  random:Random.t ->
  unit ->
  (Matrix_proto.Id.Device_id.t, Error.t) result
(** Callback-capable form of {!create_and_upload}. The [Created] callback runs
    after local construction and immediately before PUT; [Uploaded] runs only
    after the PUT and returned device-id persistence succeed. Callbacks run
    synchronously; an exception propagates and prevents subsequent work. *)

val delete : Client.t -> (unit, Error.t) result
(** [delete client] removes the user's dehydrated device. Uses
    [DELETE /_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device].
*)

val delete_if_present :
  ?on_deleted:(unit -> unit) -> Client.t -> (unit, Error.t) result
(** [delete_if_present client] deletes the device, treating [M_NOT_FOUND] and
    [M_UNRECOGNIZED] as an already-satisfied deletion. Other errors propagate;
    exactly one request is made. [on_deleted], when supplied, runs only after a
    successful server-side deletion. *)

type rehydrate_outcome = {
  device_id : Matrix_proto.Id.Device_id.t;
  room_keys_imported : int;
  to_device_events : int;
  delete_error : Error.t option;
}
(** The result of draining and importing a legacy MSC3814 V1 device. A
    successful drain is reported even when deleting the server-side device
    fails; in that case [delete_error] contains the deletion error. *)

val rehydrate :
  Encryption_driver.t ->
  Client.t ->
  pickle_key:Pickle_key.t ->
  random:Random.t ->
  unit ->
  (rehydrate_outcome option, Error.t) result
(** [rehydrate driver client ~pickle_key ~random ()] restores the downloaded
    legacy account into a temporary machine, drains its queued to-device events,
    imports the resulting room keys into [driver], and then deletes the
    server-side device. The temporary account never replaces [driver]'s primary
    identity. No device deletion is attempted when no device is present or when
    validation, pagination, or import setup fails. *)

type rehydrate_event =
  | Rehydration_started of Matrix_proto.Id.Device_id.t
  | Rehydration_progress of { room_keys_imported : int; to_device_events : int }
  | Rehydration_completed of {
      device_id : Matrix_proto.Id.Device_id.t;
      room_keys_imported : int;
      to_device_events : int;
    }

val rehydrate_with_callbacks :
  ?on_deleted:(unit -> unit) ->
  on_event:(rehydrate_event -> unit) ->
  Encryption_driver.t ->
  Client.t ->
  pickle_key:Pickle_key.t ->
  random:Random.t ->
  unit ->
  (rehydrate_outcome option, Error.t) result
(** Callback-capable form of {!rehydrate}. Progress is emitted after each
    imported page; completion is emitted only after a clean drain, before the
    best-effort deletion request. Callbacks run synchronously; an exception
    propagates and prevents subsequent work. *)

type events = {
  next_batch : string option;
      (** Token for the next page, absent when there are no more events. *)
  events : Jsont.json list;  (** The events, Olm-encrypted. *)
}
(** The type for a page of to-device events waiting for a dehydrated device. *)

val get_events :
  Client.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?from:string ->
  unit ->
  (events, Error.t) result
(** [get_events client ~device_id ()] is one page of to-device events sent to
    the dehydrated device, for a rehydrating client to decrypt. [from] is a
    previous page's [next_batch] and defaults to absent, which starts at the
    oldest event held. Uses
    [POST
     /_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device/{deviceId}/events].
*)
