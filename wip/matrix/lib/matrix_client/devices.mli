(** devices — the logins of the account in hand.

    A device is one login. It holds an access token and, in an encrypted room,
    its own set of keys. Deleting one invalidates its token. None of the
    deletions drive user-interactive authentication, so a server that demands it
    answers 401, which arrives as an {!Error.Http_error}. *)

type device = {
  device_id : Matrix_proto.Id.Device_id.t;
  display_name : string option;  (** What the user named this login. *)
  last_seen_ip : string option;
  last_seen_ts : Matrix_proto.Event.Timestamp.t option;
}
(** One device, as the server reports it. *)

val get_devices : Client.t -> (device list, Error.t) result
(** [get_devices t] is [GET /_matrix/client/v3/devices] (Matrix 1.0). *)

val get_device :
  Client.t -> device_id:Matrix_proto.Id.Device_id.t -> (device, Error.t) result
(** [get_device t ~device_id] is [GET /_matrix/client/v3/devices/{deviceId}]
    (Matrix 1.0). A device that is not the user's own is [M_NOT_FOUND]. *)

val update_device :
  Client.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  display_name:string ->
  (unit, Error.t) result
(** [update_device t ~device_id ~display_name] is
    [PUT /_matrix/client/v3/devices/{deviceId}] (Matrix 1.0). The display name
    is the only member a client may change. *)

val delete_device :
  Client.t -> device_id:Matrix_proto.Id.Device_id.t -> (unit, Error.t) result
(** [delete_device t ~device_id] is
    [DELETE /_matrix/client/v3/devices/{deviceId}] (Matrix 1.0). *)

val delete_devices :
  Client.t ->
  device_ids:Matrix_proto.Id.Device_id.t list ->
  (unit, Error.t) result
(** [delete_devices t ~device_ids] is [POST /_matrix/client/v3/delete_devices]
    (Matrix 1.0), which deletes the whole list under one authentication rather
    than one at a time. *)
