(** devices — the logins of the account in hand, raising instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Devices} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces.

    Neither deletion drives user-interactive authentication. A server that
    demands it answers 401, which arrives as a
    {!Matrix_client.Error.Http_error}. *)

type device = Matrix_client.Devices.device = {
  device_id : Matrix_proto.Id.Device_id.t;
  display_name : string option;
  last_seen_ip : string option;
  last_seen_ts : Matrix_proto.Event.Timestamp.t option;
}
(** One login of the current user. *)

val get_devices : Client.t -> device list
(** [get_devices c] is {!Matrix_client.Devices.get_devices} with the result
    unwrapped. *)

val get_device : Client.t -> device_id:Matrix_proto.Id.Device_id.t -> device
(** [get_device c ~device_id] is {!Matrix_client.Devices.get_device} with the
    result unwrapped. *)

val update_device :
  Client.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  display_name:string ->
  unit
(** [update_device c ~device_id ~display_name] is
    {!Matrix_client.Devices.update_device} with the result unwrapped. *)

val delete_device : Client.t -> device_id:Matrix_proto.Id.Device_id.t -> unit
(** [delete_device c ~device_id] is {!Matrix_client.Devices.delete_device} with
    the result unwrapped. *)

val delete_devices :
  Client.t -> device_ids:Matrix_proto.Id.Device_id.t list -> unit
(** [delete_devices c ~device_ids] is {!Matrix_client.Devices.delete_devices}
    with the result unwrapped. *)
