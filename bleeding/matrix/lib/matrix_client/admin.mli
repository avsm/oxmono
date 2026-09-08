(** Server-administration queries from the Client-Server API.

    The caller must be authorised as a server administrator by the homeserver;
    this module does not implement, or claim to implement, any broader Synapse
    administration API. *)

type connection = {
  ip : string option;
  last_seen : Matrix_proto.Event.Timestamp.t option;
  user_agent : string option;
}
(** One connection belonging to a session. The three fields are nullable and may
    also be omitted by the homeserver. *)

type connection_info = connection

type session = { connections : connection list }
(** A session belonging to a device. *)

type session_info = session

type device = { sessions : session list }
(** A device, keyed by its opaque device identifier in {!response.devices}. *)

type device_info = device

type response = {
  user_id : Matrix_proto.Id.User_id.t option;
  devices : (string * device) list;
}
(** The information returned by [GET /_matrix/client/v3/admin/whois/{userId}].

    Device identifiers are kept as strings because the wire schema uses a
    string-keyed map and does not require the keys to be known to the caller. *)

val connection_jsont : connection Jsont.t
val session_jsont : session Jsont.t
val device_jsont : device Jsont.t
val response_jsont : response Jsont.t

val whois :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> (response, Error.t) result
(** [whois t ~user_id] performs the authenticated
    [GET /_matrix/client/v3/admin/whois/{userId}] request. The user identifier
    is escaped as one path component. *)

val get_user_info :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> (response, Error.t) result
(** Alias for {!whois}. *)
