(** Raising Eio wrapper for {!Matrix_client.Admin}. *)

type connection = Matrix_client.Admin.connection = {
  ip : string option;
  last_seen : Matrix_proto.Event.Timestamp.t option;
  user_agent : string option;
}

type connection_info = connection
type session = Matrix_client.Admin.session = { connections : connection list }
type session_info = session
type device = Matrix_client.Admin.device = { sessions : session list }
type device_info = device

type response = Matrix_client.Admin.response = {
  user_id : Matrix_proto.Id.User_id.t option;
  devices : (string * device) list;
}

val whois : Client.t -> user_id:Matrix_proto.Id.User_id.t -> response
(** [whois c ~user_id] is {!Matrix_client.Admin.whois} with errors raised. *)

val get_user_info : Client.t -> user_id:Matrix_proto.Id.User_id.t -> response
(** Alias for {!whois}. *)
