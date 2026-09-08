(** thirdparty — looking up the networks an application service bridges into
    Matrix, raising instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Thirdparty} returns [Error e]. A protocol is a network an
    application service bridges into Matrix, a location is a room on it and a
    user is an account on it. *)

(** {1 Protocols} *)

type field_type = Matrix_client.Thirdparty.field_type = {
  regexp : string;
  placeholder : string;
}
(** How one field of a protocol is written. *)

type protocol_instance = Matrix_client.Thirdparty.protocol_instance = {
  network_id : string;
  desc : string;
  icon : string option;
  fields : (string * string) list;
  instance_id : string option;
}
(** One network a bridge serves. *)

type protocol = Matrix_client.Thirdparty.protocol = {
  user_fields : string list;
  location_fields : string list;
  icon : string;
  field_types : (string * field_type) list;
  instances : protocol_instance list;
}
(** A bridged network and how its rooms and users are addressed. *)

val protocols : Client.t -> (string * protocol) list
(** [protocols c] is {!Matrix_client.Thirdparty.val-protocols} with the result
    unwrapped. *)

val get_protocol : Client.t -> name:string -> protocol
(** [get_protocol c ~name] is {!Matrix_client.Thirdparty.get_protocol} with the
    result unwrapped. *)

(** {1 Locations} *)

type location = Matrix_client.Thirdparty.location = {
  alias : Matrix_proto.Id.Room_alias.t;
  protocol : string;
  fields : (string * string) list;
}
(** A room on a bridged network. *)

val locations_of_alias :
  Client.t -> alias:Matrix_proto.Id.Room_alias.t -> location list
(** [locations_of_alias c ~alias] is
    {!Matrix_client.Thirdparty.locations_of_alias} with the result unwrapped. *)

val locations :
  Client.t ->
  protocol:string ->
  ?fields:(string * string) list ->
  unit ->
  location list
(** [locations c ~protocol ()] is {!Matrix_client.Thirdparty.val-locations} with
    the result unwrapped. *)

(** {1 Users} *)

type user = Matrix_client.Thirdparty.user = {
  userid : Matrix_proto.Id.User_id.t;
  protocol : string;
  fields : (string * string) list;
}
(** An account on a bridged network. *)

val users_of_user_id :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> user list
(** [users_of_user_id c ~user_id] is
    {!Matrix_client.Thirdparty.users_of_user_id} with the result unwrapped. *)

val users :
  Client.t ->
  protocol:string ->
  ?fields:(string * string) list ->
  unit ->
  user list
(** [users c ~protocol ()] is {!Matrix_client.Thirdparty.val-users} with the
    result unwrapped. *)
