(** thirdparty — looking up the networks an application service bridges into
    Matrix, Matrix 1.0.

    All the endpoints are under [/_matrix/client/v3] and answer with a bare JSON
    object or array, with no wrapper member.

    - [GET /thirdparty/protocols]
    - [GET /thirdparty/protocol/{protocol}]
    - [GET /thirdparty/location] and [GET /thirdparty/location/{protocol}]
    - [GET /thirdparty/user] and [GET /thirdparty/user/{protocol}]

    A protocol is a bridged network, a location is a room on it and a user is an
    account on it. Each protocol names the fields that identify a location or a
    user on it, and the values of those fields are strings. *)

(** {1 Protocols} *)

type field_type = {
  regexp : string;  (** Pattern the field value must match. *)
  placeholder : string;  (** Example value, for a form's placeholder. *)
}
(** The type for how one field of a protocol is rendered and validated. *)

type protocol_instance = {
  network_id : string;  (** Identifies the instance within the protocol. *)
  desc : string;  (** Human-readable name of the instance. *)
  icon : string option;  (** An [mxc://] URI for the instance's icon. *)
  fields : (string * string) list;
      (** The field values that pin a lookup to this instance, sorted by field
          name. *)
  instance_id : string option;
      (** Added by the homeserver rather than the application service. *)
}
(** The type for one bridged network instance of a protocol. *)

type protocol = {
  user_fields : string list;
      (** The field names, in order, that identify a user on this network. *)
  location_fields : string list;
      (** The field names, in order, that identify a room on this network. *)
  icon : string;  (** An [mxc://] URI for the protocol's icon. *)
  field_types : (string * field_type) list;
      (** Validation for every name in {!user_fields} and {!location_fields},
          sorted by field name. *)
  instances : protocol_instance list;  (** The instances of this protocol. *)
}
(** The type for a bridged protocol, as its application service describes it. *)

val protocols : Client.t -> ((string * protocol) list, Error.t) result
(** [protocols t] is [GET /_matrix/client/v3/thirdparty/protocols] (Matrix 1.0),
    as the protocol names paired with their descriptions and sorted by name. *)

val get_protocol : Client.t -> name:string -> (protocol, Error.t) result
(** [get_protocol t ~name] is
    [GET /_matrix/client/v3/thirdparty/protocol/{protocol}] (Matrix 1.0) for the
    protocol called [name]. *)

(** {1 Locations} *)

type location = {
  alias : Matrix_proto.Id.Room_alias.t;
      (** The Matrix room alias the portal room uses. *)
  protocol : string;  (** The protocol the location belongs to. *)
  fields : (string * string) list;
      (** Protocol-specific identification, sorted by field name. *)
}
(** The type for a room on a third-party network. *)

val locations_of_alias :
  Client.t ->
  alias:Matrix_proto.Id.Room_alias.t ->
  (location list, Error.t) result
(** [locations_of_alias t ~alias] is
    [GET /_matrix/client/v3/thirdparty/location?alias=...] (Matrix 1.0), the
    third-party locations [alias] maps to. *)

val locations :
  Client.t ->
  protocol:string ->
  ?fields:(string * string) list ->
  unit ->
  (location list, Error.t) result
(** [locations t ~protocol ()] is
    [GET /_matrix/client/v3/thirdparty/location/{protocol}] (Matrix 1.0).
    [fields] gives values for the protocol's {!protocol.location_fields}, one
    query parameter per field, and defaults to the empty list. *)

(** {1 Users} *)

type user = {
  userid : Matrix_proto.Id.User_id.t;
      (** The Matrix user id the third-party user maps to. *)
  protocol : string;  (** The protocol the user belongs to. *)
  fields : (string * string) list;
      (** Protocol-specific identification, sorted by field name. *)
}
(** The type for a user on a third-party network. *)

val users_of_user_id :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> (user list, Error.t) result
(** [users_of_user_id t ~user_id] is
    [GET /_matrix/client/v3/thirdparty/user?userid=...] (Matrix 1.0), the
    third-party users [user_id] maps to. *)

val users :
  Client.t ->
  protocol:string ->
  ?fields:(string * string) list ->
  unit ->
  (user list, Error.t) result
(** [users t ~protocol ()] is
    [GET /_matrix/client/v3/thirdparty/user/{protocol}] (Matrix 1.0). [fields]
    gives values for the protocol's {!protocol.user_fields}, one query parameter
    per field, and defaults to the empty list. *)
