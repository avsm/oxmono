(** Zulip user presence queries and updates.

    Endpoint failures, response status failures, and JSON decoding failures are
    returned as {!Error.t} values. Presence timestamps are Unix time in seconds.
*)

type status =
  | Active
  | Idle
  | Offline
  | Other of string
      (** The type for presence states. [Other value] preserves an unknown wire
          spelling [value]. *)

type client_presence = {
  client : string;
  status : status;
  timestamp : float;
  pushable : bool;
}
(** The type for the presence reported by one client. [pushable] defaults to
    [false] when its wire member is absent. *)

type user_presence = {
  active_timestamp : float option;
  idle_timestamp : float option;
  clients : client_presence list;
}
(** The type for a user's aggregate presence. Missing active and idle timestamps
    produce [None]. Every other object member is decoded as a named client
    presence. *)

type presence_map = (string * user_presence) list
(** The type for presence values keyed by the server's user key, usually an
    email address. Entry order follows JSON object member order. *)

type user_response = {
  server_timestamp : float option;
  presence : user_presence;
  extensions : Jsont.json;
}
(** The type for a single-user presence response. [extensions] contains
    unrecognized response members. *)

type realm_response = {
  server_timestamp : float;
  presences : presence_map;
  extensions : Jsont.json;
}
(** The type for an organization-wide presence response. [extensions] contains
    unrecognized response members. *)

type update_response = {
  presence_last_update_id : int option;
  server_timestamp : float option;
  presences : presence_map option;
  extensions : Jsont.json;
}
(** The type for a presence update response. Optional fields are [None] when
    absent. [extensions] contains unrecognized response members. *)

val get_user_detailed :
  Client.t -> user_id:Zulip.Id.User.t -> (user_response, Error.t) result
(** [get_user_detailed client ~user_id] is the complete presence response for
    the user identified by [user_id]. *)

val get_user :
  Client.t -> user_id:Zulip.Id.User.t -> (user_presence, Error.t) result
(** [get_user client ~user_id] is the presence of the user identified by
    [user_id]. Response metadata is discarded. *)

val get_user_by_email_detailed :
  Client.t -> email:string -> (user_response, Error.t) result
(** [get_user_by_email_detailed client ~email] is the complete presence response
    for the user with [email]. *)

val get_user_by_email :
  Client.t -> email:string -> (user_presence, Error.t) result
(** [get_user_by_email client ~email] is the presence of the user with [email].
    Response metadata is discarded. *)

val get_all_detailed : Client.t -> (realm_response, Error.t) result
(** [get_all_detailed client] is the complete organization-wide presence
    response. *)

val get_all : Client.t -> (presence_map, Error.t) result
(** [get_all client] is the organization-wide presence map. Response metadata is
    discarded. *)

val update :
  Client.t ->
  status:status ->
  ?last_update_id:int ->
  ?history_limit_days:int ->
  ?ping_only:bool ->
  ?new_user_input:bool ->
  ?slim_presence:bool ->
  unit ->
  (update_response, Error.t) result
(** [update client ~status ()] reports the current user's [status] and is the
    server's update response. All optional arguments default to omission.
    [history_limit_days] limits returned history and must be nonnegative.
    [last_update_id] requests changes after a previous update identifier.
    [ping_only], [new_user_input], and [slim_presence] are sent unchanged when
    present. [Offline] and [Other _] produce [Error (Error.Invalid_request _)]
    without making a request. A negative [history_limit_days] produces the same
    error form. *)

val status_jsont : status Jsont.t
(** [status_jsont] is a string codec for presence states. Unknown strings decode
    as [Other value] and encode unchanged. *)

val client_presence_jsont : client_presence Jsont.t
(** [client_presence_jsont] is a codec for named client presence objects. *)

val user_presence_jsont : user_presence Jsont.t
(** [user_presence_jsont] is a codec for aggregate user presence objects. *)

val presence_map_jsont : presence_map Jsont.t
(** [presence_map_jsont] is a codec between JSON objects and keyed presence
    lists. *)

val user_response_jsont : user_response Jsont.t
(** [user_response_jsont] is a codec for single-user presence responses. It
    preserves unrecognized members in [extensions]. *)

val realm_response_jsont : realm_response Jsont.t
(** [realm_response_jsont] is a codec for organization-wide presence responses.
    It preserves unrecognized members in [extensions]. *)

val update_response_jsont : update_response Jsont.t
(** [update_response_jsont] is a codec for presence update responses. It
    preserves unrecognized members in [extensions]. *)

val status_to_string : status -> string
(** [status_to_string status] is the wire spelling of [status]. *)

val status_of_string : string -> status
(** [status_of_string value] is the presence state encoded by [value]. Unknown
    values produce [Other value]. *)

val pp_status : Format.formatter -> status -> unit
(** [pp_status ppf status] writes the wire spelling of [status] to [ppf]. *)

val pp_user_presence : Format.formatter -> user_presence -> unit
(** [pp_user_presence ppf presence] writes the client names and states in
    [presence] to [ppf]. *)
