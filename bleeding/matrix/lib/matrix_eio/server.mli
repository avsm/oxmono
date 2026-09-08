(** server — what a homeserver is, and what it will do, raising instead of
    returning.

    Every function that performs a request raises [Eio.Io] carrying [Error.E e]
    where {!Matrix_client.Server} returns [Error e]. That module documents what
    each call does, which endpoint it uses and which errors it produces. *)

(** {1 Supported versions} *)

type versions = Matrix_client.Server.versions = {
  versions : string list;
  unstable_features : (string * bool) list;
}
(** The specification versions and unstable features a homeserver supports. *)

val get_versions : Client.t -> versions
(** [get_versions c] is {!Matrix_client.Server.get_versions} with the result
    unwrapped. *)

val supports_version : versions -> string -> bool
(** [supports_version vs v] is {!Matrix_client.Server.supports_version}. It
    performs no request and raises nothing. *)

val supports_version_at_least : versions -> major:int -> minor:int -> bool
(** [supports_version_at_least vs ~major ~minor] is
    {!Matrix_client.Server.supports_version_at_least}. It performs no request
    and raises nothing. *)

val has_unstable_feature : versions -> string -> bool
(** [has_unstable_feature vs f] is {!Matrix_client.Server.has_unstable_feature}.
    It performs no request and raises nothing. *)

val invalidate_cache : Client.t -> unit
(** [invalidate_cache c] clears [c]'s cached versions and capabilities. *)

(** {1 Capabilities} *)

type room_version_stability = Matrix_client.Server.room_version_stability
(** How settled a room version is. *)

type room_versions_capability =
      Matrix_client.Server.room_versions_capability = {
  default : string;
  available : (string * room_version_stability) list;
}
(** The room versions the homeserver will create and accept. *)

type capabilities = Matrix_client.Server.capabilities = {
  change_password : bool option;
  room_versions : room_versions_capability option;
  set_displayname : bool option;
  set_avatar_url : bool option;
  thirdparty_id_changes : bool option;
  get_login_token : bool option;
  custom : (string * Jsont.json) list;
}
(** What the homeserver lets the logged-in user do. *)

type account_moderation_capability =
      Matrix_client.Server.account_moderation_capability = {
  suspend : bool;
  lock : bool;
}
(** The [m.account_moderation] actions advertised by the homeserver. *)

type profile_fields_capability =
      Matrix_client.Server.profile_fields_capability = {
  enabled : bool;
  allowed : string list option;
  disallowed : string list option;
}
(** The extended-profile policy advertised by the homeserver. *)

val get_capabilities : Client.t -> capabilities
(** [get_capabilities c] is {!Matrix_client.Server.get_capabilities} with the
    result unwrapped. *)

val refresh_capabilities : Client.t -> capabilities
(** [refresh_capabilities c] is {!Matrix_client.Server.refresh_capabilities}
    with the result unwrapped. *)

val find_capability : capabilities -> name:string -> Jsont.json option
(** [find_capability caps ~name] is {!Matrix_client.Server.find_capability}. It
    performs no request and raises nothing. *)

val can_change_password : Client.t -> bool
(** [can_change_password c] is {!Matrix_client.Server.can_change_password} with
    the result unwrapped. *)

val can_change_thirdparty_ids : Client.t -> bool
(** [can_change_thirdparty_ids c] is
    {!Matrix_client.Server.can_change_thirdparty_ids} with the result unwrapped.
*)

val can_get_login_token : Client.t -> bool
(** [can_get_login_token c] is {!Matrix_client.Server.can_get_login_token} with
    the result unwrapped. *)

val room_versions : Client.t -> room_versions_capability
(** [room_versions c] is {!Matrix_client.Server.val-room_versions} with the
    result unwrapped. *)

val account_moderation : Client.t -> account_moderation_capability
(** [account_moderation c] is {!Matrix_client.Server.account_moderation} with
    the result unwrapped. *)

val forgets_room_when_leaving : Client.t -> bool
(** [forgets_room_when_leaving c] is
    {!Matrix_client.Server.forgets_room_when_leaving} with the result unwrapped.
*)

val can_change_displayname : Client.t -> bool
(** [can_change_displayname c] is {!Matrix_client.Server.can_change_displayname}
    with the result unwrapped. It may query and cache [/capabilities] and
    [/versions]. *)

val can_change_avatar : Client.t -> bool
(** [can_change_avatar c] is {!Matrix_client.Server.can_change_avatar} with the
    result unwrapped. It may query and cache [/capabilities] and [/versions]. *)

val extended_profile_fields : Client.t -> profile_fields_capability
(** [extended_profile_fields c] is
    {!Matrix_client.Server.extended_profile_fields} with the result unwrapped.
*)

(** {1 Well-known discovery} *)

type server_info = Matrix_client.Server.server_info = { base_url : Uriz.t }
(** Where a delegated service lives. *)

type authentication_info = Matrix_client.Server.authentication_info = {
  issuer : string;
  account : string option;
}
(** The OAuth 2.0 issuer a homeserver delegates authentication to. *)

type well_known = Matrix_client.Server.well_known = {
  homeserver : server_info;
  identity_server : server_info option;
  authentication : authentication_info option;
}
(** The contents of a [/.well-known/matrix/client] document. *)

val get_well_known : Client.t -> well_known option
(** [get_well_known c] is {!Matrix_client.Server.get_well_known} with the result
    unwrapped. *)

type discovery = Matrix_client.Server.discovery = {
  base_url : Uriz.t;
  well_known : well_known option;
  server_versions : versions option;
}
(** The homeserver a domain resolves to. *)

val discover : Client.t -> discovery
(** [discover c] is {!Matrix_client.Server.discover} with the result unwrapped.
    Off-origin delegated base URLs are fetched through a fresh unauthenticated
    client using [c]'s unrestricted transport; invalid delegated URLs and
    transport/policy failures raise as [Eio.Io] errors. *)
