(** Zulip server settings and organization metadata endpoints.

    The module covers public server capabilities, linkifiers, custom emoji, and
    custom profile fields. Endpoint, encoding, and decoding failures are
    returned as {!Error.t} values. *)

type authentication_method = {
  password : bool;
  dev : bool;
  email : bool;
  ldap : bool;
  remoteuser : bool;
  github : bool;
  azuread : bool;
  gitlab : bool;
  apple : bool;
  google : bool;
  saml : bool;
  openid_connect : bool;
  discord : bool;
}
(** The type for enabled built-in authentication methods. Missing method members
    decode as [false]. *)

type external_authentication_method = {
  name : string;
  display_name : string;
  display_icon : string option;
  login_url : string;
  signup_url : string;
}
(** The type for an external authentication method. [display_icon] is [None]
    when its wire member is absent or null. *)

type t = {
  zulip_version : string;
  zulip_feature_level : int;
  zulip_merge_base : string option;
  push_notifications_enabled : bool;
  is_incompatible : bool;
  email_auth_enabled : bool;
  require_email_format_usernames : bool;
  realm_uri : string;
  realm_url : string;
  realm_name : string;
  realm_icon : string;
  realm_description : string;
  realm_web_public_access_enabled : bool;
  authentication_methods : authentication_method;
  external_authentication_methods : external_authentication_method list;
}
(** The type for public server settings. Missing push, compatibility, and web
    access booleans decode as [false]. Missing email authentication and email
    username requirements decode as [true]. Missing organization name, icon, and
    description strings decode as empty strings. Missing external methods decode
    as an empty list. *)

val jsont : t Jsont.t
(** [jsont] is a codec for public server settings objects. *)

val get_settings : Client.t -> (t, Error.t) result
(** [get_settings client] is the decoded public settings of the server used by
    [client]. *)

val get_settings_json : Client.t -> (Jsont.json, Error.t) result
(** [get_settings_json client] is the untyped public settings response of the
    server used by [client]. *)

val feature_level : Client.t -> (int, Error.t) result
(** [feature_level client] is the server's Zulip feature level. *)

val supports_feature : Client.t -> level:int -> (bool, Error.t) result
(** [supports_feature client ~level] is [true] if the server's feature level is
    at least [level]. *)

type linkifier = {
  id : Zulip.Id.Linkifier.t;
  pattern : string;
  url_template : string;
  example_input : string option;
  reverse_template : string option;
  alternative_url_templates : string list;
  extensions : Jsont.json;
}
(** The type for an organization linkifier. Example and reverse templates are
    [None] when absent or null. Missing alternative URL templates decode as an
    empty list. [extensions] contains unrecognized members. *)

type 'a field_change =
  | Clear
  | Set of 'a
      (** The type for optional nullable string changes. [Clear] is sent as an
          empty string. [Set value] sends [value]. Omitting the argument makes
          no change. *)

val linkifier_jsont : linkifier Jsont.t
(** [linkifier_jsont] is a codec for linkifier objects. It preserves
    unrecognized members in [extensions]. *)

val get_linkifiers : Client.t -> (linkifier list, Error.t) result
(** [get_linkifiers client] is the organization linkifier list in server order.
*)

val add_linkifier :
  Client.t ->
  pattern:string ->
  url_template:string ->
  ?example_input:string field_change ->
  ?reverse_template:string field_change ->
  ?alternative_url_templates:string list ->
  unit ->
  (Zulip.Id.Linkifier.t, Error.t) result
(** [add_linkifier client ~pattern ~url_template ()] creates a linkifier and is
    its identifier. All optional arguments default to omission. [Clear] sends an
    empty value for the corresponding template. *)

val update_linkifier :
  Client.t ->
  filter_id:Zulip.Id.Linkifier.t ->
  pattern:string ->
  url_template:string ->
  ?example_input:string field_change ->
  ?reverse_template:string field_change ->
  ?alternative_url_templates:string list ->
  unit ->
  (unit, Error.t) result
(** [update_linkifier client ~filter_id ~pattern ~url_template ()] replaces the
    required pattern and URL template of the linkifier identified by
    [filter_id]. All optional arguments default to omission. [Clear] sends an
    empty value for the corresponding template. *)

val delete_linkifier :
  Client.t -> filter_id:Zulip.Id.Linkifier.t -> (unit, Error.t) result
(** [delete_linkifier client ~filter_id] deletes the linkifier identified by
    [filter_id]. *)

val reorder_linkifiers :
  Client.t ->
  ordered_linkifier_ids:Zulip.Id.Linkifier.t list ->
  (unit, Error.t) result
(** [reorder_linkifiers client ~ordered_linkifier_ids] sets the organization
    linkifier order to [ordered_linkifier_ids]. *)

type emoji = {
  id : string;
  name : string;
  source_url : string;
  deactivated : bool;
  author_id : Zulip.Id.User.t option;
  still_url : string option;
  extensions : Jsont.json;
}
(** The type for a custom organization emoji. Missing [deactivated] decodes as
    [false]. Missing or null author and still-image members decode as [None].
    [extensions] contains unrecognized members. *)

val emoji_jsont : emoji Jsont.t
(** [emoji_jsont] is a codec for custom emoji objects. It preserves unrecognized
    members in [extensions]. *)

val get_emoji : Client.t -> (emoji list, Error.t) result
(** [get_emoji client] is the organization custom emoji list. Decoding rejects a
    response whose object key differs from the contained emoji identifier. *)

val upload_emoji :
  Client.t ->
  name:string ->
  filename:string ->
  content_type:string ->
  string ->
  (unit, Error.t) result
(** [upload_emoji client ~name ~filename ~content_type content] uploads
    [content] as the custom emoji named [name]. *)

val upload_emoji_stream :
  Client.t ->
  name:string ->
  filename:string ->
  content_type:string ->
  ?length:int64 ->
  _ Eio.Flow.source ->
  (unit, Error.t) result
(** [upload_emoji_stream client ~name ~filename ~content_type source] reads
    emoji bytes from [source] and uploads them under [name]. [length] defaults
    to omission. A negative [length] produces [Error (Error.Invalid_request _)]
    without reading [source]. *)

val deactivate_emoji : Client.t -> name:string -> (unit, Error.t) result
(** [deactivate_emoji client ~name] deactivates the custom emoji named [name].
*)

type profile_field_type =
  | Short_text
  | Long_text
  | Choice
  | Date
  | Link
  | User
  | External_account
  | Pronouns
  | Other of int
      (** The type for custom profile field kinds. Known kinds use wire values
          [1] through [8]. [Other n] preserves an unknown integer wire value
          [n]. *)

type profile_field = {
  id : Zulip.Id.Profile_field.t;
  field_type : profile_field_type;
  order : int;
  name : string;
  hint : string;
  field_data : string;
  display_in_profile_summary : bool option;
  required : bool;
  editable_by_user : bool;
  use_for_user_matching : bool;
  extensions : Jsont.json;
}
(** The type for a custom profile field. Missing hint and field-data strings
    decode as empty strings. Missing display-summary state decodes as [None].
    Missing user-matching state decodes as [false]. [extensions] contains
    unrecognized members. *)

val profile_field_type_jsont : profile_field_type Jsont.t
(** [profile_field_type_jsont] is an integer codec for profile field kinds.
    Unknown integers decode as [Other n] and encode unchanged. *)

val profile_field_jsont : profile_field Jsont.t
(** [profile_field_jsont] is a codec for custom profile field objects. It
    preserves unrecognized members in [extensions]. *)

val get_profile_fields : Client.t -> (profile_field list, Error.t) result
(** [get_profile_fields client] is the organization custom profile field list.
*)

val create_profile_field :
  Client.t ->
  field_type:profile_field_type ->
  name:string ->
  ?hint:string ->
  ?field_data:Jsont.json ->
  ?display_in_profile_summary:bool ->
  ?required:bool ->
  ?editable_by_user:bool ->
  ?use_for_user_matching:bool ->
  unit ->
  (Zulip.Id.Profile_field.t, Error.t) result
(** [create_profile_field client ~field_type ~name ()] creates a custom profile
    field and is its identifier. All optional arguments default to omission.
    [field_data] is JSON-encoded as the API parameter value. *)

val update_profile_field :
  Client.t ->
  field_id:Zulip.Id.Profile_field.t ->
  ?name:string ->
  ?hint:string ->
  ?field_data:Jsont.json ->
  ?display_in_profile_summary:bool ->
  ?required:bool ->
  ?editable_by_user:bool ->
  ?use_for_user_matching:bool ->
  unit ->
  (unit, Error.t) result
(** [update_profile_field client ~field_id ()] updates the custom profile field
    identified by [field_id]. All changes default to omission. [field_data] is
    JSON-encoded as the API parameter value. An invocation with no changes
    produces [Error (Error.Invalid_request _)] without making a request. *)

val delete_profile_field :
  Client.t -> field_id:Zulip.Id.Profile_field.t -> (unit, Error.t) result
(** [delete_profile_field client ~field_id] deletes the custom profile field
    identified by [field_id]. *)

val reorder_profile_fields :
  Client.t -> order:Zulip.Id.Profile_field.t list -> (unit, Error.t) result
(** [reorder_profile_fields client ~order] sets the custom profile field order
    to [order]. *)
