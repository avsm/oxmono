(** Zulip user, profile, status, and personal-account endpoints.

    Endpoint failures, response status failures, request encoding failures, and
    response decoding failures are returned as {!Error.t} values. *)

val list : Client.t -> (Zulip.User.t list, Error.t) result
(** [list client] is the list of users visible to the authenticated account. *)

val list_all :
  Client.t ->
  ?client_gravatar:bool ->
  ?include_custom_profile_fields:bool ->
  ?user_ids:Zulip.Id.User.t list ->
  unit ->
  (Zulip.User.t list, Error.t) result
(** [list_all client ()] is the list of users visible to the authenticated
    account. All optional arguments default to omission. [client_gravatar]
    requests client-side Gravatar handling. [include_custom_profile_fields]
    requests custom profile data. [user_ids] restricts the response to those
    identifiers. *)

val get :
  Client.t ->
  email:string ->
  ?client_gravatar:bool ->
  ?include_custom_profile_fields:bool ->
  unit ->
  (Zulip.User.t, Error.t) result
(** [get client ~email ()] is the user with [email]. Both optional arguments
    default to omission and have the meanings described by {!list_all}. The
    response may contain the user directly or in a [user] member. *)

val get_by_id :
  Client.t ->
  user_id:Zulip.Id.User.t ->
  ?client_gravatar:bool ->
  ?include_custom_profile_fields:bool ->
  unit ->
  (Zulip.User.t, Error.t) result
(** [get_by_id client ~user_id ()] is the user identified by [user_id]. Both
    optional arguments default to omission and have the meanings described by
    {!list_all}. The response may contain the user directly or in a [user]
    member. *)

val me : Client.t -> (Zulip.User.t, Error.t) result
(** [me client] is the authenticated user. *)

type create_result = { user_id : Zulip.Id.User.t; extensions : Jsont.json }
(** The type for user creation responses. [extensions] contains unrecognized
    response members. *)

val create_detailed :
  Client.t ->
  email:string ->
  password:string ->
  full_name:string ->
  (create_result, Error.t) result
(** [create_detailed client ~email ~password ~full_name] creates a user and is
    the complete creation response. *)

val create :
  Client.t ->
  email:string ->
  password:string ->
  full_name:string ->
  (Zulip.Id.User.t, Error.t) result
(** [create client ~email ~password ~full_name] creates a user and is the new
    user's identifier. Response extension members are discarded. *)

type profile_value =
  | Remove
  | Text of string
  | Users of Zulip.Id.User.t list
      (** The type for custom profile field updates. [Remove] encodes as JSON
          null. [Text value] encodes as a string. [Users ids] encodes as an
          identifier array. *)

type profile_update = {
  field_id : Zulip.Id.Profile_field.t;
  value : profile_value;
}
(** The type for an update to one custom profile field. *)

val profile_update_jsont : profile_update Jsont.t
(** [profile_update_jsont] is a codec for profile update objects with required
    [id] and [value] members. *)

val update :
  Client.t ->
  user_id:Zulip.Id.User.t ->
  ?full_name:string ->
  ?role:Zulip.User.Role.t ->
  ?profile_data:profile_update list ->
  ?new_email:string ->
  unit ->
  (unit, Error.t) result
(** [update client ~user_id ()] updates the user identified by [user_id]. All
    optional changes default to omission. An invocation with no changes produces
    [Error (Error.Invalid_request _)] without making a request. *)

type deactivation_actions = {
  delete_profile : bool option;
  delete_public_channel_messages : bool option;
  delete_private_channel_messages : bool option;
  delete_direct_messages : bool option;
}
(** The type for optional data-deletion actions during user deactivation. Each
    [None] field is omitted from the action object. *)

val deactivate :
  Client.t ->
  user_id:Zulip.Id.User.t ->
  ?actions:deactivation_actions ->
  ?notification_comment:string ->
  unit ->
  (unit, Error.t) result
(** [deactivate client ~user_id ()] deactivates the user identified by
    [user_id]. [actions] and [notification_comment] default to omission. *)

val deactivate_me : Client.t -> (unit, Error.t) result
(** [deactivate_me client] deactivates the authenticated user. *)

val reactivate : Client.t -> user_id:Zulip.Id.User.t -> (unit, Error.t) result
(** [reactivate client ~user_id] reactivates the user identified by [user_id].
*)

val get_alert_words : Client.t -> (string list, Error.t) result
(** [get_alert_words client] is the authenticated user's alert word list. *)

val add_alert_words :
  Client.t -> words:string list -> (string list, Error.t) result
(** [add_alert_words client ~words] adds [words] and is the resulting complete
    alert word list. *)

val remove_alert_words :
  Client.t -> words:string list -> (string list, Error.t) result
(** [remove_alert_words client ~words] removes [words] and is the resulting
    complete alert word list. *)

type reaction_type =
  | Unicode_emoji
  | Realm_emoji
  | Zulip_extra_emoji
  | Other_reaction_type of string
      (** The type for status emoji sources. [Other_reaction_type value]
          preserves an unknown wire spelling [value]. *)

type status_emoji = {
  emoji_name : string;
  emoji_code : string;
  reaction_type : reaction_type;
}
(** The type for a status emoji. All three fields appear together on the wire.
*)

type status_emoji_update =
  | Clear_emoji
  | Set_emoji of status_emoji
      (** The type for status emoji changes. [Clear_emoji] sends an empty emoji
          name. [Set_emoji emoji] sends all fields of [emoji]. *)

type user_status = {
  away : bool option;
  status_text : string option;
  emoji : status_emoji option;
  extensions : Jsont.json;
}
(** The type for a user's status. Optional fields are [None] when absent.
    [extensions] contains unrecognized response members. *)

val user_status_jsont : user_status Jsont.t
(** [user_status_jsont] is a codec for user status objects. Emoji name, code,
    and reaction type must either all be present or all be absent. Unknown
    reaction type strings are preserved. *)

val get_status :
  Client.t -> user_id:Zulip.Id.User.t -> (user_status, Error.t) result
(** [get_status client ~user_id] is the status of the user identified by
    [user_id]. *)

val update_status :
  Client.t ->
  ?away:bool ->
  ?text:string ->
  ?emoji:status_emoji_update ->
  unit ->
  (unit, Error.t) result
(** [update_status client ()] updates the authenticated user's status. All
    changes default to omission. An invocation with no changes produces
    [Error (Error.Invalid_request _)] without making a request. *)

val update_status_for_user :
  Client.t ->
  user_id:Zulip.Id.User.t ->
  ?text:string ->
  ?emoji:status_emoji_update ->
  unit ->
  (unit, Error.t) result
(** [update_status_for_user client ~user_id ()] updates the status text or emoji
    of the user identified by [user_id]. Both changes default to omission. An
    invocation with no changes produces [Error (Error.Invalid_request _)]
    without making a request. *)

val update_profile_data :
  Client.t -> updates:profile_update list -> (unit, Error.t) result
(** [update_profile_data client ~updates] applies [updates] to the authenticated
    user's custom profile fields. *)

val remove_profile_data :
  Client.t -> field_ids:Zulip.Id.Profile_field.t list -> (unit, Error.t) result
(** [remove_profile_data client ~field_ids] removes values for the custom
    profile fields identified by [field_ids] from the authenticated user. *)

val upload_avatar :
  Client.t ->
  filename:string ->
  content_type:string ->
  string ->
  (string, Error.t) result
(** [upload_avatar client ~filename ~content_type content] uploads [content] as
    the authenticated user's avatar and is the resulting avatar URL. *)

val upload_avatar_stream :
  Client.t ->
  filename:string ->
  content_type:string ->
  ?length:int64 ->
  _ Eio.Flow.source ->
  (string, Error.t) result
(** [upload_avatar_stream client ~filename ~content_type source] reads avatar
    bytes from [source], uploads them, and is the resulting avatar URL. [length]
    defaults to omission. A negative [length] produces
    [Error (Error.Invalid_request _)] without reading [source]. *)

val delete_avatar : Client.t -> (unit, Error.t) result
(** [delete_avatar client] deletes the authenticated user's avatar. *)

val mute_user : Client.t -> user_id:Zulip.Id.User.t -> (unit, Error.t) result
(** [mute_user client ~user_id] mutes the user identified by [user_id] for the
    authenticated user. *)

val unmute_user : Client.t -> user_id:Zulip.Id.User.t -> (unit, Error.t) result
(** [unmute_user client ~user_id] unmutes the user identified by [user_id] for
    the authenticated user. *)
