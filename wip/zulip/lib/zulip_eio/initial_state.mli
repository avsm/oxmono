(** Registration snapshots and optional event families.

    Typed readers return [Ok None] only when a field is absent. Present fields
    are decoded with their codec, so [null] and malformed values return
    [Error.Json] unless explicitly permitted by that codec. An absent collection
    differs from a present empty collection. *)

type t
(** The type for preserved registration snapshot objects. *)

val of_json : Jsont.json -> (t, Error.t) result
(** [of_json json] is a snapshot preserving [json]. A non-object value returns
    [Error.Invalid_request]. Family validation occurs when a typed reader is
    called. *)

val raw : t -> Jsont.json
(** [raw state] is the complete registration object. *)

val field : t -> string -> Jsont.json option
(** [field state name] is the unchanged field [name], including explicit JSON
    null, or [None] when absent. *)

val decode_field : t -> string -> 'a Jsont.t -> ('a option, Error.t) result
(** [decode_field state name codec] is the optional field [name] decoded with
    [codec]. Missing fields return [Ok None]. Codec failures return
    [Error.Json]. *)

val users : t -> (Zulip.User.t list option, Error.t) result
(** [users state] is the [realm_users] family. An omitted [is_active] field is
    treated as [true]. *)

val inactive_users : t -> (Zulip.User.t list option, Error.t) result
(** [inactive_users state] is the [realm_non_active_users] family. An omitted
    [is_active] field is treated as [false]. *)

val cross_realm_bots : t -> (Zulip.User.t list option, Error.t) result
(** [cross_realm_bots state] is the [cross_realm_bots] family. An omitted
    [is_active] field is treated as [true]. *)

val user_id : t -> (Zulip.Id.User.t option, Error.t) result
(** [user_id state] is the authenticated user identifier. *)

val subscriptions :
  t -> (Zulip.Channel.Subscription.t list option, Error.t) result
(** [subscriptions state] is the subscription list, including per-channel
    preferences. *)

val channels : t -> (Zulip.Channel.t list option, Error.t) result
(** [channels state] is the visible channel list in [streams]. *)

val alert_words : t -> (string list option, Error.t) result
(** [alert_words state] is the authenticated user's alert-word list. *)

val feature_level : t -> (int option, Error.t) result
(** [feature_level state] is the server feature level. *)

val version : t -> (string option, Error.t) result
(** [version state] is the server version string. *)

val settings : t -> (Jsont.json option, Error.t) result
(** [settings state] is the [user_settings] object. A present scalar, array or
    null returns [Error.Json]. *)

val setting : t -> string -> 'a Jsont.t -> ('a option, Error.t) result
(** [setting state name codec] is a setting decoded from [user_settings]. An
    absent settings object or member returns [Ok None]. A malformed settings
    object or member returns [Error.Json]. *)

type muted_user = {
  id : Zulip.Id.User.t;
  timestamp : float;  (** Unix time in seconds. *)
}
(** The type for a muted user and the time at which the mute was set. *)

val muted_users : t -> (muted_user list option, Error.t) result
(** [muted_users state] is the list of muted users and their mute times. *)

type topic = {
  channel_id : Zulip.Id.Channel.t;
  name : string;
  updated : float;  (** Unix time in seconds. *)
  visibility : Zulip.Topic_visibility.t;
}
(** The type for a personal topic visibility override. Future visibility values
    are preserved by {!Zulip.Topic_visibility.t.constructor-Other}. *)

val topics : t -> (topic list option, Error.t) result
(** [topics state] is the list of personal topic visibility overrides. *)

val presences :
  t -> ((string * Presence.user_presence) list option, Error.t) result
(** [presences state] is the presence map. Keys retain the server's user
    identifier or email spelling. *)

val presence_last_update_id : t -> (int option, Error.t) result
(** [presence_last_update_id state] is the cursor for incremental presence
    queries. *)

val server_timestamp : t -> (float option, Error.t) result
(** [server_timestamp state] is the server's Unix time in seconds. *)
