(** server — what a homeserver is, and what it will do.

    Three endpoints a client reads before anything else.
    [/.well-known/matrix/client] and [/_matrix/client/versions] are both served
    outside [/_matrix/client/v3], and [/_matrix/client/v3/capabilities] is not.
    {!discover} runs the first two in the order the specification's server
    discovery section prescribes.

    @see <https://spec.matrix.org/v1.11/client-server-api/#server-discovery>
      Server Discovery *)

(** {1 Supported versions} *)

type versions = {
  versions : string list;
      (** Spec versions the homeserver claims, e.g. ["v1.11"]. *)
  unstable_features : (string * bool) list;
      (** Unstable feature flags, e.g. [("org.matrix.msc3575", true)]. Sorted by
          feature name. *)
}
(** The reply to [GET /_matrix/client/versions]. *)

val get_versions : Client.t -> (versions, Error.t) result
(** [get_versions t] is what the homeserver supports. Uses
    [GET /_matrix/client/versions] (Matrix 1.0).

    The endpoint takes an access token optionally. A logged-in client may be
    told about features that anonymous callers are not. A successful response is
    cached on [t] and reused by later calls; transport, HTTP and JSON failures
    are never cached. The cache is best effort: concurrent misses may issue
    duplicate requests, and {!invalidate_cache} does not cancel an
    already-running request. *)

val invalidate_cache : Client.t -> unit
(** [invalidate_cache t] clears the cached [/versions] and [/capabilities]
    responses for [t]. It is useful after a server-side capability change. *)

val supports_version : versions -> string -> bool
(** [supports_version v version] is [true] when [version] (e.g. ["v1.11"]) is
    listed in [v.versions]. *)

val supports_version_at_least : versions -> major:int -> minor:int -> bool
(** [supports_version_at_least v ~major ~minor] is [true] when [v] advertises a
    stable Matrix Client-Server API version at least [v<major>.<minor>]. It
    ignores legacy [r0.*] and malformed version names. This is for selecting an
    endpoint introduced in a particular stable release; use {!supports_version}
    when exact membership matters. *)

val has_unstable_feature : versions -> string -> bool
(** [has_unstable_feature v feature] is [true] when [feature] is listed in
    [v.unstable_features] {e and} enabled. *)

(** {1 Capabilities} *)

type room_version_stability = [ `Stable | `Unstable | `Other of string ]
(** The stability of a room version, from [m.room_versions.available]. *)

type room_versions_capability = {
  default : string;  (** Room version used for new rooms. *)
  available : (string * room_version_stability) list;
      (** Every room version the server knows, sorted by version. *)
}
(** The [m.room_versions] capability. *)

type capabilities = {
  change_password : bool option;  (** [m.change_password.enabled]. *)
  room_versions : room_versions_capability option;  (** [m.room_versions]. *)
  set_displayname : bool option;  (** [m.set_displayname.enabled]. *)
  set_avatar_url : bool option;  (** [m.set_avatar_url.enabled]. *)
  thirdparty_id_changes : bool option;  (** [m.3pid_changes.enabled]. *)
  get_login_token : bool option;
      (** [m.get_login_token.enabled] (Matrix 1.7, MSC3882). *)
  custom : (string * Jsont.json) list;
      (** Capabilities this module does not model, verbatim. *)
}
(** The reply to [GET /_matrix/client/v3/capabilities], as the [capabilities]
    object itself.

    Every capability the specification defines is optional here. An absent one
    means the server said nothing, which the specification reads as "assume the
    default" rather than "disabled". Capabilities outside this list, a server's
    own [com.example.*] ones included, survive in {!custom}. *)

val get_capabilities : Client.t -> (capabilities, Error.t) result
(** [get_capabilities t] is [GET /_matrix/client/v3/capabilities] (Matrix 1.1).
    Requires an access token. A successful response is cached on [t] and reused
    by later calls; transport, HTTP and JSON failures are never cached. The
    cache is best effort: concurrent misses may issue duplicate requests, and
    {!invalidate_cache} does not cancel an already-running request. *)

val refresh_capabilities : Client.t -> (capabilities, Error.t) result
(** [refresh_capabilities t] fetches [/capabilities] even when a cached response
    exists and replaces that cache entry after a successful decode. A failure
    leaves the previous cached response available. *)

val find_capability : capabilities -> name:string -> Jsont.json option
(** [find_capability c ~name] is the raw JSON of the capability [name], looked
    up in {!capabilities.custom}. A modelled capability is not repeated there
    and is read off the record field instead. *)

type account_moderation_capability = {
  suspend : bool;  (** The user may suspend accounts. *)
  lock : bool;  (** The user may lock accounts. *)
}
(** The [m.account_moderation] capability. An absent member defaults both
    actions to [false], as in Ruma. *)

type profile_fields_capability = {
  enabled : bool;
  allowed : string list option;
  disallowed : string list option;
}
(** The [m.profile_fields] policy. When [allowed] is present it takes precedence
    over [disallowed]. *)

val can_change_password : Client.t -> (bool, Error.t) result
(** [can_change_password t] reports whether the logged-in user may change their
    password. An absent [m.change_password] capability defaults to [true]. *)

val can_change_thirdparty_ids : Client.t -> (bool, Error.t) result
(** [can_change_thirdparty_ids t] reports whether the logged-in user may add,
    remove or change third-party identifiers. An absent [m.3pid_changes]
    capability defaults to [true]. *)

val can_get_login_token : Client.t -> (bool, Error.t) result
(** [can_get_login_token t] reports whether the logged-in user may generate
    single-use login tokens. An absent [m.get_login_token] capability defaults
    to [false]. *)

val room_versions : Client.t -> (room_versions_capability, Error.t) result
(** [room_versions t] returns the room versions supported by the homeserver. An
    absent [m.room_versions] capability defaults to room version ["1"] as the
    sole stable version. *)

val account_moderation :
  Client.t -> (account_moderation_capability, Error.t) result
(** [account_moderation t] returns the account suspension and locking actions
    advertised by [m.account_moderation]. An absent capability defaults to both
    actions being unavailable. *)

val forgets_room_when_leaving : Client.t -> (bool, Error.t) result
(** [forgets_room_when_leaving t] reports whether the homeserver automatically
    forgets rooms after the user leaves. An absent [m.forget_forced_upon_leave]
    capability defaults to [false]. *)

val can_change_displayname : Client.t -> (bool, Error.t) result
(** [can_change_displayname t] reports whether the logged-in user may change
    their display name. It uses the [m.profile_fields] capability when present,
    including its allowlist/disallowlist policy. When that capability is absent,
    Matrix 1.16+ homeservers default to unrestricted profile fields; older
    homeservers use [m.set_displayname], whose absent value defaults to [true].
    The [/capabilities] and [/versions] responses are cached. *)

val can_change_avatar : Client.t -> (bool, Error.t) result
(** [can_change_avatar t] is the corresponding capability query for the
    [avatar_url] profile field. Its fallback and caching behavior is the same as
    {!can_change_displayname}. *)

val extended_profile_fields :
  Client.t -> (profile_fields_capability, Error.t) result
(** [extended_profile_fields t] returns the advertised [m.profile_fields]
    policy. If it is absent, Matrix 1.16+ defaults to an enabled unrestricted
    policy; an older homeserver defaults to a disabled policy, matching the
    pinned Rust facade. *)

(** {1 Well-known discovery} *)

type server_info = { base_url : Uriz.t }
(** Where a service the well-known names lives, for [m.homeserver] and
    [m.identity_server]. *)

type authentication_info = {
  issuer : string;  (** OAuth 2.0 authorization server issuer. *)
  account : string option;  (** Account-management URL, if advertised. *)
}
(** [m.authentication] (Matrix 1.15), also read from the MSC2965 unstable name
    [org.matrix.msc2965.authentication]. *)

type well_known = {
  homeserver : server_info;  (** [m.homeserver]. *)
  identity_server : server_info option;  (** [m.identity_server]. *)
  authentication : authentication_info option;  (** [m.authentication]. *)
}
(** The reply to [GET /.well-known/matrix/client] (Matrix 1.1). *)

val get_well_known : Client.t -> (well_known option, Error.t) result
(** [get_well_known t] is [GET /.well-known/matrix/client] (Matrix 1.1), fetched
    from the origin of the client's configured homeserver rather than from
    [/_matrix/client/v3].

    [Ok None] means the server answered 404, which the spec defines as "no
    well-known, use the server name as the base URL". Any other failure is an
    [Error]. *)

type get_versions_at = Uriz.t -> (versions, Error.t) result
(** A callback {!discover} may use to fetch [/versions] from a delegated
    homeserver origin. It must construct its own unauthenticated client; the
    core library never reuses the origin-restricted client for another origin.
*)

type discovery = {
  base_url : Uriz.t;
      (** The homeserver base URL to use, which is [m.homeserver.base_url] when
          the well-known resolved and the client's configured homeserver
          otherwise. *)
  well_known : well_known option;  (** What the well-known said, if any. *)
  server_versions : versions option;
      (** [GET /_matrix/client/versions] against {!base_url}, when that URL
          shares an origin with the client passed in, or when the optional
          {!get_versions_at} callback validates a delegated origin. Without that
          callback a client restricted to its own origin leaves this [None] for
          off-origin well-known results. *)
}
(** The result of {!discover}. *)

val discover :
  ?get_versions_at:get_versions_at -> Client.t -> (discovery, Error.t) result
(** [discover t] fetches [/.well-known/matrix/client], then
    [/_matrix/client/versions] against the base URL it names. A missing
    well-known is not a failure, and the client's configured homeserver becomes
    {!discovery.base_url} instead. When the well-known delegates to another
    origin, [get_versions_at] is used when supplied; otherwise [server_versions]
    remains [None]. *)
