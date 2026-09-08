(** profile — the display name, avatar and extra fields a user publishes.

    A profile is a public, server-wide object. Any user may read another's, and
    a client may only write its own, so every setter takes the user identifier
    from the session and fails with {!Error.No_session} without one. A profile
    is not room state. A per-room display name lives in that room's
    [m.room.member] event instead.

    Beyond [displayname] and [avatar_url] a profile may carry namespaced fields
    of its own, [m.tz] for a timezone among them (MSC4133, stable since Matrix
    1.16). Servers advertise support through the [m.profile_fields] capability.
*)

(** {1 The whole profile} *)

type profile = {
  displayname : string option;
  avatar_url : Media.Mxc.t option;
  fields : (string * Jsont.json) list;
      (** Every other profile field, verbatim, sorted by name. *)
}
(** A user's profile. *)

val get_profile :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> (profile, Error.t) result
(** [get_profile t ~user_id] is [GET /_matrix/client/v3/profile/{userId}]
    (Matrix 1.0). A user the server does not know, or will not disclose, is
    [M_NOT_FOUND] or [M_FORBIDDEN]. A user who has set nothing is an [Ok] whose
    members are all empty. *)

(** {1 Display name and avatar} *)

val get_displayname :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  (string option, Error.t) result
(** [get_displayname t ~user_id] is
    [GET /_matrix/client/v3/profile/{userId}/displayname] (Matrix 1.0). *)

val set_displayname : Client.t -> displayname:string -> (unit, Error.t) result
(** [set_displayname t ~displayname] is
    [PUT /_matrix/client/v3/profile/{userId}/displayname] (Matrix 1.0) for the
    logged-in user. The server propagates it into every room the user is in,
    which can take a while on a large account. *)

val clear_displayname : Client.t -> (unit, Error.t) result
(** [clear_displayname t] removes the logged-in user's display name with the
    Matrix-compatible JSON [null] value. *)

val get_avatar_url :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  (Media.Mxc.t option, Error.t) result
(** [get_avatar_url t ~user_id] is
    [GET /_matrix/client/v3/profile/{userId}/avatar_url] (Matrix 1.0). *)

val set_avatar_url :
  Client.t -> avatar_url:Media.Mxc.t -> (unit, Error.t) result
(** [set_avatar_url t ~avatar_url] is
    [PUT /_matrix/client/v3/profile/{userId}/avatar_url] (Matrix 1.0) for the
    logged-in user. [avatar_url] is normally what {!Media.val-upload} returned.
*)

val clear_avatar_url : Client.t -> (unit, Error.t) result
(** [clear_avatar_url t] removes the logged-in user's avatar URL with the
    Matrix-compatible JSON [null] value. *)

(** {1 Extra fields} *)

val find_field :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  key:string ->
  (Jsont.json option, Error.t) result
(** [find_field t ~user_id ~key] is
    [GET /_matrix/client/v3/profile/{userId}/{keyName}] on Matrix 1.16+ servers,
    or MSC4133's unstable path on older servers, selected from [/versions].

    [Ok None] means the server answered with an empty object, so the user has no
    such field. A user or field the server refuses to disclose is an [Error]
    carrying [M_NOT_FOUND] or [M_FORBIDDEN]. *)

val set_field :
  Client.t -> key:string -> value:Jsont.json -> (unit, Error.t) result
(** [set_field t ~key ~value] is the stable Matrix 1.16 or MSC4133 unstable
    [PUT] route selected from [/versions] for the logged-in user.

    The server caps a key at 255 bytes and a whole profile at 64 KiB, answering
    [M_KEY_TOO_LARGE] or [M_PROFILE_TOO_LARGE] beyond that. *)

val delete_field : Client.t -> key:string -> (unit, Error.t) result
(** [delete_field t ~key] is the stable Matrix 1.16 or MSC4133 unstable [DELETE]
    route selected from [/versions] for the logged-in user. *)
