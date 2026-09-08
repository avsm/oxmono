(** profile — the display name, avatar and extra fields a user publishes,
    raising instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Profile} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces. The setters
    act as the logged-in user and the getters read anyone's profile. *)

(** {1 The whole profile} *)

type profile = Matrix_client.Profile.profile = {
  displayname : string option;
  avatar_url : Matrix_client.Media.Mxc.t option;
  fields : (string * Jsont.json) list;
}
(** A user's profile, including the extra fields this library does not model. *)

val get_profile : Client.t -> user_id:Matrix_proto.Id.User_id.t -> profile
(** [get_profile c ~user_id] is {!Matrix_client.Profile.get_profile} with the
    result unwrapped. *)

(** {1 Display name and avatar} *)

val get_displayname :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> string option
(** [get_displayname c ~user_id] is {!Matrix_client.Profile.get_displayname}
    with the result unwrapped. *)

val set_displayname : Client.t -> displayname:string -> unit
(** [set_displayname c ~displayname] is {!Matrix_client.Profile.set_displayname}
    with the result unwrapped. *)

val clear_displayname : Client.t -> unit
(** [clear_displayname c] is {!Matrix_client.Profile.clear_displayname} with the
    result unwrapped. *)

val get_avatar_url :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  Matrix_client.Media.Mxc.t option
(** [get_avatar_url c ~user_id] is {!Matrix_client.Profile.get_avatar_url} with
    the result unwrapped. *)

val set_avatar_url : Client.t -> avatar_url:Matrix_client.Media.Mxc.t -> unit
(** [set_avatar_url c ~avatar_url] is {!Matrix_client.Profile.set_avatar_url}
    with the result unwrapped. *)

val clear_avatar_url : Client.t -> unit
(** [clear_avatar_url c] is {!Matrix_client.Profile.clear_avatar_url} with the
    result unwrapped. *)

(** {1 Extra fields} *)

val find_field :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  key:string ->
  Jsont.json option
(** [find_field c ~user_id ~key] is {!Matrix_client.Profile.find_field} with the
    result unwrapped. *)

val set_field : Client.t -> key:string -> value:Jsont.json -> unit
(** [set_field c ~key ~value] is {!Matrix_client.Profile.set_field} with the
    result unwrapped. *)

val delete_field : Client.t -> key:string -> unit
(** [delete_field c ~key] is {!Matrix_client.Profile.delete_field} with the
    result unwrapped. *)
