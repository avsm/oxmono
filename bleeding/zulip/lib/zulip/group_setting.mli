@@ portable

(** User groups selected by Zulip permission settings.

    A setting names a stored user group or contains its members and subgroups
    directly. *)

type t =
  | Group of Id.User_group.t
  | Direct of { members : Id.User.t list; subgroups : Id.User_group.t list }
      (** The type for group settings. [Group id] is the stored group identified
          by [id]. [Direct setting] is an anonymous group containing
          [setting.members] and the members of [setting.subgroups]. *)

type update = { new_ : t; old : t option }
(** The type for group-setting changes. [old] is [None] when the event omits the
    previous setting. *)

val jsont : t Jsont.t
(** [jsont] is a codec for a numeric stored-group identifier or a direct-group
    object. Missing direct member and subgroup lists decode as empty lists. *)

val update_jsont : update Jsont.t
(** [update_jsont] is a codec for group-setting changes. The [new] member is
    required and the [old] member is optional. *)
