(** Organization user groups and group-valued permissions.

    Stored groups and direct anonymous groups use distinct typed identifiers for
    users and user groups. Operations are subject to the permissions and feature
    level of the connected Zulip server. *)

type group_setting = Zulip.Group_setting.t =
  | Group of Zulip.Id.User_group.t
  | Direct of {
      members : Zulip.Id.User.t list;
      subgroups : Zulip.Id.User_group.t list;
    }
      (** The type for group-valued permission settings. [Group id] names a
          stored user group. [Direct value] names an anonymous group containing
          [value.members] and the members of [value.subgroups]. *)

type group_setting_update = Zulip.Group_setting.update = {
  new_ : group_setting;
  old : group_setting option;
}
(** The type for guarded group-setting updates. [new_] is the requested value.
    [old] is the expected current value when supplied. *)

type t = {
  id : Zulip.Id.User_group.t;
  name : string;
  description : string;
  members : Zulip.Id.User.t list;
  direct_subgroup_ids : Zulip.Id.User_group.t list;
  is_system_group : bool;
  deactivated : bool;
  creator_id : Zulip.Id.User.t option;
  date_created : float option;
  can_add_members_group : group_setting option;
  can_join_group : group_setting option;
  can_leave_group : group_setting option;
  can_manage_group : group_setting option;
  can_mention_group : group_setting;
  can_remove_members_group : group_setting option;
  extensions : Jsont.json;
}
(** The type for user groups. [members] contains direct active members.
    [direct_subgroup_ids] contains direct subgroups. System groups cannot be
    modified by users. Deactivated groups cannot be used for mentions,
    permissions, or subgroup membership. [date_created] is a Unix timestamp in
    UTC seconds. [creator_id] and [date_created] are absent when Zulip has no
    recorded creation metadata. Optional permission fields can be absent on
    older servers. [extensions] preserves unrecognized response members for
    future server fields. *)

val group_setting_jsont : group_setting Jsont.t
(** [group_setting_jsont] is the JSON codec for stored-group identifiers and
    direct anonymous groups. Missing direct member and subgroup lists decode as
    empty lists. Unsupported future representations produce a decoding error. *)

val group_setting_update_jsont : group_setting_update Jsont.t
(** [group_setting_update_jsont] is the JSON codec for guarded group-setting
    updates. The new value is required and the expected old value is optional.
*)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for user groups. Missing descriptions, member
    lists, subgroup lists, and Boolean state use their neutral defaults. Missing
    nullable creation metadata and optional permission fields decode as [None].
    Unrecognized members are retained in [extensions]. *)

val raw : t -> Jsont.json
(** [raw group] is [group] encoded as JSON, including [group.extensions].

    @raise Jsont.exception-Error if the value cannot be encoded. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf group] prints the identifier and name of [group] on [ppf]. *)

val list : Client.t -> (t list, Error.t) result
(** [list client] is the active user groups visible to [client]. Server,
    transport, and response-decoding failures are returned as [Error]. *)

val list_all :
  Client.t ->
  ?include_deactivated_groups:bool ->
  unit ->
  (t list, Error.t) result
(** [list_all client ~include_deactivated_groups ()] is the user groups visible
    to [client]. [include_deactivated_groups] is omitted by default, for which
    Zulip excludes deactivated groups. Server, transport, and response-decoding
    failures are returned as [Error]. *)

val create :
  Client.t ->
  name:string ->
  description:string ->
  members:Zulip.Id.User.t list ->
  ?subgroups:Zulip.Id.User_group.t list ->
  ?can_add_members_group:group_setting ->
  ?can_join_group:group_setting ->
  ?can_leave_group:group_setting ->
  ?can_manage_group:group_setting ->
  ?can_mention_group:group_setting ->
  ?can_remove_members_group:group_setting ->
  unit ->
  (Zulip.Id.User_group.t, Error.t) result
(** [create client ~name ~description ~members ~subgroups ~can_add_members_group
     ~can_join_group ~can_leave_group ~can_manage_group ~can_mention_group
     ~can_remove_members_group ()] creates a user group and is its typed
    identifier. [subgroups] defaults to the empty list. Permission settings are
    omitted by default, for which Zulip applies its server defaults. Encoding,
    server, transport, and response-decoding failures are returned as [Error].
*)

val update :
  Client.t ->
  group_id:Zulip.Id.User_group.t ->
  ?name:string ->
  ?description:string ->
  ?can_add_members_group:group_setting_update ->
  ?can_join_group:group_setting_update ->
  ?can_leave_group:group_setting_update ->
  ?can_manage_group:group_setting_update ->
  ?can_mention_group:group_setting_update ->
  ?can_remove_members_group:group_setting_update ->
  ?deactivated:bool ->
  unit ->
  (unit, Error.t) result
(** [update client ~group_id ~name ~description ~can_add_members_group
     ~can_join_group ~can_leave_group ~can_manage_group ~can_mention_group
     ~can_remove_members_group ~deactivated ()] applies the supplied changes to
    [group_id]. Each optional change is omitted by default. A supplied
    group-setting [old] value asks Zulip to reject a stale update. It returns
    [Error.Invalid_request] without making a request when no change is supplied.
    Encoding, server, and transport failures are returned as [Error]. *)

val update_members :
  Client.t ->
  group_id:Zulip.Id.User_group.t ->
  ?add:Zulip.Id.User.t list ->
  ?remove:Zulip.Id.User.t list ->
  ?add_subgroups:Zulip.Id.User_group.t list ->
  ?remove_subgroups:Zulip.Id.User_group.t list ->
  unit ->
  (unit, Error.t) result
(** [update_members client ~group_id ~add ~remove ~add_subgroups
     ~remove_subgroups ()] applies the supplied direct membership changes to
    [group_id]. Each optional list is omitted by default. It returns
    [Error.Invalid_request] without making a request when every list is omitted.
    Encoding, server, and transport failures are returned as [Error]. *)

val update_subgroups :
  Client.t ->
  group_id:Zulip.Id.User_group.t ->
  ?add:Zulip.Id.User_group.t list ->
  ?remove:Zulip.Id.User_group.t list ->
  unit ->
  (unit, Error.t) result
(** [update_subgroups client ~group_id ~add ~remove ()] applies the supplied
    direct subgroup changes to [group_id]. [add] and [remove] are omitted by
    default. It returns [Error.Invalid_request] without making a request when
    both are omitted. Encoding, server, and transport failures are returned as
    [Error]. *)

val delete :
  Client.t -> group_id:Zulip.Id.User_group.t -> (unit, Error.t) result
(** [delete client ~group_id] deactivates [group_id]. The group remains
    available for audit history and can be reactivated through {!update}. Server
    and transport failures are returned as [Error]. *)

val get_members :
  Client.t ->
  group_id:Zulip.Id.User_group.t ->
  ?direct_member_only:bool ->
  unit ->
  (Zulip.Id.User.t list, Error.t) result
(** [get_members client ~group_id ~direct_member_only ()] is the typed user
    identifiers of members of [group_id]. [direct_member_only] is omitted by
    default, for which Zulip includes members of subgroups. Server, transport,
    and response-decoding failures are returned as [Error]. *)

val is_member :
  Client.t ->
  group_id:Zulip.Id.User_group.t ->
  user_id:Zulip.Id.User.t ->
  ?direct_member_only:bool ->
  unit ->
  (bool, Error.t) result
(** [is_member client ~group_id ~user_id ~direct_member_only ()] is [true] if
    [user_id] belongs to [group_id]. [direct_member_only] is omitted by default,
    for which Zulip includes membership through subgroups. Server, transport,
    and response-decoding failures are returned as [Error]. *)

val get_subgroups :
  Client.t ->
  group_id:Zulip.Id.User_group.t ->
  ?direct_subgroup_only:bool ->
  unit ->
  (Zulip.Id.User_group.t list, Error.t) result
(** [get_subgroups client ~group_id ~direct_subgroup_only ()] is the typed
    identifiers of subgroups of [group_id]. [direct_subgroup_only] is omitted by
    default, for which Zulip includes transitive subgroups. Server, transport,
    and response-decoding failures are returned as [Error]. *)

val is_subgroup :
  Client.t ->
  group_id:Zulip.Id.User_group.t ->
  subgroup_id:Zulip.Id.User_group.t ->
  ?direct_subgroup_only:bool ->
  unit ->
  (bool, Error.t) result
(** [is_subgroup client ~group_id ~subgroup_id ~direct_subgroup_only ()] is
    [true] if {!get_subgroups} returns [subgroup_id]. [direct_subgroup_only] is
    omitted by default, for which Zulip includes transitive subgroups. Server,
    transport, and response-decoding failures are returned as [Error]. *)
