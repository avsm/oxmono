(** Immutable, cache-backed room-member details.

    This is the explicit-state counterpart to the pinned Rust SDK's
    [Room::members] and [RoomMember]. Cached access never performs I/O;
    {!ensure_members} returns a new facade containing the authoritative
    [/members] snapshot when lazy loading made the original partial. *)

type t

type member = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
  display_label : string;
      (** A UI-safe label. Duplicate display names include the user ID; a
          missing or empty display name falls back to the user ID. *)
  display_name_ambiguous : bool;
      (** Whether another active member has the same explicit display name. *)
  avatar_url : Media.Mxc.t option;
  membership : Matrix_proto.Event.Membership.t;
  role : Room.role;
      (** The cached creator/power-level classification from
          {!Room.suggested_role}. *)
  is_service_member : bool;
      (** Membership in the stable or legacy room service-member hint. *)
  is_account_user : bool;
      (** Whether this member is the client state's account user. *)
}

val create :
  client:Client.t -> state:Base_client.state -> Matrix_proto.Id.Room_id.t -> t
(** [create ~client ~state room_id] is a view over [state]. *)

val state : t -> Base_client.state
(** The snapshot backing this facade. Use this to adopt the state returned by
    {!ensure_members} in the application's state owner. *)

val room_id : t -> Matrix_proto.Id.Room_id.t

val members_complete : t -> bool
(** Whether cached membership is known to be exhaustive. *)

val members : t -> member list
(** Active cached members (joined and invited), ordered by user ID. Malformed
    member events are ignored. This function never performs a request; inspect
    {!members_complete} before treating the result as exhaustive. *)

val member_count : t -> int
(** The number of active cached members. *)

val service_member_count : t -> int
(** The number of active cached members named by the room's service-member hint.
*)

val human_member_count : t -> int
(** [member_count t - service_member_count t]. *)

val ensure_members : t -> (t, Error.t) result
(** [ensure_members t] returns [t] without a request if its cache is complete.
    Otherwise it calls [/rooms/{roomId}/members], folds that authoritative
    snapshot through {!Base_client.replace_members}, and returns a new facade.
    The original remains unchanged. An unknown room fails locally without a
    request. *)
