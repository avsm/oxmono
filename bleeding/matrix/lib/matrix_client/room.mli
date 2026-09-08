(** A cache-backed facade for one room.

    The state is the immutable [Base_client.state] snapshot supplied to
    {!create}. Construct a new facade after sync when projections should use a
    newer snapshot. Network mutations do not modify that snapshot. *)

type t

val create :
  client:Client.t -> state:Base_client.state -> Matrix_proto.Id.Room_id.t -> t

val room_id : t -> Matrix_proto.Id.Room_id.t

val routing_candidates : t -> string list
(** At most three joined-member servers following the Matrix routing algorithm:
    a server belonging to the highest-power member at level 50 or above, then
    the remaining servers by population with deterministic ties. IP literals and
    servers rejected by cached [m.room.server_acl] state are omitted. *)

val permalink : t -> ?via:string list -> unit -> string
(** A matrix.to room permalink. A canonical alias, or the last valid alternate
    alias, is preferred and has no [via] query. A room-id permalink uses
    {!routing_candidates}, unless [via] is supplied explicitly. *)

val event_permalink :
  t -> ?via:string list -> Matrix_proto.Id.Event_id.t -> string
(** A matrix.to event permalink. It always uses the room ID because an alias may
    be repointed after an upgrade. *)

val direct_targets : t -> Matrix_proto.Id.User_id.t list
(** Users whose cached [m.direct] entry names this room. *)

val dm_target : t -> Matrix_proto.Id.User_id.t option
(** The sole direct target, or [None] when there are zero or several. *)

val set_is_direct : t -> bool -> (unit, Error.t) result
(** Mark the room for every joined member other than the account user, or remove
    it from every [m.direct] entry. One read and at most one write are made. *)

val mark_as_dm :
  t -> user_id:Matrix_proto.Id.User_id.t -> (unit, Error.t) result

val unmark_as_dm :
  t -> user_id:Matrix_proto.Id.User_id.t -> (unit, Error.t) result

type role = Creator | Administrator | Moderator | User

val suggested_role : t -> Matrix_proto.Id.User_id.t -> role
(** [Creator] is used for explicitly privileged creators in room version 12 and
    newer. Other roles use the cached power level thresholds 100 and 50. Missing
    power-level state gives {!User}. *)

type invite_details = {
  invite_event : Store.state_event;
  inviter : Matrix_proto.Id.User_id.t;
  inviter_profile : Store.state_event option;
}

val invite_details : t -> (invite_details, Error.t) result
(** The account user's cached invite member event, its sender, and that
    inviter's cached member profile when present. Non-invited rooms and
    incomplete invites are explicit local errors. *)
