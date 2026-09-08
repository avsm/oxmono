(** push_evaluator — evaluating push rules locally.

    The homeserver decides what to deliver to a pusher, but a client that wants
    unread and highlight counts of its own, for an encrypted room whose content
    the server cannot read or between syncs, runs the rules itself. The rules
    and the actions they produce are {!Matrix_proto.Push}. {!Base_client} uses
    what is here to keep the local counterparts of the server's
    [unread_notifications].

    @see <https://spec.matrix.org/v1.11/client-server-api/#push-rules>
      Push Rules *)

(** {1 Evaluation context} *)

module Power_levels : sig
  (** The parts of an [m.room.power_levels] event a push rule consults. *)

  type t = {
    users : (Matrix_proto.Id.User_id.t * int) list;
        (** The level of each user the event names. *)
    users_default : int;  (** The level of a user the event does not name. *)
    notifications : (string * int) list;
        (** The level required to notify a room, keyed by the notification name.
            A name the list omits requires 50. *)
  }
  (** The type for power levels. *)

  val default : t
  (** [default] names no user and no notification, and gives every user level 0.
  *)

  val of_json : Jsont.json -> t
  (** [of_json content] reads an [m.room.power_levels] content. A member
      [content] omits, or holds something other than a whole number, takes its
      value from {!default}. *)
end

module Context : sig
  (** What evaluating a rule needs to know beyond the event itself. *)

  type t
  (** The type for evaluation contexts. *)

  val v :
    user_id:Matrix_proto.Id.User_id.t ->
    room_id:Matrix_proto.Id.Room_id.t ->
    ?display_name:string ->
    ?member_count:int ->
    ?power_levels:Power_levels.t ->
    unit ->
    t
  (** [v ~user_id ~room_id ()] is the context for rules run on behalf of
      [user_id] in [room_id]. [display_name] is that user's display name in the
      room and defaults to the localpart of [user_id]. [member_count] is the
      number of joined members and defaults to 0. [power_levels] defaults to
      absent, which makes a [sender_notification_permission] condition apply to
      no event. *)

  val user_id : t -> Matrix_proto.Id.User_id.t
  (** [user_id t] is the user the rules run for. *)

  val room_id : t -> Matrix_proto.Id.Room_id.t
  (** [room_id t] is the room the events come from. *)

  val display_name : t -> string
  (** [display_name t] is the display name a [contains_display_name] condition
      looks for. *)

  val member_count : t -> int
  (** [member_count t] is the number of joined members a [room_member_count]
      condition is tested against. *)

  val power_levels : t -> Power_levels.t option
  (** [power_levels t] is the room's power levels as last seen. *)
end

(** {1 Evaluation} *)

val find_matching_rule :
  Matrix_proto.Push.Ruleset.t ->
  Context.t ->
  Matrix_proto.Event.Raw_event.t ->
  Matrix_proto.Push.Rule.t option
(** [find_matching_rule rules ctx event] is the first enabled rule of [rules]
    that applies to [event], scanning the kinds in the order
    {!Matrix_proto.Push.Kind.all} gives and each kind's rules in list order. An
    event the user of [ctx] sent themselves matches no rule. *)

val evaluate :
  Matrix_proto.Push.Ruleset.t ->
  Context.t ->
  Matrix_proto.Event.Raw_event.t ->
  Matrix_proto.Push.Action.t list
(** [evaluate rules ctx event] is the actions of {!find_matching_rule}, and the
    empty list when no rule matches. *)

type notification = {
  notify : bool;
  highlight : bool;  (** The [highlight] tweak. *)
  sound : string option;  (** The [sound] tweak, absent for a silent event. *)
}
(** The type for what a client acts on once the rules have run. *)

val no_notification : notification
(** [no_notification] notifies nothing, highlights nothing and plays nothing. *)

val notification_of_actions : Matrix_proto.Push.Action.t list -> notification
(** [notification_of_actions actions] is [actions] reduced to what a client acts
    on. A [sound] tweak whose value is the empty string leaves the sound absent,
    which is how a rule asks for silence. *)

val notification_for_event :
  Matrix_proto.Push.Ruleset.t ->
  Context.t ->
  Matrix_proto.Event.Raw_event.t ->
  notification
(** [notification_for_event rules ctx event] is
    [notification_of_actions (evaluate rules ctx event)]. *)
