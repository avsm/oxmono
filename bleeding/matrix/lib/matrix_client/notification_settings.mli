(** High-level, locally coherent push-notification settings.

    The service follows the command model in matrix-rust-sdk: it computes the
    complete ordered request batch from one ruleset snapshot, runs it, and only
    then publishes the corresponding local ruleset. If a later request fails,
    earlier server mutations cannot be rolled back, but subscribers never see a
    local state which claims the whole batch succeeded. A later {!refresh}
    reconciles any such partial server update. *)

type room_notification_mode = All_messages | Mentions_and_keywords_only | Mute
type t

val create : ?ruleset:Matrix_proto.Push.Ruleset.t -> Client.t -> t
val client : t -> Client.t
val ruleset : t -> Matrix_proto.Push.Ruleset.t

val refresh : t -> (unit, Error.t) result
(** Fetch and publish the complete global ruleset. A failed fetch changes
    nothing. *)

val user_defined_room_mode :
  t -> Matrix_proto.Id.Room_id.t -> room_notification_mode option
(** The explicit mode selected by custom rules, or [None] when the room uses its
    default underride rule. A silent matching override takes precedence over a
    room rule. *)

val default_room_mode :
  t -> encrypted:bool -> one_to_one:bool -> room_notification_mode
(** Inspect the appropriate predefined underride rule. It is {!All_messages}
    only when that rule exists, is enabled and notifies; otherwise it is
    {!Mentions_and_keywords_only}. *)

val room_mode :
  t ->
  Matrix_proto.Id.Room_id.t ->
  encrypted:bool ->
  one_to_one:bool ->
  room_notification_mode
(** The user-defined mode when present, and {!default_room_mode} otherwise. *)

val rooms_with_user_defined_rules : ?enabled:bool -> t -> string list
(** Distinct room-id spellings named by custom override, room or underride
    rules, in ruleset order. [enabled] restricts the result when supplied. *)

val set_room_mode :
  t ->
  Matrix_proto.Id.Room_id.t ->
  room_notification_mode ->
  (unit, Error.t) result
(** Insert the canonical room-id rule for the mode, then remove every other
    custom rule matching the room. Setting the already selected mode performs no
    request. *)

val delete_room_mode : t -> Matrix_proto.Id.Room_id.t -> (unit, Error.t) result
(** Delete every custom override, room and underride rule matching the room. It
    is a no-op when there are none. *)

val unmute_room :
  t ->
  Matrix_proto.Id.Room_id.t ->
  encrypted:bool ->
  one_to_one:bool ->
  (unit, Error.t) result
(** Leave an already unmuted room alone. An explicit mute is removed when the
    default mode notifies; a room with no explicit mode gets an all-messages
    rule so it is definitely unmuted. *)

val set_default_room_mode :
  t ->
  encrypted:bool ->
  one_to_one:bool ->
  room_notification_mode ->
  (unit, Error.t) result
(** Set and enable the selected predefined underride rule. When the matching
    unstable poll-start rule exists it is updated in the same local batch.
    {!Mute} has the same default-rule representation as
    {!Mentions_and_keywords_only}, matching the pinned Rust SDK. *)

val contains_keyword_rules : t -> bool
val enabled_keywords : t -> string list

val add_keyword : t -> string -> (unit, Error.t) result
(** Add a notifying content rule, enable one existing disabled rule, or do
    nothing when an enabled rule already has that exact pattern. *)

val remove_keyword : t -> string -> (unit, Error.t) result
(** Delete every custom content rule with the exact pattern. *)

val is_enabled : t -> Matrix_proto.Push.Rule_id.t -> (bool, Error.t) result

val set_enabled :
  t -> Matrix_proto.Push.Rule_id.t -> enabled:bool -> (unit, Error.t) result
(** Set a rule's enabled flag. The stable user/room mention rule also updates
    any corresponding legacy rules which are present. *)

val set_actions :
  t ->
  Matrix_proto.Push.Rule_id.t ->
  actions:Matrix_proto.Push.Action.t list ->
  (unit, Error.t) result

type subscription

val subscribe : t -> (Matrix_proto.Push.Ruleset.t -> unit) -> subscription
(** Observe successful refreshes and local command batches. The initial value is
    available through {!ruleset} and is not emitted automatically. *)

val unsubscribe : t -> subscription -> unit
