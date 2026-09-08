(** Raising wrappers for {!Matrix_client.Notification_settings}. *)

type room_notification_mode =
      Matrix_client.Notification_settings.room_notification_mode =
  | All_messages
  | Mentions_and_keywords_only
  | Mute

type t = Matrix_client.Notification_settings.t
type subscription = Matrix_client.Notification_settings.subscription

val create : ?ruleset:Matrix_proto.Push.Ruleset.t -> Client.t -> t
val client : t -> Matrix_client.Client.t
val ruleset : t -> Matrix_proto.Push.Ruleset.t
val refresh : t -> unit

val user_defined_room_mode :
  t -> Matrix_proto.Id.Room_id.t -> room_notification_mode option

val default_room_mode :
  t -> encrypted:bool -> one_to_one:bool -> room_notification_mode

val room_mode :
  t ->
  Matrix_proto.Id.Room_id.t ->
  encrypted:bool ->
  one_to_one:bool ->
  room_notification_mode

val rooms_with_user_defined_rules : ?enabled:bool -> t -> string list

val set_room_mode :
  t -> Matrix_proto.Id.Room_id.t -> room_notification_mode -> unit

val delete_room_mode : t -> Matrix_proto.Id.Room_id.t -> unit

val unmute_room :
  t -> Matrix_proto.Id.Room_id.t -> encrypted:bool -> one_to_one:bool -> unit

val set_default_room_mode :
  t -> encrypted:bool -> one_to_one:bool -> room_notification_mode -> unit

val contains_keyword_rules : t -> bool
val enabled_keywords : t -> string list
val add_keyword : t -> string -> unit
val remove_keyword : t -> string -> unit
val is_enabled : t -> Matrix_proto.Push.Rule_id.t -> bool
val set_enabled : t -> Matrix_proto.Push.Rule_id.t -> enabled:bool -> unit

val set_actions :
  t ->
  Matrix_proto.Push.Rule_id.t ->
  actions:Matrix_proto.Push.Action.t list ->
  unit

val subscribe : t -> (Matrix_proto.Push.Ruleset.t -> unit) -> subscription
val unsubscribe : t -> subscription -> unit
