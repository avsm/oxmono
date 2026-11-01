(** Typing indicators for messages and edits.

    Endpoint failures are returned as {!Error.t}. Cancellation propagates.
    [Start] indicators expire on the server unless refreshed. *)

type op =
  | Start
  | Stop  (** The type for starting or stopping a typing indicator. *)

val set_dm :
  Client.t -> op:op -> user_ids:Zulip.Id.User.t list -> (unit, Error.t) result
(** [set_dm client ~op ~user_ids] applies [op] to the direct conversation with
    [user_ids]. *)

val set_channel :
  Client.t ->
  op:op ->
  channel_id:Zulip.Id.Channel.t ->
  topic:string ->
  (unit, Error.t) result
(** [set_channel client ~op ~channel_id ~topic] applies [op] to typing in
    [topic] within [channel_id]. *)

val set_edit :
  Client.t -> op:op -> message_id:Zulip.Id.Message.t -> (unit, Error.t) result
(** [set_edit client ~op ~message_id] applies [op] to editing [message_id]. *)

val set :
  Client.t ->
  op:op ->
  to_:
    [ `User_ids of Zulip.Id.User.t list
    | `Channel of Zulip.Id.Channel.t * string
    | `Message_edit of Zulip.Id.Message.t ] ->
  (unit, Error.t) result
(** [set client ~op ~to_] applies [op] to the direct conversation, channel topic
    or message edit selected by [to_]. *)
