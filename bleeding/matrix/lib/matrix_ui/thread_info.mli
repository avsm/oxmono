(** Persisted, observable summaries of the threads known in a room.

    This is deliberately a summary cache, not a thread timeline. Thread replies
    are obtained from the shared {!Event_cache}; the cache stores the root and
    the server's bundled latest reply so a restart can render a list item before
    another thread-list request completes. *)

type summary = {
  reply_count : int;
  latest_reply_id : Matrix_proto.Id.Event_id.t option;
}

type summary_status = Unknown | Known_none | Known of summary

type info = {
  room_id : Matrix_proto.Id.Room_id.t;
  root : Matrix_proto.Event.Raw_event.t;
  latest_reply : Matrix_proto.Event.Raw_event.t option;
  reply_count : int;
  summary_status : summary_status;
  subscription : Matrix_client.Thread_subscriptions.stored_subscription option;
  public_read : Matrix_client.Read_state.receipt option;
  private_read : Matrix_client.Read_state.receipt option;
  latest_read : Matrix_proto.Id.Event_id.t option;
  unread : Matrix_client.Read_state.counts;
}

type t

val create :
  ?store:Matrix_client.Store.t -> user_id:Matrix_proto.Id.User_id.t -> unit -> t
(** [create ~user_id ?store ()] restores summaries from the base store. *)

val infos : t -> Matrix_proto.Id.Room_id.t -> info Observable.List.t
(** [infos t room] is the observable list of summaries, in deterministic
    event-id order. *)

val snapshot : t -> Matrix_proto.Id.Room_id.t -> info array

val subscribe :
  t -> Matrix_proto.Id.Room_id.t -> (info array -> unit) -> unit -> unit
(** [subscribe t room callback] invokes [callback] with the current room
    snapshot and after every aggregate update. The returned function detaches
    the listener and is idempotent. Non-cancellation exceptions raised by later
    callbacks are contained and logged; [Eio.Cancel.Cancelled] propagates. If
    the initial callback raises, the listener is not retained. *)

val summary_of_root :
  Matrix_proto.Event.Raw_event.t ->
  summary_status * Matrix_proto.Event.Raw_event.t option
(** Decode [unsigned.m.relations.m.thread]. Malformed extension data is
    [Unknown] and does not reject the enclosing event. *)

val ingest_root :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Event.Raw_event.t ->
  unit
(** [ingest_root] remembers or updates one root, including its bundled summary.
*)

val refresh_room :
  t ->
  state:Matrix_client.Base_client.state ->
  room_id:Matrix_proto.Id.Room_id.t ->
  events:Matrix_proto.Event.Raw_event.t list ->
  unit
(** Recompute the observable fields that can be established from the bounded
    shared-room event window and the persisted read state. *)

val remove_room : t -> Matrix_proto.Id.Room_id.t -> unit
(** Remove persisted and observable state for one forgotten room. *)
