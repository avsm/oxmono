(** Raising wrappers for {!Matrix_client.Thread_subscriptions}. *)

type status = Matrix_client.Thread_subscriptions.status = { automatic : bool }

type subscription = Matrix_client.Thread_subscriptions.subscription = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  automatic : bool;
  bump_stamp : int64;
}

type unsubscription = Matrix_client.Thread_subscriptions.unsubscription = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  bump_stamp : int64;
}

type page = Matrix_client.Thread_subscriptions.page = {
  subscribed : subscription list;
  unsubscribed : unsubscription list;
  end_token : string option;
}

type stored_status = Matrix_client.Thread_subscriptions.stored_status =
  | Manual
  | Automatic
  | Unsubscribed

type stored_subscription =
      Matrix_client.Thread_subscriptions.stored_subscription = {
  status : stored_status;
  bump_stamp : int64 option;
}

type update = Matrix_client.Thread_subscriptions.update = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  subscription : stored_subscription;
}

type catchup_token = Matrix_client.Thread_subscriptions.catchup_token = {
  from_ : string;
  to_ : string option;
}

val is_supported : Client.t -> bool
(** [is_supported] reports whether the homeserver advertises enabled MSC4306
    support in its cached [/versions] response. *)

val subscriptions :
  Matrix_client.Store.t ->
  (Matrix_proto.Id.Room_id.t * Matrix_proto.Id.Event_id.t * stored_subscription)
  list
(** [subscriptions] loads all persisted thread subscriptions. *)

val find_stored :
  Matrix_client.Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  stored_subscription option
(** [find_stored] loads one persisted subscription. *)

val merge :
  previous:stored_subscription option ->
  stored_subscription ->
  stored_subscription option
(** [merge] applies bump-stamp ordering without touching a store. *)

val upsert :
  Matrix_client.Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  stored_subscription ->
  unit
(** [upsert] persists one subscription. *)

val upsert_many : Matrix_client.Store.t -> update list -> unit
(** [upsert_many] persists a batch of subscription updates. *)

val remove :
  Matrix_client.Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  unit
(** [remove] removes one persisted subscription. *)

val remove_room :
  Matrix_client.Store.t -> room_id:Matrix_proto.Id.Room_id.t -> unit
(** [remove_room] removes all subscriptions for a room. *)

val catchup_tokens : Matrix_client.Store.t -> catchup_token list
(** [catchup_tokens] loads queued catch-up ranges. *)

val queue_catchup_token :
  Matrix_client.Store.t -> from_:string -> to_:string option -> unit
(** [queue_catchup_token] queues a deduplicated catch-up range. *)

val catch_up_once : Client.t -> store:Matrix_client.Store.t -> bool
(** [catch_up_once] processes the newest queued range. *)

val catch_up : Client.t -> store:Matrix_client.Store.t -> unit
(** [catch_up] drains all queued ranges, newest first. *)

val get :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  status option
(** [get] fetches the current subscription, or returns [None] for a Matrix
    [M_NOT_FOUND] response. *)

val subscribe :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  ?automatic:Matrix_proto.Id.Event_id.t ->
  unit ->
  unit
(** [subscribe] enables thread subscription, optionally recording the
    automatic-subscription event. *)

val unsubscribe :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  unit ->
  unit
(** [unsubscribe] disables thread subscription. *)

val changes :
  Client.t -> ?from:string -> ?to_:string -> ?limit:int -> unit -> page
(** [changes] fetches the backwards MSC4308 subscription-change page. *)

val subscribe_and_store :
  Client.t ->
  store:Matrix_client.Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  ?automatic:Matrix_proto.Id.Event_id.t ->
  unit ->
  unit
(** [subscribe_and_store] subscribes remotely and persists the acknowledgement.
*)

val unsubscribe_and_store :
  Client.t ->
  store:Matrix_client.Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  unit ->
  unit
(** [unsubscribe_and_store] unsubscribes remotely and persists the result. *)

val get_and_store :
  Client.t ->
  store:Matrix_client.Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  status option
(** [get_and_store] fetches and persists the current status. *)

val load_or_fetch :
  Client.t ->
  store:Matrix_client.Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  status option
(** [load_or_fetch] uses persisted status only after catch-up is drained. *)

val subscribe_if_needed :
  Client.t ->
  store:Matrix_client.Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  ?automatic:Matrix_proto.Id.Event_id.t ->
  unit ->
  unit
(** [subscribe_if_needed] skips duplicate requests and upgrades automatic
    subscriptions to manual ones. *)

val apply_sliding_extension :
  Matrix_client.Store.t ->
  previous_pos:string option ->
  Matrix_proto.Sliding_sync.Response.thread_subscriptions ->
  unit
(** [apply_sliding_extension] persists one typed MSC4308 sliding-sync delta and
    its optional catch-up range. *)
