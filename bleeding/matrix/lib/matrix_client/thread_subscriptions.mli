(** MSC4306/MSC4308 thread-subscription endpoints. *)

type status = { automatic : bool }
(** The current subscription, when one exists. *)

type subscription = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  automatic : bool;
  bump_stamp : int64;
}
(** A subscription change returned by the companion endpoint. *)

type unsubscription = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  bump_stamp : int64;
}
(** An unsubscription change returned by the companion endpoint. *)

type page = {
  subscribed : subscription list;
  unsubscribed : unsubscription list;
  end_token : string option;
}
(** A page of subscription changes. [end_token] is the opaque token for the next
    page, or [None] when this is the end. *)

type stored_status =
  | Manual
  | Automatic
  | Unsubscribed  (** The persisted status of a thread subscription. *)

type stored_subscription = { status : stored_status; bump_stamp : int64 option }
(** A persisted subscription. Unstamped values are local acknowledgements. *)

type update = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  subscription : stored_subscription;
}
(** A room/thread subscription update to merge into the store. *)

type catchup_token = { from_ : string; to_ : string option }
(** A persisted MSC4308 pagination range. *)

val is_supported : Client.t -> (bool, Error.t) result
(** [is_supported t] checks the cached [/versions] response for the enabled
    [org.matrix.msc4306] unstable feature. It does not gate endpoint calls. *)

val get :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  (status option, Error.t) result
(** [get] fetches the current status with the unstable MSC4306 endpoint. A
    server [M_NOT_FOUND] response is returned as [Ok None]. *)

val subscribe :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  ?automatic:Matrix_proto.Id.Event_id.t ->
  unit ->
  (unit, Error.t) result
(** [subscribe] sends [PUT .../subscription]. [automatic], when present, is the
    latest event that caused an automatic subscription and is sent as the JSON
    [automatic] member. *)

val unsubscribe :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  unit ->
  (unit, Error.t) result
(** [unsubscribe] sends [DELETE .../subscription]. *)

val changes :
  Client.t ->
  ?from:string ->
  ?to_:string ->
  ?limit:int ->
  unit ->
  (page, Error.t) result
(** [changes] fetches a page from the unstable MSC4308 companion endpoint,
    always using the backwards direction ([dir=b]). [from] and [to_] are opaque
    pagination tokens; [limit] is the optional non-negative, JavaScript-safe
    maximum number of changes. *)

val subscriptions :
  Store.t ->
  ( (Matrix_proto.Id.Room_id.t
    * Matrix_proto.Id.Event_id.t
    * stored_subscription)
    list,
    Error.t )
  result
(** [subscriptions store] loads all persisted entries in deterministic order. *)

val find_stored :
  Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  (stored_subscription option, Error.t) result
(** [find_stored] loads one persisted room/thread entry. *)

val merge :
  previous:stored_subscription option ->
  stored_subscription ->
  stored_subscription option
(** [merge ~previous next] applies Rust-compatible bump-stamp ordering. [None]
    means that a stamped update was older than or equal to [previous]. *)

val upsert :
  Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  stored_subscription ->
  (unit, Error.t) result
(** [upsert] merges and persists one subscription entry. *)

val upsert_many : Store.t -> update list -> (unit, Error.t) result
(** [upsert_many] merges and persists a batch. Newer stamped updates win;
    unstamped acknowledgements retain a previous stamp. *)

val remove :
  Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  (unit, Error.t) result
(** [remove] removes one persisted room/thread entry. *)

val remove_room :
  Store.t -> room_id:Matrix_proto.Id.Room_id.t -> (unit, Error.t) result
(** [remove_room] removes all persisted subscriptions for a room. *)

val catchup_tokens : Store.t -> (catchup_token list, Error.t) result
(** [catchup_tokens] returns deduplicated ranges in insertion order. *)

val queue_catchup_token :
  Store.t -> from_:string -> to_:string option -> (unit, Error.t) result
(** [queue_catchup_token] appends a range unless it is already queued. *)

val catch_up_once : Client.t -> store:Store.t -> (bool, Error.t) result
(** [catch_up_once] processes the newest queued range. Subscription updates are
    flushed before the range is advanced, so a failed write leaves it retryable.
    The result is [false] when no range was queued. *)

val catch_up : Client.t -> store:Store.t -> (unit, Error.t) result
(** [catch_up] drains all queued ranges, newest first. *)

val subscribe_and_store :
  Client.t ->
  store:Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  ?automatic:Matrix_proto.Id.Event_id.t ->
  unit ->
  (unit, Error.t) result
(** [subscribe_and_store] performs the network subscription and persists its
    unstamped manual or automatic acknowledgement. *)

val unsubscribe_and_store :
  Client.t ->
  store:Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  unit ->
  (unit, Error.t) result
(** [unsubscribe_and_store] performs and persists an unstamped unsubscribe. *)

val get_and_store :
  Client.t ->
  store:Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  (status option, Error.t) result
(** [get_and_store] fetches and persists the server status, or removes a
    previously persisted entry when the server reports [M_NOT_FOUND]. *)

val load_or_fetch :
  Client.t ->
  store:Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  (status option, Error.t) result
(** [load_or_fetch] trusts persisted status only when catch-up is fully drained;
    otherwise it fetches and persists the current server status. *)

val subscribe_if_needed :
  Client.t ->
  store:Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root:Matrix_proto.Id.Event_id.t ->
  ?automatic:Matrix_proto.Id.Event_id.t ->
  unit ->
  (unit, Error.t) result
(** [subscribe_if_needed] skips duplicate subscriptions, sends a manual request
    to upgrade an automatic subscription, and treats an automatic
    [M_CONFLICTING_UNSUBSCRIPTION] response as a successful no-op. *)

val apply_sliding_extension :
  Store.t ->
  previous_pos:string option ->
  Matrix_proto.Sliding_sync.Response.thread_subscriptions ->
  (unit, Error.t) result
(** [apply_sliding_extension store ~previous_pos extension] persists typed
    MSC4308 sliding-sync changes. A [prev_batch] queues the exact
    [{from = prev_batch; to = previous_pos}] catch-up range before changes are
    applied, following the pinned Rust ordering. The caller must complete this
    operation before accepting the response's new sliding-sync position. *)
