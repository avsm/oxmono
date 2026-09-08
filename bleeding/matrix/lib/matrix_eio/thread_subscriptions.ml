module Core = Matrix_client.Thread_subscriptions

let unwrap context fn =
  Error.with_context context (fun () -> Error.unwrap (fn ()))

type status = Core.status = { automatic : bool }

type subscription = Core.subscription = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  automatic : bool;
  bump_stamp : int64;
}

type unsubscription = Core.unsubscription = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  bump_stamp : int64;
}

type page = Core.page = {
  subscribed : subscription list;
  unsubscribed : unsubscription list;
  end_token : string option;
}

type stored_status = Core.stored_status = Manual | Automatic | Unsubscribed

type stored_subscription = Core.stored_subscription = {
  status : stored_status;
  bump_stamp : int64 option;
}

type update = Core.update = {
  room_id : Matrix_proto.Id.Room_id.t;
  thread_root : Matrix_proto.Id.Event_id.t;
  subscription : stored_subscription;
}

type catchup_token = Core.catchup_token = {
  from_ : string;
  to_ : string option;
}

let is_supported client =
  unwrap "probing Matrix thread subscriptions" (fun () ->
      Core.is_supported (Client.base client))

let subscriptions store =
  unwrap "reading stored thread subscriptions" (fun () ->
      Core.subscriptions store)

let find_stored store ~room_id ~thread_root =
  unwrap "reading a stored thread subscription" (fun () ->
      Core.find_stored store ~room_id ~thread_root)

let merge = Core.merge

let upsert store ~room_id ~thread_root subscription =
  unwrap "storing a thread subscription" (fun () ->
      Core.upsert store ~room_id ~thread_root subscription)

let upsert_many store updates =
  unwrap "storing thread subscriptions" (fun () ->
      Core.upsert_many store updates)

let remove store ~room_id ~thread_root =
  unwrap "removing a stored thread subscription" (fun () ->
      Core.remove store ~room_id ~thread_root)

let remove_room store ~room_id =
  unwrap "removing a room's stored thread subscriptions" (fun () ->
      Core.remove_room store ~room_id)

let catchup_tokens store =
  unwrap "reading thread-subscription catch-up tokens" (fun () ->
      Core.catchup_tokens store)

let queue_catchup_token store ~from_ ~to_ =
  unwrap "storing a thread-subscription catch-up token" (fun () ->
      Core.queue_catchup_token store ~from_ ~to_)

let catch_up_once client ~store =
  unwrap "catching up thread subscriptions" (fun () ->
      Core.catch_up_once (Client.base client) ~store)

let catch_up client ~store =
  unwrap "catching up thread subscriptions" (fun () ->
      Core.catch_up (Client.base client) ~store)

let get client ~room_id ~thread_root =
  unwrap "fetching a thread subscription" (fun () ->
      Core.get (Client.base client) ~room_id ~thread_root)

let subscribe client ~room_id ~thread_root ?automatic () =
  unwrap "subscribing to a Matrix thread" (fun () ->
      Core.subscribe (Client.base client) ~room_id ~thread_root ?automatic ())

let unsubscribe client ~room_id ~thread_root () =
  unwrap "unsubscribing from a Matrix thread" (fun () ->
      Core.unsubscribe (Client.base client) ~room_id ~thread_root ())

let changes client ?from ?to_ ?limit () =
  unwrap "fetching thread-subscription changes" (fun () ->
      Core.changes (Client.base client) ?from ?to_ ?limit ())

let subscribe_and_store client ~store ~room_id ~thread_root ?automatic () =
  unwrap "subscribing to and storing a Matrix thread" (fun () ->
      Core.subscribe_and_store (Client.base client) ~store ~room_id ~thread_root
        ?automatic ())

let unsubscribe_and_store client ~store ~room_id ~thread_root () =
  unwrap "unsubscribing from and storing a Matrix thread" (fun () ->
      Core.unsubscribe_and_store (Client.base client) ~store ~room_id
        ~thread_root ())

let get_and_store client ~store ~room_id ~thread_root =
  unwrap "fetching and storing a thread subscription" (fun () ->
      Core.get_and_store (Client.base client) ~store ~room_id ~thread_root)

let load_or_fetch client ~store ~room_id ~thread_root =
  unwrap "loading or fetching a thread subscription" (fun () ->
      Core.load_or_fetch (Client.base client) ~store ~room_id ~thread_root)

let subscribe_if_needed client ~store ~room_id ~thread_root ?automatic () =
  unwrap "ensuring a Matrix thread subscription" (fun () ->
      Core.subscribe_if_needed (Client.base client) ~store ~room_id ~thread_root
        ?automatic ())

let apply_sliding_extension store ~previous_pos extension =
  unwrap "applying thread subscriptions from sliding sync" (fun () ->
      Core.apply_sliding_extension store ~previous_pos extension)
