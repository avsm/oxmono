let unstable_prefix = Matrix_client.Delayed_events.unstable_prefix

type delay_id = Matrix_client.Delayed_events.delay_id

let delay_id_of_string = Matrix_client.Delayed_events.delay_id_of_string
let delay_id_to_string = Matrix_client.Delayed_events.delay_id_to_string

type action = Matrix_client.Delayed_events.action = Send | Cancel | Restart

type delayed_event = Matrix_client.Delayed_events.delayed_event = {
  delay_id : delay_id;
  room_id : Matrix_proto.Id.Room_id.t;
  event_type : string;
  state_key : string option;
  content : Jsont.json;
  delay : int;
  running_since : Matrix_proto.Event.Timestamp.t;
  event_id : Matrix_proto.Id.Event_id.t option;
  finalised_ts : Matrix_proto.Event.Timestamp.t option;
  error : Matrix_client.Error.matrix_error option;
}

let send client ~room_id ~event_type ~content ~delay_ms ?txn_id () =
  Error.unwrap ~context:"sending delayed event"
    (Matrix_client.Delayed_events.send (Client.base client) ~room_id ~event_type
       ~content ~delay_ms ?txn_id ())

let send_state client ~room_id ~event_type ~state_key ~content ~delay_ms =
  Error.unwrap ~context:"sending delayed state event"
    (Matrix_client.Delayed_events.send_state (Client.base client) ~room_id
       ~event_type ~state_key ~content ~delay_ms)

let send_current client ~room_id ~event_type ~content ~delay_ms ?txn_id
    ?sticky_duration_ms ?state_key () =
  Error.unwrap ~context:"sending current delayed event"
    (Matrix_client.Delayed_events.send_current (Client.base client) ~room_id
       ~event_type ~content ~delay_ms ?txn_id ?sticky_duration_ms ?state_key ())

let send_state_current client ~room_id ~event_type ~state_key ~content ~delay_ms
    ?txn_id ?sticky_duration_ms () =
  Error.unwrap ~context:"sending current delayed state event"
    (Matrix_client.Delayed_events.send_state_current (Client.base client)
       ~room_id ~event_type ~state_key ~content ~delay_ms ?txn_id
       ?sticky_duration_ms ())

let update client ~delay_id ~action =
  Error.unwrap ~context:"updating delayed event"
    (Matrix_client.Delayed_events.update (Client.base client) ~delay_id ~action)

let send_now client ~delay_id =
  Error.unwrap ~context:"sending delayed event now"
    (Matrix_client.Delayed_events.send_now (Client.base client) ~delay_id)

let cancel client ~delay_id =
  Error.unwrap ~context:"cancelling delayed event"
    (Matrix_client.Delayed_events.cancel (Client.base client) ~delay_id)

let restart client ~delay_id =
  Error.unwrap ~context:"restarting delayed event"
    (Matrix_client.Delayed_events.restart (Client.base client) ~delay_id)

let update_current client ~delay_id ~action =
  Error.unwrap ~context:"updating current delayed event"
    (Matrix_client.Delayed_events.update_current (Client.base client) ~delay_id
       ~action)

let send_now_current client ~delay_id =
  Error.unwrap ~context:"sending current delayed event now"
    (Matrix_client.Delayed_events.send_now_current (Client.base client)
       ~delay_id)

let cancel_current client ~delay_id =
  Error.unwrap ~context:"cancelling current delayed event"
    (Matrix_client.Delayed_events.cancel_current (Client.base client) ~delay_id)

let restart_current client ~delay_id =
  Error.unwrap ~context:"restarting current delayed event"
    (Matrix_client.Delayed_events.restart_current (Client.base client) ~delay_id)

let list client ?from () =
  Error.unwrap ~context:"listing delayed events"
    (Matrix_client.Delayed_events.list (Client.base client) ?from ())

let get_current client ~delay_id =
  Error.unwrap ~context:"getting current delayed event"
    (Matrix_client.Delayed_events.get_current (Client.base client) ~delay_id)

let list_current client () =
  Error.unwrap ~context:"listing current delayed events"
    (Matrix_client.Delayed_events.list_current (Client.base client) ())

type status = Matrix_client.Delayed_events.status =
  | Scheduled
  | Sent
  | Failed
  | Cancelled

let status = Matrix_client.Delayed_events.status
