type params = Matrix_client.Sync.params = {
  filter : string option;
  since : string option;
  full_state : bool;
  set_presence : [ `Online | `Offline | `Unavailable ] option;
  timeout : int;
}

let default_params = Matrix_client.Sync.default_params

type response = Matrix_proto.Sync.Response.t

let sync client ?(params = default_params) () =
  Error.unwrap ~context:"performing a classic Matrix sync request"
    (Matrix_client.Sync.sync_once (Client.base client) ~params ())

type action = Continue | Stop | Retry_after of float

type 'a callbacks = {
  on_response : 'a -> action;
  on_error : Error.err -> action;
}

let default_on_error _ = Retry_after 5.0

let callbacks ?(on_error = default_on_error) ~on_response () =
  { on_response; on_error }

let sync_forever ~sw ~clock client ?initial_since ?(params = default_params)
    ~callbacks () =
  let rec loop since =
    Eio.Fiber.check ();
    let params = { params with since } in
    let again since = function
      | Continue -> loop since
      | Stop -> ()
      | Retry_after delay ->
          Eio.Time.sleep clock delay;
          loop since
    in
    let result =
      Error.with_context "polling classic Matrix sync" (fun () ->
          Matrix_client.Sync.sync_once (Client.base client) ~params ())
    in
    match result with
    | Error e -> again since (callbacks.on_error (Error.of_client_error e))
    | Ok response ->
        again (Some response.next_batch) (callbacks.on_response response)
  in
  Eio.Fiber.fork ~sw (fun () -> loop initial_since)

let sync_to_stream ~sw ~clock client ~stream ?initial_since
    ?(params = default_params) ?on_error () =
  sync_forever ~sw ~clock client ?initial_since ~params
    ~callbacks:
      (callbacks ?on_error
         ~on_response:(fun response ->
           Eio.Stream.add stream response;
           Continue)
         ())
    ()

let create_sync_stream ~sw ~clock client ?(capacity = 10) ?initial_since
    ?(params = default_params) () =
  let stream = Eio.Stream.create capacity in
  sync_to_stream ~sw ~clock client ~stream ?initial_since ~params ();
  stream

let iter ~sw ~clock client ?initial_since ?(params = default_params) f =
  sync_forever ~sw ~clock client ?initial_since ~params
    ~callbacks:
      (callbacks
         ~on_response:(fun response ->
           f response;
           Continue)
         ())
    ()

module Filter = Matrix_client.Sync.Filter

type filter = Filter.t

let default_filter = Filter.default
let default_room_filter = Filter.default_room
let default_event_filter = Filter.default_event
let default_room_event_filter = Filter.default_room_event

let create_filter client ~filter =
  Error.unwrap ~context:"creating a Matrix sync filter"
    (Filter.create (Client.base client) ~filter)

let get_filter client ~filter_id =
  Error.unwrap ~context:"fetching a Matrix sync filter"
    (Filter.get (Client.base client) ~filter_id)
