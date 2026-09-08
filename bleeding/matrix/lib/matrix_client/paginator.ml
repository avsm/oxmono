module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Direction = Matrix_proto.Common.Direction
module Json = Matrix_proto.Json

type state = Initial | Fetching_target | Idle | Paginating
type pagination_token = Not_started | Has_more of string | Hit_end
type tokens = { previous : pagination_token; next : pagination_token }
type thread_mode = Automatic | Force

type error =
  | Event_not_found of Id.Event_id.t
  | Invalid_state of { expected : state; actual : state }
  | Client_error of Error.t

let pp_state ppf = function
  | Initial -> Format.pp_print_string ppf "initial"
  | Fetching_target -> Format.pp_print_string ppf "fetching target"
  | Idle -> Format.pp_print_string ppf "idle"
  | Paginating -> Format.pp_print_string ppf "paginating"

let pp_error ppf = function
  | Event_not_found event_id ->
      Format.fprintf ppf "target event %s was not found"
        (Id.Event_id.to_string event_id)
  | Invalid_state { expected; actual } ->
      Format.fprintf ppf "expected paginator state %a, observed %a" pp_state
        expected pp_state actual
  | Client_error error -> Error.pp ppf error

type start_result = {
  events : Event.Raw_event.t list;
  has_previous : bool;
  has_next : bool;
}

type page = { events : Event.Raw_event.t list; hit_end : bool }

type t = {
  client : Client.t;
  room_id : Id.Room_id.t;
  mutable state : state;
  mutable previous : pagination_token;
  mutable next : pagination_token;
  mutable next_subscription : int;
  subscribers : (int, state -> unit) Hashtbl.t;
  seen : (string, unit) Hashtbl.t;
  thread_mode : thread_mode;
  mutable thread_root : Id.Event_id.t option;
  mutable hide_thread_events : bool;
}

let create ~client ~room_id ?(thread_mode = Automatic) () =
  {
    client;
    room_id;
    state = Initial;
    previous = Not_started;
    next = Not_started;
    next_subscription = 0;
    subscribers = Hashtbl.create 4;
    seen = Hashtbl.create 128;
    thread_mode;
    thread_root = None;
    hide_thread_events = false;
  }

let state t = t.state
let tokens t = { previous = t.previous; next = t.next }
let thread_root t = t.thread_root

let publish t state =
  if t.state <> state then begin
    t.state <- state;
    (* Snapshot the callbacks so an unsubscribe from inside a callback is safe
       and affects only later publications. *)
    Hashtbl.to_seq_values t.subscribers
    |> List.of_seq
    |> List.iter (fun callback -> callback state)
  end

let subscribe t callback =
  let id = t.next_subscription in
  t.next_subscription <- id + 1;
  Hashtbl.replace t.subscribers id callback;
  callback t.state;
  let subscribed = ref true in
  fun () ->
    if !subscribed then begin
      subscribed := false;
      Hashtbl.remove t.subscribers id
    end

let invalid_state ~expected actual = Error (Invalid_state { expected; actual })

let require_state t expected =
  if t.state = expected then Ok () else invalid_state ~expected t.state

let token_of_context = function None -> Hit_end | Some token -> Has_more token
let token_for_request = function Has_more token -> Some token | _ -> None

let event_key (event : Event.Raw_event.t) =
  Option.map Id.Event_id.to_string event.event_id

let deduplicate t events =
  List.filter
    (fun event ->
      match event_key event with
      | None -> false
      | Some key ->
          if Hashtbl.mem t.seen key then false
          else (
            Hashtbl.replace t.seen key ();
            true))
    events

let client_error event_id = function
  | Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ } ->
      Event_not_found event_id
  | error -> Client_error error

let thread_relation_root (event : Event.Raw_event.t) =
  match Json.find_mem "m.relates_to" event.content with
  | None -> None
  | Some relates_to -> (
      match
        ( Json.find_string "rel_type" relates_to,
          Json.find_string "event_id" relates_to )
      with
      | Some "m.thread", Some root ->
          Id.Event_id.of_string root |> Result.to_option
      | _ -> None)

let event_is_root root (event : Event.Raw_event.t) =
  Option.equal Id.Event_id.equal event.event_id (Some root)

let event_is_in_thread root event =
  event_is_root root event
  || Option.equal Id.Event_id.equal (thread_relation_root event) (Some root)

let start_from t ~event_id ?(limit = 10) () =
  if limit < 0 then invalid_arg "Matrix_client.Paginator.start_from";
  match require_state t Initial with
  | Error _ as error -> error
  | Ok () -> (
      publish t Fetching_target;
      let answer =
        try Messages.get_context t.client ~room_id:t.room_id ~event_id ~limit ()
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          publish t Initial;
          Printexc.raise_with_backtrace exn bt
      in
      match answer with
      | Error error ->
          publish t Initial;
          Error (client_error event_id error)
      | Ok context ->
          if
            not
              (Option.equal Id.Event_id.equal context.event.event_id
                 (Some event_id))
          then (
            publish t Initial;
            Error (Event_not_found event_id))
          else (
            Hashtbl.clear t.seen;
            let all_events =
              List.rev context.events_before
              @ (context.event :: context.events_after)
            in
            let thread_root =
              match t.thread_mode with
              | Automatic -> thread_relation_root context.event
              | Force ->
                  Some
                    (Option.value
                       (thread_relation_root context.event)
                       ~default:event_id)
            in
            t.thread_root <- thread_root;
            t.hide_thread_events <- false;
            (match t.thread_mode with
            | Automatic when Option.is_none (thread_relation_root context.event)
              ->
                t.thread_root <- None;
                t.hide_thread_events <- true
            | _ -> ());
            let events, previous, has_previous =
              match thread_root with
              | None ->
                  ( deduplicate t
                      (if t.hide_thread_events then
                         List.filter
                           (fun (event : Event.Raw_event.t) ->
                             Option.is_none (thread_relation_root event))
                           all_events
                       else all_events),
                    token_of_context context.prev_batch,
                    Option.is_some context.prev_batch )
              | Some root ->
                  let includes_root =
                    List.exists (event_is_root root) all_events
                  in
                  let filtered =
                    List.filter
                      (fun (event : Event.Raw_event.t) ->
                        Option.is_some event.event_id
                        && event_is_in_thread root event)
                      all_events
                  in
                  ( deduplicate t filtered,
                    (if includes_root then Hit_end
                     else token_of_context context.prev_batch),
                    (not includes_root) && Option.is_some context.prev_batch )
            in
            t.previous <- previous;
            t.next <- token_of_context context.next_batch;
            publish t Idle;
            Ok
              {
                events;
                has_previous;
                has_next = Option.is_some context.next_batch;
              }))

let paginate t direction limit =
  if limit < 0 then invalid_arg "Matrix_client.Paginator.paginate";
  match require_state t Idle with
  | Error _ as error -> error
  | Ok () -> (
      let current =
        match direction with
        | Direction.Backward -> t.previous
        | Direction.Forward -> t.next
      in
      match current with
      | Hit_end -> Ok { events = []; hit_end = true }
      | Not_started | Has_more _ -> (
          publish t Paginating;
          let answer =
            try
              match t.thread_root with
              | None ->
                  `Messages
                    (Messages.get_messages t.client ~room_id:t.room_id
                       ?from:(token_for_request current)
                       ~dir:direction ~limit ())
              | Some thread_root ->
                  `Relations
                    (Relations.get_raw_relations t.client ~room_id:t.room_id
                       ~event_id:thread_root
                       ?from:(token_for_request current)
                       ~dir:direction ~recurse:true ~limit ())
            with exn ->
              let bt = Printexc.get_raw_backtrace () in
              publish t Idle;
              Printexc.raise_with_backtrace exn bt
          in
          match answer with
          | `Messages (Error error) ->
              publish t Idle;
              Error (Client_error error)
          | `Messages (Ok response) ->
              let token =
                match response.page.next_batch with
                | None -> Hit_end
                | Some token -> Has_more token
              in
              (match direction with
              | Direction.Backward -> t.previous <- token
              | Direction.Forward -> t.next <- token);
              publish t Idle;
              let events =
                if t.hide_thread_events then
                  List.filter
                    (fun event -> Option.is_none (thread_relation_root event))
                    response.page.chunk
                else response.page.chunk
              in
              Ok { events = deduplicate t events; hit_end = token = Hit_end }
          | `Relations (Error error) ->
              publish t Idle;
              Error (Client_error error)
          | `Relations (Ok response) -> (
              let root_result =
                match direction with
                | Direction.Backward when Option.is_none response.next_batch
                  -> (
                    try
                      Result.map Option.some
                        (Messages.get_event t.client ~room_id:t.room_id
                           ~event_id:(Option.get t.thread_root))
                    with exn ->
                      let bt = Printexc.get_raw_backtrace () in
                      publish t Idle;
                      Printexc.raise_with_backtrace exn bt)
                | _ -> Ok None
              in
              match root_result with
              | Error error ->
                  (* Keep the relations token retryable when loading the root
                     fails. *)
                  publish t Idle;
                  Error (Client_error error)
              | Ok root ->
                  let token =
                    match response.next_batch with
                    | None -> Hit_end
                    | Some token -> Has_more token
                  in
                  let events =
                    List.filter
                      (fun (event : Event.Raw_event.t) ->
                        Option.is_some event.event_id)
                      (response.chunk @ Option.to_list root)
                  in
                  (match direction with
                  | Direction.Backward -> t.previous <- token
                  | Direction.Forward -> t.next <- token);
                  publish t Idle;
                  Ok
                    { events = deduplicate t events; hit_end = token = Hit_end }
              )))

let paginate_backward t ?(limit = 30) () = paginate t Direction.Backward limit
let paginate_forward t ?(limit = 30) () = paginate t Direction.Forward limit

let reset t =
  match t.state with
  | Fetching_target | Paginating -> invalid_state ~expected:Idle t.state
  | Initial | Idle ->
      t.previous <- Not_started;
      t.next <- Not_started;
      t.thread_root <- None;
      t.hide_thread_events <- false;
      Hashtbl.clear t.seen;
      publish t Initial;
      Ok ()
