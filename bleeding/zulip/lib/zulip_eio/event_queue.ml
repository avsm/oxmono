let ( let* ) = Result.bind

module Narrow = struct
  type t = string * string

  let channel name = ("stream", name)
  let topic name = ("topic", name)
  let sender id = ("sender", string_of_int (Zulip.Id.User.to_int id))
  let direct = ("is", "private")
  let mentioned = ("is", "mentioned")

  let jsont =
    Jsont.map ~kind:"Event queue narrow"
      ~dec:(function
        | [ operator; operand ] -> (operator, operand)
        | _ ->
            Jsont.Error.msgf Jsont.Meta.none "Expected an operator/operand pair")
      ~enc:(fun (operator, operand) -> [ operator; operand ])
      (Jsont.list Jsont.string)
end

type subscribers = [ `None | `All | `Partial ]
type idle_timeout = Seconds of int | Mobile

type registration_options = {
  apply_markdown : bool;
  client_gravatar : bool;
  include_subscribers : subscribers;
  slim_presence : bool;
  presence_history_limit_days : int option;
  client_capabilities : (string * bool) list;
  fetch_event_types : Zulip.Event_type.t list option;
  idle_queue_timeout : idle_timeout option;
}

let default_registration =
  {
    apply_markdown = false;
    client_gravatar = false;
    include_subscribers = `None;
    slim_presence = true;
    presence_history_limit_days = None;
    client_capabilities = [];
    fetch_event_types = None;
    idle_queue_timeout = None;
  }

type t = {
  id : string;
  mutable last_event_id : int;
  timeout : float;
  initial_state : Jsont.json;
  mutable polling : bool;
}

let id t = t.id
let last_event_id t = t.last_event_id
let longpoll_timeout t = t.timeout
let initial_state t = t.initial_state
let state t = Initial_state.of_json t.initial_state |> Error.or_raise

let cursor_jsont =
  let valid n =
    Float.is_finite n && Float.is_integer n && n >= -1.
    && n <= 9_007_199_254_740_991.
  in
  Jsont.map ~kind:"Zulip event cursor"
    ~dec:(fun n ->
      if not (valid n) then
        Jsont.Error.msgf Jsont.Meta.none
          "Expected -1 or an exact event identifier";
      int_of_float n)
    ~enc:(fun n ->
      if not (valid (float_of_int n)) then
        Jsont.Error.msgf Jsont.Meta.none "Invalid event cursor";
      float_of_int n)
    Jsont.number

let register_codec =
  Jsont.Object.map (fun id last_event_id timeout ->
      {
        id;
        last_event_id;
        timeout;
        initial_state = Jsont.Json.object' [];
        polling = false;
      })
  |> Jsont.Object.mem "queue_id" Jsont.string ~enc:id
  |> Jsont.Object.mem "last_event_id" cursor_jsont ~enc:last_event_id
  |> Jsont.Object.mem "event_queue_longpoll_timeout_seconds" Jsont.number
       ~dec_absent:(fun () -> 90.)
       ~enc:longpoll_timeout
  |> Jsont.Object.finish

let register client ?(options = default_registration) ?event_types ?narrow
    ?(all_public_streams = false) () =
  let valid_duration = function None -> true | Some n -> n >= 0 in
  if
    (not (valid_duration options.presence_history_limit_days))
    ||
    match options.idle_queue_timeout with
    | Some (Seconds n) -> n <= 0 || n > 604800
    | _ -> false
  then
    Error
      (Error.Invalid_request
         "Queue history limit must be nonnegative and idle seconds must be \
          between 1 and 604800")
  else
    let* events =
      match event_types with
      | None -> Ok []
      | Some types ->
          Codec.encode (Jsont.list Zulip.Event_type.jsont) types
          |> Result.map (fun value -> [ ("event_types", value) ])
    in
    let* narrow =
      match narrow with
      | None -> Ok []
      | Some terms ->
          Codec.encode (Jsont.list Narrow.jsont) terms
          |> Result.map (fun value -> [ ("narrow", value) ])
    in
    let* fetched =
      match options.fetch_event_types with
      | None -> Ok []
      | Some types ->
          Codec.encode (Jsont.list Zulip.Event_type.jsont) types
          |> Result.map (fun value -> [ ("fetch_event_types", value) ])
    in
    let* capabilities =
      let capabilities =
        if
          List.mem_assoc "notification_settings_null"
            options.client_capabilities
        then options.client_capabilities
        else ("notification_settings_null", true) :: options.client_capabilities
      in
      let json =
        Jsont.Json.object'
          (List.map
             (fun (key, value) ->
               ((key, Jsont.Meta.none), Jsont.Json.bool value))
             capabilities)
      in
      Codec.encode Jsont.json json
    in
    let optional_int name =
      Option.fold ~none:[] ~some:(fun n -> [ (name, string_of_int n) ])
    in
    let params =
      [
        ("apply_markdown", string_of_bool options.apply_markdown);
        ("client_gravatar", string_of_bool options.client_gravatar);
        ( "include_subscribers",
          match options.include_subscribers with
          | `None -> "false"
          | `All -> "true"
          | `Partial -> "partial" );
        ("slim_presence", string_of_bool options.slim_presence);
        ("client_capabilities", capabilities);
        ("all_public_streams", string_of_bool all_public_streams);
      ]
      @ events @ narrow @ fetched
      @ optional_int "presence_history_limit_days"
          options.presence_history_limit_days
      @
      match options.idle_queue_timeout with
      | None -> []
      | Some (Seconds n) -> [ ("idle_queue_timeout", string_of_int n) ]
      | Some Mobile -> [ ("idle_queue_timeout", "\"mobile\"") ]
    in
    let* raw =
      Client.request client ~method_:`POST ~path:"register" ~params ()
    in
    let* queue = Codec.decode register_codec raw in
    if
      queue.id = "" || queue.last_event_id < -1
      || (not (Float.is_finite queue.timeout))
      || queue.timeout <= 0. || queue.timeout > 86400.
    then Error (Error.Invalid_request "Invalid queue registration metadata")
    else Ok { queue with initial_state = raw }

let events_codec =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem "events" (Jsont.list Zulip.Event.jsont) ~enc:Fun.id
  |> Jsont.Object.finish

module Batch = struct
  type queue = t
  type t = { owner : queue; events : Zulip.Event.t array }

  let events t = Array.to_list t.events
  let length t = Array.length t.events
end

let get_events t client ?(dont_block = false) () =
  if t.polling then
    Error (Error.Invalid_request "A poll is already outstanding for this queue")
  else (
    t.polling <- true;
    Fun.protect
      ~finally:(fun () -> t.polling <- false)
      (fun () ->
        let params =
          [
            ("queue_id", t.id);
            ("last_event_id", string_of_int t.last_event_id);
            ("dont_block", string_of_bool dont_block);
          ]
        in
        let* events =
          Client.request_typed client ~method_:`GET ~path:"events" ~params
            ~timeout:(if dont_block then 30. else t.timeout +. 10.)
            ~longpoll:(not dont_block) ~codec:events_codec ()
        in
        let events =
          events
          |> List.filter (fun event ->
              Zulip.Id.Event.to_int (Zulip.Event.id event) > t.last_event_id)
          |> List.sort_uniq (fun a b ->
              Zulip.Id.Event.compare (Zulip.Event.id a) (Zulip.Event.id b))
          |> Array.of_list
        in
        Ok { Batch.owner = t; events }))

let ack ?count t batch =
  let count = Option.value ~default:(Batch.length batch) count in
  if batch.Batch.owner != t then
    Error
      (Error.Invalid_request "Event batch belongs to another queue generation")
  else if count < 0 || count > Batch.length batch then
    Error (Error.Invalid_request "Acknowledgement count is outside the batch")
  else (
    if count > 0 then
      t.last_event_id <-
        max t.last_event_id
          (Zulip.Id.Event.to_int
             (Zulip.Event.id batch.Batch.events.(count - 1)));
    Ok ())

let delete ?timeout t client =
  Client.request client ~method_:`DELETE ~path:"events"
    ~params:[ ("queue_id", t.id) ]
    ?timeout ()
  |> Result.map (fun _ -> ())

let pp ppf t = Format.fprintf ppf "Queue(%s, cursor=%d)" t.id t.last_event_id

type control = Continue | Stop

let terminal = function
  | Error.Json _ -> true
  | (Error.Api { status; _ } | Error.Http { status; _ }) as error ->
      Error.is_terminal error
      || (status >= 400 && status < 500 && status <> 408 && status <> 429)
  | error -> Error.is_terminal error

let iter client ?options ?event_types ?narrow ?all_public_streams
    ?(on_recover = fun _ -> ()) ?(on_registered = fun _ -> ()) callback =
  match Transport.clock (Client.transport client) with
  | None -> Error (Error.Invalid_request "An event collector requires a clock")
  | Some clock ->
      let current = ref None in
      let sleep error delay =
        Eio.Time.sleep clock
          (Option.value (Error.retry_after error) ~default:delay)
      in
      Fun.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
              Option.iter
                (fun q -> ignore (delete ~timeout:2. q client))
                !current))
        (fun () ->
          let rec registration delay =
            match
              register client ?options ?event_types ?narrow ?all_public_streams
                ()
            with
            | Ok q ->
                current := Some q;
                on_registered q;
                poll q 1.
            | Error error ->
                on_recover error;
                if terminal error then Error error
                else (
                  sleep error delay;
                  registration (min 30. (delay *. 2.)))
          and poll q delay =
            match get_events q client () with
            | Error error when Error.is_bad_queue error ->
                current := None;
                on_recover error;
                registration 1.
            | Error error ->
                on_recover error;
                if terminal error then Error error
                else (
                  sleep error delay;
                  poll q (min 30. (delay *. 2.)))
            | Ok batch ->
                let rec accept count = function
                  | [] -> poll q 1.
                  | event :: rest -> (
                      let action =
                        match Zulip.Event.type_ event with
                        | Zulip.Event_type.Heartbeat -> Continue
                        | _ -> callback event
                      in
                      let* () = ack ~count:(count + 1) q batch in
                      match action with
                      | Continue -> accept (count + 1) rest
                      | Stop -> Ok ())
                in
                accept 0 (Batch.events batch)
          in
          registration 1.)

exception Malformed_message of Error.t

let message_codec =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem "message" Zulip.Message.jsont ~enc:Fun.id
  |> Jsont.Object.finish

let iter_messages client ?options ?narrow ?all_public_streams ?on_recover
    callback =
  try
    iter client ?options ~event_types:[ Zulip.Event_type.Message ]
      ?narrow ?all_public_streams ?on_recover (fun event ->
        match Zulip.Event.type_ event with
        | Zulip.Event_type.Message -> (
            match Codec.decode message_codec (Zulip.Event.data event) with
            | Ok message -> callback message
            | Error error -> raise (Malformed_message error))
        | _ -> Continue)
  with Malformed_message error -> Error error
