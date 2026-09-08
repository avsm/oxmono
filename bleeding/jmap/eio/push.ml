(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type event =
  | State_change of Jmap.Proto.Push.State_change.t
  | Ping of { interval : int64 }
  | Unknown of string * string

(* RFC 8620 7.3: the data of a ping event is a JSON object with an "interval"
   property, an UnsignedInt of seconds. *)
let ping_jsont =
  Jsont.Object.map ~kind:"Ping" (fun interval -> interval)
  |> Jsont.Object.mem "interval" Jmap.Proto.Int53.Unsigned.jsont ~enc:Fun.id
  |> Jsont.Object.finish

let pp_event ppf = function
  | State_change change ->
      let pp_escaped = Jmap.Proto.Error.pp_escaped in
      let pp_type ppf ts =
        Format.fprintf ppf "%a=%a" pp_escaped
          ts.Jmap.Proto.Push.State_change.type_name pp_escaped
          ts.Jmap.Proto.Push.State_change.state
      in
      let pp_account ppf (id, types) =
        Format.fprintf ppf "@[<h>%a: %a@]" Jmap.Proto.Id.pp id
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             pp_type)
          types
      in
      Format.fprintf ppf "@[<h>StateChange {%a}@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           pp_account)
        change.Jmap.Proto.Push.State_change.changed
  | Ping { interval } -> Format.fprintf ppf "Ping %Lds" interval
  | Unknown (name, data) ->
      Format.fprintf ppf "@[<h>%a %a@]" Jmap.Proto.Error.pp_escaped name
        Jmap.Proto.Error.pp_escaped data

let max_event = 1024 * 1024

let validate_ping caller = function
  | None -> Ok ()
  | Some ping -> (
      match Jmap.Proto.Int53.Unsigned.of_int ping with
      | Ok _ -> Ok ()
      | Error _ ->
          Error
            (Fmt.str "%s: ?ping must be between 0 and 2^53-1 seconds" caller))

let event_source_url_result client ?types ?close_after ?ping () =
  let types =
    match types with
    | None | Some [] -> "*"
    | Some types -> String.concat "," types
  in
  let close_after =
    match close_after with Some `State -> "state" | Some `No | None -> "no"
  in
  let ping = string_of_int (Option.value ping ~default:0) in
  Client.expand_event_source_url client
    [
      ("types", `String types);
      ("closeafter", `String close_after);
      ("ping", `String ping);
    ]

let event_source_url client ?types ?close_after ?ping () =
  match validate_ping "Jmap_eio.Push.event_source_url" ping with
  | Error message -> invalid_arg message
  | Ok () -> (
      match event_source_url_result client ?types ?close_after ?ping () with
      | Ok url -> url
      | Error error ->
          invalid_arg
            ("Jmap_eio.Push.event_source_url: " ^ Client.error_to_string error))

let decode (event : Fetch.Sse.event) =
  let unknown () = Unknown (event.name, event.data) in
  match event.name with
  | "state" -> (
      match
        Jmap.Proto.Json.decode Jmap.Proto.Push.State_change.jsont event.data
      with
      | Ok change -> State_change change
      | Error _ -> unknown ())
  | "ping" -> (
      match Jmap.Proto.Json.decode ping_jsont event.data with
      | Ok interval -> Ping { interval }
      | Error _ -> unknown ())
  | _ -> unknown ()

(* RFC 8620 3.6.1: a refused event source answers with a problem details
   object when it has one to give. The bound on the body read here is the
   event bound rather than the client's, an error page being the only body
   this module ever holds whole. *)
let problem_codec =
  Jmap.Proto.Json.media ~media:"application/problem+json" ~accept:[]
    Jmap.Proto.Error.Request_error.jsont

let oversized_body = Fmt.str "response body exceeds the %d-byte limit" max_event

let read_body response =
  try
    Eio.Buf_read.(take_all (of_flow ~max_size:max_event (Fetch.body response)))
  with Eio.Buf_read.Buffer_limit_exceeded -> oversized_body

let error_of_response response =
  let status = Fetch.status response in
  let url = Fetch.url response in
  try
    match Fetch.decode ~limit:max_event problem_codec response with
    | err -> Client.Jmap_error err
    | exception Eio.Io (Fetch.E (Fetch.Decode_failure { error; _ }), _) -> (
        match error with
        (* The media type is not problem+json, so nothing has been read. *)
        | Fetch.Media.Unsupported _ ->
            Client.Http_error (status, read_body response)
        | Fetch.Media.Too_large _ -> Client.Http_error (status, oversized_body)
        | Fetch.Media.Malformed { message; _ } ->
            Client.Http_error (status, message))
  with Eio.Io _ as exn ->
    Client.Http_error
      ( status,
        Error_context.describe ~url
          ~operation:"reading refused JMAP event source response" exn )

let error_of_exn ?rejected ?url ~operation exn =
  match exn with
  | Fetch.Rejected response -> (
      match rejected with
      | Some error -> error
      | None -> error_of_response response)
  | Eio.Io (Fetch.E e, _) ->
      Client.Transport (e, Error_context.describe ?url ~operation exn)
  | exn ->
      let msg = Error_context.describe ?url ~operation exn in
      Client.Transport (Fetch.Protocol_error msg, msg)

let listen client ?types ?close_after ?ping ?last_event_id f =
  match validate_ping "Jmap_eio.Push.listen" ping with
  | Error message ->
      Error (Client.Transport (Fetch.Invalid_request message, message))
  | Ok () -> (
      match event_source_url_result client ?types ?close_after ?ping () with
      | Error error -> Error error
      | Ok url -> (
          (* A callback failure leaves the switch with a value rather than an
             exception, so that the transport handler below cannot claim it as
             a failure of its own. The switch still closes the response before
             it is re-raised. *)
          let run () =
            Eio.Switch.run @@ fun sw ->
            let connected =
              try
                Ok
                  (Fetch.Sse.connect ~sw ?last_event_id ~max_event
                     (Client.fetch client) url)
              with (Eio.Io _ | Unix.Unix_error _) as exn ->
                Error
                  (error_of_exn ~url ~operation:"opening JMAP event source" exn)
            in
            match connected with
            | Error error -> `Done (Error error)
            | Ok (Error response) -> `Done (Error (error_of_response response))
            | Ok (Ok events) ->
                let rec loop events =
                  let next =
                    try Ok (events ())
                    with (Eio.Io _ | Unix.Unix_error _) as exn ->
                      Error
                        (error_of_exn ~url
                           ~operation:"reading JMAP event source" exn)
                  in
                  match next with
                  | Error error -> `Done (Error error)
                  | Ok Seq.Nil -> `Done (Ok ())
                  | Ok (Seq.Cons (event, rest)) -> (
                      match f (decode event) with
                      | `Stop -> `Done (Ok ())
                      | `Continue -> loop rest
                      | exception exn ->
                          `Callback (exn, Printexc.get_raw_backtrace ()))
                in
                loop events
          in
          match
            try run ()
            with (Eio.Io _ | Unix.Unix_error _) as exn ->
              `Done
                (Error
                   (error_of_exn ~url ~operation:"closing JMAP event source" exn))
          with
          | `Done result -> result
          | `Callback (exn, backtrace) ->
              Printexc.raise_with_backtrace exn backtrace))

type item = [ `Event of event | `End ]

(* A source that reports the end of the stream once its deadline has passed.
   It is what holds a polling connection open for at most [poll] seconds: the
   server of a [closeafter=state] request keeps it open until something
   changes, and on Cyrus an open event source stops mail being delivered to
   the account. *)
module Held = struct
  type t = {
    src : Eio.Flow.source_ty Eio.Resource.t;
    clock : Eio.Time.Mono.ty Eio.Resource.t;
    deadline : Mtime.t;
  }

  let read_methods = []

  let single_read t (buf @ local) =
    let buf = Cstruct.globalize buf in
    let expired () =
      Mtime.compare (Eio.Time.Mono.now t.clock) t.deadline >= 0
    in
    if expired () then raise End_of_file;
    match
      Eio.Fiber.first
        (fun () -> Eio.Flow.single_read t.src buf)
        (fun () ->
          Eio.Time.Mono.sleep_until t.clock t.deadline;
          raise End_of_file)
    with
    | n -> n
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception _ when expired () -> raise End_of_file

  let handler =
    Eio.Flow.Pi.source
      (module struct
        type nonrec t = t

        let read_methods = read_methods
        let single_read = single_read
      end)

  let span seconds = Mtime.Span.of_float_ns (seconds *. 1e9)

  let deadline clock span =
    let now = Eio.Time.Mono.now clock in
    Option.value (Mtime.add_span now span) ~default:now

  let source ~clock ~deadline src =
    Eio.Resource.T ({ src; clock; deadline }, handler)
end

let holding ~clock ~span inner =
  Fetch.Middleware.middleware
    (fun handler ~sw req ->
      let deadline = Held.deadline clock span in
      let received = ref None in
      let response =
        match
          Eio.Time.Timeout.run_exn (Eio.Time.Timeout.v clock span) (fun () ->
              let response = handler ~sw req in
              received := Some response;
              response)
        with
        | response -> response
        | exception exn -> (
            Option.iter Fetch.close !received;
            match exn with
            | Eio.Time.Timeout ->
                raise (Fetch.err (Fetch.Connection_failure Eio.Net.Timeout))
            | _ -> raise exn)
      in
      let released, released_u = Eio.Promise.create () in
      let close () =
        Fetch.close response;
        ignore (Eio.Promise.try_resolve released_u ())
      in
      (* Release the exchange even while its producer waits for queue space. *)
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Fun.protect ~finally:close (fun () ->
              Eio.Fiber.first
                (fun () -> Eio.Promise.await released)
                (fun () -> Eio.Time.Mono.sleep_until clock deadline));
          `Stop_daemon);
      (* [Fetch.url] is a URL Fetch itself validated, so re-parsing it cannot
         fail; a response that escaped the deadline would hold the connection
         open, which is what poll mode exists to prevent. *)
      let url =
        Result.get_ok (Fetch.Middleware.Url.of_string (Fetch.url response))
      in
      Fetch.Middleware.Pi.response ~status:(Fetch.status response)
        ~headers:(Fetch.headers response) ~version:(Fetch.version response)
        ~body:(Held.source ~clock ~deadline (Fetch.body response))
        ~close
        ~trailers:(fun () -> Fetch.trailers response)
        ~scope:(Fetch.scope response)
        ~sensitive:(Fetch.Middleware.sensitive response)
        ~url ())
    inner

type subscription = {
  events : item Eio.Stream.t;
  capacity : int;
      (* The stream's own capacity, which it does not report. It is what
         makes a non-blocking add possible: in one domain nothing can take
         the slot between the length check and the add, and an add with room
         to spare does not suspend. *)
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  inner : Fetch.Sse.subscription;
  outcome : (unit, Client.error) result Eio.Promise.t;
  outcome_u : (unit, Client.error) result Eio.Promise.u;
  cancel : Eio.Cancel.t option Atomic.t;
  closed : bool Atomic.t;
}

let events t = t.events
let last_event_id t = Fetch.Sse.last_event_id t.inner
let result t = t.outcome

let rec next_item events outcome =
  match Eio.Stream.take_nonblocking events with
  | Some item -> item
  | None when Eio.Promise.is_resolved outcome -> `End
  | None -> (
      let combine a b =
        match (a, b) with
        | `Item item, _ | _, `Item item -> `Item item
        | `Stopped, `Stopped -> `Stopped
      in
      match
        Eio.Fiber.first ~combine
          (fun () -> `Item (Eio.Stream.take events))
          (fun () ->
            ignore (Eio.Promise.await outcome);
            `Stopped)
      with
      | `Item item -> item
      | `Stopped -> next_item events outcome)

let next t = next_item t.events t.outcome

exception Closed

let close t =
  if Atomic.compare_and_set t.closed false true then begin
    Fetch.Sse.close t.inner;
    Option.iter
      (fun cancel -> Eio.Cancel.cancel cancel Closed)
      (Atomic.get t.cancel)
  end

let default_backoff_initial = 1.
let default_backoff_max = 60.
let default_capacity = 64

let subscribe ~sw client ?types ?ping ?poll ?last_event_id
    ?(backoff_initial = default_backoff_initial)
    ?(backoff_max = default_backoff_max) ?(capacity = default_capacity) () =
  let clock =
    match Client.mono_clock client with
    | Some clock -> clock
    | None ->
        invalid_arg
          "Jmap_eio.Push.subscribe: the client's transport carries no \
           monotonic clock"
  in
  if
    not
      (Float.is_finite backoff_initial
      && Float.is_finite backoff_max
      && backoff_initial > 0.
      && backoff_max >= backoff_initial)
  then
    invalid_arg
      "Jmap_eio.Push.subscribe: ?backoff_initial and ?backoff_max must be \
       finite with 0 < backoff_initial <= backoff_max";
  (match validate_ping "Jmap_eio.Push.subscribe" ping with
  | Ok () -> ()
  | Error message -> invalid_arg message);
  let poll_span =
    match poll with
    | Some seconds when not (Float.is_finite seconds && seconds > 0.) ->
        invalid_arg "Jmap_eio.Push.subscribe: ?poll must be finite and positive"
    | Some seconds -> (
        match Held.span seconds with
        | Some span when Mtime.Span.equal span Mtime.Span.zero ->
            invalid_arg
              "Jmap_eio.Push.subscribe: ?poll is too small for the monotonic \
               clock"
        | Some span -> Some span
        | None ->
            invalid_arg
              "Jmap_eio.Push.subscribe: ?poll is too large for the monotonic \
               clock")
    | None -> None
  in
  if capacity < 1 then
    invalid_arg "Jmap_eio.Push.subscribe: ?capacity must be at least 1";
  let close_after = match poll with Some _ -> `State | None -> `No in
  let url =
    match event_source_url_result client ?types ~close_after ?ping () with
    | Ok url -> url
    | Error error ->
        invalid_arg ("Jmap_eio.Push.subscribe: " ^ Client.error_to_string error)
  in
  let rejected = ref None in
  (* The statuses [Fetch.Sse] retries by default. A refusal outside them ends
     the subscription, so its body is read below while the response is still
     open. *)
  let retryable_status status =
    status = 429 || (status >= 500 && status <= 599)
  in
  (* A session refresh replaces the client's scoped fetch, so each connection
     asks for the current one rather than holding the one seen at subscribe.
     The redirect walk of Fetch runs above a handler and below this one, so
     the wrappers below see one whole connection attempt rather than one hop
     of it. A hop discarded on the way keeps its body unread, and one deadline
     covers the walk. *)
  let fetch =
    Fetch.Middleware.of_handler (fun ~sw (req : Fetch.Middleware.request) ->
        Fetch.fetch ~sw
          ~headers:(Fetch.Header.of_http req.headers)
          ~body:req.body ~sensitive:req.sensitive (Client.fetch client) req.meth
          (Fetch.Middleware.Url.to_string req.url))
  in
  let fetch =
    match poll_span with
    | None -> fetch
    | Some span -> holding ~clock ~span fetch
  in
  let fetch =
    Fetch.Middleware.middleware
      (fun handler ~sw req ->
        let response = handler ~sw req in
        let status = Fetch.status response in
        (* Fetch releases a rejected response before classifying its error. *)
        (if (status < 200 || status >= 300) && not (retryable_status status)
         then
           match error_of_response response with
           | error -> rejected := Some error
           | exception exn ->
               Fetch.close response;
               raise exn);
        response)
      fetch
  in
  let backoff_initial, backoff_max =
    match poll with
    | None -> (backoff_initial, backoff_max)
    | Some seconds -> (seconds, seconds)
  in
  let backoff_initial = Duration.of_f backoff_initial in
  let backoff_max = Duration.of_f backoff_max in
  if backoff_initial = 0L then
    invalid_arg "Jmap_eio.Push.subscribe: backoff is below one nanosecond";
  let inner =
    Fetch.Sse.subscribe ~sw ~clock ?last_event_id ~max_event ~backoff_initial
      ~backoff_max ~capacity fetch url
  in
  let outcome, outcome_u = Eio.Promise.create () in
  let t =
    {
      events = Eio.Stream.create capacity;
      capacity;
      clock;
      inner;
      outcome;
      outcome_u;
      cancel = Atomic.make None;
      closed = Atomic.make false;
    }
  in
  let settle v = ignore (Eio.Promise.try_resolve t.outcome_u v) in
  let end_nonblocking () =
    if Eio.Stream.length t.events < t.capacity then Eio.Stream.add t.events `End
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Cancel.sub @@ fun cancel ->
      Atomic.set t.cancel (Some cancel);
      if Atomic.get t.closed then Eio.Cancel.cancel cancel Closed;
      let rec pump () =
        match
          next_item (Fetch.Sse.events t.inner) (Fetch.Sse.result t.inner)
        with
        | `Event event ->
            Eio.Stream.add t.events (`Event (decode event));
            pump ()
        | `End -> ()
      in
      let outcome =
        match pump () with
        | () -> (
            (* The inner result is resolved before its [`End] is published. *)
            match Eio.Promise.await (Fetch.Sse.result t.inner) with
            | Ok () -> Ok ()
            | Error exn ->
                Error
                  (error_of_exn ?rejected:!rejected ~url
                     ~operation:"subscribing to JMAP event source" exn))
        | exception Eio.Cancel.Cancelled _ -> Ok ()
        | exception exn ->
            (* An unexpected exception is kept in the result rather than
               dropped. *)
            Error
              (error_of_exn ~url ~operation:"subscribing to JMAP event source"
                 exn)
      in
      Atomic.set t.cancel None;
      settle outcome;
      (* Never wait for a consumer during teardown. [close] cancels this
         forwarding fiber even when it is parked on a full stream, and the
         owning switch must still be able to finish. Resolving a promise and
         adding to a stream with room are both non-suspending, so the
         protected section cannot park. *)
      Eio.Cancel.protect end_nonblocking;
      `Stop_daemon);
  t

let state_of_change ~type_ ~account_id change =
  match
    List.assoc_opt account_id change.Jmap.Proto.Push.State_change.changed
  with
  | None -> None
  | Some types ->
      Option.map
        (fun ts -> ts.Jmap.Proto.Push.State_change.state)
        (List.find_opt
           (fun ts ->
             String.equal ts.Jmap.Proto.Push.State_change.type_name type_)
           types)

let wait_for_state t ?timeout ?since ~type_ ~account_id () =
  let is_since state =
    match since with Some s -> String.equal s state | None -> false
  in
  let rec wait () =
    match next t with
    | `End -> None
    | `Event (State_change change) -> (
        match state_of_change ~type_ ~account_id change with
        | Some state when not (is_since state) -> Some state
        | Some _ | None -> wait ())
    | `Event (Ping _ | Unknown _) -> wait ()
  in
  match timeout with
  | None -> wait ()
  | Some seconds -> (
      (* Both fibers answer with a value, so a state taken in the same
         scheduler round as the deadline is returned rather than discarded
         with the cancelled fiber. *)
      let combine a b =
        match (a, b) with
        | `Found state, _ | _, `Found state -> `Found state
        | `Expired, `Expired -> `Expired
      in
      match
        Eio.Fiber.first ~combine
          (fun () -> `Found (wait ()))
          (fun () ->
            Eio.Time.Mono.sleep t.clock seconds;
            `Expired)
      with
      | `Found state -> state
      | `Expired -> None)
