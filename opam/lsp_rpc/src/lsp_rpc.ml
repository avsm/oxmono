open Core
open Async
open Jsonrpc
include Lsp_rpc_intf
module Rpc_error = Rpc_error

(* Demonstrate that [Json.t] and [Yojson.Safe.t] are the same. *)
let (T : (Json.t, Yojson.Safe.t) Type_equal.t) = T

module Id = struct
  include Id

  module T = struct
    type t =
      [ `String of string
      | `Int of int
      ]
    [@@deriving compare, hash, sexp_of]
  end

  let to_string = function
    | `Int int -> Int.to_string int
    | `String string -> string
  ;;

  include T
  include Hashable.Make_plain (T)

  let (T : (t, Id.t) Type_equal.t) = T
end

module Io : sig
  val read
    :  Reader.t
    -> [ `Eof | `Ok of Packet.t | `Bad_message of string * Json.t ] Deferred.Or_error.t

  val write : Writer.t -> Packet.t -> unit
end = struct
  module Error_monad = struct
    module O = struct
      let ( let+ ) t f = Deferred.Or_error.map t ~f
      let ( let* ) t f = Deferred.Or_error.bind t ~f
    end

    type 'a t = 'a Deferred.Or_error.t

    let return = Deferred.Or_error.return
    let raise exn = Deferred.Or_error.of_exn exn
  end

  module Channel = struct
    type input = Reader.t
    type output = Writer.t

    let read_line reader =
      match%map.Deferred.Or_error
        Deferred.Or_error.try_with (fun () -> Reader.read_line reader)
      with
      | `Eof -> None
      | `Ok value -> Some value
    ;;

    let read_exactly reader bytes =
      let bytes = Bytes.create bytes in
      match%map.Deferred.Or_error
        Deferred.Or_error.try_with (fun () -> Reader.really_read reader bytes)
      with
      | `Eof _ -> None
      | `Ok -> Some (Bytes.to_string bytes)
    ;;

    let write writer messages =
      List.iter messages ~f:(Writer.write writer);
      Deferred.Or_error.return ()
    ;;
  end

  include Lsp.Io.Make (Error_monad) (Channel)

  let read reader =
    (* The [Of_json] exceptions raised during [read] are recoverable. Since we've already
       extracted all the relevant bytes from the [Reader.t], we are ready to parse the
       next message. By contrast, a failure raised during header parsing or due to missing
       bytes leaves the [Reader.t] in a broken state. *)
    match%map Monitor.try_with (fun () -> read reader) with
    | Error (Json.Of_json (msg, json)) -> Ok (`Bad_message (msg, json))
    | Error exn -> Error (Error.of_exn exn)
    | Ok (Error _ as error) -> error
    | Ok (Ok None) -> Ok `Eof
    | Ok (Ok (Some packet)) -> Ok (`Ok packet)
  ;;

  let write writer packet =
    don't_wait_for
      (match Writer.is_closed writer with
       | true -> return ()
       | false -> write writer packet >>| ok_exn)
  ;;
end

module type Sendable_notification = sig
  type t

  val to_jsonrpc : t -> Notification.t
end

module Or_cancellation = struct
  type 'a t =
    | Ok of 'a
    | Cancellation of Id.t
end

module type Receivable_notification = sig
  type t

  val of_jsonrpc : Notification.t -> t Notification_parse_result.t Or_cancellation.t
end

module type Sendable_request = sig
  type 'a t

  val to_jsonrpc_request : 'a t -> id:Id.t -> Request.t
  val response_of_json : 'a t -> Json.t -> 'a
end

module type Receivable_request = sig
  type 'a t
  type packed = E : 'a t -> packed

  val yojson_of_result : 'a t -> 'a -> Json.t
  val of_jsonrpc : Request.t -> (packed, string) result
  val is_shutdown : 'a t -> bool
end

module type Request_error = sig
  type t

  val to_response : t -> Request.t -> ('a, Response.Error.t) Result.t
end

let response_error ?message request code =
  let data =
    match request.Request.params with
    | None -> None
    | Some ((`Assoc _ | `List _) as json) -> Some json
  in
  let prefix = sprintf "'%s' request failed" request.method_ in
  let message =
    match message with
    | None -> prefix
    | Some message -> sprintf "%s: %s" prefix message
  in
  Result.fail (Response.Error.make ?data ~code ~message ())
;;

module Connection_closed_reason = struct
  type t =
    | Reader_reached_eof
    | Read_failure of Core.Error.t
    | Write_failure of Core.Error.t
end

module Make
    (Sendable_notification : Sendable_notification)
    (Receivable_notification : Receivable_notification)
    (Sendable_request : Sendable_request)
    (Receivable_request : Receivable_request)
    (Request_error : Request_error) =
struct
  type 'a on_request =
    'a Receivable_request.t
    -> cancelled:unit Deferred.t
    -> ('a, Request_error.t) Deferred.Result.t

  type t =
    { reader : Reader.t
    ; writer : Writer.t
    ; connection_closed : (Connection_closed_reason.t -> unit) Bus.Read_write.t
    ; pending_requests_sent : (Json.t, Response.Error.t) Result.t Ivar.t Id.Table.t
    ; pending_requests_received : (Request.t * unit Ivar.t) Id.Table.t
    ; on_request : 'a. 'a on_request
    ; on_notification :
        Receivable_notification.t Notification_parse_result.t -> unit Deferred.t
    ; create_id : unit -> Id.t
    ; on_error : Rpc_error.t -> unit
    }

  let close_connection t reason =
    match Bus.is_closed t.connection_closed with
    | true -> ()
    | false ->
      Bus.write t.connection_closed reason;
      Bus.close t.connection_closed;
      (* We can close the [Reader.t] because even though there still may be unconsumed
         data, we have already responded to all the pending requests. *)
      don't_wait_for
        (let%map () = Writer.close t.writer
         and () = Reader.close t.reader in
         ())
  ;;

  let event_loop t =
    let%map parse_result =
      let sequencer = Throttle.Sequencer.create () in
      Deferred.repeat_until_finished () (fun () ->
        (* We use [map] purposely here so that the handling logic does not push back on
           the reader. We use [sequencer] to ensure that requests and notifications are
           processed in order (except for cancellations and shutdown, which are processed
           with priority). *)
        match%map Io.read t.reader with
        | Error _ as error -> `Finished error
        | Ok `Eof -> `Finished (Ok ())
        | Ok (`Bad_message (error, data)) ->
          t.on_error (Invalid_rpc_message { error; data });
          `Repeat ()
        | Ok (`Ok packet) ->
          (* Note that if [t.on_notification] or [t.on_request] raises, we don't do
             anything special to handle it. Clients that want to prevent raising can use
             [Monitor.try_with] in their implementations. If this function raises due to
             its own logic, that's a bug. *)
          (match packet with
           | Notification notification ->
             (match Receivable_notification.of_jsonrpc notification with
              | Ok notification ->
                Throttle.enqueue sequencer (fun () -> t.on_notification notification)
                |> don't_wait_for
              | Cancellation id ->
                (match Hashtbl.find_and_remove t.pending_requests_received id with
                 | None -> ()
                 | Some (request, cancelled) ->
                   Ivar.fill_exn cancelled ();
                   let result = response_error request RequestCancelled in
                   Io.write t.writer (Response { id; result })))
           | Request request ->
             (match Receivable_request.of_jsonrpc request with
              | Error message ->
                Io.write
                  t.writer
                  (Response
                     { id = request.id
                     ; result = response_error request InvalidParams ~message
                     })
              | Ok (E request') ->
                let cancelled = Ivar.create () in
                (match
                   Hashtbl.add
                     t.pending_requests_received
                     ~key:request.id
                     ~data:(request, cancelled)
                 with
                 | `Duplicate ->
                   (* If we receive a request with a duplicate ID, it is not obvious
                      whether to reply with one error or two. We are probably in an
                      irrevocably bad state anyway if this happens, but for simplicity we
                      reply with two, maintaining the 1:1 relationship between requests
                      and responses. *)
                   Io.write
                     t.writer
                     (Response
                        { id = request.id
                        ; result =
                            response_error
                              request
                              InvalidRequest
                              ~message:
                                [%string "Request id currently in use %{request.id#Id}"]
                        })
                 | `Ok ->
                   (* We enqueue the Shutdown request to the front so it can be processed
                      immediately, avoiding needless work that delays shutdown. We don't
                      need to do the same for the Exit notification because (1) the queue
                      should clear quickly since all requests will be failed and (2) Exit
                      is a notification, not a request, so delayed handling doesn't delay
                      the client from shutting down. *)
                   let enqueue =
                     match Receivable_request.is_shutdown request' with
                     | true -> Throttle.enqueue_front
                     | false -> Throttle.enqueue
                   in
                   enqueue sequencer (fun () ->
                     match Hashtbl.mem t.pending_requests_received request.id with
                     | false -> return ()
                     | true ->
                       (* We only remove the request from the table after we finish
                          processing it so that we can still look it up if we receive a
                          cancel notification. Once we have the result, we need to make
                          sure we haven't already returned a cancellation response so we
                          don't send two responses. *)
                       let%map result =
                         match%map
                           t.on_request request' ~cancelled:(Ivar.read cancelled)
                         with
                         | Ok result ->
                           Ok (Receivable_request.yojson_of_result request' result)
                         | Error request_error ->
                           Request_error.to_response request_error request
                       in
                       (match Hashtbl.mem t.pending_requests_received request.id with
                        | false -> ()
                        | true ->
                          Hashtbl.remove t.pending_requests_received request.id;
                          Io.write t.writer (Response { id = request.id; result })))
                   |> don't_wait_for))
           | Response response ->
             (match Hashtbl.find t.pending_requests_sent response.id with
              | None -> t.on_error (Response_for_unknown_request response)
              | Some box -> Ivar.fill_exn box response.result)
           | Batch_call batch_call -> t.on_error (Batch_call batch_call)
           | Batch_response batch_response -> t.on_error (Batch_response batch_response));
          `Repeat ())
    in
    let (reason : Connection_closed_reason.t) =
      match parse_result with
      | Ok () -> Reader_reached_eof
      | Error error ->
        t.on_error (Fatal_parse_failure error);
        Read_failure (Error.tag ~tag:"Connection closed due to fatal parse failure" error)
    in
    close_connection t reason
  ;;

  type request_handler = { f : 'a. 'a on_request }

  let create reader writer ~on_notification ~on_request:{ f = on_request } ~on_error =
    let connection_closed =
      (* Although we will only ever write a single value to [connection_closed], we
         implement it as a bus rather than as an ivar to allow listeners to unsubscribe,
         thereby avoiding a memory leak. *)
      Bus.create_exn
        ~on_subscription_after_first_write:Raise
        ~on_callback_raise:Core.Error.raise
        ()
    in
    let t =
      { reader
      ; writer
      ; connection_closed
      ; pending_requests_sent = Id.Table.create ()
      ; pending_requests_received = Id.Table.create ()
      ; on_request
      ; on_notification
      ; create_id =
          (* The "integer" type in LSP is a signed 32-bit integer. *)
          (let r = ref Int32.zero in
           fun () ->
             let id = `Int (Int32.to_int_trunc !r) in
             Int32.incr r;
             id)
      ; on_error
      }
    in
    don't_wait_for (event_loop t);
    (* If the [write] system call fails, close the connection. *)
    don't_wait_for
      (let%map exn = Monitor.detach_and_get_next_error (Writer.monitor writer) in
       close_connection t (Write_failure (Error.of_exn (Monitor.extract_exn exn))));
    t
  ;;

  let write_packet t ~packet ~on_successful_flush =
    let packet_string packet =
      packet |> [%yojson_of: Packet.t] |> Yojson.Safe.pretty_to_string
    in
    match Writer.is_closed t.writer with
    | true ->
      Deferred.Or_error.error_s
        [%message
          "Failed to send JSON RPC message: writer is closed" (packet_string packet)]
    | false ->
      let connection_closed = Ivar.create () in
      let subscriber =
        (* We subscribe to [t.connection_closed] before calling [write] to avoid a race
           where the write fails and we miss the message that the connection was closed. *)
        Bus.subscribe_exn t.connection_closed ~f:(Ivar.fill_exn connection_closed)
      in
      let connection_closed = Ivar.read connection_closed in
      Io.write t.writer packet;
      let%map result =
        match%bind Writer.flushed_or_failed_with_result t.writer with
        | Flushed _ -> on_successful_flush ~connection_closed
        | Force_closed ->
          Deferred.Or_error.error_s
            [%message
              "Failed to send JSON RPC message: writer was closed in parallel"
                (packet_string packet)]
        | Consumer_left ->
          failwith
            "BUG: [Consumer_left] should only be possible when \
             [raise_when_consumer_leaves] is set to false."
        | Error ->
          (* The write side of the connection closing can race with the read side - we
             return whichever error is reported first. *)
          (match%map connection_closed with
           | Read_failure error | Write_failure error ->
             Or_error.error_s
               [%message
                 "Failed to send JSON RPC message"
                   (packet_string packet)
                   (error : Core.Error.t)]
           | Reader_reached_eof ->
             Or_error.error_s
               [%message
                 "Failed to send JSON RPC message: consumer left" (packet_string packet)])
      in
      Bus.unsubscribe t.connection_closed subscriber;
      result
  ;;

  let call' (type a) t (request : a Sendable_request.t) =
    let id = t.create_id () in
    let request' = Sendable_request.to_jsonrpc_request request ~id in
    let result = Ivar.create () in
    Hashtbl.set t.pending_requests_sent ~key:id ~data:result;
    let%map result =
      write_packet
        t
        ~packet:(Request request')
        ~on_successful_flush:(fun ~connection_closed ->
          choose
            [ choice connection_closed (fun reason ->
                let request =
                  request' |> [%yojson_of: Request.t] |> Yojson.Safe.pretty_to_string
                in
                match reason with
                | Read_failure error ->
                  Or_error.error_s
                    [%message
                      "While this request was waiting for a response, a message was \
                       received that failed to parse"
                        request
                        (error : Core.Error.t)]
                | Reader_reached_eof ->
                  Or_error.error_s [%message "Consumer left without responding" request]
                | Write_failure unrelated_error ->
                  Or_error.error_s
                    [%message
                      "While this request was waiting for a response, an unrelated write \
                       failed and caused the connection to close."
                        request
                        (unrelated_error : Core.Error.t)])
            ; choice (Ivar.read result) (fun result -> Ok result)
            ])
    in
    Hashtbl.remove t.pending_requests_sent id;
    Or_error.map result ~f:(Result.map ~f:(Sendable_request.response_of_json request))
  ;;

  let call t request =
    match%map call' t request with
    | Error _ as result -> result
    | Ok (Ok _ as result) -> result
    | Ok (Error error) -> Or_error.of_exn (Response.Error.E error)
  ;;

  let notify t notification =
    let notification = Sendable_notification.to_jsonrpc notification in
    write_packet
      t
      ~packet:(Notification notification)
      ~on_successful_flush:(fun ~connection_closed:_ -> Deferred.Or_error.return ())
  ;;
end

module Client = struct
  include
    Make
      (Lsp.Client_notification)
      (struct
        include Lsp.Server_notification

        let of_jsonrpc notification =
          match of_jsonrpc notification with
          | Ok (CancelRequest id) -> Or_cancellation.Cancellation id
          | Ok notification -> Ok (Notification_parse_result.T notification)
          | Error message -> Ok (Invalid_params { message; notification })
        ;;
      end)
      (Lsp.Client_request)
      (struct
        include Lsp.Server_request

        let is_shutdown _ = false
      end)
      (struct
        include Client_error

        let to_response t request =
          match t with
          | `Method_not_implemented -> response_error request MethodNotFound
          | `Request_failed message -> response_error request RequestFailed ~message
        ;;
      end)
end

module Server = struct
  module Server_error = struct
    type public = Server_error.t

    type t =
      [ public
      | `Server_not_initialized
      | `Invalid_request of string
      ]
  end

  module Or_server_error = struct
    type 'a t = ('a, Server_error.t) Result.t
  end

  module Implementation =
    Make
      (Lsp.Server_notification)
      (struct
        include Lsp.Client_notification

        let of_jsonrpc notification =
          match of_jsonrpc notification with
          | Ok (CancelRequest id) -> Or_cancellation.Cancellation id
          | Ok notification -> Ok (Notification_parse_result.T notification)
          | Error message -> Ok (Invalid_params { message; notification })
        ;;
      end)
      (Lsp.Server_request)
      (struct
        include Lsp.Client_request

        let is_shutdown (type a) : a t -> bool = function
          | Shutdown -> true
          | _ -> false
        ;;
      end)
      (struct
        include Server_error

        let to_response t request =
          match t with
          | `Method_not_implemented -> response_error request MethodNotFound
          | `Request_failed message -> response_error request RequestFailed ~message
          | `Content_modified -> response_error request ContentModified
          | `Server_cancelled -> response_error request ServerCancelled
          | `Server_not_initialized -> response_error request ServerNotInitialized
          | `Invalid_request message -> response_error request InvalidRequest ~message
        ;;
      end)

  type t =
    { implementation : Implementation.t
    ; encoding : [ `UTF_8 | `UTF_16 ] Set_once.t
    }

  let call t request = Implementation.call t.implementation request
  let call' t request = Implementation.call' t.implementation request
  let notify t notification = Implementation.notify t.implementation notification

  let encoding ~(here : [%call_pos]) t =
    (* [t.encoding] is set before any client callback is invoked, so this should always
       succeed. If due to some bug it does not, we use [%call_pos] to attribute the
       location properly. *)
    Set_once.get_exn t.encoding ~here
  ;;

  type request_handler =
    { f :
        'a.
        'a Lsp.Client_request.t
        -> cancelled:unit Deferred.t
        -> ('a, Server_error.public) Deferred.Result.t
    }

  type state =
    | Uninitialized
    | Running
    | Shutdown_requested

  let create
    ?(on_client_termination = On_client_termination.Do_nothing)
    reader
    writer
    ~on_notification
    ~on_request:{ f = on_request }
    ~on_error
    =
    let state = ref Uninitialized in
    let encoding = Set_once.create () in
    let on_request (type a) (request : a Lsp.Client_request.t) ~cancelled =
      match !state with
      | Shutdown_requested -> Deferred.Result.fail (`Invalid_request "Shutdown requested")
      | Uninitialized ->
        (match request with
         | Initialize initialize ->
           state := Running;
           let () =
             match on_client_termination with
             | Do_nothing -> ()
             | Detect_and_exit { client_pid; poll_every } ->
               let client_pid =
                 match client_pid with
                 | `Pid pid -> Some pid
                 | `Use_pid_in_initialize_request_if_provided ->
                   Option.map initialize.processId ~f:Pid.of_int
               in
               (match client_pid with
                | None -> ()
                | Some client_pid ->
                  Clock_ns.every' poll_every (fun () ->
                    match Signal_unix.send Signal.zero (`Pid client_pid) with
                    | `Ok -> return ()
                    | `No_such_process -> Shutdown.exit 0))
           in
           let position_encoding =
             let client_supports_utf8 =
               (* Note: the underlying LSP library does not support UTF-32. *)
               match initialize.capabilities with
               | { general = Some { positionEncodings = Some encodings; _ }; _ } ->
                 List.exists encodings ~f:(function
                   | UTF8 -> true
                   | _ -> false)
               | _ -> false
             in
             match client_supports_utf8 with
             | true -> `UTF_8
             | false -> `UTF_16
           in
           Set_once.set_exn encoding position_encoding;
           (match%map on_request (Initialize initialize) ~cancelled with
            | Error _ as error -> (error :> (a, _) Result.t)
            | Ok initialize_result ->
              Ok
                { initialize_result with
                  capabilities =
                    { initialize_result.capabilities with
                      positionEncoding =
                        (match position_encoding with
                         | `UTF_8 -> Some UTF8
                         | `UTF_16 -> Some UTF16)
                    }
                }
             :> a Or_server_error.t Deferred.t)
         | Shutdown ->
           state := Shutdown_requested;
           (on_request request ~cancelled :> _ Or_server_error.t Deferred.t)
         | _ -> Deferred.Result.fail `Server_not_initialized)
      | Running ->
        (match request with
         | Initialize _ -> Deferred.Result.fail (`Invalid_request "Already initialized")
         | Shutdown ->
           state := Shutdown_requested;
           (on_request request ~cancelled :> _ Or_server_error.t Deferred.t)
         | _ -> (on_request request ~cancelled :> _ Or_server_error.t Deferred.t))
    in
    let on_notification notification =
      match !state with
      | Shutdown_requested ->
        (match (notification : Lsp.Client_notification.t Notification_parse_result.t) with
         | T Exit -> if am_running_test then return () else Shutdown.exit 0
         | _ -> return ())
      | Uninitialized ->
        (match notification with
         | T Exit -> if am_running_test then return () else Shutdown.exit 1
         | _ -> return ())
      | Running ->
        (match notification with
         | T Exit -> if am_running_test then return () else Shutdown.exit 1
         | _ -> on_notification notification)
    in
    let implementation =
      Implementation.create
        reader
        writer
        ~on_notification
        ~on_request:{ f = on_request }
        ~on_error
    in
    { implementation; encoding }
  ;;
end
