module Ev = Matrix_proto.Event
module Id = Matrix_proto.Id
module Cancel_code = Verification_base.Cancel_code
module Method = Verification_base.Method
module Transaction = Verification_base.Transaction
module Message = Verification_base.Message
module Sas = Verification_sas
module Qr = Verification_qr

type stage =
  | Requested
  | Ready of Method.t list
  | Sas of Sas.t
  | Showing_qr of Qr.t
  | Scanned_qr of Qr.t
  | Done
  | Cancelled of Cancel_code.t

type session = {
  transaction : Transaction.t;
  their_user_id : Id.User_id.t;
  requested_devices : To_device.recipient list;
  mutable their_device_id : Id.Device_id.t option;
  mutable their_methods : Method.t list;
  mutable stage : stage;
  mutable last_sas : Sas.t option;
  we_requested : bool;
  from_request : bool;
  created_at : Ev.Timestamp.t;
  timeout : int64;
}

type routed_send = session * Message.t

type t = {
  sessions : (string, session) Hashtbl.t;
  timeout : int64;
  mutable pending : routed_send list;
}

type directed_send = Id.Device_id.t * Message.t

type step = {
  session : session option;
  send : Message.t list;
  send_to : directed_send list;
}

let default_timeout_ms = 600_000L

(* A verification request is cheap for a peer to create but expensive for a
   user to inspect. Keep the table bounded without evicting an unrelated
   verification that is already in progress. *)
let max_active_inbound_per_device = 8
let max_active_inbound = 128

let create ?(timeout = default_timeout_ms) () =
  { sessions = Hashtbl.create 8; timeout; pending = [] }

let find t id = Hashtbl.find_opt t.sessions id
let remove t id = Hashtbl.remove t.sessions id
let sessions t = Hashtbl.fold (fun _ s acc -> s :: acc) t.sessions []
let session_transaction s = s.transaction
let session_stage s = s.stage
let session_their_user_id s = s.their_user_id
let session_their_device_id s = s.their_device_id
let session_their_methods s = s.their_methods
let session_we_requested s = s.we_requested
let session_requested_devices s = s.requested_devices
let session_sas s = s.last_sas

let take_pending_sends t =
  let pending = List.rev t.pending in
  t.pending <- [];
  pending

let new_session ?their_device_id ?(their_methods = []) ?(we_requested = false)
    ?(from_request = false) ?(requested_devices = []) ~transaction
    ~their_user_id ~now ~timeout stage =
  {
    transaction;
    their_user_id;
    requested_devices;
    their_device_id;
    their_methods;
    stage;
    last_sas = None;
    we_requested;
    from_request;
    created_at = now;
    timeout;
  }

let active_request s =
  s.from_request && match s.stage with Done | Cancelled _ -> false | _ -> true

let active_inbound s =
  (not s.we_requested)
  && match s.stage with Done | Cancelled _ -> false | _ -> true

let same_device a b =
  match (a, b) with Some a, Some b -> Id.Device_id.equal a b | _ -> false

let inbound_count t =
  List.fold_left
    (fun n s -> if active_inbound s then n + 1 else n)
    0 (sessions t)

let inbound_device_count t device_id =
  List.fold_left
    (fun n s ->
      if active_inbound s && same_device s.their_device_id device_id then n + 1
      else n)
    0 (sessions t)

let inbound_capacity t s =
  (not (active_inbound s))
  || inbound_count t < max_active_inbound
     && inbound_device_count t s.their_device_id < max_active_inbound_per_device

let reject_inbound t s =
  s.stage <- Cancelled Cancel_code.User;
  t.pending <- (s, Message.cancel s.transaction Cancel_code.User) :: t.pending

let track t s =
  if inbound_capacity t s then
    Hashtbl.replace t.sessions (Transaction.id s.transaction) s
  else reject_inbound t s;
  s

let cancel_for_competing_request t s =
  s.stage <- Cancelled Cancel_code.User;
  t.pending <- (s, Message.cancel s.transaction Cancel_code.User) :: t.pending

let track_request t s =
  let id = Transaction.id s.transaction in
  let accepted = inbound_capacity t s in
  let competing =
    List.filter
      (fun old ->
        active_request old
        && Id.User_id.equal old.their_user_id s.their_user_id
        && not (String.equal (Transaction.id old.transaction) id))
      (sessions t)
  in
  ignore (track t s : session);
  if not accepted then s
  else
    match competing with
    | [] -> s
    | _ ->
        List.iter (cancel_for_competing_request t) competing;
        cancel_for_competing_request t s;
        s

type request = {
  session : session;
  message : Message.t;
  to_device : To_device.messages;
}

let request ~random ~now ~from_device ?methods ~their_user_id ~devices t =
  let r =
    Verification_base.request_to_device ~random ~now ~from_device ?methods
      ~their_user_id ~devices ()
  in
  let session =
    track_request t
      (new_session ~transaction:r.Verification_base.transaction ~their_user_id
         ~now ~timeout:t.timeout ~we_requested:true ~requested_devices:devices
         ~from_request:true Requested)
  in
  {
    session;
    message = r.Verification_base.message;
    to_device = r.Verification_base.to_device;
  }

let request_in_room ~now ~from_device ~room_id ~event_id ~their_user_id ~content
    t =
  if
    (not
       (String.equal
          (Ev.Key_verification_request_message_content.from_device content)
          (Id.Device_id.to_string from_device)))
    || not
         (String.equal
            (Ev.Key_verification_request_message_content.to_ content)
            (Id.User_id.to_string their_user_id))
  then
    invalid_arg
      "Matrix_client.Verification.Flow.request_in_room: content addressing";
  let transaction = Transaction.in_room ~room_id ~event_id in
  let message = Message.v transaction (Message.Request_in_room content) in
  let session =
    track_request t
      (new_session ~transaction ~their_user_id ~now ~timeout:t.timeout
         ~we_requested:true ~from_request:true Requested)
  in
  { session; message; to_device = [] }

let fail ?reason s code =
  s.stage <- Cancelled code;
  {
    session = Some s;
    send = [ Message.cancel ?reason s.transaction code ];
    send_to = [];
  }

let cancel ?reason s code =
  match s.stage with
  | Cancelled _ | Done -> { session = Some s; send = []; send_to = [] }
  | _ -> fail ?reason s code

let apply_sas s (o : Sas.step) =
  s.last_sas <- Some o.Sas.sas;
  s.stage <-
    (match Sas.stage o.Sas.sas with
    | Sas.Done -> Done
    | Sas.Cancelled c -> Cancelled c
    | _ -> Sas o.Sas.sas);
  { session = Some s; send = o.Sas.send; send_to = [] }

let start_sas ~random ~now ~ours ~theirs s =
  apply_sas s
    (Sas.start ~random ~now ~timeout:s.timeout ~transaction:s.transaction ~ours
       ~theirs ())

let confirm s =
  match s.stage with
  | Sas sas -> apply_sas s (Sas.confirm sas)
  | _ -> { session = Some s; send = []; send_to = [] }

let mismatch s =
  match s.stage with
  | Sas sas -> apply_sas s (Sas.mismatch sas)
  | _ -> cancel s Cancel_code.Mismatched_sas

let accept s ~from_device ~our_methods =
  match s.stage with
  | Requested -> (
      match
        Verification_base.ready_response ~transaction:s.transaction ~from_device
          ~our_methods ~their_methods:s.their_methods
      with
      | Error code -> fail s code
      | Ok (methods, msg) ->
          s.stage <- Ready methods;
          { session = Some s; send = [ msg ]; send_to = [] })
  | _ -> { session = Some s; send = []; send_to = [] }

let cancellation_sends ?except s code =
  List.filter_map
    (function
      | To_device.Device device_id
        when Option.for_all
               (fun selected -> not (Id.Device_id.equal device_id selected))
               except ->
          Some (device_id, Message.cancel s.transaction code)
      | _ -> None)
    s.requested_devices

let show_qr s qr = s.stage <- Showing_qr qr
let scanned_qr s qr = s.stage <- Scanned_qr qr

let tick t ~now =
  let expired =
    List.filter
      (fun s ->
        match s.stage with
        | Done | Cancelled _ -> false
        | _ ->
            Int64.sub (Ev.Timestamp.to_ms now) (Ev.Timestamp.to_ms s.created_at)
            > s.timeout)
      (sessions t)
  in
  List.concat_map (fun s -> (fail s Cancel_code.Timeout).send) expired

let device_of_string s = Result.to_option (Id.Device_id.of_string s)

let request_timestamp_is_valid ~now = function
  | None -> false
  | Some timestamp ->
      let now = Ev.Timestamp.to_ms now in
      let timestamp = Ev.Timestamp.to_ms timestamp in
      Int64.compare timestamp (Int64.sub now 600_000L) >= 0
      && Int64.compare timestamp (Int64.add now 300_000L) <= 0

let known_request_device ~lookup ~sender from_device =
  match device_of_string from_device with
  | Some device_id when Option.is_some (lookup ~user_id:sender ~device_id) ->
      Some device_id
  | Some _ | None -> None

let no_step = { session = None; send = []; send_to = [] }

let handle t ~random ~now ?event_timestamp ~ours ~lookup ~sender msg =
  let id = Transaction.id (Message.transaction msg) in
  let existing = find t id in
  match existing with
  | Some s when not (Id.User_id.equal s.their_user_id sender) ->
      (* This transaction id is already tracked for a different sender. The
         event is not routed to that session, on the chance of a stray
         collision or a deliberate attempt to hijack it. *)
      {
        session = None;
        send =
          [ Message.cancel (Message.transaction msg) Cancel_code.User_mismatch ];
        send_to = [];
      }
  | _ -> (
      match (Message.payload msg, existing) with
      | Message.Request c, _ -> (
          match
            ( request_timestamp_is_valid ~now
                (Ev.Key_verification_request_content.timestamp c),
              known_request_device ~lookup ~sender
                (Ev.Key_verification_request_content.from_device c) )
          with
          | false, _ | _, None -> no_step
          | true, Some their_device_id -> (
              match existing with
              | Some session ->
                  { session = Some session; send = []; send_to = [] }
              | None ->
                  let session =
                    track_request t
                      (new_session ~transaction:(Message.transaction msg)
                         ~their_user_id:sender ~now ~timeout:t.timeout
                         ~their_device_id ~from_request:true
                         ~their_methods:
                           (List.map Method.of_string
                              (Ev.Key_verification_request_content.methods c))
                         Requested)
                  in
                  { session = Some session; send = []; send_to = [] }))
      | Message.Request_in_room c, _ -> (
          match
            ( request_timestamp_is_valid ~now event_timestamp,
              known_request_device ~lookup ~sender
                (Ev.Key_verification_request_message_content.from_device c) )
          with
          | false, _ | _, None -> no_step
          | true, Some their_device_id -> (
              match existing with
              | Some session ->
                  { session = Some session; send = []; send_to = [] }
              | None ->
                  let session =
                    track_request t
                      (new_session ~transaction:(Message.transaction msg)
                         ~their_user_id:sender ~now ~timeout:t.timeout
                         ~their_device_id ~from_request:true
                         ~their_methods:
                           (List.map Method.of_string
                              (Ev.Key_verification_request_message_content
                               .methods c))
                         Requested)
                  in
                  { session = Some session; send = []; send_to = [] }))
      | Message.Ready c, Some s -> (
          match s.stage with
          | Requested ->
              s.their_device_id <-
                device_of_string
                  (Ev.Key_verification_ready_content.from_device c);
              s.their_methods <-
                List.map Method.of_string
                  (Ev.Key_verification_ready_content.methods c);
              s.stage <- Ready s.their_methods;
              {
                session = Some s;
                send = [];
                send_to =
                  cancellation_sends ?except:s.their_device_id s
                    Cancel_code.Accepted;
              }
          | _ -> { session = Some s; send = []; send_to = [] })
      | Message.Cancel c, Some s -> (
          (* A cancel is never answered, on either side. *)
          match s.stage with
          | Done -> { session = Some s; send = []; send_to = [] }
          | Cancelled _ -> { session = Some s; send = []; send_to = [] }
          | _ ->
              let code =
                Cancel_code.of_string
                  (Ev.Key_verification_cancel_content.code c)
              in
              s.stage <- Cancelled code;
              {
                session = Some s;
                send = [];
                (* For a request sent to several concrete devices, mirror the
                   peer's cancellation to every original recipient. The
                   cancelling event has no [from_device], so none can be
                   excluded reliably. *)
                send_to =
                  (if s.we_requested then cancellation_sends s code else []);
              })
      | Message.Start c, _ -> (
          let from_device =
            device_of_string (Ev.Key_verification_start_content.from_device c)
          in
          let s =
            match existing with
            | Some s -> s
            | None ->
                let s =
                  new_session ~transaction:(Message.transaction msg)
                    ~their_user_id:sender ~now ~timeout:t.timeout Requested
                in
                s.their_device_id <- from_device;
                track t s
          in
          (* [track] returns a cancelled, untracked session when the inbound
             capacity is exhausted. It still gets the normal cancellation
             transport, but must not be advanced into a SAS flow below. *)
          let tracked =
            match find t id with Some current -> current == s | None -> false
          in
          if not tracked then { session = Some s; send = []; send_to = [] }
          else
            match Ev.Key_verification_start_content.method_ c with
            | "m.sas.v1" -> (
                match
                  Option.bind from_device (fun device_id ->
                      lookup ~user_id:sender ~device_id)
                with
                | None -> fail s Cancel_code.Key_mismatch
                | Some theirs ->
                    apply_sas s
                      (Sas.from_start ~random ~now ~timeout:s.timeout
                         ~transaction:s.transaction ~ours ~theirs c))
            | "m.reciprocate.v1" -> (
                match (s.stage, Ev.Key_verification_start_content.secret c) with
                | Showing_qr qr, Some secret -> (
                    match Qr.check_reciprocate qr ~secret with
                    | Ok () ->
                        s.stage <- Done;
                        {
                          session = Some s;
                          send = [ Message.done_ s.transaction ];
                          send_to = [];
                        }
                    | Error code -> fail s code)
                | _ -> fail s Cancel_code.Unexpected_message)
            | _ -> fail s Cancel_code.Unknown_method)
      | (Message.Accept _ | Message.Key _ | Message.Mac _), Some s -> (
          match s.stage with
          | Sas sas -> apply_sas s (Sas.handle sas ~now msg)
          | _ -> fail s Cancel_code.Unexpected_message)
      | Message.Done _, Some s -> (
          match s.stage with
          | Sas sas -> apply_sas s (Sas.handle sas ~now msg)
          | Scanned_qr _ | Showing_qr _ ->
              s.stage <- Done;
              { session = Some s; send = []; send_to = [] }
          | _ -> { session = Some s; send = []; send_to = [] })
      | ( (Message.Accept _ | Message.Key _ | Message.Mac _ | Message.Ready _),
          None ) ->
          {
            session = None;
            send =
              [
                Message.cancel (Message.transaction msg)
                  Cancel_code.Unknown_transaction;
              ];
            send_to = [];
          }
      | (Message.Done _ | Message.Cancel _), None ->
          { session = None; send = []; send_to = [] })
