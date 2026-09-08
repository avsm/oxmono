module Base = Matrix_client.Send_queue

let src = Logs.Src.create "matrix.eio.send_queue" ~doc:"Send queue fibers"

module Log = (val Logs.src_log src : Logs.LOG)

type upload_result = Base.upload_result =
  | Clear_upload of { mxc : Matrix_client.Media.Mxc.t }
  | Encrypted_upload of {
      mxc : Matrix_client.Media.Mxc.t;
      metadata : Matrix_client.Encrypted_attachment.metadata;
    }

type kind = Base.kind =
  | Event of { event_type : string; content : Jsont.json }
  | Reaction of { relates_to : Matrix_proto.Id.Event_id.t; key : string }
  | Redaction of {
      event_id : Matrix_proto.Id.Event_id.t;
      reason : string option;
    }
  | Upload_request of {
      role : [ `Original | `Thumbnail ];
      content_type : string;
      filename : string option;
      data : string;
      encrypted_metadata : Matrix_client.Encrypted_attachment.metadata option;
    }
  | Attachment of {
      content : Jsont.json;
      original_upload : int;
      thumbnail_upload : int option;
    }

type attachment_upload = Base.attachment_upload

type attachment_edit_result = Base.attachment_edit_result =
  | Updated
  | Deferred
  | Already_sent

type status = Base.status =
  | Pending
  | Sending
  | Sent of Matrix_proto.Id.Event_id.t
  | Uploaded of upload_result
  | Wedged
  | Cancelled

type outcome = Base.outcome =
  | Sent_ok of Matrix_proto.Id.Event_id.t
  | Uploaded_ok of upload_result
  | Retry_in of float
  | Failed of Matrix_client.Error.t

type request = Base.request
type t = Base.t

type dependency_result = Base.dependency_result =
  | Event_id of Matrix_proto.Id.Event_id.t
  | Upload of upload_result

type progress = Base.progress = { current_bytes : int64; total_bytes : int64 }

let create = Base.create
let enqueue = Base.enqueue
let send_message = Base.send_message
let upload = Base.upload
let upload_encrypted = Base.upload_encrypted
let attachment_upload = Base.attachment_upload
let attachment_upload_encrypted = Base.attachment_upload_encrypted
let send_attachment = Base.send_attachment
let edit_attachment_caption = Base.edit_attachment_caption
let send_text = Base.send_text
let send_edit = Base.send_edit
let send_reaction = Base.send_reaction
let send_redaction = Base.send_redaction
let id = Base.id
let room_id = Base.room_id
let kind = Base.kind
let txn_id = Base.txn_id
let status = Base.status
let attempts = Base.attempts
let last_error = Base.last_error
let requests = Base.requests
let room_requests = Base.room_requests
let dependencies = Base.dependencies
let resolved_dependencies = Base.resolved_dependencies
let dependency_results = Base.dependency_results
let rooms = Base.rooms
let next = Base.next
let pending_count = Base.pending_count
let is_empty = Base.is_empty
let local_echo = Base.local_echo
let cancel = Base.cancel
let cancel_with_reason = Base.cancel_with_reason
let forget_room = Base.forget_room
let unwedge = Base.unwedge
let enabled = Base.enabled
let set_enabled = Base.set_enabled
let room_enabled = Base.room_enabled
let set_room_enabled = Base.set_room_enabled
let on_change = Base.on_change
let on_progress = Base.on_progress
let retry_delay = Base.retry_delay
let save = Base.save

type payload = Base.payload =
  | Send of { event_type : string; content : Jsont.json }
  | Redact of { event_id : Matrix_proto.Id.Event_id.t; reason : string option }
  | Upload_payload of {
      role : [ `Original | `Thumbnail ];
      content_type : string;
      filename : string option;
      data : string;
      encrypted_metadata : Matrix_client.Encrypted_attachment.metadata option;
    }

let payload = Base.payload

let send_one ?send ?upload ?on_progress queue client request =
  let room = Matrix_proto.Id.Room_id.to_string (Base.room_id request) in
  Error.with_context (Fmt.str "sending a queued request in room %S" room)
    (fun () ->
      Base.send_one queue ?send ?upload ?on_progress (Client.base client)
        request)

(* Sharing the Megolm session with everybody who has to read it — a
   [/keys/query], a [/keys/claim], the [m.room_key] sends — happens inside
   [Encryption_driver.encrypt_room_event], and so is retried along with the
   request: a claim that failed on a rate limit must not silently drop the
   message. *)

let encrypting_send enc members ~event_type ~content client request =
  let room_id = Base.room_id request in
  match
    Matrix_client.Encryption_driver.encrypt_room_event enc client room_id
      ~event_type ~content ~members:(members room_id)
  with
  | Error e -> Error e
  | Ok encrypted ->
      Base.send_as ~event_type:"m.room.encrypted" ~content:encrypted client
        request

let sender_for ?encryption ?members request =
  match (encryption, payload request) with
  | Some enc, Send _
    when Matrix_client.Encryption.is_room_encrypted
           (Matrix_client.Encryption_driver.machine enc)
           (Base.room_id request) -> (
      match members with
      | Some members -> (
          match Base.content_for_send request with
          | Ok (event_type, content) ->
              Some (encrypting_send enc members ~event_type ~content)
          | Error error -> Some (fun _ _ -> Error error))
      | None ->
          Log.warn (fun m ->
              m
                "send queue: no member list for encrypted room %s, sending in \
                 the clear"
                (Matrix_proto.Id.Room_id.to_string (Base.room_id request)));
          None)
  | _ -> None

(* The wait between passes when there is nothing to send. A condition
   broadcast on every queue change wakes the fibers immediately; the timeout
   is a backstop against a change that lands between a poll and the wait. *)
let idle_timeout = 1.0

let start ~sw ~clock ?encryption ?members client queue =
  let cond = Eio.Condition.create () in
  Base.on_change queue (fun _ -> Eio.Condition.broadcast cond);
  let flush () =
    match Base.store queue with
    | None -> ()
    | Some store ->
        Error.with_context "flushing send queue state" (fun () ->
            match Matrix_client.Store.flush store with
            | Ok () -> ()
            | Error e ->
                Log.warn (fun m ->
                    m "send queue: saving the queue failed: %a"
                      Matrix_client.Error.pp e))
  in
  let idle () =
    Eio.Fiber.first
      (fun () -> Eio.Condition.await_no_mutex cond)
      (fun () -> Eio.Time.sleep clock idle_timeout)
  in
  let run_room room_id =
    let rec loop () =
      Eio.Fiber.check ();
      (match Base.next queue room_id with
      | None -> idle ()
      | Some request -> (
          let send = sender_for ?encryption ?members request in
          match send_one ?send queue client request with
          | Sent_ok _ -> flush ()
          | Uploaded_ok _ -> flush ()
          | Retry_in d ->
              flush ();
              Eio.Time.sleep clock d
          | Failed _ ->
              flush ();
              idle ()));
      loop ()
    in
    loop ()
  in
  let started = Hashtbl.create 8 in
  Eio.Fiber.fork ~sw (fun () ->
      let rec supervise () =
        Eio.Fiber.check ();
        List.iter
          (fun rid ->
            let key = Matrix_proto.Id.Room_id.to_string rid in
            if not (Hashtbl.mem started key) then (
              Hashtbl.replace started key ();
              Eio.Fiber.fork ~sw (fun () -> run_room rid)))
          (Base.rooms queue);
        idle ();
        supervise ()
      in
      supervise ())
