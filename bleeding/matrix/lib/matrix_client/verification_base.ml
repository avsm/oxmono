module Ev = Matrix_proto.Event
module Id = Matrix_proto.Id

type error = [ `Msg of string ]

let err fmt = Format.kasprintf (fun m -> Error (`Msg m)) fmt

module Cancel_code = struct
  type t =
    | User
    | Timeout
    | Unknown_transaction
    | Unknown_method
    | Unexpected_message
    | Key_mismatch
    | User_mismatch
    | Invalid_message
    | Accepted
    | Mismatched_commitment
    | Mismatched_sas
    | Other of string

  let to_string = function
    | User -> "m.user"
    | Timeout -> "m.timeout"
    | Unknown_transaction -> "m.unknown_transaction"
    | Unknown_method -> "m.unknown_method"
    | Unexpected_message -> "m.unexpected_message"
    | Key_mismatch -> "m.key_mismatch"
    | User_mismatch -> "m.user_mismatch"
    | Invalid_message -> "m.invalid_message"
    | Accepted -> "m.accepted"
    | Mismatched_commitment -> "m.mismatched_commitment"
    | Mismatched_sas -> "m.mismatched_sas"
    | Other s -> s

  let of_string = function
    | "m.user" -> User
    | "m.timeout" -> Timeout
    | "m.unknown_transaction" -> Unknown_transaction
    | "m.unknown_method" -> Unknown_method
    | "m.unexpected_message" -> Unexpected_message
    | "m.key_mismatch" -> Key_mismatch
    | "m.user_mismatch" -> User_mismatch
    | "m.invalid_message" -> Invalid_message
    | "m.accepted" -> Accepted
    | "m.mismatched_commitment" -> Mismatched_commitment
    | "m.mismatched_sas" -> Mismatched_sas
    | s -> Other s

  let reason = function
    | User -> "The user cancelled the verification"
    | Timeout -> "The verification process timed out"
    | Unknown_transaction -> "The transaction is unknown"
    | Unknown_method -> "The requested method is not supported"
    | Unexpected_message -> "The message was received out of order"
    | Key_mismatch -> "The key was not verified"
    | User_mismatch -> "The expected user did not match the verified user"
    | Invalid_message -> "The message received was invalid"
    | Accepted -> "The request was accepted by a different device"
    | Mismatched_commitment -> "The hash commitment did not match"
    | Mismatched_sas -> "The short authentication string did not match"
    | Other s -> s

  let equal a b = String.equal (to_string a) (to_string b)
  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Method = struct
  type t =
    | Sas_v1
    | Qr_code_show_v1
    | Qr_code_scan_v1
    | Reciprocate_v1
    | Other of string

  let to_string = function
    | Sas_v1 -> "m.sas.v1"
    | Qr_code_show_v1 -> "m.qr_code.show.v1"
    | Qr_code_scan_v1 -> "m.qr_code.scan.v1"
    | Reciprocate_v1 -> "m.reciprocate.v1"
    | Other s -> s

  let of_string = function
    | "m.sas.v1" -> Sas_v1
    | "m.qr_code.show.v1" -> Qr_code_show_v1
    | "m.qr_code.scan.v1" -> Qr_code_scan_v1
    | "m.reciprocate.v1" -> Reciprocate_v1
    | s -> Other s

  let equal a b = String.equal (to_string a) (to_string b)
  let pp ppf t = Format.pp_print_string ppf (to_string t)
  let all = [ Sas_v1; Qr_code_show_v1; Qr_code_scan_v1; Reciprocate_v1 ]

  let common ~ours ~theirs =
    List.filter (fun m -> List.exists (equal m) theirs) ours
end

module Transaction = struct
  type t =
    | To_device of Id.Transaction_id.t
    | In_room of { room_id : Id.Room_id.t; event_id : Id.Event_id.t }

  let to_device id = To_device id
  let in_room ~room_id ~event_id = In_room { room_id; event_id }

  let id = function
    | To_device id -> "d:" ^ Id.Transaction_id.to_string id
    | In_room { room_id; event_id } ->
        let room = Id.Room_id.to_string room_id in
        Printf.sprintf "r:%d:%s%s" (String.length room) room
          (Id.Event_id.to_string event_id)

  let room_id = function
    | To_device _ -> None
    | In_room { room_id; _ } -> Some room_id

  let transaction_id = function To_device id -> Some id | In_room _ -> None

  let relates_to = function
    | To_device _ -> None
    | In_room { event_id; _ } -> Some (Ev.Relates_to.reference event_id)

  let pp ppf = function
    | To_device id ->
        Format.fprintf ppf "to-device %s" (Id.Transaction_id.to_string id)
    | In_room { room_id; event_id } ->
        Format.fprintf ppf "in-room %s/%s"
          (Id.Room_id.to_string room_id)
          (Id.Event_id.to_string event_id)

  let equal a b =
    match (a, b) with
    | To_device a, To_device b -> Id.Transaction_id.equal a b
    | In_room a, In_room b ->
        Id.Room_id.equal a.room_id b.room_id
        && Id.Event_id.equal a.event_id b.event_id
    | _ -> false
end

module Message = struct
  type payload =
    | Request of Ev.Key_verification_request_content.t
    | Request_in_room of Ev.Key_verification_request_message_content.t
    | Ready of Ev.Key_verification_ready_content.t
    | Start of Ev.Key_verification_start_content.t
    | Accept of Ev.Key_verification_accept_content.t
    | Key of Ev.Key_verification_key_content.t
    | Mac of Ev.Key_verification_mac_content.t
    | Done of Ev.Key_verification_done_content.t
    | Cancel of Ev.Key_verification_cancel_content.t

  type t = { transaction : Transaction.t; payload : payload }

  let v transaction payload = { transaction; payload }
  let transaction t = t.transaction
  let payload t = t.payload

  let payload_event_type = function
    | Request _ -> "m.key.verification.request"
    | Request_in_room _ -> "m.room.message"
    | Ready _ -> "m.key.verification.ready"
    | Start _ -> "m.key.verification.start"
    | Accept _ -> "m.key.verification.accept"
    | Key _ -> "m.key.verification.key"
    | Mac _ -> "m.key.verification.mac"
    | Done _ -> "m.key.verification.done"
    | Cancel _ -> "m.key.verification.cancel"

  let event_type t = payload_event_type t.payload

  let enc codec value =
    match Jsont.Json.encode codec value with
    | Ok json -> Ok json
    | Error m -> Error (`Msg m)

  let to_json t =
    match t.payload with
    | Request c -> enc Ev.Key_verification_request_content.jsont c
    | Request_in_room c ->
        enc Ev.Key_verification_request_message_content.jsont c
    | Ready c -> enc Ev.Key_verification_ready_content.jsont c
    | Start c -> enc Ev.Key_verification_start_content.jsont c
    | Accept c -> enc Ev.Key_verification_accept_content.jsont c
    | Key c -> enc Ev.Key_verification_key_content.jsont c
    | Mac c -> enc Ev.Key_verification_mac_content.jsont c
    | Done c -> enc Ev.Key_verification_done_content.jsont c
    | Cancel c -> enc Ev.Key_verification_cancel_content.jsont c

  let to_string t =
    Result.bind (to_json t) Matrix_proto.Signed_json.canonical_json_result

  let ( let* ) = Result.bind

  let transaction_of ~room_id ~transaction_id ~relates_to =
    match room_id with
    | Some room_id -> (
        match relates_to with
        | Some r ->
            Ok (Transaction.in_room ~room_id ~event_id:r.Ev.Relates_to.event_id)
        | None -> err "an in-room verification event needs m.relates_to")
    | None -> (
        match transaction_id with
        | Some id -> Ok (Transaction.to_device id)
        | None -> err "a to-device verification event needs transaction_id")

  let msgtype_of = function
    | Jsont.Object (mems, _) -> (
        match
          List.find_opt (fun ((n, _), _) -> String.equal n "msgtype") mems
        with
        | Some (_, Jsont.String (s, _)) -> Some s
        | _ -> None)
    | _ -> None

  let of_json ~event_type ?room_id ?event_id json =
    let dec codec =
      match Jsont.Json.decode codec json with
      | Ok v -> Ok v
      | Error m -> Error (`Msg m)
    in
    let addressed ~transaction_id ~relates_to payload =
      let* transaction = transaction_of ~room_id ~transaction_id ~relates_to in
      Ok { transaction; payload }
    in
    match event_type with
    | "m.key.verification.request" -> (
        match room_id with
        | Some _ -> err "m.key.verification.request is a to-device event"
        | None ->
            let* c = dec Ev.Key_verification_request_content.jsont in
            let* transaction =
              transaction_of ~room_id:None
                ~transaction_id:
                  (Ev.Key_verification_request_content.transaction_id c)
                ~relates_to:None
            in
            Ok { transaction; payload = Request c })
    | "m.room.message" ->
        if
          msgtype_of json
          <> Some Ev.Key_verification_request_message_content.msgtype
        then err "m.room.message is not a key verification request"
        else
          let* c = dec Ev.Key_verification_request_message_content.jsont in
          (* The flow id of an in-room request is the event id of the
             message itself, which the caller reads from the timeline. *)
          (match (room_id, event_id) with
            | Some room_id, Some event_id ->
                Ok (Transaction.in_room ~room_id ~event_id)
            | _ ->
                err
                  "an in-room verification request needs its room id and event \
                   id")
          |> Result.map (fun transaction ->
              { transaction; payload = Request_in_room c })
    | "m.key.verification.ready" ->
        let* c = dec Ev.Key_verification_ready_content.jsont in
        addressed
          ~transaction_id:(Ev.Key_verification_ready_content.transaction_id c)
          ~relates_to:(Ev.Key_verification_ready_content.relates_to c)
          (Ready c)
    | "m.key.verification.start" ->
        let* c = dec Ev.Key_verification_start_content.jsont in
        addressed
          ~transaction_id:(Ev.Key_verification_start_content.transaction_id c)
          ~relates_to:(Ev.Key_verification_start_content.relates_to c)
          (Start c)
    | "m.key.verification.accept" ->
        let* c = dec Ev.Key_verification_accept_content.jsont in
        addressed
          ~transaction_id:(Ev.Key_verification_accept_content.transaction_id c)
          ~relates_to:(Ev.Key_verification_accept_content.relates_to c)
          (Accept c)
    | "m.key.verification.key" ->
        let* c = dec Ev.Key_verification_key_content.jsont in
        addressed
          ~transaction_id:(Ev.Key_verification_key_content.transaction_id c)
          ~relates_to:(Ev.Key_verification_key_content.relates_to c)
          (Key c)
    | "m.key.verification.mac" ->
        let* c = dec Ev.Key_verification_mac_content.jsont in
        addressed
          ~transaction_id:(Ev.Key_verification_mac_content.transaction_id c)
          ~relates_to:(Ev.Key_verification_mac_content.relates_to c)
          (Mac c)
    | "m.key.verification.done" ->
        let* c = dec Ev.Key_verification_done_content.jsont in
        addressed
          ~transaction_id:(Ev.Key_verification_done_content.transaction_id c)
          ~relates_to:(Ev.Key_verification_done_content.relates_to c)
          (Done c)
    | "m.key.verification.cancel" ->
        let* c = dec Ev.Key_verification_cancel_content.jsont in
        addressed
          ~transaction_id:(Ev.Key_verification_cancel_content.transaction_id c)
          ~relates_to:(Ev.Key_verification_cancel_content.relates_to c)
          (Cancel c)
    | t -> err "not a verification event type: %s" t

  let of_string ~event_type ?room_id ?event_id s =
    match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s with
    | Error m -> Error (`Msg m)
    | Ok json -> of_json ~event_type ?room_id ?event_id json

  let pp ppf t =
    Format.fprintf ppf "%s(%a)" (event_type t) Transaction.pp t.transaction

  let cancel ?reason transaction code =
    let transaction_id = Transaction.transaction_id transaction in
    let relates_to = Transaction.relates_to transaction in
    let reason = Option.value reason ~default:(Cancel_code.reason code) in
    v transaction
      (Cancel
         (Ev.Key_verification_cancel_content.make ?transaction_id
            ~code:(Cancel_code.to_string code)
            ~reason ?relates_to ()))

  let done_ transaction =
    let transaction_id = Transaction.transaction_id transaction in
    let relates_to = Transaction.relates_to transaction in
    v transaction
      (Done
         (Ev.Key_verification_done_content.make ?transaction_id ?relates_to ()))

  let ready transaction ~from_device ~methods =
    let transaction_id = Transaction.transaction_id transaction in
    let relates_to = Transaction.relates_to transaction in
    v transaction
      (Ready
         (Ev.Key_verification_ready_content.make
            ~from_device:(Id.Device_id.to_string from_device)
            ~methods:(List.map Method.to_string methods)
            ?transaction_id ?relates_to ()))
end

type request = {
  transaction : Transaction.t;
  message : Message.t;
  to_device : To_device.messages;
}

let new_transaction_id ~random =
  Id.Transaction_id.v
    ("m" ^ Matrix_proto.Base64.encode (Random.generate random 16))

let request_to_device ~random ~now ~from_device ?(methods = Method.all)
    ~their_user_id ~devices () =
  let transaction_id = new_transaction_id ~random in
  let content =
    Ev.Key_verification_request_content.make
      ~from_device:(Id.Device_id.to_string from_device)
      ~methods:(List.map Method.to_string methods)
      ~transaction_id ~timestamp:now ()
  in
  let transaction = Transaction.to_device transaction_id in
  let message = Message.v transaction (Message.Request content) in
  let json =
    match Message.to_json message with
    | Ok json -> json
    (* The content was built here from validated values, so the encoder
       has nothing to reject. *)
    | Error (`Msg m) ->
        invalid_arg ("Matrix_client.Verification.request_to_device: " ^ m)
  in
  let to_device = [ (their_user_id, List.map (fun d -> (d, json)) devices) ] in
  { transaction; message; to_device }

let request_in_room ~from_device ?(methods = Method.all) ~their_user_id ?body ()
    =
  let body =
    Option.value body
      ~default:
        (Printf.sprintf "%s is requesting to verify your key, but your client "
           (Id.Device_id.to_string from_device)
        ^ "does not support in-chat key verification.")
  in
  Ev.Key_verification_request_message_content.make ~body
    ~from_device:(Id.Device_id.to_string from_device)
    ~methods:(List.map Method.to_string methods)
    ~to_:(Id.User_id.to_string their_user_id)
    ()

let ready_response ~transaction ~from_device ~our_methods ~their_methods =
  let common = Method.common ~ours:our_methods ~theirs:their_methods in
  if common = [] then Error Cancel_code.Unknown_method
  else Ok (common, Message.ready transaction ~from_device ~methods:common)
