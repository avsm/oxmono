type envelope = {
  room : Room.t;
  sender : Zulip.Id.User.t;
  event_id : Zulip.Id.Event.t option;
}

type message = {
  envelope : envelope;
  message : Zulip.Message.t;
  flags : Zulip.Message_flag.t list;
  body : string;
}

type command = {
  message : message;
  name : string;
  args : string;
  argv : string list;
}

type edit = {
  envelope : envelope;
  message_id : Zulip.Id.Message.t;
  raw : Jsont.json;
}

type reaction = {
  envelope : envelope;
  message_id : Zulip.Id.Message.t;
  raw : Jsont.json;
}

type delete = {
  room : Room.t option;
  event_id : Zulip.Id.Event.t;
  message_ids : Zulip.Id.Message.t list;
  raw : Jsont.json;
}

type custom = {
  envelope : envelope option;
  event_type : string;
  raw : Jsont.json;
}

type sync = Connecting | Live | Recovering of Zulip_eio.Error.t | Stopped

type malformed = {
  event_type : string;
  raw : Jsont.json;
  error : Zulip.Event_payload.error;
}

type t =
  | Message of message
  | Command of command
  | Edit of edit
  | Reaction of reaction
  | Delete of delete
  | Custom of custom
  | Malformed of malformed
  | Sync of sync

let envelope = function
  | Message m -> Some m.envelope
  | Command c -> Some c.message.envelope
  | Edit e -> Some e.envelope
  | Reaction r -> Some r.envelope
  | Delete _ -> None
  | Custom c -> c.envelope
  | Malformed _ | Sync _ -> None

let room = function
  | Message m -> Some m.envelope.room
  | Command c -> Some c.message.envelope.room
  | Edit e -> Some e.envelope.room
  | Reaction e -> Some e.envelope.room
  | Delete d -> d.room
  | Custom c ->
      Option.map (fun (envelope : envelope) -> envelope.room) c.envelope
  | Malformed _ | Sync _ -> None

let sender event = Option.map (fun envelope -> envelope.sender) (envelope event)
let room_key event = Option.map Room.key (room event)
let reply (envelope : envelope) text = Room.send_text envelope.room text
let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let argv text =
  String.split_on_char ' '
    (String.map (fun c -> if is_space c then ' ' else c) text)
  |> List.filter (fun x -> x <> "")

let command ~prefix message =
  let body = String.trim message.body in
  if not (String.starts_with ~prefix body) then None
  else
    let rest =
      String.trim
        (String.sub body (String.length prefix)
           (String.length body - String.length prefix))
    in
    match argv rest with
    | [] -> None
    | name :: words ->
        let args =
          String.trim
            (String.sub rest (String.length name)
               (String.length rest - String.length name))
        in
        Some { message; name; args; argv = words }

let with_body message ~body = { message with body }

let destination context message_id =
  match Context_runtime.find_destination context message_id with
  | Some destination -> Some destination
  | None -> (
      match Zulip_eio.Messages.get (Context.client context) ~message_id with
      | Error _ -> None
      | Ok message ->
          Context_runtime.remember_message context message;
          Some (Zulip.Message.destination message))

let of_zulip context event =
  let raw = Zulip.Event.data event in
  let event_type = Zulip.Event_type.to_string (Zulip.Event.type_ event) in
  let event_id = Zulip.Event.id event in
  let custom () = Some (Custom { envelope = None; event_type; raw }) in
  let envelope sender destination =
    {
      room = Room.of_destination context destination;
      sender;
      event_id = Some event_id;
    }
  in
  match Zulip.Event_payload.of_event event with
  | Error error -> Some (Malformed { event_type; raw; error })
  | Ok payload -> (
      Context_runtime.observe_payload context payload;
      match payload with
      | Zulip.Event_payload.Message payload ->
          let message = payload.message in
          Context_runtime.remember_message context message;
          Some
            (Message
               {
                 envelope = envelope message.sender_id message.destination;
                 message;
                 flags = payload.flags;
                 body = message.content;
               })
      | Zulip.Event_payload.Message_edit edit -> (
          match (edit.user_id, destination context edit.message_id) with
          | Some sender, Some target ->
              let target =
                match target with
                | Zulip.Message.Direct _ -> target
                | Zulip.Message.Channel channel ->
                    Zulip.Message.Channel
                      {
                        channel with
                        channel_id =
                          Option.value ~default:channel.channel_id
                            edit.new_channel_id;
                        topic = Option.value ~default:channel.topic edit.topic;
                      }
              in
              List.iter
                (fun id ->
                  Context_runtime.remember_destination context id target)
                edit.message_ids;
              Some
                (Edit
                   {
                     envelope = envelope sender target;
                     message_id = edit.message_id;
                     raw;
                   })
          | _ -> custom ())
      | Zulip.Event_payload.Reaction reaction -> (
          match destination context reaction.message_id with
          | Some target ->
              Some
                (Reaction
                   {
                     envelope = envelope reaction.user_id target;
                     message_id = reaction.message_id;
                     raw;
                   })
          | None -> custom ())
      | Zulip.Event_payload.Message_delete deletion -> (
          match
            List.find_map
              (Context_runtime.find_destination context)
              deletion.message_ids
          with
          | Some target ->
              Some
                (Delete
                   {
                     room = Some (Room.of_destination context target);
                     event_id;
                     message_ids = deletion.message_ids;
                     raw;
                   })
          | None -> custom ())
      | _ when Zulip.Event.type_ event = Zulip.Event_type.Heartbeat -> None
      | _ -> custom ())

let payload (custom : custom) =
  Zulip.Event_payload.decode
    (Zulip.Event_type.of_string custom.event_type)
    custom.raw

let of_message context ?flags message =
  Context_runtime.remember_message context message;
  let flags = Option.value ~default:(Zulip.Message.flags message) flags in
  let room = Room.of_destination context (Zulip.Message.destination message) in
  {
    envelope =
      { room; sender = Zulip.Message.sender_id message; event_id = None };
    message;
    flags;
    body = Zulip.Message.content message;
  }

let pp ppf event =
  let kind =
    match event with
    | Message _ -> "message"
    | Command _ -> "command"
    | Edit _ -> "edit"
    | Reaction _ -> "reaction"
    | Delete _ -> "delete"
    | Custom c -> c.event_type
    | Malformed m -> "malformed " ^ m.event_type
    | Sync _ -> "sync"
  in
  Format.pp_print_string ppf kind
