let send_event client ~room_id ~event_type ~content =
  Error.unwrap ~context:"sending room event"
    (Matrix_client.Messages.send_event (Client.base client) ~room_id ~event_type
       ~content)

let send_text client ~room_id ~body ?format ?formatted_body ?extra_content () =
  Error.unwrap ~context:"sending text message"
    (Matrix_client.Messages.send_text (Client.base client) ~room_id ~body
       ?format ?formatted_body ?extra_content ())

let send_emote ?extra_content client ~room_id ~body =
  Error.unwrap ~context:"sending emote message"
    (Matrix_client.Messages.send_emote (Client.base client) ~room_id ~body
       ?extra_content)

let send_notice ?extra_content client ~room_id ~body =
  Error.unwrap ~context:"sending notice message"
    (Matrix_client.Messages.send_notice (Client.base client) ~room_id ~body
       ?extra_content)

let send_image client ~room_id ~body ~url ?info ?extra_content () =
  Error.unwrap ~context:"sending image message"
    (Matrix_client.Messages.send_image (Client.base client) ~room_id ~body ~url
       ?info ?extra_content ())

let send_file client ~room_id ~body ~url ?info ?extra_content () =
  Error.unwrap ~context:"sending file message"
    (Matrix_client.Messages.send_file (Client.base client) ~room_id ~body ~url
       ?info ?extra_content ())

let redact client ~room_id ~event_id ?reason () =
  Error.unwrap ~context:"redacting room event"
    (Matrix_client.Messages.redact (Client.base client) ~room_id ~event_id
       ?reason ())

type messages_response = Matrix_client.Messages.messages_response = {
  page : Matrix_proto.Event.Raw_event.t Matrix_proto.Common.Page.t;
  state : Matrix_proto.Event.Raw_event.t list;
}

let get_messages client ~room_id ?from ?dir ?limit ?filter () =
  Error.unwrap ~context:"getting room messages"
    (Matrix_client.Messages.get_messages (Client.base client) ~room_id ?from
       ?dir ?limit ?filter ())

type context = Matrix_client.Messages.context = {
  event : Matrix_proto.Event.Raw_event.t;
  events_before : Matrix_proto.Event.Raw_event.t list;
  events_after : Matrix_proto.Event.Raw_event.t list;
  prev_batch : string option;
  next_batch : string option;
  state : Matrix_proto.Event.Raw_event.t list;
}

let get_context client ~room_id ~event_id ?limit () =
  Error.unwrap ~context:"getting event context"
    (Matrix_client.Messages.get_context (Client.base client) ~room_id ~event_id
       ?limit ())

let get_event client ~room_id ~event_id =
  Error.unwrap ~context:"getting room event"
    (Matrix_client.Messages.get_event (Client.base client) ~room_id ~event_id)
