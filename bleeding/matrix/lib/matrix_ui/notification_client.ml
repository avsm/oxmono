module Event = Matrix_proto.Event
module Id = Matrix_proto.Id
module Base_client = Matrix_client.Base_client
module Encryption = Matrix_client.Encryption
module Push_evaluator = Matrix_client.Push_evaluator

type decrypt =
  Id.Room_id.t ->
  Event.Raw_event.t ->
  (Event.Raw_event.t, Encryption.decrypt_error) result

type notification_event = {
  event : Event_cache.event;
  notification : Push_evaluator.notification;
}

type status =
  | Event of notification_event
  | Event_filtered_out of Event_cache.event
  | Event_redacted of Event_cache.event
  | Event_not_found
  | Unable_to_decrypt of {
      event : Event_cache.event;
      error : Encryption.decrypt_error option;
    }

type t = {
  client : Matrix_client.Client.t;
  cache : Event_cache.t;
  state : unit -> Base_client.state;
  decrypt : decrypt option;
  fetched : (string, Event_cache.event) Hashtbl.t;
}

let create ~client ~cache ~state ?decrypt () =
  { client; cache; state; decrypt; fetched = Hashtbl.create 16 }

let decrypted_raw (encrypted : Event.Raw_event.t)
    (decrypted : Encryption.decrypted_event) =
  {
    encrypted with
    type_ = Event.Event_type.of_string decrypted.decrypted_type;
    content = decrypted.decrypted_content;
    room_id = Some decrypted.decrypted_room_id;
    sender = decrypted.decrypted_sender;
  }

let decrypt_with encryption room_id event =
  Matrix_eio.Encryption.decrypt_room_event encryption room_id event
  |> Result.map (decrypted_raw event)

let key room_id event_id =
  Id.Room_id.to_string room_id ^ "\x00" ^ Id.Event_id.to_string event_id

let find_cached cache room_id event_id =
  Array.find_opt
    (fun (event : Event_cache.event) ->
      Option.equal Id.Event_id.equal event.event.event_id (Some event_id))
    (Event_cache.snapshot cache room_id)

let is_redacted (event : Event.Raw_event.t) =
  Option.exists
    (fun unsigned -> Option.is_some (Event.Unsigned.redacted_because unsigned))
    event.unsigned

let is_ignored state sender =
  match Base_client.find_account_data state "m.ignored_user_list" with
  | None -> false
  | Some content -> (
      match
        Option.bind
          (Matrix_proto.Json.find_mem "ignored_users" content)
          Matrix_proto.Json.as_object
      with
      | None -> false
      | Some users ->
          let sender = Id.User_id.to_string sender in
          List.exists (fun ((name, _), _) -> String.equal name sender) users)

let evaluate t room_id event =
  let state = t.state () in
  if is_ignored state (Event_cache.effective event).sender then
    Event_filtered_out event
  else
    let notification =
      Push_evaluator.notification_for_event
        (Base_client.ruleset state)
        (Base_client.push_context state room_id)
        (Event_cache.effective event)
    in
    if notification.notify then Event { event; notification }
    else Event_filtered_out event

let resolve t room_id ~shared event =
  if is_redacted event.Event_cache.event then Event_redacted event
  else
    match
      ( event.clear_event,
        Event.Event_type.equal event.event.type_
          Event.Event_type.Room_message_encrypted )
    with
    | Some _, _ | None, false -> evaluate t room_id event
    | None, true -> (
        match t.decrypt with
        | None -> Unable_to_decrypt { event; error = None }
        | Some decrypt -> (
            match decrypt room_id event.event with
            | Error error -> Unable_to_decrypt { event; error = Some error }
            | Ok clear_event ->
                let event = { event with clear_event = Some clear_event } in
                if shared then
                  ignore
                    (Event_cache.set_decrypted t.cache room_id
                       ~encrypted:event.event ~plaintext:clear_event);
                evaluate t room_id event))

let fetch t room_id event_id =
  let event_key = key room_id event_id in
  match find_cached t.cache room_id event_id with
  | Some event ->
      Hashtbl.remove t.fetched event_key;
      Ok (resolve t room_id ~shared:true event)
  | None -> (
      match Hashtbl.find_opt t.fetched event_key with
      | Some event -> Ok (resolve t room_id ~shared:false event)
      | None -> (
          match
            Matrix_client.Messages.get_context t.client ~room_id ~event_id
              ~limit:0 ()
          with
          | Error
              (Matrix_client.Error.Matrix_error
                 { errcode = Matrix_client.Error.M_NOT_FOUND; _ }) ->
              Ok Event_not_found
          | Error error -> Error error
          | Ok context ->
              let wire = context.event in
              if
                not
                  (Option.equal Id.Event_id.equal wire.event_id (Some event_id))
              then
                Error
                  (Matrix_client.Error.Json_error
                     "notification /context returned a different target event")
              else
                let event =
                  {
                    Event_cache.stable_id = Id.Event_id.to_string event_id;
                    event = wire;
                    clear_event = None;
                    delivery = Event_cache.Synced;
                  }
                in
                Hashtbl.replace t.fetched event_key event;
                Ok (resolve t room_id ~shared:false event)))
