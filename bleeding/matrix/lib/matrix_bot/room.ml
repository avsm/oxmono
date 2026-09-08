module Ui = Matrix_ui
module Id = Matrix_proto.Id

type t = {
  runtime : Ui.Runtime.t;
  client : Matrix_eio.Client.t;
  sender : Sent.Internal.tracker;
  clock : Context.clock;
  encryption : Matrix_eio.Encryption.t option;
  room_id : Id.Room_id.t;
}

let v ~runtime ~client ~sender ~clock ~encryption room_id =
  { runtime; client; sender; clock; encryption; room_id }

let id t = t.room_id
let base t = Matrix_eio.Client.base t.client
let info t = Ui.Room_list.find (Ui.Runtime.room_list t.runtime) t.room_id

let field f default t =
  match info t with Some room -> f room | None -> default

let name t =
  field (fun room -> room.Ui.Room_list.name) (Id.Room_id.to_string t.room_id) t

let topic t = field (fun room -> room.Ui.Room_list.topic) None t
let encrypted t = field (fun room -> room.Ui.Room_list.encrypted) false t
let is_dm t = field (fun room -> room.Ui.Room_list.is_dm) false t

let members t =
  Matrix_eio.Sync_service.members (Ui.Runtime.sync_service t.runtime) t.room_id

let sync_members t =
  let sync = Ui.Runtime.sync_service t.runtime in
  match
    Matrix_client.Base_client.find_room
      (Matrix_eio.Sync_service.state sync)
      t.room_id
  with
  | Some room when room.members_complete -> Ok ()
  | Some _ | None -> (
      match Matrix_client.Rooms.get_members (base t) ~room_id:t.room_id () with
      | Error _ as error -> error
      | Ok members ->
          Matrix_eio.Sync_service.replace_members sync t.room_id members;
          Ok ())

(* Absence of encryption is meaningful only after an unfiltered sync covered
   that state. The same applies to the recipient list: encrypting to a partial
   list would silently exclude devices. *)
let ready_to_send t =
  let sync = Ui.Runtime.sync_service t.runtime in
  match
    Matrix_client.Base_client.find_room
      (Matrix_eio.Sync_service.state sync)
      t.room_id
  with
  | None -> false
  | Some room when not room.encryption_state_complete -> false
  | Some room when Option.is_none room.encryption -> true
  | Some room -> (
      room.members_complete
      && members t <> []
      &&
      match t.encryption with
      | None -> false
      | Some machine ->
          Matrix_eio.Encryption.is_room_encrypted machine t.room_id)

let await_ready_to_send ?(timeout = 120.) t =
  if Float.is_nan timeout || timeout < 0. then
    invalid_arg "Matrix_bot.Room.await_ready_to_send: negative or NaN timeout";
  let deadline = Eio.Time.now t.clock +. timeout in
  let rec loop () =
    if ready_to_send t then true
    else
      let remaining = deadline -. Eio.Time.now t.clock in
      if remaining <= 0. then false
      else (
        Eio.Time.sleep t.clock (Float.min 0.1 remaining);
        loop ())
  in
  loop ()

let timeline t = Ui.Runtime.timeline t.runtime t.room_id
let sent t request = Sent.Internal.v t.sender request

let send ~msgtype t ?formatted ?reply_to body =
  sent t
    (Ui.Room_timeline.send_message (timeline t) ~msgtype ?formatted ?reply_to
       ~body ())

let send_text t ?html ?reply_to body =
  send ~msgtype:`Text t ?formatted:html ?reply_to body

let send_notice t ?html ?reply_to body =
  send ~msgtype:`Notice t ?formatted:html ?reply_to body

let send_emote t body = send ~msgtype:`Emote t body

let react t target key =
  sent t (Ui.Room_timeline.send_reaction (timeline t) ~relates_to:target ~key)

let redact t ?reason event_id =
  sent t (Ui.Room_timeline.redact (timeline t) ~event_id ?reason ())

let set_topic t topic =
  Result.map ignore
    (Matrix_client.State.set_topic (base t) ~room_id:t.room_id ~topic)

let set_name t name =
  Result.map ignore
    (Matrix_client.State.set_name (base t) ~room_id:t.room_id ~name)

let invite t user_id =
  Matrix_client.Rooms.invite (base t) ~room_id:t.room_id ~user_id ()

let kick t ?reason user_id =
  Matrix_client.Rooms.kick (base t) ~room_id:t.room_id ~user_id ?reason ()

let ban t ?reason user_id =
  Matrix_client.Rooms.ban (base t) ~room_id:t.room_id ~user_id ?reason ()

let leave t = Ui.Runtime.leave t.runtime t.room_id

(* The base client folds [m.room.power_levels] as it syncs and hands it out
   through the push context, so a level is a lookup rather than a request. *)
let power_level t user_id =
  let state =
    Matrix_eio.Sync_service.state (Ui.Runtime.sync_service t.runtime)
  in
  match
    Matrix_client.Push_evaluator.Context.power_levels
      (Matrix_client.Base_client.push_context state t.room_id)
  with
  | None -> 0
  | Some levels ->
      Option.value ~default:levels.users_default
        (List.find_map
           (fun (id, level) ->
             if Id.User_id.equal id user_id then Some level else None)
           levels.users)

let backfill t ?(limit = 50) ?(pages = 20) () =
  let timeline = timeline t in
  let rec go remaining =
    if remaining <= 0 then Ok `More
    else
      match Ui.Room_timeline.paginate_back timeline ~limit () with
      | Error _ as error -> error
      | Ok ((`Reached_start | `Nothing_to_do) as done_) -> Ok done_
      | Ok `More -> go (remaining - 1)
  in
  go pages

module Internal = struct
  let v = v
end
