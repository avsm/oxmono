module M = Matrix_eio
module Base = Matrix_client.Base_client
module Id = Matrix_proto.Id
module Log = Diagnostics.Log

type t = {
  client : M.Client.t;
  encryption : M.Encryption.t option;
  store : Store.t;
  state : unit -> Base.state option;
  mutable saved : Id.Room_id.t option;
  mutex : Eio.Mutex.t;
}

let create ~client ?encryption ~store ~state () =
  { client; encryption; store; state; saved = None;
    mutex = Eio.Mutex.create () }

let private_members t room =
  M.Rooms.get_members t.client ~room_id:room ()
  |> List.filter (fun (m : M.Rooms.member) ->
      m.membership = Matrix_proto.Event.Membership.Join
      || m.membership = Matrix_proto.Event.Membership.Invite)
  |> List.map (fun m -> Id.User_id.to_string m.M.Rooms.user_id)
  |> List.sort_uniq String.compare
  |> (=) (List.sort String.compare
            [Id.User_id.to_string (M.Client.user_id t.client); Store.admin t.store])

let room t =
  Persistence.locked t.mutex (fun () ->
    let candidates = match t.state () with
      | None -> []
      | Some state ->
          Base.rooms_with state Base.Joined
          |> List.filter (fun (r : Base.room_info) ->
              (Option.is_some t.encryption = Option.is_some r.encryption)
              && (r.is_dm
                  || Store.direct_peer t.store (Id.Room_id.to_string r.room_id)
                     = Some (Store.admin t.store)
                  || List.mem (Id.User_id.of_string_exn (Store.admin t.store))
                       (Base.members state r.room_id)))
          |> List.map (fun (r : Base.room_info) -> r.room_id)
    in
    let candidates = match t.saved with
      | None -> candidates
      | Some room -> room :: List.filter ((<>) room) candidates
    in
    let admin = Id.User_id.of_string_exn (Store.admin t.store) in
    let room = match List.find_opt (private_members t) candidates with
      | Some room -> room
      | None ->
          let room = M.Rooms.create t.client ~preset:M.Rooms.Private_chat
              ~is_direct:true ~invite:[admin] ~encrypted:(t.encryption <> None) () in
          (match Matrix_client.Account_data.mark_as_dm (M.Client.base t.client)
                   ~user_id:admin ~room_id:room with
           | Ok () -> ()
           | Error _ -> Log.warn (fun m -> m "Could not mark admin room as DM"));
          room
    in
    t.saved <- Some room;
    Store.add_direct_room t.store ~room:(Id.Room_id.to_string room)
      ~peer:(Store.admin t.store);
    room)

let send t text =
  let room = room t in
  let event = match t.encryption with
    | None -> M.Messages.send_notice t.client ~room_id:room ~body:text
    | Some encryption ->
        let content = Jsont.Json.object'
            [ (("msgtype", Jsont.Meta.none), Jsont.Json.string "m.notice");
              (("body", Jsont.Meta.none), Jsont.Json.string text) ] in
        M.Encryption.send_encrypted encryption t.client room
          ~event_type:"m.room.message" ~content
          ~members:[M.Client.user_id t.client;
                    Id.User_id.of_string_exn (Store.admin t.store)]
  in
  Log.info (fun m -> m "Admin onboarding notice sent room=%S event=%S"
      (Id.Room_id.to_string room) (Id.Event_id.to_string event))
