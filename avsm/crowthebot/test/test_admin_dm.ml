open Crowthebot
module M = Matrix_eio
module Id = Matrix_proto.Id

let check label value = if not value then failwith label
let admin = "@admin:example.org"
let self = Id.User_id.of_string_exn "@crow:example.org"
let headers = Http.Header.of_list [("Content-Type", "application/json")]
let member user membership = Printf.sprintf
    {|{"type":"m.room.member","state_key":"%s","sender":"%s","origin_server_ts":0,"content":{"membership":"%s"}}|}
    user user membership

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let store = Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin in
  let creations = ref [] and sent = ref [] and compromised = ref false in
  let members_calls = ref 0 in
  let fetch = Fetch_mock.client (fun (req : Fetch.Middleware.request) ->
    let path = Fetch.Middleware.Url.path_and_query req.url in
    let body = match req.body with Fetch.String s -> s | _ -> "" in
    let json = match List.rev (String.split_on_char '/' path) with
      | "createRoom" :: _ ->
          creations := body :: !creations;
          Printf.sprintf {|{"room_id":"!new%d:example.org"}|}
            (List.length !creations)
      | "members" :: _ ->
          incr members_calls;
          let others = if !compromised then
              [member "@guest:example.org" "join"] else [] in
          Printf.sprintf {|{"chunk":[%s]}|} (String.concat ","
            (member (Id.User_id.to_string self) "join"
             :: member admin "invite" :: others))
      | "m.direct" :: "account_data" :: _ -> "{}"
      | _txn :: "m.room.message" :: "send" :: room :: _ ->
          sent := (room, body) :: !sent;
          {|{"event_id":"$notice"}|}
      | _ -> failwith ("unexpected endpoint " ^ path)
    in
    Fetch_mock.respond ~headers json req) in
  let client = M.Client.create ~env ~sw
      ~homeserver:(Uriz.of_string_exn "https://matrix.example.org") ~fetch ()
    |> fun c -> M.Client.with_session c Matrix_client.Client.{
      user_id = self; device_id = Id.Device_id.of_string_exn "CROW";
      access_token = "synthetic"; refresh_token = None } in
  let t = Admin_dm.create ~client ~store ~state:(fun () -> None) () in
  Admin_dm.send t "Approve the group";
  check "new DM created" (List.length !creations = 1);
  let fields = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json
                               (List.hd !creations)) in
  let get name json = match json with
    | Jsont.Object (xs, _) -> List.find_map (fun ((k,_),v) ->
        if k = name then Some v else None) xs
    | _ -> None in
  check "room is a DM" (get "is_direct" fields = Some (Jsont.Json.bool true));
  check "only admin invited"
    (get "invite" fields = Some (Jsont.Json.list [Jsont.Json.string admin]));
  check "DM is invite only"
    (get "preset" fields = Some (Jsont.Json.string "private_chat"));
  check "local DM remembered"
    (Store.direct_peer store "!new1:example.org" = Some admin);
  Admin_dm.send t "Second notice";
  check "existing DM rechecked before reuse"
    (!members_calls = 1 && List.length !creations = 1);
  compromised := true;
  Admin_dm.send t "Third notice";
  check "expanded DM replaced" (List.length !creations = 2);
  check "notice never goes to expanded DM"
    (fst (List.hd !sent) = "!new2:example.org");
  compromised := false;
  let response = Printf.sprintf
      {|{"next_batch":"one","rooms":{"join":{"!old:example.org":{"state":{"events":[%s,%s]},"timeline":{"events":[],"limited":false}}}}}|}
      (member (Id.User_id.to_string self) "join") (member admin "join") in
  let initial = Matrix_client.Base_client.create ~user_id:self () in
  let state, _ = Matrix_client.Base_client.apply initial
      (match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont response with
       | Ok response -> response | Error message -> failwith message) in
  let existing = Admin_dm.create ~client ~store ~state:(fun () -> Some state) () in
  check "existing admin pair reused"
    (Id.Room_id.to_string (Admin_dm.room existing) = "!old:example.org");
  let encryption = M.Encryption.create
      ~random:(Matrix_client.Random.of_source (Eio.Stdenv.secure_random env))
      ~user_id:self ~device_id:(Id.Device_id.of_string_exn "CROW") () in
  let encrypted = Admin_dm.create ~client ~encryption ~store
      ~state:(fun () -> Some state) () in
  let before = !members_calls in
  ignore (Admin_dm.room encrypted);
  check "plaintext room excluded for encrypted profile" (!members_calls = before);
  let fields = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json
                               (List.hd !creations)) in
  check "new encrypted DM includes encryption state"
    (get "initial_state" fields <> None);
  print_endline "crowthebot: private admin DM creation, reuse and membership passed"
