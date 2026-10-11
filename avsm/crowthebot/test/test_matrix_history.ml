open Crowthebot
module M = Matrix_eio
module Id = Matrix_proto.Id
let check label value = if not value then failwith label
let user = "@admin:example.org"
let self = "@crow:example.org"
let room = "!room:example.org"
let uid = Id.User_id.of_string_exn
let rid = Id.Room_id.of_string_exn room
let obj fields = Jsont.Json.object' (List.map (fun (k,v) ->
    ((k, Jsont.Meta.none), v)) fields)
let str = Jsont.Json.string
let encode j = Result.get_ok (Jsont_bytesrw.encode_string Jsont.json j)
let decode text = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json text)
let field name j = Option.get (Matrix_proto.Json.find_mem name j)
let string name j = Option.get (Matrix_proto.Json.find_string name j)
let array name j = Result.get_ok (Jsont.Json.decode (Jsont.list Jsont.json) (field name j))
let event id typ content = obj [("event_id", str id); ("sender", str user);
    ("origin_server_ts", Jsont.Json.int 1); ("type", str typ); ("content", content)]
let text id body = event id "m.room.message" (obj [("msgtype", str "m.text"); ("body", str body)])
let membership who = obj [("event_id", str ("$" ^ who)); ("sender", str who);
    ("origin_server_ts", Jsont.Json.int 1); ("type", str "m.room.member");
    ("state_key", str who); ("content", obj [("membership", str "join")])]
let page ?next ?(start = "start") events = obj
    ([ ("start", str start); ("chunk", Jsont.Json.list events) ]
     @ Option.fold ~none:[] ~some:(fun n -> [("end", str n)]) next)

let () = Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let store = Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin:user in
  let state, _ = Matrix_client.Base_client.apply
      (Matrix_client.Base_client.create ~user_id:(uid self) ())
      (Result.get_ok (Jsont.Json.decode Matrix_proto.Sync.Response.jsont
        (decode {|{"next_batch":"sync","rooms":{"join":{"!room:example.org":{"state":{"events":[{"type":"m.room.canonical_alias","state_key":"","sender":"@admin:example.org","event_id":"$alias","origin_server_ts":1,"content":{"alias":"#workshop:example.org"}}]},"timeline":{"events":[]}}}}}|}))) in
  let random = Matrix_client.Random.of_source (Eio.Stdenv.secure_random env) in
  let encryption = M.Encryption.create ~random ~user_id:(uid self)
      ~device_id:(Id.Device_id.of_string_exn "CROW") () in
  let cipher = Result.get_ok (Matrix_client.Encryption.encrypt_room_event
      (M.Encryption.machine encryption) rid ~event_type:"m.room.message"
      ~content:(obj [("msgtype", str "m.text"); ("body", str "encrypted history")])
      ~members:[uid self]) |> fst in
  let encrypted = obj [("event_id", str "$encrypted"); ("sender", str self);
      ("origin_server_ts", Jsont.Json.int 2); ("type", str "m.room.encrypted");
      ("content", cipher)] in
  let other = M.Encryption.create ~random ~user_id:(uid user)
      ~device_id:(Id.Device_id.of_string_exn "OTHER") () in
  let missing_cipher = Result.get_ok (Matrix_client.Encryption.encrypt_room_event
      (M.Encryption.machine other) rid ~event_type:"m.room.message"
      ~content:(obj [("msgtype", str "m.text"); ("body", str "not available")])
      ~members:[uid user]) |> fst in
  let undecryptable = event "$missing" "m.room.encrypted" missing_cipher in
  let members = ref [membership user; membership self] in
  let pages = ref [] and fetched = ref [] in
  let fetch = Fetch_mock.client (fun req ->
    let path = Fetch.Middleware.Url.path_and_query req.url in
    check "history requests never write"
      (req.meth = `GET);
    if String.ends_with ~suffix:"/members" path then
      Fetch_mock.respond (encode (obj [("chunk", Jsont.Json.list !members)])) req
    else begin
      fetched := path :: !fetched;
      match !pages with
      | p :: rest -> pages := rest; Fetch_mock.respond (encode p) req
      | [] -> failwith "unexpected history request"
    end) in
  let client = M.Client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn "https://matrix.example.org") ~fetch ()
    |> fun c -> M.Client.with_session c Matrix_client.Client.{
      user_id = uid self; device_id = Id.Device_id.of_string_exn "CROW";
      access_token = "synthetic"; refresh_token = None} in
  let tools = Matrix_rooms.create ~store ~self ~state:(fun () -> Some state)
      ~client ~encryption () in
  let query ?(actor = user) args = Matrix_rooms.invoke tools ~actor ~room
      "matrix_history" args in
  let read args = decode (Result.get_ok (query args)) in
  pages := [page ~next:"older" [text "$new" "newest"]; page ~start:"older" [encrypted]];
  let first = read {|{"room":"#workshop:example.org"}|} in
  check "alias resolved in history output" (string "name" first = "#workshop:example.org");
  check "cursor returned" (string "next_before" first = "older");
  check "message metadata and text returned"
    (string "event" (List.hd (array "messages" first)) = "$new"
     && string "body" (List.hd (array "messages" first)) = "newest");
  let second = read {|{"before":"older"}|} in
  check "encrypted history decrypts with existing keys"
    (string "body" (List.hd (array "messages" second)) = "encrypted history");
  check "last page has no cursor" (field "next_before" second = Jsont.Json.null ());
  check "cursor sent to the server"
    (List.exists (fun path ->
      List.assoc_opt "from" (Uriz.query_params
        (Uriz.of_string_exn ("https://matrix.example.org" ^ path))) = Some (Some "older"))
      !fetched);
  let before = List.length !fetched in
  Store.set_person store ~actor:user ~user:"@friend:example.org" ~role:Friend ~allowed:true;
  check "friend absent from the room cannot read history"
    (Result.is_error (query ~actor:"@friend:example.org" "{}"));
  check "membership rejection fetches no messages" (List.length !fetched = before);
  check "unknown room is refused" (Result.is_error (query {|{"room":"!other:example.org"}|}));
  check "invalid limits are refused" (Result.is_error (query {|{"limit":6}|}));
  check "unknown options are refused" (Result.is_error (query {|{"delete":true}|}));
  members := [membership self];
  check "a requester who left is refused" (Result.is_error (query "{}"));
  members := [membership user; membership self];
  pages := [page ~next:"empty-next" []; page ~next:"last" [undecryptable]];
  let empty = read "{}" in
  check "empty filtered pages retain continuation"
    (array "messages" empty = [] && string "next_before" empty = "empty-next");
  let missing = read {|{"before":"empty-next"}|} in
  check "missing keys are explicit" (string "kind"
      (List.hd (array "messages" missing)) = "unable_to_decrypt");
  pages := [page ~start:"repeat" ~next:"repeat" []];
  check "repeated cursor terminates" (field "next_before"
      (read {|{"before":"repeat"}|}) = Jsont.Json.null ());
  let edit = text "$edit" (String.make 10000 'x') in
  let content = field "content" edit in
  let edit = obj [("event_id", str "$edit"); ("sender", str user);
      ("origin_server_ts", Jsont.Json.int 3); ("type", str "m.room.message");
      ("content", obj [("msgtype", str "m.text"); ("body", field "body" content);
         ("m.relates_to", obj [("rel_type", str "m.replace"); ("event_id", str "$new")])])] in
  pages := [page [edit]];
  let output = read "{}" in
  let message = List.hd (array "messages" output) in
  check "long bodies are explicitly truncated"
    (Matrix_proto.Json.find_bool "truncated" message = Some true
     && String.length (encode output) <= 3900);
  check "edit targets preserved"
    (string "target" (field "relation" message) = "$new");
  print_endline "crowthebot: paginated history, encrypted excerpts and current membership passed"
