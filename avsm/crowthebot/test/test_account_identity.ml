open Crowthebot
module M = Matrix_eio
module Cs = Matrix_client.Cross_signing
module Id = Matrix_proto.Id

let check label value = if not value then failwith label
let user = Id.User_id.of_string_exn "@crow:example.org"
let device = Id.Device_id.of_string_exn "CROW"
let json codec value = Result.get_ok (Jsont.Json.encode codec value)
let field name json = Option.get (Matrix_proto.Json.find_mem name json)
let decode codec json = Result.get_ok (Jsont.Json.decode codec json)
let obj fields = Jsont.Json.object' (List.map (fun (k,v) ->
    ((k, Jsont.Meta.none), v)) fields)

let () = Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let root = Filename.temp_file "crow-identity-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () ->
    Array.iter (fun name ->
      let dir = Filename.concat root name in
      Array.iter (fun file -> Unix.unlink (Filename.concat dir file))
        (Sys.readdir dir);
      Unix.rmdir dir) (Sys.readdir root);
    Unix.rmdir root) @@ fun () ->
  let dir name =
    let task_dir = Filename.concat root name in
    Unix.mkdir task_dir 0o700;
    Eio.Path.(Eio.Stdenv.fs env / task_dir)
  in
  let random = Matrix_client.Random.of_source (Eio.Stdenv.secure_random env) in
  let enc = M.Encryption.create ~random ~user_id:user ~device_id:device () in
  let keys = ref (M.Encryption.device_keys_for_upload enc) in
  let published = ref None and uploads = ref 0 in
  let require_password = ref false and reject_signature = ref false in
  let body req = match req.Fetch.Middleware.body with
    | Fetch.String s -> Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)
    | _ -> obj [] in
  let fetch = Fetch_mock.client (fun req ->
    let path = Fetch.Middleware.Url.path_and_query req.url in
    let respond value = Fetch_mock.respond
        (Result.get_ok (Jsont_bytesrw.encode_string Jsont.json value)) req in
    if String.ends_with ~suffix:"/keys/query" path then
      let cross get = match !published with
        | None -> obj []
        | Some upload -> obj [(Id.User_id.to_string user,
            json Matrix_client.Keys.cross_signing_key_jsont (get upload))] in
      respond (obj [
        ("failures", obj []);
        ("device_keys", obj [(Id.User_id.to_string user,
           obj [(Id.Device_id.to_string device,
             json Matrix_client.Keys.device_keys_jsont !keys)])]);
        ("master_keys", cross (fun u -> u.Cs.master_key));
        ("self_signing_keys", cross (fun u -> u.Cs.self_signing_key));
        ("user_signing_keys", cross (fun u -> u.Cs.user_signing_key))])
    else if String.ends_with ~suffix:"/keys/device_signing/upload" path then begin
      let request = body req in
      if !require_password && Matrix_proto.Json.find_mem "auth" request = None then
        Fetch_mock.respond ~status:401
          {|{"session":"setup","flows":[{"stages":["m.login.password"]}]}|} req
      else begin
        if !require_password then begin
          let auth = field "auth" request in
          check "UIAA uses server session"
            (Matrix_proto.Json.find_string "session" auth = Some "setup");
          check "UIAA carries the supplied password"
            (Matrix_proto.Json.find_string "password" auth = Some "synthetic")
        end;
        let decode_key name = decode Matrix_client.Keys.cross_signing_key_jsont
            (field name request) in
        published := Some Cs.{master_key = decode_key "master_key";
          self_signing_key = decode_key "self_signing_key";
          user_signing_key = decode_key "user_signing_key"};
        incr uploads;
        respond (obj [])
      end
    end
    else if String.ends_with ~suffix:"/keys/signatures/upload" path then
      if !reject_signature then
        respond (obj [("failures", obj [(Id.User_id.to_string user,
          obj [(Id.Device_id.to_string device, obj [("errcode", Jsont.Json.string "M_FORBIDDEN")])])])])
      else begin
        keys := decode Matrix_client.Keys.device_keys_jsont
          (field (Id.Device_id.to_string device)
             (field (Id.User_id.to_string user) (body req)));
        respond (obj [("failures", obj [])])
      end
    else failwith ("unexpected endpoint " ^ path)) in
  let client = M.Client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn "https://matrix.example.org") ~fetch ()
    |> fun client -> M.Client.with_session client
         Matrix_client.Client.{user_id = user; device_id = device;
           access_token = "synthetic"; refresh_token = None} in
  let setup dir auth_callback = ignore (Account_identity.setup ~dir ~client
      ~encryption:enc ~random ~auth_callback) in
  let expect_failure label fn =
    let failed = try fn (); false with Failure _ | Eio.Io _ -> true in
    check label failed in
  let first = dir "first" in
  setup first (fun _ -> None);
  check "first setup publishes one identity" (!uploads = 1);
  let saved = Eio.Path.load Eio.Path.(first / "cross-signing.json") in
  check "saved keys are private"
    ((Unix.stat (Eio.Path.native_exn Eio.Path.(first / "cross-signing.json"))).st_perm
     land 0o777 = 0o600);
  let upload = Option.get !published in
  check "server device is signed by its owner"
    (Cs.verify_device_signature
       ~self_signing_key:(Cs.key ~role:Cs.Self_signing upload.self_signing_key)
       ~device:(Cs.create_device !keys));
  setup first (fun _ -> failwith "unexpected auth prompt");
  check "repeat setup never rotates the identity" (!uploads = 1
    && Eio.Path.load Eio.Path.(first / "cross-signing.json") = saved);
  let other = dir "other" in
  expect_failure "existing identity is never replaced"
    (fun () -> setup other (fun _ -> None));
  check "refused setup saves no replacement secrets"
    (not (Eio.Path.is_file Eio.Path.(other / "cross-signing.json")));
  let changed = Cs.create_private_identity ~user_id:user in
  Cs.generate_private_keys ~random changed;
  published := Cs.build_upload changed;
  expect_failure "setup cannot overwrite changed remote keys"
    (fun () -> setup first (fun _ -> None));
  check "failure retains the saved identity"
    (Eio.Path.load Eio.Path.(first / "cross-signing.json") = saved);
  published := None;
  keys := M.Encryption.device_keys_for_upload enc;
  require_password := true;
  let interrupted = dir "interrupted" in
  expect_failure "unanswered authentication leaves setup incomplete"
    (fun () -> setup interrupted (fun _ -> None));
  let pending = Eio.Path.load Eio.Path.(interrupted / "cross-signing.json") in
  setup interrupted (fun challenge -> Some (Matrix_client.Uiaa.password_auth
      ~user:(Id.User_id.to_string user) ~password:"synthetic"
      ?session:challenge.session ()));
  check "authenticated retry reuses pending keys"
    (Eio.Path.load Eio.Path.(interrupted / "cross-signing.json") = pending);
  reject_signature := true;
  expect_failure "rejected own-device signature is not accepted"
    (fun () -> setup interrupted (fun _ -> None));
  reject_signature := false;
  setup interrupted (fun _ -> None);
  check "signature upload can be retried"
    (Cs.verify_device_signature
       ~self_signing_key:(Cs.key ~role:Cs.Self_signing
                          (Option.get !published).self_signing_key)
       ~device:(Cs.create_device !keys));
  print_endline "crowthebot: account key ownership, persistence, UIAA and self-signing passed"
