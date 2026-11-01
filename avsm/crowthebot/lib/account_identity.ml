module M = Matrix_eio
module Cs = Matrix_client.Cross_signing
module Key = Matrix_client.Crypto_key
module Id = Matrix_proto.Id

let filename = "cross-signing.json"

type seeds = { user : string; master : string; self : string; signing : string }

let jsont =
  Jsont.Object.map (fun user master self signing -> {user; master; self; signing})
  |> Jsont.Object.mem "user" Jsont.string ~enc:(fun t -> t.user)
  |> Jsont.Object.mem "master" Jsont.string ~enc:(fun t -> t.master)
  |> Jsont.Object.mem "self_signing" Jsont.string ~enc:(fun t -> t.self)
  |> Jsont.Object.mem "user_signing" Jsont.string ~enc:(fun t -> t.signing)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let read dir client =
  let path = Eio.Path.(dir / filename) in
  if not (Eio.Path.is_file path) then None
  else
    let text = Profile.read_secret (Eio.Path.native_exn path) in
    let seeds = match Jsont_bytesrw.decode_string jsont text with
      | Ok seeds -> seeds
      | Error _ -> failwith "invalid local cross-signing keys"
    in
    let user_id = M.Client.user_id client in
    if seeds.user <> Id.User_id.to_string user_id then
      failwith "local cross-signing keys belong to another account";
    match Cs.private_identity_of_secrets_unchecked ~user_id
            ~master:seeds.master ~self_signing:seeds.self
            ~user_signing:seeds.signing with
    | Ok identity -> Some identity
    | Error _ -> failwith "invalid local cross-signing keys"

let save dir identity =
  let seed get =
    Base64.encode_exn (Key.Ed25519.Private.to_bytes (Option.get (get identity)))
  in
  let seeds = {user = Id.User_id.to_string (Cs.identity_user_id identity);
               master = seed Cs.master_secret; self = seed Cs.self_signing_secret;
               signing = seed Cs.user_signing_secret} in
  let text = Result.get_ok (Jsont_bytesrw.encode_string ~format:Jsont.Minify
                             jsont seeds) in
  M.Error.unwrap ~context:"saving Crow's private account keys"
    (Matrix_client.Profile_store.atomic_write ~path:Eio.Path.(dir / filename)
       ~data:text)

let query client encryption =
  let response = M.Keys.query_keys client ~users:[(M.Client.user_id client, [])] () in
  if response.failures <> [] then failwith "cross-signing key query failed";
  let device = M.Client.device_id client in
  if not (List.exists (fun (u, devices) ->
      Id.User_id.equal u (M.Client.user_id client)
      && List.exists (fun (d, _) -> Id.Device_id.equal d device) devices)
      response.device_keys) then
    failwith "the server did not return Crow's saved device keys";
  M.Encryption.receive_keys_query encryption response;
  response

let published response user =
  [ List.assoc_opt user response.M.Keys.master_keys;
    List.assoc_opt user response.M.Keys.self_signing_keys;
    List.assoc_opt user response.M.Keys.user_signing_keys ]

let check identity keys ~complete =
  let upload = Option.get (Cs.build_upload identity) in
  let expected = [upload.master_key; upload.self_signing_key; upload.user_signing_key] in
  List.iter2 (fun expected actual ->
    match actual with
    | Some actual when actual.M.Keys.user_id = expected.M.Keys.user_id
                       && actual.usage = expected.usage
                       && actual.keys = expected.keys -> ()
    | None when not complete -> ()
    | _ -> failwith
        "published cross-signing identity differs from Crow's saved keys")
    expected keys

let setup ~dir ~client ~encryption ~random ~auth_callback =
  let user = M.Client.user_id client in
  let keys = published (query client encryption) user in
  let identity = match read dir client with
    | Some identity -> check identity keys ~complete:false; identity
    | None ->
        if List.exists Option.is_some keys then
          failwith
            "Crow already has a cross-signing identity. Recover it with \
             --recovery-key-file instead of replacing it";
        let identity = Cs.create_private_identity ~user_id:user in
        Cs.generate_private_keys ~random identity;
        (* Retain the generated seeds before any remote mutation, so a failed
           upload can be resumed with the same identity. *)
        save dir identity;
        identity
  in
  if List.exists Option.is_none keys then begin
    let upload = Option.get (Cs.build_upload identity) in
    match M.Keys.upload_signing_keys_uiaa client
            ~master_key:upload.master_key ~self_signing_key:upload.self_signing_key
            ~user_signing_key:upload.user_signing_key ~auth_callback () with
    | None -> ()
    | Some _ -> failwith "cross-signing setup needs authentication. Run crowthebot setup-verification with this profile"
  end;
  check identity (published (query client encryption) user) ~complete:true;
  M.Secrets.sign_own_device client ~encryption identity;
  identity
