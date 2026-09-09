module M = Matrix_eio
module V = M.Verification_service
module Id = Matrix_proto.Id
module Cs = Matrix_client.Cross_signing

let check name value = if not value then failwith name

let obj fields =
  Jsont.Json.object'
    (List.map (fun (name, value) -> ((name, Jsont.Meta.none), value)) fields)

let str = Jsont.Json.string
let arr = Jsont.Json.list

let fields = function
  | Jsont.Object (fields, _) -> List.map (fun ((k, _), v) -> (k, v)) fields
  | _ -> []

let field name json = List.assoc_opt name (fields json)
let encode j = Result.get_ok (Jsont_bytesrw.encode_string Jsont.json j)
let decode s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)
let uid = Id.User_id.of_string_exn
let did = Id.Device_id.of_string_exn
let crow = uid "@crow:example.org"
let admin = uid "@admin:example.org"

(* Both real sync loops and SAS services communicate only through this server.
   There is no Matrix account or network access in these tests. *)
let scenario env ~cross_sign ~accept_request ~match_sas =
  Eio.Switch.run @@ fun sw ->
  let device_keys = Hashtbl.create 2 in
  let inbox = Hashtbl.create 2 in
  let signatures = ref [] in
  let random = Matrix_client.Random.of_source (Eio.Stdenv.secure_random env) in
  let identity user =
    let identity = Cs.create_private_identity ~user_id:user in
    Cs.generate_private_keys ~random identity;
    (identity, Option.get (Cs.build_upload identity))
  in
  let crow_identity, crow_upload = identity crow in
  let admin_identity, admin_upload = identity admin in
  let cross =
    [
      (Id.User_id.to_string crow, crow_upload);
      (Id.User_id.to_string admin, admin_upload);
    ]
  in
  let query () =
    let cross_keys get =
      if not cross_sign then obj []
      else
        obj
          (List.map
             (fun (user, upload) ->
               ( user,
                 Result.get_ok
                   (Jsont.Json.encode Matrix_client.Keys.cross_signing_key_jsont
                      (get upload)) ))
             cross)
    in
    obj
      [
        ( "device_keys",
          obj
            (Hashtbl.fold
               (fun user key acc -> (user, obj [ ("DEVICE", key) ]) :: acc)
               device_keys []) );
        ("master_keys", cross_keys (fun u -> u.Cs.master_key));
        ("self_signing_keys", cross_keys (fun u -> u.Cs.self_signing_key));
        ("user_signing_keys", cross_keys (fun u -> u.Cs.user_signing_key));
      ]
  in
  let handler user (req : Fetch.Middleware.request) =
    let user = Id.User_id.to_string user in
    let body = match req.body with Fetch.String s -> decode s | _ -> obj [] in
    let path = Fetch.Middleware.Url.path_and_query req.url in
    let path = List.hd (String.split_on_char '?' path) in
    let result =
      match List.rev (String.split_on_char '/' path) with
      | "upload" :: "keys" :: _ ->
          Option.iter
            (fun key -> Hashtbl.replace device_keys user key)
            (field "device_keys" body);
          obj
            [
              ( "one_time_key_counts",
                obj [ ("signed_curve25519", Jsont.Json.number 50.) ] );
            ]
      | "query" :: "keys" :: _ -> query ()
      | "upload" :: "signatures" :: "keys" :: _ ->
          signatures := body :: !signatures;
          obj [ ("failures", obj []) ]
      | _txn :: event_type :: "sendToDevice" :: _ ->
          let messages =
            Option.value ~default:(obj []) (field "messages" body)
          in
          List.iter
            (fun (recipient, devices) ->
              List.iter
                (fun (_, content) ->
                  let event =
                    obj
                      [
                        ("sender", str user);
                        ("type", str event_type);
                        ("content", content);
                      ]
                  in
                  let queued =
                    Option.value ~default:[] (Hashtbl.find_opt inbox recipient)
                  in
                  Hashtbl.replace inbox recipient (queued @ [ event ]))
                (fields devices))
            (fields messages);
          obj []
      | "sync" :: _ ->
          Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.002;
          let events = Option.value ~default:[] (Hashtbl.find_opt inbox user) in
          Hashtbl.replace inbox user [];
          obj
            [
              ("next_batch", str "next");
              ("to_device", obj [ ("events", arr events) ]);
              ( "device_one_time_keys_count",
                obj [ ("signed_curve25519", Jsont.Json.number 50.) ] );
            ]
      | _ -> failwith ("unexpected Matrix endpoint: " ^ path)
    in
    Fetch_mock.respond
      ~headers:(Http.Header.of_list [ ("Content-Type", "application/json") ])
      (encode result) req
  in
  let peer user =
    let client =
      M.Client.create ~sw ~env
        ~homeserver:(Uriz.of_string_exn "https://matrix.example.org")
        ~fetch:(Fetch_mock.client (handler user))
        ()
    in
    let client =
      M.Client.with_session client
        Matrix_client.Client.
          {
            user_id = user;
            device_id = did "DEVICE";
            access_token = "synthetic";
            refresh_token = None;
          }
    in
    let encryption =
      M.Encryption.create ~random ~user_id:user ~device_id:(did "DEVICE") ()
    in
    M.Encryption.execute_requests encryption client
      (M.Encryption.outgoing_requests encryption);
    (client, encryption)
  in
  let crow_client, crow_enc = peer crow in
  let admin_client, admin_enc = peer admin in
  let crow_prompts = ref [] and admin_prompts = ref [] in
  let run client encryption identity target listen prompts =
    Crowthebot.Verification.run ~env ~client ~encryption
      ?private_identity:(if cross_sign then Some identity else None)
      ~target ~listen
      ~ask:(fun text ->
        prompts := text :: !prompts;
        (* A waiting operator must not prevent the other sync loop advancing. *)
        Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.01;
        if String.starts_with ~prefix:"Accept" text then accept_request
        else match_sas)
      ()
  in
  let a, b =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        Eio.Fiber.pair
          (fun () ->
            run crow_client crow_enc crow_identity admin false crow_prompts)
          (fun () ->
            run admin_client admin_enc admin_identity crow true admin_prompts))
  in
  let verified = function V.Verified _ -> true | _ -> false in
  let cancelled = function V.Cancelled _ -> true | _ -> false in
  if accept_request && match_sas then begin
    check "both clients verified" (verified a && verified b);
    check "one SAS confirmation and one accept"
      (List.length !crow_prompts = 1 && List.length !admin_prompts = 2);
    let sas text =
      String.split_on_char '\n' text
      |> List.filter (String.starts_with ~prefix:"  ")
    in
    let ours = sas (List.hd !crow_prompts) in
    check "all seven emoji agree"
      (List.length ours = 7 && ours = sas (List.hd !admin_prompts));
    if cross_sign then
      check "both cross-signing signatures published"
        (List.length !signatures = 2)
    else check "device verification publishes no signature" (!signatures = [])
  end
  else begin
    check "refusal or mismatch cancels both clients" (cancelled a && cancelled b);
    check "no signatures after refusal" (!signatures = [])
  end

let () =
  Eio_main.run @@ fun env ->
  scenario env ~cross_sign:false ~accept_request:true ~match_sas:true;
  scenario env ~cross_sign:true ~accept_request:true ~match_sas:true;
  scenario env ~cross_sign:true ~accept_request:false ~match_sas:true;
  scenario env ~cross_sign:true ~accept_request:true ~match_sas:false;
  print_endline "crowthebot: SAS, cross-signing, refusal and mismatch passed"
