open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

type medium = Email | Msisdn

let medium_to_string = function Email -> "email" | Msisdn -> "msisdn"

let medium_of_string = function
  | "email" -> Ok Email
  | "msisdn" -> Ok Msisdn
  | s -> Error (`Msg (Printf.sprintf "unknown 3PID medium %S" s))

let medium_jsont =
  Jsont.of_of_string ~kind:"medium" ~enc:medium_to_string (fun s ->
      match medium_of_string s with Ok m -> Ok m | Error (`Msg e) -> Error e)

type threepid = {
  medium : medium;
  address : string;
  validated_at : Event.Timestamp.t;
  added_at : Event.Timestamp.t;
}

let threepid_jsont =
  Jsont.Object.(
    map ~kind:"threepid" (fun medium address validated_at added_at ->
        { medium; address; validated_at; added_at })
    |> mem "medium" medium_jsont ~enc:(fun t -> t.medium)
    |> mem "address" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.address)
    |> mem "validated_at" Event.Timestamp.jsont ~enc:(fun t -> t.validated_at)
    |> mem "added_at" Event.Timestamp.jsont ~enc:(fun t -> t.added_at)
    |> finish)

let threepids_response_jsont =
  Jsont.Object.(
    map ~kind:"threepids" Fun.id
    |> mem "threepids"
         (Jsont.list threepid_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:Fun.id
    |> finish)

let get_threepids client =
  let* body = Client.Http.get client ~path:"/account/3pid" () in
  Client.Http.decode_response threepids_response_jsont body

type email_token_request = {
  email : string;
  client_secret : string;
  send_attempt : int;
}

let email_token_request_jsont =
  Jsont.Object.(
    map ~kind:"email_token" (fun email client_secret send_attempt ->
        { email; client_secret; send_attempt })
    |> mem "email" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.email)
    |> mem "client_secret" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.client_secret)
    |> mem "send_attempt" Matrix_proto.Json.Codec.int ~enc:(fun t ->
        t.send_attempt)
    |> finish)

let sid_response_jsont =
  Jsont.Object.(
    map ~kind:"sid" Fun.id
    |> mem "sid" Matrix_proto.Json.Codec.string ~enc:Fun.id
    |> finish)

let request_email_token client ~email ~client_secret ~send_attempt =
  let* body =
    Client.Http.encode_body email_token_request_jsont
      { email; client_secret; send_attempt }
  in
  let* body =
    Client.Http.post client ~path:"/account/3pid/email/requestToken" ~body ()
  in
  Client.Http.decode_response sid_response_jsont body

type msisdn_token_request = {
  country : string;
  phone_number : string;
  m_client_secret : string;
  m_send_attempt : int;
}

let msisdn_token_request_jsont =
  Jsont.Object.(
    map ~kind:"msisdn_token"
      (fun country phone_number m_client_secret m_send_attempt ->
        { country; phone_number; m_client_secret; m_send_attempt })
    |> mem "country" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.country)
    |> mem "phone_number" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.phone_number)
    |> mem "client_secret" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.m_client_secret)
    |> mem "send_attempt" Matrix_proto.Json.Codec.int ~enc:(fun t ->
        t.m_send_attempt)
    |> finish)

let request_msisdn_token client ~country ~phone_number ~client_secret
    ~send_attempt =
  let* body =
    Client.Http.encode_body msisdn_token_request_jsont
      {
        country;
        phone_number;
        m_client_secret = client_secret;
        m_send_attempt = send_attempt;
      }
  in
  let* body =
    Client.Http.post client ~path:"/account/3pid/msisdn/requestToken" ~body ()
  in
  Client.Http.decode_response sid_response_jsont body

type add_threepid_request = { client_secret : string; sid : string }

let add_threepid_request_jsont =
  Jsont.Object.(
    map ~kind:"add_3pid" (fun client_secret sid -> { client_secret; sid })
    |> mem "client_secret" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.client_secret)
    |> mem "sid" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.sid)
    |> finish)

let add_threepid client ~client_secret ~sid =
  let* body =
    Client.Http.encode_body add_threepid_request_jsont { client_secret; sid }
  in
  let+ _ = Client.Http.post client ~path:"/account/3pid/add" ~body () in
  ()

type delete_threepid_request = { d_medium : medium; d_address : string }

let delete_threepid_request_jsont =
  Jsont.Object.(
    map ~kind:"delete_3pid" (fun d_medium d_address -> { d_medium; d_address })
    |> mem "medium" medium_jsont ~enc:(fun t -> t.d_medium)
    |> mem "address" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.d_address)
    |> finish)

let delete_threepid client ~medium ~address =
  let* body =
    Client.Http.encode_body delete_threepid_request_jsont
      { d_medium = medium; d_address = address }
  in
  let+ _ = Client.Http.post client ~path:"/account/3pid/delete" ~body () in
  ()

(* A user-interactive stage is spliced into the body rather than encoded with
   it, so that the same request can be replayed with a different answer. *)
let with_auth body = function
  | None -> Ok body
  | Some auth -> Uiaa.add_auth_to_body ~body ~auth:(Uiaa.auth_data_to_json auth)

type change_password_request = { new_password : string; logout_devices : bool }

let change_password_request_jsont =
  Jsont.Object.(
    map ~kind:"change_password" (fun new_password logout_devices ->
        { new_password; logout_devices })
    |> mem "new_password" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.new_password)
    |> mem "logout_devices" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.logout_devices)
    |> finish)

let change_password client ~new_password ?(logout_devices = false) ?auth () =
  let* body =
    Client.Http.encode_body change_password_request_jsont
      { new_password; logout_devices }
  in
  let* body = with_auth body auth in
  let+ _ = Client.Http.post client ~path:"/account/password" ~body () in
  ()

let deactivate_request_jsont =
  Jsont.Object.(
    map ~kind:"deactivate" Fun.id
    |> mem "erase" Jsont.bool ~dec_absent:(fun () -> false) ~enc:Fun.id
    |> finish)

let deactivate client ?(erase = false) ?auth () =
  let* body = Client.Http.encode_body deactivate_request_jsont erase in
  let* body = with_auth body auth in
  let+ _ = Client.Http.post client ~path:"/account/deactivate" ~body () in
  ()

(* The whole [m.ignored_user_list] object is rewritten on every change. Its
   members are user ids mapped to an object the specification leaves empty and
   reserves. *)
let ignored_users_jsont =
  Jsont.Object.(
    map ~kind:"m.ignored_user_list" Fun.id
    |> mem "ignored_users"
         (Json_codec.keyed_map ~what:"user id" ~of_string:Id.User_id.of_string
            ~to_string:Id.User_id.to_string Matrix_proto.Json.Codec.json)
         ~dec_absent:(fun () -> [])
         ~enc:Fun.id
    |> finish)

let get_ignored_users client =
  match
    Account_data.get client ~event_type:Event.Event_type.Ignored_user_list
  with
  | Error (Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ }) -> Ok []
  | Error e -> Error e
  | Ok json -> (
      match Jsont.Json.decode ignored_users_jsont json with
      | Ok members -> Ok (List.map fst members)
      | Error e -> Error (Error.Json_error e))

let set_ignored_users client users =
  let members = List.map (fun u -> (u, Jsont.Json.object' [])) users in
  match Jsont.Json.encode ignored_users_jsont members with
  | Error e -> Error (Error.Json_error e)
  | Ok content ->
      Account_data.set client ~event_type:Event.Event_type.Ignored_user_list
        ~content

let ignore_user client ~user_id =
  let* current = get_ignored_users client in
  if List.exists (Id.User_id.equal user_id) current then Ok ()
  else set_ignored_users client (user_id :: current)

let unignore_user client ~user_id =
  let* current = get_ignored_users client in
  set_ignored_users client
    (List.filter (fun u -> not (Id.User_id.equal user_id u)) current)
