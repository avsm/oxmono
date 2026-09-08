let src = Logs.Src.create "matrix.auth" ~doc:"Matrix authentication"

module Log = (val Logs.src_log src : Logs.LOG)

type login_flow = Password | Token | Sso | Unknown of string

let login_flow_of_string = function
  | "m.login.password" -> Password
  | "m.login.token" -> Token
  | "m.login.sso" -> Sso
  | s -> Unknown s

let login_flow_to_string = function
  | Password -> "m.login.password"
  | Token -> "m.login.token"
  | Sso -> "m.login.sso"
  | Unknown s -> s

let login_flow_jsont =
  Jsont.of_of_string ~kind:"login_flow" ~enc:login_flow_to_string (fun s ->
      Ok (login_flow_of_string s))

let login_flow_obj_jsont =
  Jsont.Object.map ~kind:"login_flow_object" (fun flow_type -> flow_type)
  |> Jsont.Object.mem "type" login_flow_jsont
  |> Jsont.Object.finish

let login_flows_response_jsont =
  Jsont.Object.map ~kind:"login_flows_response" (fun flows -> flows)
  |> Jsont.Object.mem "flows" (Jsont.list login_flow_obj_jsont)
  |> Jsont.Object.finish

let get_login_flows client =
  match Client.Http.get client ~path:"/login" () with
  | Error e -> Error e
  | Ok body -> Client.Http.decode_response login_flows_response_jsont body

type login_params = {
  device_id : string option;
  initial_device_display_name : string option;
}

let default_login_params =
  { device_id = None; initial_device_display_name = None }

(* The request records below are encode-only: their fields are set and
   serialised, never read back, which is what the warning 69 suppression
   acknowledges. *)
type login_request = {
  req_type : string;
  identifier : login_identifier;
  password : string option;
  token : string option;
  device_id : string option;
  initial_device_display_name : string option;
  request_refresh_token : bool option;
}
[@@warning "-69"]

and login_identifier = { id_type : string; user : string option }
[@@warning "-69"]

let login_identifier_jsont =
  Jsont.Object.(
    map ~kind:"login_identifier" (fun id_type user -> { id_type; user })
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.id_type)
    |> opt_mem "user" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.user)
    |> finish)

let login_request_jsont =
  Jsont.Object.(
    map ~kind:"login_request"
      (fun
        req_type
        identifier
        password
        token
        device_id
        initial_device_display_name
        request_refresh_token
      ->
        {
          req_type;
          identifier;
          password;
          token;
          device_id;
          initial_device_display_name;
          request_refresh_token;
        })
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.req_type)
    |> mem "identifier" login_identifier_jsont ~enc:(fun t -> t.identifier)
    |> opt_mem "password" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.password)
    |> opt_mem "token" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.token)
    |> opt_mem "device_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.device_id)
    |> opt_mem "initial_device_display_name" Matrix_proto.Json.Codec.string
         ~enc:(fun t -> t.initial_device_display_name)
    |> opt_mem "refresh_token" Jsont.bool ~enc:(fun t ->
        t.request_refresh_token)
    |> finish)

type login_response = {
  user_id : Matrix_proto.Id.User_id.t;
  access_token : string;
  device_id : Matrix_proto.Id.Device_id.t;
  refresh_token : string option;
  expires_in_ms : int option;
}

let login_response_jsont =
  Jsont.Object.map ~kind:"login_response"
    (fun user_id access_token device_id refresh_token expires_in_ms ->
      { user_id; access_token; device_id; refresh_token; expires_in_ms })
  |> Jsont.Object.mem "user_id" Matrix_proto.Id.User_id.jsont
  |> Jsont.Object.mem "access_token" Matrix_proto.Json.Codec.string
  |> Jsont.Object.mem "device_id" Matrix_proto.Id.Device_id.jsont
  |> Jsont.Object.opt_mem "refresh_token" Matrix_proto.Json.Codec.string
  |> Jsont.Object.opt_mem "expires_in_ms" Matrix_proto.Json.Codec.int
  |> Jsont.Object.finish

type login = { session : Client.session; expires_at : Ptime.t option }

let expires_at_of_ms = function
  | None -> None
  | Some ms ->
      Option.bind
        (Ptime.Span.of_float_s (float ms /. 1000.))
        (Ptime.add_span (Ptime_clock.now ()))

let response_to_login resp =
  {
    session =
      {
        Client.user_id = resp.user_id;
        access_token = resp.access_token;
        device_id = resp.device_id;
        refresh_token = resp.refresh_token;
      };
    expires_at = expires_at_of_ms resp.expires_in_ms;
  }

let login_password_with_expiry client ~user ~password
    ?(params = default_login_params) ?(request_refresh_token = false) () =
  Log.info (fun m -> m "Logging in as %s" user);
  let request =
    {
      req_type = "m.login.password";
      identifier = { id_type = "m.id.user"; user = Some user };
      password = Some password;
      token = None;
      device_id = params.device_id;
      initial_device_display_name = params.initial_device_display_name;
      request_refresh_token =
        (if request_refresh_token then Some true else None);
    }
  in
  match Client.Http.encode_body login_request_jsont request with
  | Error e -> Error e
  | Ok body -> (
      match Client.Http.post_unauthenticated client ~path:"/login" ~body () with
      | Error e ->
          Log.err (fun m -> m "Login failed for user %s" user);
          Error e
      | Ok body -> (
          match Client.Http.decode_response login_response_jsont body with
          | Error e -> Error e
          | Ok resp ->
              Log.info (fun m ->
                  m "Login successful: user_id=%s device_id=%s"
                    (Matrix_proto.Id.User_id.to_string resp.user_id)
                    (Matrix_proto.Id.Device_id.to_string resp.device_id));
              Ok (response_to_login resp)))

let login_password client ~user ~password ?params ?request_refresh_token () =
  Result.map
    (fun login -> login.session)
    (login_password_with_expiry client ~user ~password ?params
       ?request_refresh_token ())

let login_token_with_expiry client ~token ?(params = default_login_params)
    ?(request_refresh_token = false) () =
  let request =
    {
      req_type = "m.login.token";
      identifier = { id_type = "m.id.user"; user = None };
      password = None;
      token = Some token;
      device_id = params.device_id;
      initial_device_display_name = params.initial_device_display_name;
      request_refresh_token =
        (if request_refresh_token then Some true else None);
    }
  in
  match Client.Http.encode_body login_request_jsont request with
  | Error e -> Error e
  | Ok body -> (
      match Client.Http.post_unauthenticated client ~path:"/login" ~body () with
      | Error e -> Error e
      | Ok body -> (
          match Client.Http.decode_response login_response_jsont body with
          | Error e -> Error e
          | Ok resp -> Ok (response_to_login resp)))

let login_token client ~token ?params ?request_refresh_token () =
  Result.map
    (fun login -> login.session)
    (login_token_with_expiry client ~token ?params ?request_refresh_token ())

type refresh_request = { refresh_token : string } [@@warning "-69"]

let refresh_request_jsont =
  Jsont.Object.(
    map ~kind:"refresh_request" (fun refresh_token -> { refresh_token })
    |> mem "refresh_token" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.refresh_token)
    |> finish)

type refreshed = { access_token : string; refresh_token : string option }

type refreshed_with_expiry = {
  refreshed : refreshed;
  expires_at : Ptime.t option;
}

let refreshed_jsont =
  Jsont.Object.map ~kind:"refreshed" (fun access_token refresh_token ->
      { access_token; refresh_token })
  |> Jsont.Object.mem "access_token" Matrix_proto.Json.Codec.string
  |> Jsont.Object.opt_mem "refresh_token" Matrix_proto.Json.Codec.string
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let refreshed_with_expiry_jsont =
  Jsont.Object.map ~kind:"refreshed_with_expiry"
    (fun access_token refresh_token expires_in_ms ->
      {
        refreshed = { access_token; refresh_token };
        expires_at =
          Option.bind expires_in_ms (fun ms ->
              Option.bind
                (Ptime.Span.of_float_s (float ms /. 1000.))
                (Ptime.add_span (Ptime_clock.now ())));
      })
  |> Jsont.Object.mem "access_token" Matrix_proto.Json.Codec.string
  |> Jsont.Object.opt_mem "refresh_token" Matrix_proto.Json.Codec.string
  |> Jsont.Object.opt_mem "expires_in_ms" Matrix_proto.Json.Codec.int
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let refresh_token client ~refresh_token =
  let request = { refresh_token } in
  match Client.Http.encode_body refresh_request_jsont request with
  | Error e -> Error e
  | Ok body -> (
      match
        Client.Http.post_unauthenticated client ~path:"/refresh" ~body ()
      with
      | Error e -> Error e
      | Ok body -> Client.Http.decode_response refreshed_jsont body)

let refresh_token_with_expiry client ~refresh_token =
  let request = { refresh_token } in
  match Client.Http.encode_body refresh_request_jsont request with
  | Error e -> Error e
  | Ok body -> (
      match
        Client.Http.post_unauthenticated client ~path:"/refresh" ~body ()
      with
      | Error e -> Error e
      | Ok body -> Client.Http.decode_response refreshed_with_expiry_jsont body)

let logout client =
  match Client.Http.post client ~path:"/logout" ~body:"{}" () with
  | Error e -> Error e
  | Ok _ -> Ok ()

let logout_all client =
  match Client.Http.post client ~path:"/logout/all" ~body:"{}" () with
  | Error e -> Error e
  | Ok _ -> Ok ()

type registration_kind = User | Guest

type register_request = {
  username : string option;
  password : string option;
  device_id : string option;
  initial_device_display_name : string option;
  inhibit_login : bool option;
}

let register_request_jsont =
  Jsont.Object.(
    map ~kind:"register_request"
      (fun
        username password device_id initial_device_display_name inhibit_login ->
        {
          username;
          password;
          device_id;
          initial_device_display_name;
          inhibit_login;
        })
    |> opt_mem "username" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.username)
    |> opt_mem "password" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.password)
    |> opt_mem "device_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.device_id)
    |> opt_mem "initial_device_display_name" Matrix_proto.Json.Codec.string
         ~enc:(fun t -> t.initial_device_display_name)
    |> opt_mem "inhibit_login" Jsont.bool ~enc:(fun t -> t.inhibit_login)
    |> finish)

type register_response = {
  user_id : Matrix_proto.Id.User_id.t;
  access_token : string option;
  device_id : string option;
  refresh_token : string option;
}

let register_response_jsont =
  Jsont.Object.map ~kind:"register_response"
    (fun user_id access_token device_id refresh_token ->
      { user_id; access_token; device_id; refresh_token })
  |> Jsont.Object.mem "user_id" Matrix_proto.Id.User_id.jsont
  |> Jsont.Object.opt_mem "access_token" Matrix_proto.Json.Codec.string
  |> Jsont.Object.opt_mem "device_id" Matrix_proto.Json.Codec.string
  |> Jsont.Object.opt_mem "refresh_token" Matrix_proto.Json.Codec.string
  |> Jsont.Object.finish

let register_query = function
  | Some Guest -> Some [ ("kind", "guest") ]
  | Some User | None -> None

let registration_body ?username ?password ?(params = default_login_params)
    ?inhibit_login () =
  let request =
    {
      username;
      password;
      device_id = params.device_id;
      initial_device_display_name = params.initial_device_display_name;
      inhibit_login;
    }
  in
  Client.Http.encode_body register_request_jsont request

let register_request client ~query ~body ~auth_json =
  let body =
    match auth_json with
    | None -> Ok body
    | Some auth -> Uiaa.add_auth_to_body ~body ~auth
  in
  Result.bind body (fun body ->
      Client.Http.post_unauthenticated client ~path:"/register" ?query ~body ())

let decode_register_response body =
  match Client.Http.decode_response register_response_jsont body with
  | Error e -> Error e
  | Ok resp -> (
      match (resp.access_token, resp.device_id) with
      | Some access_token, Some device_id -> (
          match Matrix_proto.Id.Device_id.of_string device_id with
          | Error (`Msg msg) -> Error (Error.Json_error msg)
          | Ok device_id ->
              Ok
                {
                  Client.user_id = resp.user_id;
                  access_token;
                  device_id;
                  refresh_token = resp.refresh_token;
                })
      | _ -> Error Error.No_content)

let register client ?kind ?username ?password ?(params = default_login_params)
    ?inhibit_login ?auth () =
  let query = register_query kind in
  match registration_body ?username ?password ~params ?inhibit_login () with
  | Error e -> Error e
  | Ok body -> (
      let auth_json = Option.map Uiaa.auth_data_to_json auth in
      match register_request client ~query ~body ~auth_json with
      | Error e -> Error e
      | Ok body -> decode_register_response body)

let register_uiaa client ?kind ?username ?password
    ?(params = default_login_params) ?inhibit_login ~auth_callback () =
  let query = register_query kind in
  match registration_body ?username ?password ~params ?inhibit_login () with
  | Error e -> Uiaa.Uiaa_error e
  | Ok body ->
      Uiaa.with_uiaa ~auth_callback ~make_request:(fun auth_json ->
          match register_request client ~query ~body ~auth_json with
          | Error e -> Error e
          | Ok body -> decode_register_response body)

type whoami_response = { user_id : Matrix_proto.Id.User_id.t }

let whoami_response_jsont =
  Jsont.Object.map ~kind:"whoami_response" (fun user_id -> { user_id })
  |> Jsont.Object.mem "user_id" Matrix_proto.Id.User_id.jsont
  |> Jsont.Object.finish

let whoami client =
  match Client.Http.get client ~path:"/account/whoami" () with
  | Error e -> Error e
  | Ok body -> (
      match Client.Http.decode_response whoami_response_jsont body with
      | Error e -> Error e
      | Ok resp -> Ok resp.user_id)

open Result.Syntax

type token_login = { login_token : string; expires_in_ms : int }

let token_login_jsont =
  Jsont.Object.(
    map ~kind:"login_token" (fun login_token expires_in_ms ->
        { login_token; expires_in_ms })
    |> mem "login_token" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : token_login) -> t.login_token)
    |> mem "expires_in_ms" Matrix_proto.Json.Codec.int ~enc:(fun t ->
        t.expires_in_ms)
    |> finish)

let get_login_token client ?auth () =
  (* MSC3882 used an unstable prefix before the endpoint became stable in
     Matrix 1.7.  Select from /versions before sending the UIAA-bearing POST;
     this also avoids retrying a single-use operation after an error. *)
  let* versions = Server.get_versions client in
  let path =
    if Server.supports_version_at_least versions ~major:1 ~minor:7 then
      "/_matrix/client/v1/login/get_token"
    else "/_matrix/client/unstable/org.matrix.msc3882/login/get_token"
  in
  let* body =
    match auth with
    | None -> Ok "{}"
    | Some auth ->
        Uiaa.add_auth_to_body ~body:"{}" ~auth:(Uiaa.auth_data_to_json auth)
  in
  let* body =
    Client.Http.post_bytes client ~path ~content_type:"application/json" ~body
      ()
  in
  Client.Http.decode_response token_login_jsont body

type available_response = { available : bool }

let available_response_jsont =
  Jsont.Object.(
    map ~kind:"available_response" (fun available -> { available })
    |> mem "available" Jsont.bool
         ~dec_absent:(fun () -> true)
         ~enc:(fun t -> t.available)
    |> finish)

let register_available client ~username =
  let query = [ ("username", username) ] in
  let* body = Client.Http.get client ~path:"/register/available" ~query () in
  let+ resp = Client.Http.decode_response available_response_jsont body in
  resp.available

type validity_response = { valid : bool }

let validity_response_jsont =
  Jsont.Object.(
    map ~kind:"validity_response" (fun valid -> { valid })
    |> mem "valid" Jsont.bool ~enc:(fun t -> t.valid)
    |> finish)

let check_registration_token client ~token =
  let path =
    "/_matrix/client/v1/register/m.login.registration_token/validity"
  in
  let query = [ ("token", token) ] in
  let* body, _content_type = Client.Http.get_bytes client ~path ~query () in
  let+ resp = Client.Http.decode_response validity_response_jsont body in
  resp.valid
