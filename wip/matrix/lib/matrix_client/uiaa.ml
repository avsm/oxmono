let ( let* ) = Result.bind

type auth_type =
  | Password
  | Recaptcha
  | OAuth2
  | OAuth
  | Email_identity
  | Msisdn
  | Dummy
  | Registration_token
  | Terms
  | Sso
  | Sso_fallback
  | Custom of string

let auth_type_of_string = function
  | "m.login.password" -> Password
  | "m.login.recaptcha" -> Recaptcha
  | "m.login.oauth2" -> OAuth2
  | "m.oauth" -> OAuth
  | "m.login.email.identity" -> Email_identity
  | "m.login.msisdn" -> Msisdn
  | "m.login.dummy" -> Dummy
  | "m.login.registration_token" -> Registration_token
  | "m.login.terms" -> Terms
  | "m.login.sso" -> Sso
  | "org.matrix.login.sso.fallback" -> Sso_fallback
  | s -> Custom s

let auth_type_to_string = function
  | Password -> "m.login.password"
  | Recaptcha -> "m.login.recaptcha"
  | OAuth2 -> "m.login.oauth2"
  | OAuth -> "m.oauth"
  | Email_identity -> "m.login.email.identity"
  | Msisdn -> "m.login.msisdn"
  | Dummy -> "m.login.dummy"
  | Registration_token -> "m.login.registration_token"
  | Terms -> "m.login.terms"
  | Sso -> "m.login.sso"
  | Sso_fallback -> "org.matrix.login.sso.fallback"
  | Custom s -> s

type auth_flow = { stages : auth_type list }
type auth_flow_json = { stages_json : string list }

let auth_flow_jsont =
  let json_type =
    Jsont.Object.(
      map (fun stages_json -> { stages_json })
      |> mem "stages"
           (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.stages_json)
      |> finish)
  in
  Jsont.map
    ~dec:(fun flow ->
      { stages = List.map auth_type_of_string flow.stages_json })
    ~enc:(fun flow ->
      { stages_json = List.map auth_type_to_string flow.stages })
    json_type

type uiaa_response = {
  session : string option;
  flows : auth_flow list;
  completed : auth_type list;
  params : Jsont.json option;
  error : string option;
  errcode : string option;
}

type uiaa_response_json = {
  session_json : string option;
  flows_json : auth_flow list;
  completed_json : string list;
  params_json : Jsont.json option;
  error_json : string option;
  errcode_json : string option;
}

let uiaa_response_jsont =
  let json_type =
    Jsont.Object.(
      map
        (fun
          session_json
          flows_json
          completed_json
          params_json
          error_json
          errcode_json
        ->
          {
            session_json;
            flows_json;
            completed_json;
            params_json;
            error_json;
            errcode_json;
          })
      |> opt_mem "session" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.session_json)
      |> mem "flows"
           (Jsont.list auth_flow_jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.flows_json)
      |> mem "completed"
           (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.completed_json)
      |> opt_mem "params" Matrix_proto.Json.Codec.json ~enc:(fun t ->
          t.params_json)
      |> opt_mem "error" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.error_json)
      |> opt_mem "errcode" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.errcode_json)
      |> finish)
  in
  Jsont.map
    ~dec:(fun r ->
      {
        session = r.session_json;
        flows = r.flows_json;
        completed = List.map auth_type_of_string r.completed_json;
        params = r.params_json;
        error = r.error_json;
        errcode = r.errcode_json;
      })
    ~enc:(fun r ->
      {
        session_json = r.session;
        flows_json = r.flows;
        completed_json = List.map auth_type_to_string r.completed;
        params_json = r.params;
        error_json = r.error;
        errcode_json = r.errcode;
      })
    json_type

type auth_data =
  | Password_auth of {
      identifier : user_identifier;
      password : string;
      session : string option;
    }
  | Recaptcha_auth of { response : string; session : string option }
  | Email_identity_auth of {
      threepid_creds : threepid_creds;
      session : string option;
    }
  | Msisdn_auth of { threepid_creds : threepid_creds; session : string option }
  | Dummy_auth of { session : string option }
  | Token_auth of { token : string; session : string option }
  | OAuth_auth of { session : string option }
  | Terms_auth of { session : string option }

and user_identifier =
  | User of string
  | ThirdParty of { medium : string; address : string }
  | Phone of { country : string; phone : string }

and threepid_creds = {
  sid : string;
  client_secret : string;
  id_server : string option;
  id_access_token : string option;
}

let jname n = Jsont.Json.name n
let jmem n v = Jsont.Json.mem (jname n) v
let jstr n s = jmem n (Jsont.Json.string s)
let jobj mems = Jsont.Json.object' mems
let jopt_str n = function None -> [] | Some s -> [ jstr n s ]

(* [Jsont.json]'s encoder cannot fail on a value this module built, so the
   error branch is unreachable; it stays total rather than raising. *)
let json_to_string (j : Jsont.json) =
  match Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json j with
  | Ok s -> s
  | Error _ -> "{}"

let user_identifier_json = function
  | User user_id -> jobj [ jstr "type" "m.id.user"; jstr "user" user_id ]
  | ThirdParty { medium; address } ->
      jobj
        [
          jstr "type" "m.id.thirdparty";
          jstr "medium" medium;
          jstr "address" address;
        ]
  | Phone { country; phone } ->
      jobj
        [ jstr "type" "m.id.phone"; jstr "country" country; jstr "phone" phone ]

let user_identifier_to_json id = json_to_string (user_identifier_json id)

let threepid_creds_json (creds : threepid_creds) =
  jobj
    ([ jstr "sid" creds.sid; jstr "client_secret" creds.client_secret ]
    @ jopt_str "id_server" creds.id_server
    @ jopt_str "id_access_token" creds.id_access_token)

let auth_data_json = function
  | Password_auth { identifier; password; session } ->
      jobj
        ([
           jstr "type" "m.login.password";
           jmem "identifier" (user_identifier_json identifier);
           jstr "password" password;
         ]
        @ jopt_str "session" session)
  | Recaptcha_auth { response; session } ->
      jobj
        ([ jstr "type" "m.login.recaptcha"; jstr "response" response ]
        @ jopt_str "session" session)
  | Email_identity_auth { threepid_creds = creds; session } ->
      jobj
        ([
           jstr "type" "m.login.email.identity";
           jmem "threepid_creds" (threepid_creds_json creds);
         ]
        @ jopt_str "session" session)
  | Msisdn_auth { threepid_creds = creds; session } ->
      jobj
        ([
           jstr "type" "m.login.msisdn";
           jmem "threepid_creds" (threepid_creds_json creds);
         ]
        @ jopt_str "session" session)
  | Dummy_auth { session } ->
      jobj ([ jstr "type" "m.login.dummy" ] @ jopt_str "session" session)
  | Token_auth { token; session } ->
      jobj
        ([ jstr "type" "m.login.registration_token"; jstr "token" token ]
        @ jopt_str "session" session)
  | OAuth_auth { session } ->
      jobj ([ jstr "type" "m.oauth" ] @ jopt_str "session" session)
  | Terms_auth { session } ->
      jobj ([ jstr "type" "m.login.terms" ] @ jopt_str "session" session)

let auth_data_to_json auth = json_to_string (auth_data_json auth)

type 'a uiaa_result =
  | Uiaa_success of 'a
  | Uiaa_auth_required of uiaa_response
  | Uiaa_error of Error.t

let parse_uiaa_response body =
  match Client.Http.decode_response uiaa_response_jsont body with
  | Ok r -> Some r
  | Error _ -> None

let password_auth ~user ~password ?session () =
  Password_auth { identifier = User user; password; session }

let dummy_auth ?session () = Dummy_auth { session }
let recaptcha_auth ~response ?session () = Recaptcha_auth { response; session }

let email_identity_auth ~sid ~client_secret ?id_server ?id_access_token ?session
    () =
  Email_identity_auth
    {
      threepid_creds = { sid; client_secret; id_server; id_access_token };
      session;
    }

let msisdn_auth ~sid ~client_secret ?id_server ?id_access_token ?session () =
  Msisdn_auth
    {
      threepid_creds = { sid; client_secret; id_server; id_access_token };
      session;
    }

let token_auth ~token ?session () = Token_auth { token; session }
let oauth_auth ?session () = OAuth_auth { session }
let terms_auth ?session () = Terms_auth { session }

let flow_contains_only flow types =
  List.for_all (fun stage -> List.mem stage types) flow.stages

let has_dummy_flow uiaa =
  List.exists (fun flow -> flow_contains_only flow [ Dummy ]) uiaa.flows

let with_uiaa ~make_request ~auth_callback =
  match make_request None with
  | Ok result -> Uiaa_success result
  | Error e -> (
      match e with
      | Error.Http_error { status = 401; body; _ } -> (
          match parse_uiaa_response body with
          | Some uiaa -> (
              match auth_callback uiaa with
              | Some auth_data -> (
                  match make_request (Some (auth_data_to_json auth_data)) with
                  | Ok result -> Uiaa_success result
                  | Error e2 -> (
                      match e2 with
                      | Error.Http_error { status = 401; body = body2; _ } -> (
                          match parse_uiaa_response body2 with
                          | Some uiaa2 -> Uiaa_auth_required uiaa2
                          | None -> Uiaa_error e2)
                      | _ -> Uiaa_error e2))
              | None -> Uiaa_auth_required uiaa)
          | None -> Uiaa_error e)
      | _ -> Uiaa_error e)

let add_auth_to_body ~body ~auth =
  let decode s = Client.Http.decode_response Matrix_proto.Json.Codec.json s in
  match (decode body, decode auth) with
  | Ok (Jsont.Object (mems, _)), Ok auth ->
      let mems = Jsont.Json.remove_mem "auth" mems in
      Ok (json_to_string (jobj (jmem "auth" auth :: mems)))
  | Ok _, Ok _ -> Error (Error.Json_error "request body is not a JSON object")
  | Error error, _ | _, Error error -> Error error

type request_token_response = { sid : string; submit_url : string option }

let request_token_response_jsont =
  Jsont.Object.(
    map (fun sid submit_url -> { sid; submit_url })
    |> mem "sid" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.sid)
    |> opt_mem "submit_url" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.submit_url)
    |> finish)

type token_use = Bind | Register | Password

let path_of_use use medium =
  match use with
  | Bind -> "/account/3pid/" ^ medium ^ "/requestToken"
  | Register -> "/register/" ^ medium ^ "/requestToken"
  | Password -> "/account/password/" ^ medium ^ "/requestToken"

(* Binding an identifier to the logged-in account needs the access token.
   Creating an account has none, and a forgotten password cannot produce one
   either. *)
let post_of_use = function
  | Bind -> Client.Http.post
  | Register | Password -> Client.Http.post_unauthenticated

type email_token_request = {
  client_secret : string;
  email : string;
  send_attempt : int;
  next_link : string option;
  id_server : string option;
  id_access_token : string option;
}

let email_token_request_jsont =
  Jsont.Object.(
    map ~kind:"email_token_request"
      (fun
        client_secret email send_attempt next_link id_server id_access_token ->
        {
          client_secret;
          email;
          send_attempt;
          next_link;
          id_server;
          id_access_token;
        })
    |> mem "client_secret" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : email_token_request) -> t.client_secret)
    |> mem "email" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.email)
    |> mem "send_attempt" Matrix_proto.Json.Codec.int
         ~enc:(fun (t : email_token_request) -> t.send_attempt)
    |> opt_mem "next_link" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : email_token_request) -> t.next_link)
    |> opt_mem "id_server" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : email_token_request) -> t.id_server)
    |> opt_mem "id_access_token" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : email_token_request) -> t.id_access_token)
    |> finish)

type msisdn_token_request = {
  client_secret : string;
  country : string;
  phone_number : string;
  send_attempt : int;
  next_link : string option;
  id_server : string option;
  id_access_token : string option;
}

let msisdn_token_request_jsont =
  Jsont.Object.(
    map ~kind:"msisdn_token_request"
      (fun
        client_secret
        country
        phone_number
        send_attempt
        next_link
        id_server
        id_access_token
      ->
        {
          client_secret;
          country;
          phone_number;
          send_attempt;
          next_link;
          id_server;
          id_access_token;
        })
    |> mem "client_secret" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : msisdn_token_request) -> t.client_secret)
    |> mem "country" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.country)
    |> mem "phone_number" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.phone_number)
    |> mem "send_attempt" Matrix_proto.Json.Codec.int
         ~enc:(fun (t : msisdn_token_request) -> t.send_attempt)
    |> opt_mem "next_link" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : msisdn_token_request) -> t.next_link)
    |> opt_mem "id_server" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : msisdn_token_request) -> t.id_server)
    |> opt_mem "id_access_token" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : msisdn_token_request) -> t.id_access_token)
    |> finish)

let request_email_token client ?(use = Bind) ~email ~client_secret ~send_attempt
    ?next_link ?id_server ?id_access_token () =
  let request =
    {
      client_secret;
      email;
      send_attempt;
      next_link;
      id_server;
      id_access_token;
    }
  in
  let* body = Client.Http.encode_body email_token_request_jsont request in
  let* body =
    (post_of_use use) client ~path:(path_of_use use "email") ~body ()
  in
  Client.Http.decode_response request_token_response_jsont body

let request_msisdn_token client ?(use = Bind) ~country ~phone_number
    ~client_secret ~send_attempt ?next_link ?id_server ?id_access_token () =
  let request =
    {
      client_secret;
      country;
      phone_number;
      send_attempt;
      next_link;
      id_server;
      id_access_token;
    }
  in
  let* body = Client.Http.encode_body msisdn_token_request_jsont request in
  let* body =
    (post_of_use use) client ~path:(path_of_use use "msisdn") ~body ()
  in
  Client.Http.decode_response request_token_response_jsont body

let validate_email_token client ~sid ~client_secret ~token =
  let path = "/account/3pid/email/validate" in
  let body =
    json_to_string
      (jobj
         [
           jstr "sid" sid;
           jstr "client_secret" client_secret;
           jstr "token" token;
         ])
  in
  let* (_ : string) = Client.Http.post client ~path ~body () in
  Ok ()
