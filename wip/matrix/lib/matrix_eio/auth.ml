type login_params = Matrix_client.Auth.login_params = {
  device_id : string option;
  initial_device_display_name : string option;
}

let default_login_params = Matrix_client.Auth.default_login_params

type login_flow = Matrix_client.Auth.login_flow =
  | Password
  | Token
  | Sso
  | Unknown of string

let get_login_flows client =
  Error.unwrap ~context:"getting login flows"
    (Matrix_client.Auth.get_login_flows (Client.base client))

let login_password_with_expiry client ~user ~password
    ?(params = default_login_params) ?request_refresh_token () =
  let login =
    Error.unwrap ~context:"logging in with password"
      (Matrix_client.Auth.login_password_with_expiry (Client.base client) ~user
         ~password ~params ?request_refresh_token ())
  in
  (Client.with_session client login.session, login.expires_at)

let login_password client ~user ~password ?params ?request_refresh_token () =
  fst
    (login_password_with_expiry client ~user ~password ?params
       ?request_refresh_token ())

let login_token_with_expiry client ~token ?(params = default_login_params)
    ?request_refresh_token () =
  let login =
    Error.unwrap ~context:"logging in with token"
      (Matrix_client.Auth.login_token_with_expiry (Client.base client) ~token
         ~params ?request_refresh_token ())
  in
  (Client.with_session client login.session, login.expires_at)

let login_token client ~token ?params ?request_refresh_token () =
  fst (login_token_with_expiry client ~token ?params ?request_refresh_token ())

let refresh_token client ~refresh_token =
  Error.unwrap ~context:"refreshing login token"
    (Matrix_client.Auth.refresh_token (Client.base client) ~refresh_token)

let refresh_token_with_expiry client ~refresh_token =
  Error.unwrap ~context:"refreshing login token with expiry"
    (Matrix_client.Auth.refresh_token_with_expiry (Client.base client)
       ~refresh_token)

let logout client =
  Error.unwrap ~context:"logging out"
    (Matrix_client.Auth.logout (Client.base client))

let logout_session ?http client (auth : Matrix_client.Session.Auth.t) =
  match auth.method_ with
  | Matrix_client.Session.Auth.Matrix -> logout client
  | Matrix_client.Session.Auth.OAuth { client_id } ->
      Error.with_context "logging out an OAuth session" (fun () ->
          let http = Option.value ~default:(Client.http client) http in
          let metadata = Oauth.Metadata.fetch ~http client in
          Oauth.Metadata.validate metadata;
          let tokens : Oauth.Token.t =
            {
              access_token = auth.access_token;
              token_type = "Bearer";
              refresh_token = auth.refresh_token;
              scope = None;
              expires_at = auth.access_token_expires_at;
            }
          in
          Oauth.Token.logout ~http client metadata ~client_id tokens)

let logout_all client =
  Error.unwrap ~context:"logging out all sessions"
    (Matrix_client.Auth.logout_all (Client.base client))

type registration_kind = Matrix_client.Auth.registration_kind = User | Guest

let register client ?kind ?username ?password ?params ?inhibit_login ?auth () =
  let session =
    Error.unwrap ~context:"registering account"
      (Matrix_client.Auth.register (Client.base client) ?auth ?kind ?username
         ?password ?params ?inhibit_login ())
  in
  Client.with_session client session

let register_uiaa client ?kind ?username ?password ?params ?inhibit_login
    ~auth_callback () =
  match
    Matrix_client.Auth.register_uiaa (Client.base client) ?kind ?username
      ?password ?params ?inhibit_login ~auth_callback ()
  with
  | Matrix_client.Uiaa.Uiaa_success session ->
      Matrix_client.Uiaa.Uiaa_success (Client.with_session client session)
  | Matrix_client.Uiaa.Uiaa_auth_required challenge ->
      Matrix_client.Uiaa.Uiaa_auth_required challenge
  | Matrix_client.Uiaa.Uiaa_error e ->
      Error.raise_client_error ~context:"registering account with UIAA" e

let whoami client =
  Error.unwrap ~context:"getting authenticated user"
    (Matrix_client.Auth.whoami (Client.base client))

type token_login = Matrix_client.Auth.token_login = {
  login_token : string;
  expires_in_ms : int;
}

let get_login_token client ?auth () =
  Error.unwrap ~context:"getting login token"
    (Matrix_client.Auth.get_login_token (Client.base client) ?auth ())

let register_available client ~username =
  Error.unwrap ~context:"checking username availability"
    (Matrix_client.Auth.register_available (Client.base client) ~username)

let check_registration_token client ~token =
  Error.unwrap ~context:"checking registration token"
    (Matrix_client.Auth.check_registration_token (Client.base client) ~token)

let request_registration_email_token client ~email ~client_secret ~send_attempt
    ?next_link ?id_server ?id_access_token () =
  Error.unwrap ~context:"requesting registration email token"
    (Matrix_client.Uiaa.request_email_token (Client.base client)
       ~use:Matrix_client.Uiaa.Register ~email ~client_secret ~send_attempt
       ?next_link ?id_server ?id_access_token ())

let request_registration_msisdn_token client ~country ~phone_number
    ~client_secret ~send_attempt ?next_link ?id_server ?id_access_token () =
  Error.unwrap ~context:"requesting registration MSISDN token"
    (Matrix_client.Uiaa.request_msisdn_token (Client.base client)
       ~use:Matrix_client.Uiaa.Register ~country ~phone_number ~client_secret
       ~send_attempt ?next_link ?id_server ?id_access_token ())

let request_password_email_token client ~email ~client_secret ~send_attempt
    ?next_link ?id_server ?id_access_token () =
  Error.unwrap ~context:"requesting password email token"
    (Matrix_client.Uiaa.request_email_token (Client.base client)
       ~use:Matrix_client.Uiaa.Password ~email ~client_secret ~send_attempt
       ?next_link ?id_server ?id_access_token ())

let request_password_msisdn_token client ~country ~phone_number ~client_secret
    ~send_attempt ?next_link ?id_server ?id_access_token () =
  Error.unwrap ~context:"requesting password MSISDN token"
    (Matrix_client.Uiaa.request_msisdn_token (Client.base client)
       ~use:Matrix_client.Uiaa.Password ~country ~phone_number ~client_secret
       ~send_attempt ?next_link ?id_server ?id_access_token ())
