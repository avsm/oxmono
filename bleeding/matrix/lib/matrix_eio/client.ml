type t = {
  base : Matrix_client.Client.t;
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Time.clock;
  http : Fetch.plain;
  media_fetcher : Matrix_client.Media_fetcher.t Atomic.t;
}

let create ~sw ~env ~homeserver ?user_agent ?well_known_policy ?fetch
    ?request_timeout ?media_fetcher () =
  let config =
    Matrix_client.Client.config ~homeserver ?user_agent ?well_known_policy ()
  in
  let fetch =
    match fetch with
    | Some f -> f
    | None ->
        Fetch_httpz.std
          ~retry:(Matrix_client.Http_retry.default ~homeserver)
          env
  in
  let random = Matrix_client.Random.of_env env in
  let base = Matrix_client.Client.create ~config ~fetch ~random in
  let base =
    Option.fold ~none:base
      ~some:(fun seconds ->
        Matrix_client.Client.with_request_timeout ~mono_clock:env#mono_clock
          seconds base)
      request_timeout
  in
  {
    base;
    sw;
    clock = env#clock;
    http = fetch;
    media_fetcher =
      Atomic.make
        (Option.value media_fetcher ~default:Matrix_client.Media_fetcher.default);
  }

let base t = t.base
let switch t = t.sw
let http t = t.http
let media_fetcher t = Atomic.get t.media_fetcher
let get_media_fetcher = media_fetcher
let set_media_fetcher t fetcher = Atomic.set t.media_fetcher fetcher
let homeserver t = Matrix_client.Client.homeserver t.base
let well_known_policy t = Matrix_client.Client.well_known_policy t.base
let session t = Matrix_client.Client.session t.base
let is_logged_in t = Option.is_some (session t)
let sync_presence t = Matrix_client.Client.sync_presence t.base

let set_sync_presence t state =
  Matrix_client.Client.set_sync_presence t.base state

let register_presence_wakeup t wake =
  Matrix_client.Client.register_presence_wakeup t.base wake

let require ~context t =
  match session t with
  | Some s -> s
  | None ->
      Error.with_context context (fun () ->
          raise (Error.err Error.Not_logged_in))

let user_id t = (require ~context:"getting logged-in Matrix user ID" t).user_id

let device_id t =
  (require ~context:"getting logged-in Matrix device ID" t).device_id

let access_token t =
  (require ~context:"getting Matrix access token" t).access_token

let with_session t session =
  { t with base = Matrix_client.Client.with_session t.base session }

let with_access_token t access_token =
  { t with base = Matrix_client.Client.with_access_token t.base access_token }

let persisted_refresh t store prepare =
  let module P = Matrix_client.Profile_store in
  let module S = Matrix_client.Session in
  let ( let* ) = Result.bind in
  (* Bind provenance at construction: a later login must not silently transfer
     this client's refresh callback to a different account or OAuth client. *)
  let original = P.load_session store in
  fun (old : Matrix_client.Client.session) ->
    let* original = original in
    let* original =
      match original with
      | Some session
        when Uriz.equal session.server.homeserver (homeserver t)
             && Matrix_proto.Id.User_id.equal session.server.user_id old.user_id
             && Matrix_proto.Id.Device_id.equal session.auth.device_id
                  old.device_id ->
          Ok session
      | _ ->
          Error
            (Matrix_client.Error.Policy_denied
               "refresh profile does not match the client")
    in
    let expected =
      {
        original with
        auth =
          {
            original.auth with
            access_token = old.access_token;
            refresh_token = old.refresh_token;
          };
      }
    in
    let* saved =
      P.refresh_session_prepared store ~clock:t.clock ~expected
        ~prepare:(fun latest ->
          let* exchange = prepare old in
          Ok
            (fun () ->
              let* (tokens : Matrix_client.Client.refreshed_tokens_with_expiry)
                  =
                exchange ()
              in
              Ok
                S.Auth.
                  {
                    latest.auth with
                    access_token = tokens.refreshed_tokens.access_token;
                    refresh_token =
                      (match tokens.refreshed_tokens.refresh_token with
                      | Some _ as token -> token
                      | None -> latest.auth.refresh_token);
                    access_token_expires_at = tokens.expires_at;
                  }))
    in
    Ok
      Matrix_client.Client.
        {
          refreshed_tokens =
            {
              access_token = saved.auth.access_token;
              refresh_token = saved.auth.refresh_token;
            };
          expires_at = saved.auth.access_token_expires_at;
        }

let with_prepared_auto_refresh_expiry ?store ?on_session_update ?expires_at
    ?early_refresh ?now ~prepare t =
  let refresh =
    match store with
    | None ->
        fun session ->
          Result.bind (prepare session) (fun exchange -> exchange ())
    | Some store -> persisted_refresh t store prepare
  in
  {
    t with
    base =
      Matrix_client.Client.with_auto_refresh_expiry
        ~spawn_refresh:(fun run -> Eio.Fiber.fork ~sw:t.sw run)
        ?on_session_update ?expires_at ?early_refresh ?now ~refresh t.base;
  }

let with_auto_refresh_expiry ?store ?on_session_update ?expires_at
    ?early_refresh ?now ~refresh t =
  with_prepared_auto_refresh_expiry ?store ?on_session_update ?expires_at
    ?early_refresh ?now
    ~prepare:(fun session -> Ok (fun () -> refresh session))
    t

let with_auto_refresh ?store ?on_session_update ~refresh t =
  let refresh session =
    Result.map
      (fun refreshed_tokens ->
        Matrix_client.Client.{ refreshed_tokens; expires_at = None })
      (refresh session)
  in
  let on_session_update =
    Option.map (fun callback session _ -> callback session) on_session_update
  in
  with_auto_refresh_expiry ?store ?on_session_update ~refresh t
