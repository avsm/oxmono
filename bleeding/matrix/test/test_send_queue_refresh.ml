module C = Matrix_client.Client
module E = Matrix_client.Error
module Q = Matrix_client.Send_queue
module EC = Matrix_eio.Client
module EQ = Matrix_eio.Send_queue
module Id = Matrix_proto.Id

let user = Id.User_id.of_string_exn "@alice:example.org"
let room = Id.Room_id.of_string_exn "!room:example.org"
let homeserver = Uriz.of_string_exn "https://hs.example"

let rejected =
  E.Matrix_error
    {
      errcode = E.M_UNKNOWN_TOKEN;
      error = "expired";
      retry_after_ms = None;
      soft_logout = None;
    }

let forbidden =
  E.Matrix_error
    {
      errcode = E.M_FORBIDDEN;
      error = "forbidden";
      retry_after_ms = None;
      soft_logout = None;
    }

let tokens access_token = Ok C.{ access_token; refresh_token = Some "refresh" }

let response_unknown req =
  Fetch_mock.respond ~status:401
    {|{"errcode":"M_UNKNOWN_TOKEN","error":"expired"}|} req

let response_sent req =
  Fetch_mock.respond {|{"event_id":"$sent:example.org"}|} req

let auth req = Http.Header.get req.Fetch.Middleware.headers "authorization"
let path req = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url
let is_send req = req.Fetch.Middleware.meth = `PUT

let create_client ?store ~sw ~env ~refresh handler =
  EC.create ~sw ~env ~homeserver ~fetch:(Fetch_mock.client handler) ()
  |> fun c ->
  EC.with_session c
    C.
      {
        user_id = user;
        device_id = Id.Device_id.of_string_exn "DEV";
        access_token = "old";
        refresh_token = Some "refresh";
      }
  |> EC.with_auto_refresh ?store ~refresh

let queue ?store env =
  Q.create ?store ~user_id:user
    ~random:(Matrix_client.Random.of_env env)
    ~base_delay_ms:1 ~max_delay_ms:2 ~max_retries:3 ()

let wait env predicate =
  Eio.Time.with_timeout_exn env#clock 5. (fun () ->
      while not (predicate ()) do
        Eio.Time.sleep env#clock 0.001
      done)

let sent r = match Q.status r with Q.Sent _ -> true | _ -> false

let worker env client q f =
  Eio.Fiber.first
    (fun () ->
      Eio.Switch.run (fun sw ->
          EQ.start ~sw ~clock:env#clock client q;
          Eio.Fiber.await_cancel ()))
    f

let probe c =
  match C.Http.get (EC.base c) ~path:"/probe" () with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (E.to_string e)

let run f () = Eio_main.run (fun env -> Eio.Switch.run (fun sw -> f ~sw env))

let test_persistence ~sw env =
  let c =
    create_client ~sw ~env ~refresh:(fun _ -> Error rejected) response_unknown
  in
  let store = Matrix_client.Store.memory () in
  let q = queue ~store env in
  let expired = Q.send_text q ~room_id:room ~body:"retry" in
  let denied =
    Q.send_text q
      ~room_id:(Id.Room_id.of_string_exn "!denied:example.org")
      ~body:"no"
  in
  let pending = Q.send_text q ~room_id:room ~body:"later" in
  ignore (Q.send_one q ~send:(fun _ _ -> Error rejected) (EC.base c) expired);
  ignore (Q.send_one q ~send:(fun _ _ -> Error forbidden) (EC.base c) denied);
  let txn = Q.txn_id expired in
  let restored = queue ~store env in
  let get r =
    List.find (fun entry -> Q.id entry = Q.id r) (Q.requests restored)
  in
  let expired = get expired and denied = get denied and pending = get pending in
  Q.retry_expired_tokens restored;
  Alcotest.(check bool)
    "expired retry restored" true
    (Q.status expired = Q.Pending);
  Alcotest.(check bool)
    "other failure stays blocked" true
    (Q.status denied = Q.Wedged);
  Alcotest.(check bool) "pending unaffected" true (Q.status pending = Q.Pending);
  Alcotest.(check int) "attempt count retained" 1 (Q.attempts expired);
  Alcotest.(check string) "transaction retained" txn (Q.txn_id expired);
  for _ = 1 to 3 do
    ignore
      (Q.send_one restored
         ~send:(fun _ _ -> Error rejected)
         (EC.base c) expired);
    Q.retry_expired_tokens restored
  done;
  Alcotest.(check bool)
    "exhausted auth retries stay blocked" true
    (Q.status expired = Q.Wedged);
  Alcotest.(check int) "bounded attempts" 4 (Q.attempts expired)

let test_recovery ?(replacement = "new") ~sw env =
  let allow = ref false
  and accepted = ref false
  and sends = ref []
  and refreshes = ref 0 in
  let c =
    create_client ~sw ~env
      ~refresh:(fun _ ->
        incr refreshes;
        if !allow then begin
          accepted := true;
          tokens replacement
        end
        else Error rejected)
      (fun req ->
        if is_send req then sends := path req :: !sends;
        if !accepted && auth req = Some ("Bearer " ^ replacement) then
          if is_send req then response_sent req else Fetch_mock.respond "{}" req
        else response_unknown req)
  in
  let q = queue env in
  let r = Q.send_text q ~room_id:room ~body:"queued" in
  let denied =
    Q.send_text q
      ~room_id:(Id.Room_id.of_string_exn "!denied:example.org")
      ~body:"denied"
  in
  ignore (Q.send_one q ~send:(fun _ _ -> Error forbidden) (EC.base c) denied);
  worker env c q (fun () ->
      wait env (fun () -> Q.status r = Q.Wedged);
      Alcotest.(check int)
        "failed refresh leaves revision" 0
        (C.session_revision (EC.base c));
      Eio.Time.sleep env#clock 0.03;
      Alcotest.(check int)
        "no repeated auth attempt without recovery" 1 !refreshes;
      allow := true;
      probe c;
      wait env (fun () -> sent r);
      Alcotest.(check int)
        "successful refresh advances revision" 1
        (C.session_revision (EC.base c));
      Alcotest.(check int) "one recovered send" 2 (List.length !sends);
      Alcotest.(check bool)
        "same transaction path" true
        (List.hd !sends = List.hd (List.tl !sends));
      Alcotest.(check bool)
        "forbidden unaffected by refresh" true
        (Q.status denied = Q.Wedged))

let test_late_rejection ~sw env =
  let started, started_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let sends = ref 0 in
  let c =
    create_client ~sw ~env
      ~refresh:(fun _ -> tokens "new")
      (fun req ->
        if is_send req then begin
          incr sends;
          if !sends = 1 then begin
            Eio.Promise.resolve started_u ();
            Eio.Promise.await release
          end;
          if !sends <= 2 then response_unknown req else response_sent req
        end
        else if auth req = Some "Bearer new" then Fetch_mock.respond "{}" req
        else response_unknown req)
  in
  let q = queue env in
  let r = Q.send_text q ~room_id:room ~body:"racing" in
  worker env c q (fun () ->
      Eio.Promise.await started;
      probe c;
      Eio.Time.sleep env#clock 1.1;
      Eio.Promise.resolve release_u ();
      wait env (fun () -> sent r);
      Alcotest.(check int)
        "late rejection retried after observer saw refresh" 3 !sends)

let test_transient_refresh ~sw env =
  let refreshes = ref 0 in
  let c =
    create_client ~sw ~env
      ~refresh:(fun _ ->
        incr refreshes;
        if !refreshes = 1 then Error (E.Network_error "temporarily offline")
        else tokens "new")
      (fun req ->
        if auth req = Some "Bearer new" then response_sent req
        else response_unknown req)
  in
  let q = queue env in
  let r = Q.send_text q ~room_id:room ~body:"transient" in
  worker env c q (fun () ->
      wait env (fun () -> sent r);
      Alcotest.(check int) "transient failure retried" 2 !refreshes;
      Alcotest.(check int)
        "only success advances revision" 1
        (C.session_revision (EC.base c)))

let test_permanent_rejection ~sw env =
  let refreshes = ref 0 in
  let c =
    create_client ~sw ~env
      ~refresh:(fun _ ->
        incr refreshes;
        tokens (string_of_int !refreshes))
      response_unknown
  in
  let q = queue env in
  let r = Q.send_text q ~room_id:room ~body:"never accepted" in
  worker env c q (fun () ->
      wait env (fun () -> Q.attempts r = 4 && Q.status r = Q.Wedged);
      Eio.Time.sleep env#clock 1.1;
      Alcotest.(check int)
        "successful rotations cannot reset the retry budget" 4 !refreshes)

let test_restart ~sw env =
  let allow = ref false in
  let c =
    create_client ~sw ~env
      ~refresh:(fun _ -> if !allow then tokens "new" else Error rejected)
      (fun req ->
        if auth req = Some "Bearer new" then
          if is_send req then response_sent req else Fetch_mock.respond "{}" req
        else response_unknown req)
  in
  let store = Matrix_client.Store.memory () in
  let q = queue ~store env in
  let r = Q.send_text q ~room_id:room ~body:"restart" in
  worker env c q (fun () -> wait env (fun () -> Q.status r = Q.Wedged));
  allow := true;
  probe c;
  Eio.Time.sleep env#clock 0.03;
  Alcotest.(check bool)
    "stopped worker does not react" true
    (Q.status r = Q.Wedged);
  let restored = queue ~store env in
  let r = List.hd (Q.requests restored) in
  worker env c restored (fun () -> wait env (fun () -> sent r))

let test_uncertain_persisted_refresh ~sw env =
  let module P = Matrix_client.Profile_store in
  let module S = Matrix_client.Session in
  let directory = Filename.temp_dir "matrix-queue-refresh-" "" in
  let root = Eio.Path.(env#fs / directory) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree root)
    (fun () ->
      let store = P.create_at ~root ~profile:"test" in
      let saved : S.Session_file.t =
        {
          server = { homeserver; user_id = user };
          auth =
            {
              access_token = "old";
              refresh_token = Some "refresh";
              access_token_expires_at = None;
              device_id = Id.Device_id.of_string_exn "DEV";
              method_ = OAuth { client_id = "test" };
            };
          sync = { next_batch = None; filter_id = None };
          metadata =
            {
              created_at = Ptime.epoch;
              last_used_at = Ptime.epoch;
              client_name = "test";
            };
        }
      in
      ignore (Result.get_ok (P.save_session store saved));
      let exchanges = ref 0 in
      let c =
        create_client ~store ~sw ~env
          ~refresh:(fun _ ->
            incr exchanges;
            Error (E.Network_error "response lost after rotation"))
          response_unknown
      in
      let q = queue env in
      let r = Q.send_text q ~room_id:room ~body:"uncertain" in
      worker env c q (fun () ->
          wait env (fun () -> Q.status r = Q.Wedged);
          Eio.Time.sleep env#clock 0.03;
          Alcotest.(check int)
            "possibly consumed refresh token exchanged only once" 1 !exchanges;
          Alcotest.(check int)
            "no credential revision without durable recovery" 0
            (C.session_revision (EC.base c));
          (match Q.last_error r with
          | Some (E.Policy_denied _) -> ()
          | _ -> Alcotest.fail "uncertain refresh must require login");
          Q.retry_expired_tokens q;
          Alcotest.(check bool)
            "policy denial is not automatically retried" true
            (Q.status r = Q.Wedged)))

let () =
  Alcotest.run "send queue credential recovery"
    [
      ( "recovery",
        [
          Alcotest.test_case "persist expired-token reason and budget" `Quick
            (run test_persistence);
          Alcotest.test_case "resume only expired-token sends" `Quick
            (run (test_recovery ~replacement:"new"));
          Alcotest.test_case "refresh retaining the access token" `Quick
            (run (test_recovery ~replacement:"old"));
          Alcotest.test_case "late rejection after refresh" `Quick
            (run test_late_rejection);
          Alcotest.test_case "uncertain persisted refresh is never replayed"
            `Quick
            (run test_uncertain_persisted_refresh);
          Alcotest.test_case "transient refresh failure" `Quick
            (run test_transient_refresh);
          Alcotest.test_case "permanent rejection has bounded retries" `Quick
            (run test_permanent_rejection);
          Alcotest.test_case "stop and restart with recovered credentials"
            `Quick (run test_restart);
        ] );
    ]
