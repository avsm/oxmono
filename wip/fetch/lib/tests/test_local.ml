open Fetch

(* Fixed-size test storage uses the same checked local allocation primitive
   as the protocol parsers. No borrowed string is created on the heap. *)
external create_local : int -> bytes @ local = "caml_create_local_bytes"
[@@noalloc]

let with_body (fn : (string @ local -> 'a) @ local) =
  let local_ bytes = create_local 7 in
  Bytes.blit_string "payload" 0 bytes 0 7;
  let local_ body = Bytes.unsafe_to_string bytes in
  let result = fn body in
  result

let () =
  Eio_mock.Backend.run_full @@ fun env ->
  let retained = ref None in
  let calls = ref 0 in
  let backend =
    Fetch_mock.client_local (fun req ->
        incr calls;
        Eio.Fiber.yield ();
        (match req.Middleware.body with
        | Middleware.String s -> assert (String.equal s "payload")
        | _ -> assert false);
        retained := Some (Middleware.globalize req);
        let status, headers =
          if !calls = 1 then (503, Http.Header.init ())
          else if !calls = 2 then
            (307, Http.Header.of_list [ ("location", "/next") ])
          else (200, Http.Header.init ())
        in
        Middleware.Pi.response ~status ~headers ~version:`HTTP_1_1
          ~body:(Eio.Flow.string_source "ok")
          ~close:ignore ~url:req.url ())
  in
  let client =
    backend
    |> with_retry ~clock:env#mono_clock
         ~random:(Eio.Flow.string_source "")
         ~config:
           (Retry.v ~max_retries:1 ~jitter:false ~allowed_methods:[ `POST ]
              ~backoff_factor:0L
              ~retry_request:(fun req -> Middleware.body_replayable req.body)
              ())
    |> restrict ~under:[ "http://example.test" ] ~filter:(fun req ->
           assert (Middleware.body_replayable req.body);
           `Allow)
    |> with_headers Header.[ (user_agent, "local-test") ]
    |> with_credentials ~scope:[ "http://example.test" ] ~allow_insecure:true
         [ Credential.bearer "token" ]
  in
  with_body (fun body ->
      let () =
        with_response
          ~body:(stack_ String body)
          client `POST "http://example.test/start"
          (fun response -> assert (status response = 200))
      in
      ());
  assert (!calls = 3);
  Gc.full_major ();
  (match !retained with
  | Some { Middleware.body = Middleware.String s; _ } -> assert (s = "payload")
  | _ -> assert false);
  let before = !calls in
  with_body (fun body ->
      let () =
        Eio.Switch.run (fun sw ->
            let response =
              post ~sw
                ~body:(stack_ String body)
                client "http://example.test/next"
            in
            assert (status response = 200);
            close response)
      in
      ());
  assert (!calls = before + 1)

exception Borrowed_failure

let () =
  Eio_mock.Backend.run @@ fun () ->
  let key = Eio.Fiber.create_key () in
  let released = ref false in
  with_body (fun body ->
      match
        Eio.Switch.run (fun sw ->
            Eio.Switch.on_release sw (fun () -> released := true);
            let result =
              Eio.Fiber.with_binding key "bound" (fun () ->
                  let result =
                    Eio.Private.Trace.with_span "borrowed failure" (fun () ->
                        Eio.Fiber.yield ();
                        assert (String.length body = 7);
                        assert (Eio.Fiber.get key = Some "bound");
                        raise Borrowed_failure)
                  in
                  result)
            in
            result)
      with
      | _ -> assert false
      | exception Borrowed_failure -> ());
  assert !released;
  assert (Eio.Fiber.get key = None)
