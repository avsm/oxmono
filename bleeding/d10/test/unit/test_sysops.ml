let with_server f =
  Helpers.with_eio_temp_dir ~prefix:"http" @@ fun ~fs ~clock ~dir env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, p) -> p
    | _ -> assert false
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error:raise (fun flow _ ->
            let reader = Eio.Buf_read.of_flow flow ~max_size:4096 in
            let request = Eio.Buf_read.line reader in
            let rec headers () =
              if Eio.Buf_read.line reader <> "" then headers ()
            in
            headers ();
            let meth, path =
              match String.split_on_char ' ' request with
              | m :: p :: _ -> (m, p)
              | _ -> assert false
            in
            let response =
              match path with
              | "/redirect" ->
                  "HTTP/1.1 302 Found\r\nLocation: /ok\r\nContent-Length: 0\r\n"
              | "/missing" -> "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n"
              | _ -> "HTTP/1.1 200 OK\r\nContent-Length: 7\r\n"
            in
            Eio.Flow.copy_string (response ^ "Connection: close\r\n\r\n") flow;
            if meth <> "HEAD" && (path = "/ok" || path = "/slow") then (
              if path = "/slow" then Eio.Time.sleep clock 5.;
              Eio.Flow.copy_string "payload" flow))
      done);
  let before = Sys.getenv_opt "GIT_TERMINAL_PROMPT" in
  let sys =
    D10.Sysops.v ~fs ~clock ~net:(Eio.Stdenv.net env)
      ~proc_mgr:(Eio.Stdenv.process_mgr env)
      ()
  in
  Alcotest.(check (option string))
    "parent environment unchanged" before
    (Sys.getenv_opt "GIT_TERMINAL_PROMPT");
  Alcotest.(check string)
    "child Git cannot prompt" "0"
    (D10.Sysops.Cmd.run_out sys
       [ "/bin/sh"; "-c"; "printf %s \"$GIT_TERMINAL_PROMPT\"" ]);
  f ~sw ~sys ~clock
    ~dst:Eio.Path.(fs / dir / "download")
    ~url:(Printf.sprintf "http://127.0.0.1:%d" port)

let test_fetch () =
  with_server @@ fun ~sw ~sys ~clock:_ ~dst ~url ->
  let progress = ref None in
  let on_progress ~received ~total = progress := Some (received, total) in
  Alcotest.(check bool)
    "redirect download" true
    (D10.Sysops.Http.fetch ~on_progress sys ~url:(url ^ "/redirect") ~dst);
  Alcotest.(check string) "streamed bytes" "payload" (Eio.Path.load dst);
  Alcotest.(check (option (pair int64 (option int64))))
    "final progress"
    (Some (7L, Some 7L))
    !progress;
  Alcotest.(check bool)
    "HEAD" true
    (D10.Sysops.Http.head sys ~url:(url ^ "/ok"));
  Alcotest.(check bool)
    "404" false
    (D10.Sysops.Http.fetch sys ~url:(url ^ "/missing") ~dst);
  Alcotest.(check string)
    "HTTP error preserves destination" "payload" (Eio.Path.load dst);
  D10.Sysops.Http.with_session ~sw sys (fun session ->
      for _ = 1 to 2 do
        Alcotest.(check bool)
          "session download" true
          (D10.Sysops.Http.fetch_session session ~url:(url ^ "/ok") ~dst)
      done)

let test_cancellation () =
  with_server @@ fun ~sw:_ ~sys ~clock ~dst ~url ->
  let cancelled =
    try
      Eio.Time.with_timeout_exn clock 0.1 (fun () ->
          ignore (D10.Sysops.Http.fetch sys ~url:(url ^ "/slow") ~dst));
      false
    with Eio.Time.Timeout -> true
  in
  Alcotest.(check bool) "caller cancellation propagates" true cancelled

let suite =
  ( "sysops",
    [
      Alcotest.test_case "HTTP and child environment" `Quick test_fetch;
      Alcotest.test_case "cancellation" `Quick test_cancellation;
    ] )
