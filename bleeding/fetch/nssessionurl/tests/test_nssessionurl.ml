let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in

  (* Transport error surfaces as Nssessionurl.Error, not a crash. *)
  (match Nssessionurl.fetch_string "https://nonexistent.invalid/" with
   | exception Nssessionurl.Error _ -> print_endline "bad-host: ok"
   | _ -> failwith "expected Nssessionurl.Error");

  (* Invalid URL rejected synchronously. *)
  (match Nssessionurl.fetch_string "not a url" with
   | exception Failure _ -> print_endline "bad-url: ok"
   | _ -> failwith "expected Failure");

  (* Fibre cancellation cancels the underlying NSURLSession task and the
     fibre gets unstuck promptly. *)
  (match
     Eio.Time.with_timeout_exn clock 0.2 (fun () ->
         Nssessionurl.fetch_string "https://httpbin.org/delay/10")
   with
   | exception Eio.Time.Timeout -> print_endline "cancel: ok"
   | _ -> failwith "expected timeout");

  (* Local echo server, serving the eleven remaining requests. *)
  let port =
    let ic =
      Unix.open_process_in
        (Filename.quote_command "python3"
           [ Filename.concat (Filename.dirname Sys.executable_name) "echo_server.py";
             "11" ])
    in
    let port = int_of_string (String.trim (input_line ic)) in
    at_exit (fun () -> ignore (Unix.close_process_in ic));
    port
  in
  let url path = Printf.sprintf "http://127.0.0.1:%d%s" port path in

  (* POST round-trips a body. *)
  (match
     Nssessionurl.fetch_string ~meth:"POST"
       ~headers:[ ("Content-Type", "text/plain") ]
       ~body:(Nssessionurl.Fixed "hello from ocaml")
       (url "/echo")
   with
   | { status = 200; _ }, "hello from ocaml" -> print_endline "post: ok"
   | { status; _ }, body ->
     Printf.ksprintf failwith "post: unexpected status %d body %S" status body);

  (* Stream a 5MB body through a small max_buffer with small reads, so the
     chunk queue and the suspend/resume backpressure path both get
     exercised; verify every byte arrives intact and reads were actually
     incremental. *)
  let size = 5_000_000 in
  Eio.Switch.run (fun sw ->
      let info, src =
        Nssessionurl.fetch ~sw ~max_buffer:65536 (url ("/big/" ^ string_of_int size))
      in
      assert (info.Nssessionurl.status = 200);
      let out = Buffer.create size in
      let buf = Cstruct.create 8192 in
      let rec go reads =
        match Eio.Flow.single_read src buf with
        | n ->
          Buffer.add_string out (Cstruct.to_string (Cstruct.sub buf 0 n));
          go (reads + 1)
        | exception End_of_file -> reads
      in
      let reads = go 0 in
      let expected =
        let p = "0123456789abcdef" in
        let b = Buffer.create size in
        while Buffer.length b < size do
          Buffer.add_string b p
        done;
        Buffer.sub b 0 size
      in
      if Buffer.contents out <> expected then failwith "stream: corrupt body";
      if reads < 2 then failwith "stream: expected multiple reads";
      Printf.printf "stream: ok (%d bytes in %d reads)\n" size reads);

  (* Streamed upload: push 3MB from an Eio flow through a small buffer and
     check the echo comes back byte-identical (exercises the bound-pair
     pump, chunked transfer-encoding, and upload backpressure). *)
  let upsize = 3_000_000 in
  let payload =
    let p = "abcdefghijklmnop" in
    let b = Buffer.create (upsize + 16) in
    while Buffer.length b < upsize do
      Buffer.add_string b p
    done;
    Buffer.sub b 0 upsize
  in
  Eio.Switch.run (fun sw ->
      let info, src =
        Nssessionurl.post ~sw
          ~body:(Nssessionurl.Stream (Eio.Flow.string_source payload))
          (url "/echo")
      in
      assert (info.Nssessionurl.status = 200);
      let got = Eio.Flow.read_all src in
      if got <> payload then failwith "upload-stream: corrupt echo";
      Printf.printf "upload-stream: ok (%d bytes round-tripped)\n" upsize);

  (* The same streamed upload with a small watermark through the buffering
     convenience wrapper. *)
  (match
     Nssessionurl.fetch_string ~meth:"POST" ~max_buffer:65536
       ~body:(Nssessionurl.Stream (Eio.Flow.string_source payload))
       (url "/echo")
   with
   | { status = 200; _ }, body when body = payload ->
     print_endline "upload-stream-string: ok"
   | { status; _ }, _ ->
     Printf.ksprintf failwith "upload-stream-string: unexpected status %d" status);

  (* Two concurrent requests multiplexed over one explicitly shared
     session, exercising the taskIdentifier routing table. *)
  let session = Nssessionurl.Session.create () in
  let fetch_n n =
    let info, body =
      Nssessionurl.fetch_string ~session (url ("/big/" ^ string_of_int n))
    in
    assert (info.Nssessionurl.status = 200);
    assert (String.length body = n)
  in
  Eio.Fiber.both (fun () -> fetch_n 100_000) (fun () -> fetch_n 50_000);
  print_endline "shared-session: ok";

  (* Session configuration: additional_headers are merged into requests
     (the server reflects the header back).  Response streamed into a
     caller-supplied sink via fetch_into. *)
  let configured =
    Nssessionurl.Session.create ~ephemeral:true
      ~cache_policy:Nssessionurl.Session.Reload_ignoring_local_cache
      ~additional_headers:[ ("x-nssessionurl-test", "configured") ]
      ()
  in
  let out = Buffer.create 32 in
  let info =
    Nssessionurl.fetch_into ~session:configured
      ~sink:(Eio.Flow.buffer_sink out)
      (url "/header/x-nssessionurl-test")
  in
  assert (info.Nssessionurl.status = 200);
  if Buffer.contents out <> "configured" then
    Printf.ksprintf failwith "config-headers: got %S" (Buffer.contents out);
  print_endline "config-headers: ok";

  (* Redirects are followed inside NSURLSession by default (two requests:
     the hop and its target)... *)
  (match Nssessionurl.fetch_string (url "/redirect") with
   | { status = 200; _ }, "0123456789abcdef" -> print_endline "redirect-follow: ok"
   | { status; _ }, body ->
     Printf.ksprintf failwith "redirect-follow: status %d body %S" status body);

  (* ...and delivered raw when disabled, Location header intact. *)
  (match Nssessionurl.fetch_string ~follow_redirects:false (url "/redirect") with
   | { status = 302; headers }, _ ->
     let location =
       List.find_map
         (fun (k, v) ->
            if String.lowercase_ascii k = "location" then Some v else None)
         headers
     in
     if location <> Some "/big/16" then
       Printf.ksprintf failwith "redirect-manual: Location %s"
         (Option.value location ~default:"<absent>");
     print_endline "redirect-manual: ok"
   | { status; _ }, _ ->
     Printf.ksprintf failwith "redirect-manual: status %d" status);

  (* request_timeout fires while the server sits on the response, and the
     error carries the structured NSURLErrorTimedOut code. *)
  let impatient = Nssessionurl.Session.create ~request_timeout:0.3 () in
  (match Nssessionurl.fetch_string ~session:impatient (url "/slow/3") with
   | exception Nssessionurl.Error { domain = "NSURLErrorDomain"; code = -1001; _ } ->
     print_endline "config-timeout: ok"
   | exception Nssessionurl.Error e ->
     Printf.ksprintf failwith "config-timeout: unexpected error %s %d: %s"
       e.domain e.code e.message
   | _ -> failwith "expected a timeout error");

  print_endline "all tests passed"
