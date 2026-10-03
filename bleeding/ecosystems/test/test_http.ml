(* Serve canned responses on a loopback port and record what arrives. *)

let serve ~sw env respond =
  let net = Eio.Stdenv.net env in
  let sock =
    Eio.Net.listen ~reuse_addr:true ~backlog:8 ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with `Tcp (_, p) -> p | _ -> assert false
  in
  let seen = ref [] in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server sock ~on_error:raise (fun flow _ ->
          let r = Eio.Buf_read.of_flow ~max_size:65536 flow in
          let rec headers acc =
            match Eio.Buf_read.line r with "" -> acc | l -> headers (l :: acc)
          in
          let hs = headers [] in
          seen := hs :: !seen;
          let status, extra, body = respond (List.length !seen) in
          Eio.Flow.copy_string
            (Printf.sprintf
               "HTTP/1.1 %d X\r\nContent-Type: application/json\r\n\
                Content-Length: %d\r\nConnection: close\r\n%s\r\n%s"
               status (String.length body) extra body)
            flow));
  (Printf.sprintf "http://127.0.0.1:%d" port, seen)

let header name hs =
  let p = String.lowercase_ascii name ^ ": " in
  List.find_map
    (fun l ->
      let l' = String.lowercase_ascii l in
      if String.starts_with ~prefix:p l' then
        Some (String.sub l (String.length p) (String.length l - String.length p))
      else None)
    hs

let with_server env respond f =
  Eio.Switch.run @@ fun sw ->
  let base_url, seen = serve ~sw env respond in
  f ~sw ~base_url seen

let () =
  Eio_main.run @@ fun env ->
  (* The User-Agent given to [create] reaches the server. *)
  with_server env (fun _ -> (200, "", "[]")) (fun ~sw ~base_url seen ->
      let c = Ecosystems_client.create ~user_agent:"probe-ua/1" ~base_url ~sw env in
      assert (Ecosystems.Registry.get_registries c () = []);
      assert (header "user-agent" (List.hd !seen) = Some "probe-ua/1"));
  (* A 404 is the client's error and not a decode failure. *)
  with_server env (fun _ -> (404, "", {|{"error":"nope"}|}))
    (fun ~sw ~base_url _ ->
      let c = Ecosystems_client.create ~base_url ~sw env in
      match Ecosystems.Registry.get_registry ~registry_name:"x" c () with
      | _ -> assert false
      | exception Openapi.Runtime.Api_error { status; _ } -> assert (status = 404));
  (* A 503 is retried, so a walk survives transient failures. *)
  with_server env
    (function
      | 1 -> (503, "Retry-After: 0\r\n", "{}")
      | _ -> (200, "", "[]"))
    (fun ~sw ~base_url seen ->
      let c = Ecosystems_client.create ~base_url ~sw env in
      assert (Ecosystems.Registry.get_registries c () = []);
      assert (List.length !seen = 2))
