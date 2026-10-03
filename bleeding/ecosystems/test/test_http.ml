open Loopback

let () =
  Eio_main.run @@ fun env ->
  (* The User-Agent given to [create] reaches the server. *)
  with_server env (fun _ _ -> (200, "", "[]")) (fun ~sw ~base_url seen ->
      let c =
        Ecosystems_client.create ~user_agent:"probe-ua/1" ~base_url ~sw env
      in
      assert (Ecosystems.Registry.get_registries c () = []);
      assert (header "user-agent" (List.hd !seen) = Some "probe-ua/1"));
  (* A 404 is the client's error and not a decode failure. *)
  with_server env (fun _ _ -> (404, "", {|{"error":"nope"}|}))
    (fun ~sw ~base_url _ ->
      let c = Ecosystems_client.create ~base_url ~sw env in
      match Ecosystems.Registry.get_registry ~registry_name:"x" c () with
      | _ -> assert false
      | exception Openapi.Runtime.Api_error { status; _ } ->
          assert (status = 404));
  (* A 503 is retried, so a walk survives transient failures. *)
  with_server env
    (fun n _ ->
      if n = 1 then (503, "Retry-After: 0\r\n", "{}") else (200, "", "[]"))
    (fun ~sw ~base_url seen ->
      let c = Ecosystems_client.create ~base_url ~sw env in
      assert (Ecosystems.Registry.get_registries c () = []);
      assert (List.length !seen = 2))

let () =
  Eio_main.run @@ fun env ->
  (* A supplied session is used as it stands. *)
  with_server env (fun _ _ -> (200, "", "[]")) (fun ~sw ~base_url seen ->
      let session = Fetch_curl.v ~sw ~user_agent:"mine/1" () in
      let c = Ecosystems_client.create ~session ~base_url ~sw env in
      assert (Ecosystems.Registry.get_registries c () = []);
      assert (header "user-agent" (List.hd !seen) = Some "mine/1"));
  (* The response size limit is enforced. *)
  with_server env (fun _ _ -> (200, "", "[" ^ String.make 200 ' ' ^ "]"))
    (fun ~sw ~base_url _ ->
      let c =
        Ecosystems_client.create ~max_response_bytes:50 ~base_url ~sw env
      in
      match Ecosystems.Registry.get_registries c () with
      | _ -> assert false
      | exception _ -> ())
