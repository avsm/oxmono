(* SPDX-License-Identifier: ISC *)
module W = Httpz_websocket

let test env ~valid ~version =
  Eio.Switch.run
  @@ fun sw ->
  let listener =
    Eio.Net.listen
      ~sw
      ~reuse_addr:true
      ~backlog:1
      env#net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  Eio.Fiber.both
    (fun () ->
       let flow, _ = Eio.Net.accept ~sw listener in
       let reader = Eio.Buf_read.of_flow ~max_size:16384 flow in
       ignore (Eio.Buf_read.line reader);
       let rec headers acc =
         match Eio.Buf_read.line reader with
         | "" -> acc
         | line ->
           let i = String.index line ':' in
           headers
             (( String.sub line 0 i
              , String.trim (String.sub line (i + 1) (String.length line - i - 1)) )
              :: acc)
       in
       let fields =
         Result.get_ok (W.Handshake.accept ~meth:"GET" ~http_1_1:true (headers []))
       in
       let fields =
         if valid
         then fields
         else
           ("Sec-WebSocket-Accept", "bad")
           :: List.remove_assoc "Sec-WebSocket-Accept" fields
       in
       (* A peer may send the first frame in the same TCP write as the upgrade. *)
       let response =
         version
         ^ " 101 Switching Protocols\r\n"
         ^ String.concat
             ""
             (List.map (fun (key, value) -> key ^ ": " ^ value ^ "\r\n") fields)
         ^ "\r\n\129\005hello"
       in
       Eio.Flow.copy_string response flow)
    (fun () ->
       let received = ref false in
       let accepted =
         try
           Httpz_websocket_eio.with_connection
             env
             (Printf.sprintf "ws://127.0.0.1:%d/events?cursor=12" port)
             (fun ws ->
                assert (
                  W.receive ws ~f:(fun kind bytes ~off ~len ->
                    assert (kind = W.Text);
                    assert (Bytes.sub_string bytes off len = "hello");
                    received := true)));
           true
         with
         | Failure _ -> false
       in
       assert (accepted = (valid && version = "HTTP/1.1"));
       assert (!received = accepted))
;;

let () =
  Eio_main.run
  @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 5. (fun () ->
    test env ~valid:true ~version:"HTTP/1.1";
    test env ~valid:false ~version:"HTTP/1.1";
    test env ~valid:true ~version:"HTTP/1.0");
  print_endline
    "WebSocket Eio: coalesced frame, accept validation and HTTP version passed"
;;
