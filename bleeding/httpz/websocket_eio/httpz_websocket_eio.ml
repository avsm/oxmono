(* SPDX-License-Identifier: ISC *)
module W = Httpz_websocket
module U = Httpz_uri
let i16 = Httpz.Buf_read.i16
let to_int = Httpz.Buf_read.to_int

let with_connection ?(tls = Httpz_tls.system) ?(max_message = 1048576)
    env url f =
  let uri = U.of_string_exn url in
  let secure = match U.scheme uri with
    | This "wss" -> true | This "ws" -> false
    | _ -> invalid_arg "WebSocket URL requires ws or wss" in
  if U.has_userinfo uri || U.has_fragment uri then
    invalid_arg "WebSocket URL cannot contain credentials or a fragment";
  let host = match U.decoded_host uri with
    | This host when host <> "" -> host
    | _ -> invalid_arg "WebSocket URL requires a host" in
  let port = match U.port uri with
    | This port when port > 0 && port <= 65535 -> port
    | Null -> if secure then 443 else 80
    | _ -> invalid_arg "WebSocket port is out of range" in
  if String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) host then
    invalid_arg "WebSocket host contains a control character";
  let authority = if String.contains host ':' then "[" ^ host ^ "]" else host in
  let authority = if port = (if secure then 443 else 80) then authority
    else authority ^ ":" ^ string_of_int port in
  let path = match U.encoded_path uri with "" -> "/" | path -> path in
  let target = path ^ match U.encoded_query uri with
    | Null -> "" | This query -> "?" ^ query in
  let random_reader = Eio.Buf_read.of_flow env#secure_random
      ~initial_size:32 ~max_size:128 in
  let nonce = Eio.Buf_read.take 16 random_reader in
  let headers = W.Handshake.request ~nonce () in
  let key = List.assoc "Sec-WebSocket-Key" headers in
  let request = "GET " ^ target ^ " HTTP/1.1\r\nHost: " ^ authority ^ "\r\n"
    ^ String.concat "" (List.map (fun (name, value) ->
      name ^ ": " ^ value ^ "\r\n") headers) ^ "\r\n" in
  if String.length request > 16384 then invalid_arg "WebSocket URL too long";
  Eio.Switch.run @@ fun sw ->
  let flow, reader, buffer, head_end, buffered =
    Eio.Time.with_timeout_exn env#clock 15. @@ fun () ->
    let addresses = Eio.Net.getaddrinfo_stream env#net host
        ~service:(string_of_int port) in
    let rec connect = function
      | [] -> failwith "no WebSocket address"
      | [address] -> Eio.Net.connect ~sw env#net address
      | address :: rest ->
          (try Eio.Net.connect ~sw env#net address
           with Eio.Io _ -> connect rest) in
    let raw = (connect addresses :> Httpz_tls.flow) in
    let flow = if secure then
        let http = U.with_scheme uri (This "https") in tls http raw
      else raw in
    Eio.Flow.copy_string request flow;
    let reader = Eio.Buf_read.of_flow ~initial_size:4096 ~max_size:32767 flow in
    let buffer = Bytes.create 32767 in
    let rec head length interim =
      let #(status, response, fields) = Httpz.Res.parse buffer
        ~len:(i16 length) ~limits:Httpz.Buf_read.default_limits in
      match status with
      | Httpz.Buf_read.Partial ->
          if length = Bytes.length buffer then failwith "WebSocket head limit";
          Eio.Buf_read.ensure reader 1;
          let size = min (Eio.Buf_read.buffered_bytes reader)
              (Bytes.length buffer - length) in
          Bytes.blit_string (Eio.Buf_read.take size reader) 0 buffer length size;
          head (length + size) interim
      | Httpz.Buf_read.Complete ->
          let code = to_int response.#code and end_ = to_int response.#body_off in
          if code >= 100 && code < 200 && code <> 101 && interim < 8 then (
            Bytes.blit buffer end_ buffer 0 (length - end_);
            head (length - end_) (interim + 1))
          else (
            if response.#version <> Httpz.Version.Http_1_1 then
              failwith "WebSocket upgrade requires HTTP/1.1";
            let fields = Httpz.Header.to_rev_string_pairs_local buffer fields in
            (match W.Handshake.verify ~key ~status:code fields with
             | Ok _ -> () | Error message -> failwith message);
            flow, reader, buffer, end_, length)
      | status -> failwith (Httpz.Buf_read.status_to_string status) in
    head 0 0 in
  let offset = ref head_end in
  let read bytes ~off ~len =
    if !offset < buffered then (
      let size = min len (buffered - !offset) in
      Bytes.blit buffer !offset bytes off size; offset := !offset + size; size)
    else Eio.Time.with_timeout_exn env#clock 120. (fun () ->
      match Eio.Buf_read.ensure reader 1 with
      | () ->
          let size = min len (Eio.Buf_read.buffered_bytes reader) in
          Bytes.blit_string (Eio.Buf_read.take size reader) 0 bytes off size; size
      | exception End_of_file -> 0) in
  let write bytes ~off ~len =
    Eio.Time.with_timeout_exn env#clock 10. (fun () ->
      Eio.Flow.copy_string (Bytes.sub_string bytes off len) flow) in
  let random bytes ~off ~len =
    Bytes.blit_string (Eio.Buf_read.take len random_reader) 0 bytes off len in
  let lock = Eio.Mutex.create () in
  let socket = W.create ~role:Client ~max_message ~random ~read ~write
    ~with_write_lock:(fun f ->
      Eio.Mutex.lock lock;
      match f () with
      | () -> Eio.Mutex.unlock lock
      | exception exn -> Eio.Mutex.unlock lock; raise exn) () in
  Fun.protect ~finally:(fun () ->
    Eio.Cancel.protect (fun () ->
      try Httpz_tls.close ~clock:env#mono_clock flow with _ -> ()))
    (fun () -> Eio.Fiber.first (fun () -> f socket) (fun () ->
      let rec heartbeat () =
        Eio.Time.sleep env#clock 30.;
        W.ping socket Bytes.empty ~off:0 ~len:0; heartbeat () in
      heartbeat ()))
