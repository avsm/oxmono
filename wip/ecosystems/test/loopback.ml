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
          let target =
            let request_line = List.nth hs (List.length hs - 1) in
            match String.split_on_char ' ' request_line with
            | _ :: t :: _ -> t
            | _ -> ""
          in
          let status, extra, body = respond (List.length !seen) target in
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
        let n = String.length p in
        Some (String.sub l n (String.length l - n))
      else None)
    hs

let with_server env respond f =
  Eio.Switch.run @@ fun sw ->
  let base_url, seen = serve ~sw env respond in
  f ~sw ~base_url seen
