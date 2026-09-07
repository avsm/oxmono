(* Exercise the upstream additions through the OxCaml interfaces and both
   Linux backends, independently of the stock-compiler MDX transcripts. *)

let connect : _ @ portable = fun ~sw net addr ->
  Eio.Net.connect ~sw net addr
    ~bind_to:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    ~options:Eio.Net.Sockopt.[
      (SO_KEEPALIVE, false);
      (SO_KEEPALIVE, true);
    ]

let exercise env =
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let server = Eio.Net.listen net ~sw ~backlog:1
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let addr = Eio.Net.listening_addr server in
  let client = connect ~sw net addr in
  let peer, _ = Eio.Net.accept ~sw server in
  assert (Eio.Net.getsockopt client Eio.Net.Sockopt.SO_KEEPALIVE);
  Eio.Flow.copy_string "ok" client;
  let received = Cstruct.create 2 in
  Eio.Flow.read_exact peer received;
  assert (Cstruct.to_string received = "ok");
  (* The listener already owns this address, so binding an outbound socket to
     it must fail. This catches backends silently ignoring [bind_to]. *)
  (match Eio.Net.connect ~sw net addr ~bind_to:addr with
   | _ -> failwith "connect ignored the occupied bind address"
   | exception Eio.Io _ -> ());
  let module Env = Eio.Process.Env in
  let original = Env.of_array [| "MESSAGE=old"; "MESSAGE=duplicate"; "REMOVE=yes" |] in
  let updated = Env.override ["MESSAGE", Some "new"; "REMOVE", None] original in
  assert (Env.to_array updated = [| "MESSAGE=new" |]);
  assert (Env.get_opt "MESSAGE" original = Some "old");
  let output = Eio.Process.parse_out (Eio.Stdenv.process_mgr env)
      Eio.Buf_read.take_all ~env:updated
      ["/bin/sh"; "-c"; "printf '%s:%s' \"$MESSAGE\" \"${REMOVE-unset}\""] in
  assert (output = "new:unset")

let () =
  Eio_linux.run exercise;
  Eio_posix.run exercise;
  print_endline "Eio upstream refresh: Linux and POSIX checks passed"
