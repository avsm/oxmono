(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Dune_rpc.Private

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

(* A stand-in dune server. It answers the handshake, selecting the highest
   version the client offered for each method, and then hands every other
   request to [f] as its method name. [cut] drops the last four bytes of one
   reply and hangs up, which is the one thing a real server cannot be asked to
   do on cue. *)
let serve ?(cut = false) flow ~f =
  let chan = Dune_rpc_eio.Chan.create flow in
  let send sexp = Eio.Flow.copy_string (Csexp.to_string sexp) flow in
  let reply id payload =
    let s =
      Csexp.to_string
        (Conv.to_sexp Packet.sexp (Packet.Response (id, Ok payload)))
    in
    if cut then begin
      Eio.Flow.copy_string (String.sub s 0 (String.length s - 4)) flow;
      Eio.Flow.shutdown flow `All
    end
    else Eio.Flow.copy_string s flow
  in
  let rec go () =
    match Dune_rpc_eio.Chan.read chan with
    | None -> ()
    | Some sexp -> (
        match Conv.of_sexp Packet.sexp ~version:Version.latest sexp with
        | Error _ -> ()
        | Ok (Packet.Request (id, call)) -> (
            match Method.Name.to_string call.method_ with
            | "initialize" ->
                send
                  (Conv.to_sexp Packet.sexp
                     (Packet.Response
                        ( id,
                          Ok
                            (Initialize.Response.to_response
                               (Initialize.Response.create ())) )));
                go ()
            | "version_menu" ->
                let (Menu offered) =
                  Result.get_ok
                    (Version_negotiation.Request.of_call call
                       ~version:Version.latest)
                in
                let picked =
                  List.map (fun (m, vs) -> (m, List.fold_left max 1 vs)) offered
                in
                send
                  (Conv.to_sexp Packet.sexp
                     (Packet.Response
                        ( id,
                          Ok
                            (Version_negotiation.Response.to_response
                               (Version_negotiation.Response.create picked)) )));
                go ()
            | m ->
                reply id (f m);
                go ())
        | Ok _ -> go ())
  in
  go ()

let pair ~sw = Eio_unix.Net.socketpair_stream ~sw ()

(* A request reaches the server and its answer reaches the caller. *)
let round_trip () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let client_end, server_end = pair ~sw in
  Eio.Fiber.fork ~sw (fun () -> serve server_end ~f:(fun _ -> Csexp.List []));
  let chan = Dune_rpc_eio.Chan.create client_end in
  let init = Initialize.Request.create ~id:(Id.make (Csexp.Atom "rpc_eio")) in
  let answered =
    Dune_rpc_eio.Client.connect chan init ~f:(fun c ->
        let ping =
          Result.get_ok
            (Dune_rpc_eio.Client.Versioned.prepare_request c Public.Request.ping)
        in
        Dune_rpc_eio.Client.request c ping ())
  in
  check "a ping is answered" (answered = Ok ())

(* A reply cut off part way through is a server that went away, not bytes that
   were wrong, so the client reports a dead connection rather than raising. *)
let cut_reply () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let client_end, server_end = pair ~sw in
  Eio.Fiber.fork ~sw (fun () ->
      serve ~cut:true server_end ~f:(fun _ -> Csexp.List []));
  let chan = Dune_rpc_eio.Chan.create client_end in
  let init = Initialize.Request.create ~id:(Id.make (Csexp.Atom "rpc_eio")) in
  let answered =
    Dune_rpc_eio.Client.connect chan init ~f:(fun c ->
        let ping =
          Result.get_ok
            (Dune_rpc_eio.Client.Versioned.prepare_request c Public.Request.ping)
        in
        Dune_rpc_eio.Client.request c ping ())
  in
  check "a cut reply reports a dead connection"
    (match answered with
    | Error e -> Response.Error.kind e = Response.Error.Connection_dead
    | Ok () -> false)

(* A build directory of its own, removed when [f] returns. It goes under /tmp
   rather than TMPDIR because dune points TMPDIR deep inside its own build
   directory, where a unix socket address is longer than one may be. *)
let with_build_dir name f =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_dir ~temp_dir:"/tmp" "rpc_eio" name in
  let root = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
    (fun () -> f env root)

let no_env _ = None

let write_socket_file root where =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / ".rpc");
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(root / ".rpc" / "dune")
    (Where.to_string where)

(* The address a build directory names is read through the capability on that
   directory, and nothing else. *)
let where_reads_the_file () =
  with_build_dir "file" @@ fun _env root ->
  write_socket_file root (`Unix "/ws/_build/.rpc/dune");
  Eio.Path.with_subtree root @@ fun build_dir ->
  check "get reads the address out of .rpc/dune"
    (Dune_rpc_eio.Where.get ~env:no_env ~build_dir
    = Ok (Some (`Unix "/ws/_build/.rpc/dune")))

(* Nothing to read is a refusal to answer rather than an answer of nothing.
   [Ok None] would say a server is there and names nowhere, which sends a
   caller looking for a server that was never started. *)
let where_reports_a_missing_socket () =
  with_build_dir "missing" @@ fun _env root ->
  Eio.Path.with_subtree root @@ fun build_dir ->
  check "get errors when there is no .rpc/dune"
    (match Dune_rpc_eio.Where.get ~env:no_env ~build_dir with
    | Error (Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _)) -> true
    | Error _ | Ok _ -> false)

(* The environment moves where a dune looks, so it settles the question before
   any file is opened. *)
let where_prefers_the_environment () =
  with_build_dir "env" @@ fun _env root ->
  write_socket_file root (`Unix "/from/the/file");
  Eio.Path.with_subtree root @@ fun build_dir ->
  let env v =
    if v = Where.env_var then Some (Where.to_string (`Unix "/from/the/env"))
    else None
  in
  check "get takes DUNE_RPC over the socket file"
    (Dune_rpc_eio.Where.get ~env ~build_dir = Ok (Some (`Unix "/from/the/env")))

(* The capability is the whole of the authority. A build directory that reaches
   out of it is refused, and the file it would have read is there, so an answer
   of [Ok] would be one this capability had no right to give. *)
let where_refuses_an_escape () =
  with_build_dir "escape" @@ fun _env root ->
  write_socket_file root (`Unix "/outside/the/capability");
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(root / "ws");
  Eio.Path.with_subtree Eio.Path.(root / "ws") @@ fun ws ->
  check "get refuses a build_dir outside its capability"
    (match
       Dune_rpc_eio.Where.get ~env:no_env ~build_dir:Eio.Path.(ws / "..")
     with
    | Error _ -> true
    | Ok _ -> false)

(* A listening socket answers for itself, with no file to read. *)
let where_finds_a_socket () =
  with_build_dir "socket" @@ fun env root ->
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / ".rpc");
  let path = Eio.Path.native_exn Eio.Path.(root / ".rpc" / "dune") in
  Eio.Switch.run @@ fun sw ->
  let _ = Eio.Net.listen ~sw ~backlog:1 (Eio.Stdenv.net env) (`Unix path) in
  Eio.Path.with_subtree root @@ fun build_dir ->
  check "get names a socket that is listening"
    (Dune_rpc_eio.Where.get ~env:no_env ~build_dir = Ok (Some (`Unix path)))

(* A host is resolved, so a name reaches a server bound to one of the addresses
   that name has. On most hosts "localhost" has both ::1 and 127.0.0.1 and only
   one of them is bound here, so an attempt that stopped at the first address
   would fail. A port nothing listens on must still be a refusal, since that is
   what a caller reads as nobody being there. *)
let connect_resolves_a_host () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let port l =
    match Eio.Net.listening_addr l with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  let listening =
    Eio.Net.listen ~sw ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork ~sw listening ~on_error:raise (fun flow _ ->
          serve flow ~f:(fun _ -> Csexp.List [])));
  let chan =
    Dune_rpc_eio.connect ~sw ~net
      (`Ip (`Host "localhost", `Port (port listening)))
  in
  let init = Initialize.Request.create ~id:(Id.make (Csexp.Atom "rpc_eio")) in
  let answered =
    Dune_rpc_eio.Client.connect chan init ~f:(fun c ->
        let ping =
          Result.get_ok
            (Dune_rpc_eio.Client.Versioned.prepare_request c Public.Request.ping)
        in
        Dune_rpc_eio.Client.request c ping ())
  in
  check "connect reaches a server named by a host name" (answered = Ok ());
  (* A port bound and then given up, so that nothing is listening on it. *)
  let closed =
    let l =
      Eio.Net.listen ~sw ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let p = port l in
    Eio.Net.close l;
    p
  in
  check "a host with nothing listening is a refusal"
    (match
       Dune_rpc_eio.connect ~sw ~net (`Ip (`Host "localhost", `Port closed))
     with
    | _ -> false
    | exception
        Eio.Io (Eio.Net.E (Eio.Net.Connection_failure (Eio.Net.Refused _)), _)
      ->
        true
    | exception _ -> false)

let () =
  round_trip ();
  cut_reply ();
  connect_resolves_a_host ();
  where_reads_the_file ();
  where_reports_a_missing_socket ();
  where_prefers_the_environment ();
  where_refuses_an_escape ();
  where_finds_a_socket ();
  if !failures > 0 then exit 1
