(* Spike for the proffer backend architecture (see PROFFER.md).

   A miniature multi-domain accept loop in the shape a proffer backend
   would use, on the annotated vendored Eio:
   - the crossing uses the checked [Domain_manager.run], so the compiler
     verifies every capture
   - per-domain code calls the annotated [Switch.run], [Net.accept] and
     [Flow.copy_string] directly from checked-portable context
   - the one assertion is [Obj.magic_portable] on the listening socket,
     which posix sockets support (accept(2) is thread-safe) but
     [Eio.Resource.t] cannot yet express *)

type env = { domain_name : string }

type site = { handle : env -> string -> string @@ portable }

let site =
  { handle = (fun env path -> Printf.sprintf "%s:%s" env.domain_name path) }

let backend_run ~mgr ~sock ~(mk_env @ portable) ~(site : site) n_domains =
  let sock = Obj.magic_portable sock in
  List.init n_domains (fun i ->
      Eio.Domain_manager.run mgr (fun () ->
          let sock = Obj.magic_uncontended sock in
          let env = mk_env i in
          Eio.Switch.run @@ fun sw ->
          let conn, _addr = Eio.Net.accept ~sw sock in
          Eio.Flow.copy_string (site.handle env "/") conn;
          env.domain_name))

let () =
  Eio_main.run @@ fun stdenv ->
  let mgr = Eio.Stdenv.domain_mgr stdenv in
  let net = Eio.Stdenv.net stdenv in
  Eio.Switch.run @@ fun sw ->
  let sock =
    Eio.Net.listen net ~sw ~backlog:8 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let addr = Eio.Net.listening_addr sock in
  let request () =
    Eio.Switch.run @@ fun sw ->
    let conn = Eio.Net.connect ~sw net addr in
    Eio.Buf_read.(parse_exn take_all) conn ~max_size:1024
  in
  let names, replies =
    Eio.Fiber.pair
      (fun () ->
        backend_run ~mgr ~sock
          ~mk_env:(fun i -> { domain_name = Printf.sprintf "d%d" i })
          ~site 2)
      (fun () ->
        let a = request () in
        let b = request () in
        [ a; b ])
  in
  assert (List.sort compare names = [ "d0"; "d1" ]);
  assert (List.sort compare replies = [ "d0:/"; "d1:/" ]);
  print_endline "proffer backend spike: ok (checked Domain_manager.run)"
