(* Spike for the proffer design (see PROFFER.md).

   Checks, against the vendored Eio with a [portable] annotation on
   [Eio.Domain_manager.run], that:
   - a compiled-site value whose handlers are portable closures can be
     shared across domains, with the compiler enforcing the discipline
   - per-domain environments are built inside each spawned domain
   - genuinely shared state crosses as an [Atomic.t] *)

(* A stand-in for [Proffer.Compiled.t]: a record of handler closures.
   The [@@ portable] modality on the field makes the record cross
   portability, so one site value can be captured by the closure given
   to [Domain_manager.run]. *)
type env = { domain_name : string }

type site = { handle : env -> string -> string @@ portable }

let hits : int Atomic.t = Atomic.make 0

let site =
  { handle =
      (fun env path ->
        Atomic.incr hits;
        Printf.sprintf "[%s] 200 %s" env.domain_name path)
  }

let () =
  Eio_main.run @@ fun stdenv ->
  let mgr = Eio.Stdenv.domain_mgr stdenv in
  let run_domain name =
    (* This closure crosses domains, so it must be portable. It captures
       [site] (portable record) and [name] (immutable string) only. The
       env is built inside the new domain, as a backend would. *)
    Eio.Domain_manager.run mgr (fun () ->
        let env = { domain_name = name } in
        site.handle env "/index.html")
  in
  Eio.Switch.run @@ fun sw ->
  let a = Eio.Fiber.fork_promise ~sw (fun () -> run_domain "d1") in
  let b = Eio.Fiber.fork_promise ~sw (fun () -> run_domain "d2") in
  let ra = Eio.Promise.await_exn a in
  let rb = Eio.Promise.await_exn b in
  assert (String.equal ra "[d1] 200 /index.html");
  assert (String.equal rb "[d2] 200 /index.html");
  assert (Atomic.get hits = 2);
  print_endline "proffer portability spike: ok"
