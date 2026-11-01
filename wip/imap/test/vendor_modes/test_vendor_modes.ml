(* Compile-time probes of the kind and mode claims in the vendored Eio patch
   recorded in vendor/eio/VENDORED.md. The abbreviation in [Kinds] compiles
   only when its kind holds. Each probe is a closure bound at portable mode,
   as in [let (f @ portable) = fun () -> ...], that captures an Eio value or
   a module-level value of an Eio type, so it compiles only when the value is
   portable or the type crosses portability. The runtime checks confirm the
   annotated values still behave. *)

module Kinds = struct
  type key : value mod portable contended = int Eio.Fiber.key
end

type Eio.Exn.err += Probe of string

let key : int Eio.Fiber.key = Eio.Fiber.create_key ()

let (raise_io @ portable) = fun msg -> raise (Eio.Exn.create (Probe msg))

let (caught_io @ portable) = fun f ->
  match f () with
  | () -> false
  | exception ex -> Eio.Exn.is_io ex

(* Only exception constructors need their arguments to cross, so [err] and
   [Backend.t] constructors with payloads that do not cross still work. *)
type Eio.Exn.Backend.t += Backend_probe of string

let (backend @ portable) = fun (b : Eio.Exn.Backend.t) ->
  match Eio.Exn.X b with
  | Eio.Exn.X (Backend_probe s) -> Eio.Exn.create (Probe s)
  | e -> Eio.Exn.create e

let (read_key @ portable) = fun () ->
  let outer = Eio.Fiber.get key in
  outer, Eio.Fiber.with_binding key 2 (fun () -> Eio.Fiber.get key)

(* The systhread closure captures a ref, so it is not portable. *)
let (systhread @ portable) = fun () ->
  let r = ref 0 in
  Eio_unix.run_in_systhread ~label:"probe" (fun () -> incr r; !r)

let (first @ portable) = fun () ->
  Eio.Fiber.first (fun () -> Eio_unix.sleep 0.001; 1) (fun () -> 2)

let (protect @ portable) = fun () -> Eio.Cancel.protect (fun () -> 3)

let (timeout @ portable) = fun clock ->
  Eio.Time.with_timeout_exn clock 1.0 (fun () -> 4),
  match Eio.Time.with_timeout_exn clock 0.001 Eio.Fiber.await_cancel with
  | () -> false
  | exception Eio.Time.Timeout -> true

let (native @ portable) = fun path ->
  Eio.Path.native_exn path, Format.asprintf "%a" Eio.Path.pp path

let () =
  Eio_main.run @@ fun env ->
  assert (caught_io (fun () -> raise_io "probe"));
  assert (not (caught_io (fun () -> ())));
  assert (not (caught_io (fun () -> try raise Exit with Exit -> ())));
  assert (caught_io (fun () -> raise (backend (Backend_probe "b"))));
  assert (read_key () = (None, Some 2));
  assert (Eio.Fiber.with_binding key 1 read_key = (Some 1, Some 2));
  assert (systhread () = 1);
  assert (first () = 2);
  assert (protect () = 3);
  assert (timeout (Eio.Stdenv.clock env) = (4, true));
  let path, shown = native Eio.Path.(Eio.Stdenv.fs env / "/tmp") in
  assert (path = "/tmp");
  assert (shown = "<fs:/tmp>");
  print_endline "vendor modes: ok"
