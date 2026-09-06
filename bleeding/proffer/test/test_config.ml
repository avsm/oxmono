open Proffer

let checks = ref 0

let check_rejected stdenv name config =
  incr checks;
  match
    Proffer_httpz.run ~config stdenv ~env:() (Site.of_routes [])
  with
  | () -> failwith (name ^ " was accepted")
  | exception Invalid_argument _ -> ()

let () =
  Eio_main.run @@ fun stdenv ->
  let default = Proffer_httpz.default_config in
  check_rejected stdenv "zero backlog" { default with backlog = 0 };
  check_rejected stdenv "negative connection limit"
    { default with max_connections = -1 };
  check_rejected stdenv "zero first-byte timeout"
    { default with first_byte_timeout = Duration.of_sec 0 };
  check_rejected stdenv "zero idle timeout"
    { default with idle_timeout = (Duration.of_sec 0) };
  check_rejected stdenv "zero request timeout"
    { default with request_timeout = (Duration.of_sec 0) };
  check_rejected stdenv "zero write timeout"
    { default with write_timeout = (Duration.of_sec 0) };
  Eio.Switch.run (fun sw ->
      let exception Listening_failed in
      let bound = ref None in
      (match Proffer_httpz.run ~sw ~port:0
           ~on_listening:(fun addr -> bound := Some addr; raise Listening_failed)
           stdenv ~env:() (Site.of_routes []) with
      | () -> failwith "listening callback failure was ignored"
      | exception Listening_failed -> ());
      incr checks;
      match Eio.Net.connect ~sw stdenv#net (Option.get !bound) with
      | flow -> Eio.Net.close flow; failwith "failed listener retained its port"
      | exception Eio.Io _ -> ());
  Printf.printf "test_config: %d checks ok\n" !checks
