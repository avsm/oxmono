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
    { default with first_byte_timeout = 0. };
  check_rejected stdenv "NaN idle timeout"
    { default with idle_timeout = Float.nan };
  check_rejected stdenv "infinite request timeout"
    { default with request_timeout = Float.infinity };
  check_rejected stdenv "negative write timeout"
    { default with write_timeout = -1. };
  Printf.printf "test_config: %d checks ok\n" !checks
