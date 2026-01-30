(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Simple test to isolate the issue - tests One module directly *)
let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Printf.printf "Making One.head request...\n%!";
  try
    let response = Requests.One.head ~sw ~clock:env#clock ~net:env#net
      "https://opam.ocaml.org" in
    Printf.printf "Status: %d\n%!" (Requests.Response.status_code response)
  with e ->
    Printf.printf "Exception: %s\n%!" (Printexc.to_string e);
    Printexc.print_backtrace stdout
