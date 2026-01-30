(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Test using One module directly without connection pooling *)
let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  try
    let response = Requests.One.get ~sw ~clock:env#clock ~net:env#net
      "https://opam.ocaml.org" in
    Printf.printf "Status: %d\n%!" (Requests.Response.status_code response)
  with e ->
    Printf.printf "Exception: %s\n%!" (Printexc.to_string e);
    Printexc.print_backtrace stdout
