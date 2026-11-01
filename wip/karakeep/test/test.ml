(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Basic smoke tests for generated Karakeep types *)

let () =
  (* Test that we can create a karakeep client type *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let _client = Karakeep.create ~sw env ~base_url:"http://localhost:9090" in
  print_endline "Karakeep client created successfully"
