(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let () =
  Eio_main.run @@ fun env ->
  let args = Sys.argv in
  if Array.length args < 2 then (
    Printf.eprintf "Usage: %s <cookies.txt>\n" args.(0);
    exit 1
  );
  let file_path = args.(1) in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(fs / file_path) in
  let jar = Cookeio_jar.load ~clock path in
  Format.printf "%a@." Cookeio_jar.pp jar
