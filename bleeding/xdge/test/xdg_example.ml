(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let run (xdg, cfg) =
  Fmt.pr "%a@.%a@.@.%a@.%a@."
    Fmt.(styled `Bold string)
    "=== Cmdliner Config ===" Xdge.Cmd.pp cfg
    Fmt.(styled `Bold string)
    "=== XDG Directories ==="
    (Xdge.pp ~brief:false ~sources:true)
    xdg

open Cmdliner

let () =
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  let app_name = "xdg_example" in
  let doc =
    "Example program demonstrating XDG directory selection with Cmdliner"
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This example shows how to use the Xdge library with Cmdliner to \
         handle XDG Base Directory Specification paths with command-line and \
         environment variable overrides.";
      `S Manpage.s_environment;
      `P (Xdge.Cmd.env_docs app_name);
    ]
  in
  let info = Cmdliner.Cmd.info "xdg_example" ~version:"1.0" ~doc ~man in
  Eio_main.run @@ fun env ->
  let create_xdg_term = Xdge.Cmd.term app_name env#fs () in
  let main_term = Term.(const run $ create_xdg_term) in
  let cmd = Cmdliner.Cmd.v info main_term in
  exit @@ Cmdliner.Cmd.eval cmd
