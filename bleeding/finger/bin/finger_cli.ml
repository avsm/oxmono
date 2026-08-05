(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner

let setup_logging style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t =
  Term.(const setup_logging $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let port_t =
  let doc = "TCP port to connect to." in
  Arg.(value & opt int Finger.Server.default_port
       & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let target_t =
  let doc = "[user][@host] query target. Defaults to localhost." in
  Arg.(value & pos 0 string "" & info [] ~docv:"TARGET" ~doc)

let run () port target =
  let user, host =
    match String.index_opt target '@' with
    | Some i ->
      let user = String.sub target 0 i in
      let host = String.sub target (i + 1) (String.length target - i - 1) in
      (user, if host = "" then "localhost" else host)
    | None ->
      (target, "localhost")
  in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let response = Finger.Client.query ~net ~host ~port user in
  print_string response;
  0

let cmd =
  let doc = "Query a Finger server (RFC 1288)." in
  let man = [
    `S Manpage.s_description;
    `P "Connects to a Finger protocol server and displays the response.";
    `P "The target can be a username, @hostname, or user@hostname.";
    `S Manpage.s_examples;
    `Pre "  finger-cli anil@example.com";
    `Pre "  finger-cli @example.com";
    `Pre "  finger-cli -p 7979 anil";
  ] in
  Cmd.v (Cmd.info "finger-cli" ~version:"0.1.0" ~doc ~man)
    Term.(const run $ logging_t $ port_t $ target_t)

let () =
  match Cmd.eval_value cmd with
  | Ok (`Ok exit_code) -> exit exit_code
  | Ok `Help | Ok `Version -> ()
  | Error _ -> exit 1
