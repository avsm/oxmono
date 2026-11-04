(* SPDX-License-Identifier: ISC *)
open Cmdliner

let submit cap pool cache_hint repo commits spec =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level
    (match Sys.getenv_opt "OCLUSTER_LOG" with
    | Some "debug" -> Some Logs.Debug
    | Some "info" -> Some Logs.Info
    | _ -> Some Logs.Warning);
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec = In_channel.with_open_bin spec In_channel.input_all in
  let submission = Ocluster_client.connect ~sw env#net cap in
  let src = Option.map (fun repo -> (repo, commits)) repo in
  let ticket = Ocluster_client.submit submission ~pool ~cache_hint ?src spec in
  prerr_endline "submitted";
  let job = Ocluster_client.job ticket in
  Ocluster_client.stream_log job (fun data ->
      print_string data;
      flush stdout);
  match Ocluster_client.result job with
  | Ok output ->
      if output <> "" then print_endline output;
      Ocluster_client.release ticket;
      0
  | Error message ->
      prerr_endline message;
      Ocluster_client.release ticket;
      1

let () =
  let file name doc =
    Arg.(required & opt (some file) None & info [ name ] ~docv:"FILE" ~doc)
  in
  let term =
    Term.(
      const submit
      $ file "cap" "The scheduler's submission capability file."
      $ Arg.(
          value & opt string "linux-x86_64"
          & info [ "pool" ] ~doc:"The pool to run the job on.")
      $ Arg.(
          value & opt string "oxmono"
          & info [ "cache-hint" ]
              ~doc:"Jobs with the same hint prefer the same worker.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "repo" ] ~docv:"URL" ~doc:"A Git repository to check out.")
      $ Arg.(
          value & opt_all string []
          & info [ "commit" ] ~docv:"SHA" ~doc:"A commit of $(b,--repo).")
      $ Arg.(
          required
          & pos 0 (some file) None
          & info [] ~docv:"SPEC" ~doc:"The OBuilder spec to build."))
  in
  exit
    (Cmd.eval'
       (Cmd.v
          (Cmd.info "ocluster-submit" ~doc:"Run an OBuilder job on OCluster")
          term))
