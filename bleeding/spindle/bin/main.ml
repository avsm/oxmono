(* SPDX-License-Identifier: ISC *)
open Cmdliner

let serve hostname owner repo source plc state_dir port addr jetstream
    allow_http operations =
  let repo =
    match (repo, source) with
    | None, None -> None
    | Some repo, Some source -> Some (repo, source)
    | _ -> invalid_arg "--repo and --source must be supplied together"
  in
  Eio_main.run @@ fun env ->
  Spindle.run ~addr ~operations env
    {
      hostname;
      owner;
      repo;
      plc;
      state_dir;
      port;
      jobs = [ Spindle.Job.inspect ];
      jetstream;
      allow_http;
    }

let required name doc =
  Arg.(required & opt (some string) None & info [ name ] ~doc)

let option name default doc =
  Arg.(value & opt string default & info [ name ] ~doc)

let operations =
  let integer name default doc =
    Arg.(value & opt int default & info [ name ] ~doc)
  in
  Term.(
    const
      (fun
        history_days
        history_limit
        history_megabytes
        receipt_days
        receipt_limit
        inbox_limit
        inbox_megabytes
        replay_hours
        reconcile_seconds
        maintenance_seconds
      ->
        Spindle.Operations.v ~history_days ~history_limit ~history_megabytes
          ~receipt_days ~receipt_limit ~inbox_limit ~inbox_megabytes
          ~replay_hours ~reconcile_seconds ~maintenance_seconds ())
    $ integer "history-days" 30 "Completed pipeline retention in days."
    $ integer "history-limit" 1000 "Maximum retained completed pipelines."
    $ integer "history-megabytes" 1024
        "Completed history payload budget in MiB."
    $ integer "receipt-days" 7 "Event receipt retention in days."
    $ integer "receipt-limit" 100000 "Maximum retained event receipts."
    $ integer "inbox-limit" 10000
        "Maximum pending events before applying backpressure."
    $ integer "inbox-megabytes" 64 "Pending event payload budget in MiB."
    $ integer "replay-hours" 24
        "Upstream replay window. Older cursors trigger current-state recovery."
    $ integer "reconcile-seconds" 300
        "Interval between PDS and Git ref reconciliation."
    $ integer "maintenance-seconds" 60
        "Interval between automatic retention passes.")

let command =
  let term =
    Term.(
      const serve
      $ option "hostname" "spindle.tangled.test" "Spindle did:web hostname."
      $ required "owner" "Spindle owner DID."
      $ Arg.(
          value
          & opt (some string) None
          & info [ "repo" ] ~doc:"Optional static repository DID.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "source" ]
              ~doc:"Static Git clone URL or absolute repository path.")
      $ required "plc" "PLC HTTP(S) origin used for signature verification."
      $ option "state-dir" "spindle-state" "Persistent state directory."
      $ Arg.(value & opt int 9000 & info [ "port" ] ~doc:"HTTP listening port.")
      $ option "addr" "127.0.0.1" "HTTP listening IP address."
      $ Arg.(
          value
          & opt (some string) None
          & info [ "jetstream" ]
              ~doc:
                "Jetstream WSS URL for automatic Tangled discovery and events.")
      $ Arg.(
          value & flag
          & info [ "allow-http" ]
              ~doc:"Permit HTTP and WS on a development network.")
      $ operations)
  in
  Cmd.v (Cmd.info "spindle" ~doc:"Run an OCaml Tangled inspection job") term

let () = exit (Cmd.eval command)
