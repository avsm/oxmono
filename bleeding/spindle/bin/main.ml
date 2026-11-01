(* SPDX-License-Identifier: ISC *)
open Cmdliner

let serve hostname owner admins admission repo source plc state_dir port addr
    jetstream allow_http repository_workflows workflow_timeout quota_repo
    quota_user private_webhooks operations =
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
      admins;
      admission;
      repo;
      plc;
      state_dir;
      port;
      jobs = [ Spindle.Job.inspect ];
      jetstream;
      allow_http;
      repository_workflows;
      workflow_timeout;
      quotas = { repo = quota_repo; user = quota_user };
      private_webhooks;
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
        "Jetstream replay window. An older cursor restarts from now."
    $ integer "reconcile-seconds" 300
        "How long a repository's acceptance is cached."
    $ integer "maintenance-seconds" 60
        "Interval between automatic retention passes.")

let quota name whose =
  Arg.(
    value
    & opt_all (pair ~sep:'=' string int) []
    & info [ name ] ~docv:"RESOURCE=N"
        ~doc:
          (Printf.sprintf
             "Limit %s to N of RESOURCE unless an administrator sets \
              otherwise. The enforced resources are $(b,workflows) running at \
              once, $(b,webhooks) and $(b,ci_minutes_monthly). Repeatable."
             whose))

let command =
  let term =
    Term.(
      const serve
      $ option "hostname" "spindle.tangled.test" "Spindle did:web hostname."
      $ required "owner" "Spindle owner DID."
      $ Arg.(
          value & opt_all string []
          & info [ "admin" ] ~docv:"DID"
              ~doc:"Additional administrator DID (repeatable).")
      $ Arg.(
          value
          & opt
              (enum
                 [
                   ("closed", Spindle.Catalog.Closed);
                   ("open", Spindle.Catalog.Open);
                 ])
              Spindle.Catalog.Closed
          & info [ "admission" ]
              ~doc:
                "Which repository owners are served: $(b,closed) admits \
                 administrators and allow-listed owners, $(b,open) admits \
                 anyone.")
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
                "Jetstream WSS URL to follow pull requests and manifest \
                 changes.")
      $ Arg.(
          value & flag
          & info [ "allow-http" ]
              ~doc:"Permit HTTP and WS on a development network.")
      $ Arg.(
          value & flag
          & info [ "repository-workflows" ]
              ~doc:
                "Also run the YAML workflows in each repository's \
                 $(b,.tangled/workflows). Their steps run on this host with \
                 this service's access, without the reference spindle's \
                 containers or virtual machines, so admit only owners whose \
                 code you trust.")
      $ Arg.(
          value & opt float 60.
          & info [ "workflow-timeout" ] ~docv:"SECONDS"
              ~doc:"How long a workflow may run before it is stopped.")
      $ quota "quota-repo" "a repository"
      $ quota "quota-user" "an owner's repositories together"
      $ Arg.(
          value & flag
          & info [ "private-webhooks" ]
              ~doc:
                "Let webhooks reach loopback and private addresses, on a \
                 development network.")
      $ operations)
  in
  Cmd.v (Cmd.info "spindle" ~doc:"Run an OCaml Tangled inspection job") term

let () =
  Console_tty.setup ();
  exit (Cmd.eval command)
