(* SPDX-License-Identifier: ISC *)
open Cmdliner

type 'a make =
  Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  'a ->
  Spindle.Job.t list * Spindle.Executor.t

let serve make hostname owner admins admission repo source plc state_dir port
    addr jetstream allow_http workflow_timeout quota_repo quota_user
    private_webhooks cors_origins contact_email privacy_policy terms_of_service
    concurrency extra operations =
  let repo =
    match (repo, source) with
    | None, None -> None
    | Some repo, Some source -> Some (repo, source)
    | _ -> invalid_arg "--repo and --source must be supplied together"
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let jobs, executor = make env ~sw extra in
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
      jobs;
      jetstream;
      allow_http;
      executor;
      workflow_timeout;
      quotas = { repo = quota_repo; user = quota_user };
      private_webhooks;
      cors_origins;
      contact_email;
      privacy_policy;
      terms_of_service;
      concurrency;
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

(* An origin is a scheme and host, with an optional port, and no path. *)
let origin =
  let parse s =
    let ok =
      s = "*"
      ||
      match String.index_opt s ':' with
      | Some i when i > 0 && String.length s > i + 3 ->
          String.sub s i 3 = "://"
          &&
          let rest = String.sub s (i + 3) (String.length s - i - 3) in
          rest <> ""
          && not (String.exists (fun c -> c = '/' || c = '?' || c = '#') rest)
      | _ -> false
    in
    if ok then Ok s
    else
      Error
        (`Msg
           (Printf.sprintf
              "%S is not an origin such as https://tangled.org, or *" s))
  in
  Arg.conv (parse, Format.pp_print_string)

let main ?(name = "spindle") ?(doc = "Run a Tangled spindle")
    ?(workflow_timeout = 60.) extra make =
  let term =
    Term.(
      const (serve make)
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
          value & opt float workflow_timeout
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
      $ Arg.(
          value & opt_all origin []
          & info [ "cors-origin" ] ~docv:"ORIGIN"
              ~doc:
                "Let browser pages from $(docv), such as \
                 $(b,https://tangled.org), call XRPC methods. $(b,*) allows \
                 any origin. Repeatable.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "contact-email" ] ~docv:"EMAIL"
              ~doc:"Operator contact that $(b,describeSpindle) reports.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "privacy-policy" ] ~docv:"URL"
              ~doc:"Privacy policy that $(b,describeSpindle) links to.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "terms-of-service" ] ~docv:"URL"
              ~doc:"Terms of service that $(b,describeSpindle) links to.")
      $ Arg.(
          value & opt int 2
          & info [ "concurrency" ] ~docv:"N"
              ~doc:
                "Run at most $(docv) workflows at once. An executor that waits \
                 on remote builders can allow many.")
      $ extra $ operations)
  in
  Console_tty.setup ();
  exit (Cmd.eval (Cmd.v (Cmd.info name ~doc) term))
