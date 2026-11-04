open Cmdliner
module J = Taposaur_jobs
module S = Taposaur_review_store
let ( / ) = Eio.Path.( / )
let get = function Ok x -> x | Error e -> failwith e
let component = "taposaur.jobs"

let execute f = try
  Eio_main.run (fun env -> Eio.Switch.run (fun sw -> f env sw)); Ok ()
  with
  | Failure e | Invalid_argument e | Sqlite3.Error e | Sqlite3.SqliteError e ->
      Error e
  | Taposaur_lock.Busy owner | Taposaur_accept.Busy owner ->
      Error (match owner with
        | None -> "Another process holds the lock"
        | Some o -> Printf.sprintf "Lock held by pid %d on %s (%s since %s)"
            o.pid o.host o.component o.at)
  | S.Not_found -> Error "Review record not found"
  | Eio.Io _ as ex -> Error (Printexc.to_string ex)
let mutate ?(details=[]) action subject f = execute (fun env sw ->
  let audit = Taposaur_audit_cli.open_session ~sw ~fs:env#fs ~component in
  Taposaur_audit.with_operation ~session:audit ~details ~action ~subject
    (fun scoped -> f env sw (Option.get scoped)))

(* Only submission and serving create a queue database. Inspection and
   cancellation of a mistyped path must not leave an empty store behind. *)
let existing path =
  if not (Sys.file_exists path) then
    failwith ("Queue database does not exist: " ^ path)
(* Interrupts only count. A fiber turns the count into a graceful drain or
   an abort, so no Eio state is touched inside the signal handler. *)
let interrupts = Atomic.make 0

let open_store ?(create = false) env sw path =
  if not create then existing path;
  Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
    (env#fs / path)
let open_reader env sw path =
  existing path;
  Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
    ~read_only:true (env#fs / path)
let now env = match Ptime.of_float_s (Eio.Time.now env#clock) with
  | Some t -> t | None -> failwith "Clock is outside the Ptime range"
let timestamp env = Ptime.to_rfc3339 ~frac_s:3 ~tz_offset_s:0 (now env)

let route = function
  | "grobid" -> Taposaur.Manifest.Route.Grobid | "pub2tei" -> Pub2tei
  | r -> invalid_arg ("Unknown conversion route: " ^ r)
let routes grobid pub2tei = (if grobid <> None then [Taposaur.Manifest.Route.Grobid] else [])
  @ (if pub2tei <> None then [Taposaur.Manifest.Route.Pub2tei] else [])

(* Planning reads a frozen snapshot and current archive manifests. Only the
   selected decisions become work, and only when [submit] is used. *)
let plan env sw ~reviews_db ~db ~root ~snapshot ~target ~revision ~actor
    ~grobid ~pub2tei ~limit ~only ~skip_refused ~strategies =
  let store = S.open_read ~sw (env#fs / reviews_db) in
  let archive = Taposaur_archive.open_root ~sw (env#fs / root) in
  let catalog = if Sys.file_exists db then
    Some (Taposaur_catalog.open_read ~sw (env#fs / db)) else None in
  let target = match target with
    | "fulltext" -> J.Plan.Fulltext | "tei" -> J.Plan.Tei
    | _ -> invalid_arg "target must be fulltext or tei" in
  let grobid = Clients.grobid ~sw ?server:grobid env in
  let pub2tei = Clients.pub2tei ~sw ?server:pub2tei env in
  let members = match only with
    | [] -> None
    | dois -> Some (List.map (fun d -> match Taposaur.Uid.of_input d with
        | Ok uid -> uid | Error e -> invalid_arg e) dois) in
  let entries = J.Plan.review ?limit ?members ?catalog ~archive ~skip_refused
    ~configured:(Clients.configured (Clients.secrets ~sw env))
    ~converters:(routes grobid pub2tei) ~target ~revision ~requested_by:actor
    store ~snapshot in
  let entries = match strategies with
    | [] -> entries
    | wanted ->
        List.iter (fun s -> if not (List.mem s J.Acquisition.strategies) then
          invalid_arg ("Unknown strategy " ^ s)) wanted;
        (* Members routed elsewhere stay in the report as unconfigured, so
           the counts still describe the whole snapshot. *)
        List.map (fun (e : J.Plan.entry) -> match e.decision with
          | Acquire r when not (List.mem r.strategy wanted) ->
              {e with decision = Unconfigured r.strategy}
          | _ -> e) entries in
  entries

let report entries =
  List.iter (fun e -> print_endline
    (get (Jsont_bytesrw.encode_string J.Plan.entry_jsont e))) entries;
  let s = J.Plan.summarize entries in
  Printf.eprintf
    "Members %d; acquire %d; convert %d; held %d; refused %d; \
     unsupported %d; unconfigured %d; invalid %d\n%!"
    s.total s.acquire s.convert s.held s.refused s.unsupported s.unconfigured
    s.invalid

let plan_cmd reviews_db db root snapshot target revision actor grobid pub2tei
    limit only skip_refused strategies = execute (fun env sw ->
  report (plan env sw ~reviews_db ~db ~root ~snapshot ~target ~revision
    ~actor ~grobid ~pub2tei ~limit ~only ~skip_refused ~strategies))

(* Single-paper requests and whole-snapshot sweeps get separate dispatch
   lanes by default, so a paper asked for during a sweep is not queued
   behind the sweep. *)
let default_lane ~snapshot ~only = match only with
  | [] -> Printf.sprintf "snapshot:%d" snapshot
  | _ -> "doi"
let submit_cmd queue_db reviews_db db root snapshot target revision actor
    grobid pub2tei limit only skip_refused strategies lane dry_run =
  let lane = Option.value lane ~default:(default_lane ~snapshot ~only) in
  mutate ~details:["snapshot",string_of_int snapshot;"target",target;
    "revision",string_of_int revision;"queue",queue_db;"lane",lane;
    "dry_run",string_of_bool dry_run] "jobs.submit" queue_db (fun env sw audit ->
  let entries = plan env sw ~reviews_db ~db ~root ~snapshot ~target
    ~revision ~actor ~grobid ~pub2tei ~limit ~only ~skip_refused ~strategies in
  report entries;
  if not dry_run then begin
    let queues = J.Queues.v (open_store ~create:true env sw queue_db) in
    let outcome, ids = J.Submit.entries ~lane queues entries in
    List.iter (fun ((e : J.Plan.entry), id) -> Option.iter (fun id ->
      Printf.printf "%s\t%s\n" (Duraml.Id.to_string id) e.doi) id) ids;
    Taposaur_audit.record audit ~action:"jobs.submit.result" ~subject:queue_db
      ~details:["added",string_of_int outcome.added;
        "existing",string_of_int outcome.existing;
        "conflicts",string_of_int outcome.conflicts;
        "skipped",string_of_int outcome.skipped] ();
    Printf.eprintf "Added %d; existing %d; conflicts %d; skipped %d\n%!"
      outcome.added outcome.existing outcome.conflicts outcome.skipped;
    if outcome.conflicts > 0 then
      prerr_endline "Conflicts are the same work requested earlier with \
                     another review or actor; those jobs are reused"
  end)

let init_cmd queue_db =
  mutate ~details:["queue",queue_db] "jobs.init" queue_db (fun env sw _ ->
  let existed = Sys.file_exists queue_db in
  let store = open_store ~create:true env sw queue_db in
  Printf.printf "%s %s (identity %s)\n"
    (if existed then "Opened" else "Created") queue_db
    (Duraml.identity store))

(* Presence only. Values never reach the terminal. *)
let credentials_cmd () = execute (fun env sw ->
  let secrets = Clients.secrets ~sw env in
  Printf.printf "Keychain: %s\n" (if Taposaur_secrets.keychain secrets
    then Sekrit.Paths.root () else "absent (sekrit init creates it)");
  List.iter (fun c ->
    Printf.printf "%-32s %-20s %s\n" (Taposaur_secrets.name c)
      (Taposaur_secrets.variable c)
      (match Taposaur_secrets.origin secrets c with
       | Some Keychain -> "keychain" | Some Environment -> "environment"
       | None -> "absent")) Taposaur_secrets.credentials;
  Printf.printf "Taylor & Francis proxy (TANDF_PROXY): %s\n"
    (if Clients.tandf_proxy () = None then "unset, direct egress"
     else "set");
  Printf.printf "Configured strategies: %s\n"
    (String.concat ", " (Clients.configured secrets)))

let print_job (j : J.Inspect.job) =
  Printf.printf "%s\t%s\t%s\tattempt %d\t%s\t%s\t%s\n" j.id j.queue j.state
    j.attempt (Option.value ~default:"-" j.uid)
    (Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 j.updated)
    (Option.value ~default:"" j.outcome)
let lane_name = function "" -> "unnamed" | lane -> lane
let inspect_cmd queue_db queue state after limit counts = execute (fun env sw ->
  let store = open_reader env sw queue_db in
  if counts then List.iter (fun (c : J.Inspect.counts) ->
    Printf.printf "%s\tqueued %d\trunning %d\tdone %d\tfailed %d\tcancelled %d\n"
      c.queue c.queued c.running c.done_ c.failed c.cancelled)
    (J.Inspect.counts store)
  else begin
    let state = Option.map (fun s -> match J.Inspect.state_of_string s with
      | Some state -> state
      | None -> invalid_arg ("Unknown state: " ^ s)) state in
    let after = Option.map (fun s -> get (Duraml.Id.of_string s)) after in
    List.iter print_job (J.Inspect.list ?queue ?state ?after ~limit store)
  end)
let show_cmd queue_db id = execute (fun env sw ->
  let store = open_reader env sw queue_db in
  let id = get (Duraml.Id.of_string id) in
  match J.Inspect.find store id with
  | None -> failwith "No such job"
  | Some j ->
      print_job j;
      Printf.printf "key: %s\nkind: %s v%d\nlane: %s\ncreated: %s\n\
                     available: %s\n%s\n"
        (Option.value ~default:"-" (Duraml.Spec.key j.spec))
        (Duraml.Spec.kind j.spec) (Duraml.Spec.version j.spec)
        (lane_name j.lane)
        (Ptime.to_rfc3339 ~tz_offset_s:0 j.created)
        (Ptime.to_rfc3339 ~tz_offset_s:0 j.available)
        (Duraml.Spec.payload j.spec))
(* Bulk cancellation revokes only queued jobs: running attempts keep their
   leases and settle normally, so no external effect is abandoned
   half-way. Each page is read after the previous page's cancellations,
   so a job is never cancelled twice. *)
let cancel_queue store queue =
  let rec drain total =
    match J.Inspect.list ~queue ~state:Duraml.Queued ~limit:1000 store with
    | [] -> total
    | page ->
        let n = List.fold_left (fun n (j : J.Inspect.job) ->
          if Duraml.cancel store (get (Duraml.Id.of_string j.id)) then n + 1
          else n) 0 page in
        if n = 0 then total else drain (total + n) in
  drain 0
let cancel_cmd queue_db id queue =
  match id, queue with
  | None, None -> Error "Give a job identifier or --queue"
  | Some _, Some _ -> Error "Give either a job identifier or --queue"
  | Some id, None ->
      mutate ~details:["queue",queue_db] "jobs.cancel" id (fun env sw _ ->
      let store = open_store env sw queue_db in
      if not (Duraml.cancel store (get (Duraml.Id.of_string id))) then
        failwith "Job is missing or already terminal")
  | None, Some queue ->
      mutate ~details:["queue",queue_db;"jobs_queue",queue] "jobs.cancel-queue"
        queue (fun env sw audit ->
      if not (List.mem queue J.Queues.names) then
        failwith ("Unknown queue: " ^ queue);
      let store = open_store env sw queue_db in
      let n = cancel_queue store queue in
      Taposaur_audit.record audit ~action:"jobs.cancel-queue.result"
        ~subject:queue ~details:["cancelled",string_of_int n] ();
      Printf.printf "Cancelled %d queued jobs in %s\n" n queue)

let sync_cmd queue_db cursor follow interval =
  mutate ~details:["queue",queue_db] "jobs.audit-sync" queue_db
    (fun env sw audit ->
  let store = open_reader env sw queue_db in
  let cursor = Option.value cursor ~default:(queue_db ^ ".audit-cursor.json") in
  if follow then
    J.Audit_bridge.run ~mono_clock:env#mono_clock
      ~interval:(Ptime.Span.of_int_s interval) ~cursor store audit
  else Printf.printf "Delivered %d events\n"
    (J.Audit_bridge.deliver ~cursor store audit))

(* One process serves each selected queue. Queue locks live beside the
   database so a second worker for the same publisher is refused instead of
   doubling that publisher's request rate. *)
let work_cmd queue_db staging root db acquire convert refresh concurrency
    lease grobid pub2tei timeout max_bytes publisher_pdf converter_revision
    sync =
  mutate ~details:["queue",queue_db;"staging",staging;"archive_root",root;
    "acquire",String.concat "," acquire;"convert",String.concat "," convert;
    "refresh",string_of_bool refresh;"concurrency",string_of_int concurrency]
    "jobs.work" queue_db (fun env sw audit ->
  if acquire = [] && convert = [] && not refresh && not sync then
    invalid_arg "Select queues with --acquire, --convert, --refresh or --sync";
  if concurrency < 1 || concurrency > 64 then
    invalid_arg "concurrency must be 1..64";
  if converter_revision < 1 then
    invalid_arg "converter-revision must be positive";
  let lease = Ptime.Span.of_int_s lease in
  let store = open_store ~create:true env sw queue_db in
  let queues = J.Queues.v store in
  let owned = List.map (fun queue ->
    J.Worker.own_queue ~component ~prefix:queue_db queue)
    (List.map (fun s -> J.Acquisition.queue ~strategy:s) acquire @
     List.map (fun r -> J.Conversion.queue (route r)) convert @
     (if refresh then [J.Refresh.queue] else [])) in
  Eio.Switch.on_release sw (fun () -> List.iter Taposaur_lock.release owned);
  let stop, resolve = Eio.Promise.create () in
  let previous = Sys.Safe.signal Sys.sigint
    (Sys.Signal_handle (fun _ -> Atomic.incr interrupts)) in
  Eio.Switch.on_release sw (fun () -> Sys.Safe.set_signal Sys.sigint previous);
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec watch () =
      Eio.Time.Mono.sleep env#mono_clock 0.25;
      let n = Atomic.get interrupts in
      if n >= 2 then exit 130;
      if n = 1 && not (Eio.Promise.is_resolved stop) then begin
        prerr_endline "Draining workers; interrupt again to abort";
        Eio.Promise.resolve resolve ()
      end;
      watch () in
    watch ());
  let grobid = Clients.grobid ~sw ?server:grobid env in
  let pub2tei = Clients.pub2tei ~sw ?server:pub2tei env in
  let converters = routes grobid pub2tei in
  let converter r = Taposaur.Manifest.Route.to_string r ^ "-" ^
    string_of_int converter_revision in
  let archive = Taposaur_accept.open_root ~audit ~sw (env#fs / root) in
  let reader = Taposaur_archive.open_root ~sw (env#fs / root) in
  let cmd = "taposaur-jobs" in
  (* Pools live in a nested switch, so the operation's success record is
     written only after every handler has drained. *)
  Eio.Switch.run @@ fun sw ->
  if acquire <> [] then begin
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (env#fs / staging);
    let staging_root = Eio.Path.open_subtree ~sw (env#fs / staging) in
    let c = {J.Worker.downloader = Clients.downloader ~busy_retries:0
        ~publisher_pdf ~timeout ~max_bytes ~sw ~audit
        ~secrets:(Clients.secrets ~sw env) env;
      reader; sw;
      staging_root = (staging_root :> Eio.Fs.dir_ty Eio.Path.t);
      archive; converters; converter;
      clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t);
      audit = Some audit; cmd} in
    List.iter (fun strategy ->
      J.Worker.acquire ~sw ~mono_clock:env#mono_clock ~concurrency ~lease
        ~stop c queues ~strategy) acquire
  end;
  if convert <> [] then begin
    let c = {J.Worker.reader; archive; grobid; pub2tei;
      timeout = Float.max timeout 600.; max_bytes;
      clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t);
      mono_clock = (env#mono_clock :> Eio.Time.Mono.ty Eio.Resource.t);
      audit = Some audit; cmd} in
    List.iter (fun r -> J.Worker.convert ~sw ~mono_clock:env#mono_clock
      ~concurrency ~lease ~stop c queues (route r)) convert
  end;
  if refresh then begin
    let catalog = Taposaur_catalog.open_write ~sw (env#fs / db) in
    let mirrors = Taposaur_mirrors.open_configured ~sw ~fs:env#fs in
    let publisher_lookup doi = Option.bind mirrors (fun m ->
      Taposaur_mirrors.find m doi) in
    let abstracts = Taposaur_abstracts.open_configured ~sw ~fs:env#fs in
    let abstract_lookup doi = Option.bind abstracts (fun a ->
      Taposaur_abstracts.find a doi) in
    let c = {J.Worker.reader; archive; catalog; publisher_lookup; abstract_lookup;
      clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t);
      audit = Some audit} in
    J.Worker.refresh ~sw ~mono_clock:env#mono_clock ~concurrency ~lease
      ~stop c queues
  end;
  if sync then Eio.Fiber.fork ~sw (fun () ->
    J.Audit_bridge.run ~mono_clock:env#mono_clock ~stop
      ~cursor:(queue_db ^ ".audit-cursor.json") store audit);
  Eio.Promise.await stop)

let queue_db = Arg.(value & opt string "taposaur-jobs.db" & info ["queue-db"]
  ~doc:"Duraml SQLite job queue on a local filesystem.")
let reviews_db = Arg.(value & opt string "reviews.db" & info ["reviews-db"]
  ~doc:"Review database, opened read-only.")
let root = Arg.(value & opt string "/data/papers/taposaur" & info ["root"]
  ~doc:"Archive root containing papers and index directories.")
let db = Arg.(value & opt string "taposaur.db" & info ["db"]
  ~doc:"Local SQLite catalogue. Planning reads its holdings when present; \
        refresh workers update it after publication.")
let snapshot = Arg.(required & pos 0 (some int) None & info [] ~docv:"SNAPSHOT")
let target = Arg.(value & opt string "fulltext" & info ["target"]
  ~doc:"fulltext selects papers lacking a PDF or publisher XML; tei selects \
        held inputs lacking TEI.")
let revision = Arg.(value & opt int 1 & info ["revision"]
  ~doc:"Strategy or converter configuration revision in job keys.")
let actor = Arg.(value & opt string "system" & info ["by"]
  ~doc:"Actor recorded as the requester.")
let grobid = Arg.(value & opt (some string) None & info ["grobid"]
  ~doc:"GROBID server URL. Defaults to GROBID_SERVER_URL.")
let pub2tei = Arg.(value & opt (some string) None & info ["pub2tei"]
  ~doc:"Pub2TEI server URL. Defaults to PUB2TEI_SERVER_URL.")
let limit = Arg.(value & opt (some int) None & info ["limit"]
  ~doc:"Maximum snapshot rows to examine.")
let only = Arg.(value & opt_all string [] & info ["doi"]
  ~doc:"Restrict to one snapshot member. Repeatable.")
let strategies = Arg.(value & opt_all string [] & info ["strategy"]
  ~doc:"Submit only acquisitions through this strategy. Repeatable. \
        Members routed to other strategies are reported as unconfigured.")
let skip_refused = Arg.(value & flag & info ["skip-refused"]
  ~doc:"Read each acquirable member's archive manifest and leave out \
        papers the chosen strategy has permanently refused.")
let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"JOB")
let cmd name doc term = Cmd.v (Cmd.info name ~doc) term
let command = Cmd.group (Cmd.info "jobs"
  ~doc:"Plan, submit, run and inspect durable acquisition and conversion work.")
  [cmd "init" "Create an empty queue database, or report an existing one."
     Term.(const init_cmd $ queue_db);
   cmd "credentials" "Report which strategy credentials are available."
     Term.(const credentials_cmd $ const ());
   cmd "plan" "Decide work for a review snapshot without submitting it."
     Term.(const plan_cmd $ reviews_db $ db $ root $ snapshot $ target $
       revision $ actor $ grobid $ pub2tei $ limit $ only $ skip_refused $
       strategies);
   cmd "submit" "Plan a snapshot and submit the selected work."
     Term.(const submit_cmd $ queue_db $ reviews_db $ db $ root $ snapshot $
       target $ revision $ actor $ grobid $ pub2tei $ limit $ only $
       skip_refused $ strategies $
       Arg.(value & opt (some string) None & info ["lane"]
         ~doc:"Dispatch lane shared fairly with other lanes. Defaults to \
               snapshot:SNAPSHOT, or doi when --doi restricts the run.") $
       Arg.(value & flag & info ["dry-run"] ~doc:"Report without submitting."));
   cmd "inspect" "List jobs or per-queue counts."
     Term.(const inspect_cmd $ queue_db $
       Arg.(value & opt (some string) None & info ["queue"]) $
       Arg.(value & opt (some string) None & info ["state"]
         ~doc:"queued, running, done, failed or cancelled.") $
       Arg.(value & opt (some string) None & info ["after"] ~doc:"Job cursor.") $
       Arg.(value & opt int 100 & info ["limit"] ~doc:"Page size, 1..1000.") $
       Arg.(value & flag & info ["counts"] ~doc:"Show per-queue totals."));
   cmd "show" "Describe one job and its request payload."
     Term.(const show_cmd $ queue_db $ id);
   cmd "cancel" "Revoke a queued or running job, or every queued job of a \
                 queue. External effects remain."
     Term.(const cancel_cmd $ queue_db $
       Arg.(value & pos 0 (some string) None & info [] ~docv:"JOB") $
       Arg.(value & opt (some string) None & info ["queue"]
         ~doc:"Cancel every queued job of this queue. Running attempts \
               finish normally."));
   cmd "audit-sync" "Bridge durable queue events into the audit database."
     Term.(const sync_cmd $ queue_db $
       Arg.(value & opt (some string) None & info ["cursor"]
         ~doc:"Cursor file. Defaults beside the queue database.") $
       Arg.(value & flag & info ["follow"] ~doc:"Keep delivering.") $
       Arg.(value & opt int 5 & info ["interval"] ~doc:"Seconds between deliveries."));
   cmd "work" "Serve selected queues until interrupted."
     Term.(const work_cmd $ queue_db $
       Arg.(value & opt string "taposaur-staging" & info ["staging"]
         ~doc:"Local staging tree for downloads before acceptance.") $
       root $ db $
       Arg.(value & opt_all string [] & info ["acquire"]
         ~doc:"Serve one publisher acquisition queue. Repeatable.") $
       Arg.(value & opt_all string [] & info ["convert"]
         ~doc:"Serve a conversion route: grobid or pub2tei. Repeatable.") $
       Arg.(value & flag & info ["refresh"] ~doc:"Serve catalogue refreshes.") $
       Arg.(value & opt int 1 & info ["concurrency"]
         ~doc:"Handlers per served queue, 1..64.") $
       Arg.(value & opt int 300 & info ["lease"] ~doc:"Lease seconds.") $
       grobid $ pub2tei $
       Arg.(value & opt float 120. & info ["timeout"]
         ~doc:"Transfer timeout in seconds. Conversions use at least 600.") $
       Arg.(value & opt int (128 * 1024 * 1024) & info ["max-bytes"]
         ~doc:"Maximum artifact size.") $
       Arg.(value & flag & info ["publisher-pdf"]
         ~doc:"Also try Springer's PDF route using network entitlement.") $
       Arg.(value & opt int 1 & info ["converter-revision"]
         ~doc:"Converter configuration revision in staged conversion keys.") $
       Arg.(value & flag & info ["sync"]
         ~doc:"Also bridge queue events into the audit database."))]
