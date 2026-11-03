(* SPDX-License-Identifier: ISC *)
open Json

type pipeline = {
  id : string;
  repo : string;
  owner : string;
  source : string;
  request : Jsont.json;
  metadata : Jsont.json;
  commit : string;
  created : string;
  trusted : bool;
  variables : (string * string) list;
  workflows : Runner.t list;
}

type reader = did:string -> commit:string -> (string * string) list

type t = {
  store : Store.t;
  catalog : Catalog.t;
  settings : Settings.t;
  hostname : string;
  jobs : Job.t list;
  read_workflows : reader option;
  runner : Runner.env;
  sw : Eio.Switch.t;
  pipelines : (string, pipeline) Hashtbl.t;
  lock : Eio.Mutex.t;
  mutable last_tid : int64;
  running : (string, int) Hashtbl.t;
  quota_changed : Eio.Condition.t;
}

exception Capacity

let terminal p = List.for_all Runner.terminal p.workflows

let push = "org.tangled.event.push"
let pull_request = "org.tangled.event.pullRequest"
let manual = "org.tangled.ci.trigger.manual"

let kind trigger =
  match get "$type" trigger with
  | t when t = push -> Job.Push
  | t when t = pull_request -> Job.Pull_request
  | t when t = manual -> Job.Manual
  | _ -> invalid "unknown trigger type"

(* Workflows are trusted OCaml values configured by the operator, so their
   definition is external to the repository. *)
let definition hostname name =
  obj
    [
      ("id", str name);
      ("name", str name);
      ( "source",
        obj
          [
            ("$type", str "org.tangled.ci.defs#externalSource");
            ("name", str "OCaml workflow");
            ("link", str ("https://" ^ hostname));
          ] );
      ("triggers", arr []);
    ]

let view_run hostname (run : Runner.t) =
  obj
    (members (Runner.view run)
    @ [
        ( "definition",
          match run.job.definition with
          | Some d -> d
          | None -> definition hostname run.job.name );
      ])

(* A view shows the lexicon's fields of a trigger, not everything its sender
   included, such as a push's options. *)
let public_trigger p =
  let trigger = required "trigger" p.request in
  let keep names =
    List.filter_map
      (fun name ->
        match field name trigger with
        | Some (Jsont.String ("", _)) | None -> None
        | Some v -> Some (name, v))
      names
  in
  let kind = get "$type" trigger in
  obj
    (("$type", str kind)
    ::
    (if kind = push then
       ("repo", str p.repo) :: keep [ "ref"; "oldSha"; "newSha"; "actor" ]
     else if kind = pull_request then
       keep
         [
           "action";
           "pull";
           "sourceRepo";
           "sourceBranch";
           "sourceSha";
           "targetBranch";
         ]
     else if kind = manual then
       [ ("repo", str p.repo); ("commit", str p.commit) ]
       @ keep [ "ref" ]
       @ [ ("inputs", Option.value ~default:(arr []) (field "inputs" trigger)) ]
     else List.remove_assoc "$type" (members trigger)))

let view_in hostname p =
  obj
    ([
       ("id", str p.id);
       ("repo", str p.repo);
       ("trigger", public_trigger p);
       ("commit", str p.commit);
       ("createdAt", str p.created);
       ("workflows", arr (List.map (view_run hostname) p.workflows));
     ]
    @
    match field "sourceRepo" (required "trigger" p.request) with
    | Some (Jsont.String (source, _) as value) when source <> p.repo ->
        [ ("sourceRepo", value) ]
    | _ -> [])

let snapshot t p =
  encode
    (obj
       [
         ("pipeline", view_in t.hostname p);
         ("source", str p.source);
         ("request", p.request);
         ("metadata", p.metadata);
         ("owner", str p.owner);
         ("trusted", bool p.trusted);
         ("variables", Runner.pairs p.variables);
         ("runs", arr (List.map Runner.snapshot p.workflows));
       ])

(* A banned repository's pipelines are wiped, and a workflow still finishing
   must not write its pipeline back. *)
let wiped t p =
  Settings.banned t.settings p.repo || Settings.banned t.settings p.owner

let persist t p =
  if wiped t p then
    Lock.protect t.lock (fun () -> Hashtbl.remove t.pipelines p.id)
  else
    Lock.protect t.lock (fun () ->
        let logs =
          List.map
            (fun (run : Runner.t) -> (p.id, run.job.name, run.log_seq))
            p.workflows
        in
        let finished = terminal p in
        Store.batch ~logs t.store
          ~puts:
            [
              ("pipeline", p.id, snapshot t p);
              ("pipeline-view", p.id, encode (view_in t.hostname p));
            ]
          ~deletes:[];
        if finished then Hashtbl.remove t.pipelines p.id)

(* Quotas. A workflow waits while its repository or owner is at its
   [workflows] limit, and fails at once if their monthly CI minutes are used
   up. Its minutes are charged when it ends, rounded up. *)

let counted t p = List.sort_uniq compare [ p.repo; p.owner ]

let fits t p =
  let room scope did =
    match Settings.limit t.settings scope did "workflows" with
    | None -> true
    | Some limit ->
        Option.value ~default:0 (Hashtbl.find_opt t.running did) < limit
  in
  room Settings.Repo p.repo && room Settings.User p.owner

let acquire t p (workflow : Runner.t) =
  Lock.protect t.lock (fun () ->
      while (not (fits t p)) && not workflow.cancelled do
        Eio.Condition.await t.quota_changed t.lock
      done;
      List.iter
        (fun did ->
          Hashtbl.replace t.running did
            (1 + Option.value ~default:0 (Hashtbl.find_opt t.running did)))
        (counted t p))

let release t p =
  Lock.protect t.lock (fun () ->
      List.iter
        (fun did ->
          match Hashtbl.find_opt t.running did with
          | Some n when n > 1 -> Hashtbl.replace t.running did (n - 1)
          | _ -> Hashtbl.remove t.running did)
        (counted t p));
  Eio.Condition.broadcast t.quota_changed

let minutes_left t p =
  List.find_map
    (fun (scope, did, who) ->
      match Settings.limit t.settings scope did "ci_minutes_monthly" with
      | Some limit when Settings.minutes t.settings did >= limit ->
          Some
            (Printf.sprintf "monthly CI minutes used up (%s): %d of %d" who
               (Settings.minutes t.settings did)
               limit)
      | _ -> None)
    [ (Settings.Repo, p.repo, "repo"); (Settings.User, p.owner, "owner") ]

let running t did = Option.value ~default:0 (Hashtbl.find_opt t.running did)

let start t p =
  List.iter
    (fun workflow ->
      if workflow.Runner.status = "pending" then
        Eio.Fiber.fork ~sw:t.sw (fun () ->
            let input () : Runner.input =
              {
                id = p.id;
                repo = p.repo;
                source = p.source;
                commit = p.commit;
                metadata = p.metadata;
                variables = p.variables;
                secrets =
                  (if p.trusted then Settings.secrets t.settings p.repo
                   else []);
              }
            in
            let fail message =
              workflow.status <- "failed";
              workflow.finished <- Some (Runner.now ());
              workflow.error <- Some message;
              persist t p
            in
            try
              match minutes_left t p with
              | Some message -> fail message
              | None ->
                  acquire t p workflow;
                  let began = Eio.Time.now t.runner.system#clock in
                  Fun.protect ~finally:(fun () ->
                      release t p;
                      let spent = Eio.Time.now t.runner.system#clock -. began in
                      if workflow.started <> None then
                        Settings.add_minutes t.settings (counted t p)
                          (int_of_float (Float.ceil (spent /. 60.))))
                  @@ fun () ->
                  Runner.execute t.runner (input ())
                    ~record:(fun event ->
                      Store.append_log t.store ~pipeline:p.id
                        ~workflow:workflow.job.name event)
                    ~persist:(fun () -> persist t p)
                    workflow
            with exn ->
              Eio.Cancel.protect (fun () ->
                  workflow.status <- "failed";
                  workflow.finished <- Some (Runner.now ());
                  workflow.error <- Some (Runner.message exn);
                  persist t p)))
    p.workflows

let v ~store ~catalog ~settings ~hostname ~jobs ?read_workflows ?(timeout = 60.)
    ~executor ~system ~directory ~sw () =
  if jobs = [] || List.length jobs > 50 then invalid "define 1 to 50 workflows";
  let names =
    List.map
      (fun (j : Job.t) ->
        if
          String.length j.name < 1
          || String.length j.name > 40
          || not
               (String.for_all
                  (function
                    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
                    | _ -> false)
                  j.name)
        then
          invalid "workflow names must be 1 to 40 ASCII letters, digits, _ or -";
        j.name)
      jobs
  in
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    invalid "duplicate workflow name";
  {
    store;
    catalog;
    settings;
    hostname;
    jobs;
    read_workflows;
    sw;
    runner =
      {
        Runner.system;
        directory;
        slots = Eio.Semaphore.make 2;
        timeout;
        executor;
      };
    pipelines = Hashtbl.create 32;
    lock = Eio.Mutex.create ();
    last_tid = 0L;
    running = Hashtbl.create 16;
    quota_changed = Eio.Condition.create ();
  }

let migrate t static =
  if Store.get t.store "schema" "json-import" = None then
    let open Eio.Path in
    let files =
      read_dir t.runner.directory
      |> List.filter (fun name ->
          Filename.check_suffix name ".json"
          && Atp.Tid.is_valid (Filename.remove_extension name))
    in
    let puts =
      List.filter_map
        (fun file ->
          let id = Filename.remove_extension file in
          if Store.get t.store "pipeline" id <> None then None
          else
            let data = decode (load (t.runner.directory / file)) in
            let pipeline = required "pipeline" data in
            let repo = get "repo" pipeline in
            let source =
              match static with
              | Some (configured, source) when configured = repo -> source
              | _ ->
                  invalid
                    "legacy state requires its original --repo and --source"
            in
            let workflow =
              match list (required "workflows" pipeline) with
              | [ workflow ] -> workflow
              | _ -> invalid "invalid legacy workflows"
            in
            let name = get "name" workflow in
            let job =
              match List.find_opt (fun (j : Job.t) -> j.name = name) t.jobs with
              | Some job -> job
              | None -> invalid "legacy workflow is not configured"
            in
            let run = Runner.snapshot (Runner.v job) in
            let run =
              obj
                [
                  ("workflow", workflow);
                  ("events", required "events" data);
                  ("steps", required "steps" run);
                ]
            in
            Some
              ( "pipeline",
                id,
                encode
                  (obj
                     [
                       ("pipeline", pipeline);
                       ("source", str source);
                       ("request", required "request" data);
                       ("metadata", required "metadata" data);
                       ("runs", arr [ run ]);
                     ]) ))
        files
    in
    Store.batch t.store
      ~puts:(("schema", "json-import", "1") :: puts)
      ~deletes:[]

let restore t id raw =
  if not (Atp.Tid.is_valid id) then invalid "invalid stored pipeline ID";
  let data = decode raw in
  let v = required "pipeline" data in
  let repo = get "repo" v in
  {
    id;
    repo;
    owner = Option.fold ~none:repo ~some:string (field "owner" data);
    source = get "source" data;
    request = required "request" data;
    metadata = required "metadata" data;
    commit = sha (get "commit" v);
    created = get "createdAt" v;
    (* Pipelines stored before secrets existed get none. *)
    trusted =
      (match field "trusted" data with
      | Some (Jsont.Bool (b, _)) -> b
      | _ -> false);
    variables =
      Option.fold ~none:[] ~some:Runner.unpairs (field "variables" data);
    workflows =
      List.map
        (fun raw ->
          let run = Runner.restore raw in
          Store.logs t.store ~pipeline:id ~workflow:run.job.name
          |> List.iter (fun (seq, raw) ->
              run.events <- decode raw :: run.events;
              run.log_bytes <- run.log_bytes + String.length raw;
              run.log_seq <- seq);
          run)
        (list (required "runs" data));
  }

let load t =
  if Store.get t.store "schema" "pipeline-views" = None then
    Store.fold t.store "pipeline" ~init:()
      ~f:(fun () (id, raw) ->
        Store.put t.store "pipeline-view" id
          (encode (required "pipeline" (decode raw))))
      ();
  Store.put t.store "schema" "pipeline-views" "1";
  Store.fold t.store "pipeline-view" ~init:()
    ~f:(fun () (id, raw) ->
      if not (Atp.Tid.is_valid id) then invalid "invalid stored pipeline ID";
      t.last_tid <-
        Int64.max t.last_tid (Atp.Tid.timestamp_us (Atp.Tid.of_string id));
      let active =
        list (required "workflows" (decode raw))
        |> List.exists (fun w ->
            List.mem (get "status" w) [ "pending"; "running"; "blocked" ])
      in
      if active then (
        let p = restore t id (Option.get (Store.get t.store "pipeline" id)) in
        Hashtbl.add t.pipelines id p;
        persist t p;
        start t p))
    ()

let view t p = view_in t.hostname p

let first t namespace key =
  Lock.protect t.lock (fun () ->
      Store.get t.store namespace key = None
      &&
      (Store.put t.store namespace key "";
       true))

let managed t id =
  match Catalog.managed t.catalog id with
  | Some repo -> repo
  | None ->
      Xrpc_error.fail Httpz.Res.Not_found "RepoNotFound"
        "failed to access repository"

let check_actor t repo actor =
  if not (Catalog.permitted t.catalog repo actor) then
    Xrpc_error.fail Httpz.Res.Forbidden "AccessControl"
      ("DID does not have sufficient access permissions for this operation: "
     ^ actor)

let authorize t ~principal repo = check_actor t (managed t repo) principal

let find t id =
  if not (Atp.Tid.is_valid id) then None
  else
    match Hashtbl.find_opt t.pipelines id with
    | Some p -> Some p
    | None -> Option.map (restore t id) (Store.get t.store "pipeline" id)

let not_found () =
  Xrpc_error.fail Httpz.Res.Not_found "PipelineNotFound" "pipeline not found"

let query t ~repo ~limit ~cursor ~triggers ~commits =
  let total, more, count, page =
    Store.fold t.store "pipeline-view" ~descending:true ~init:(0, false, 0, [])
      ~f:(fun ((total, more, count, page) as acc) (id, raw) ->
        let p = decode raw in
        let trigger = get "$type" (required "trigger" p) in
        let commit = get "commit" p in
        if
          get "repo" p <> repo
          || (triggers <> [] && not (List.mem trigger triggers))
          || (commits <> [] && not (List.mem commit commits))
        then acc
        else if
          Option.fold ~none:false
            ~some:(fun cursor -> String.compare id cursor >= 0)
            cursor
        then (total + 1, more, count, page)
        else if count = limit then (total + 1, true, count, page)
        else (total + 1, more, count + 1, p :: page))
      ()
  in
  let next =
    if more then [ ("cursor", str (get "id" (List.hd page))) ] else []
  in
  obj ([ ("total", int total); ("pipelines", arr (List.rev page)) ] @ next)

let select_workflows p names =
  let names = List.sort_uniq String.compare names in
  List.iter
    (fun name ->
      if not (List.exists (fun (r : Runner.t) -> r.job.name = name) p.workflows)
      then
        Xrpc_error.fail Httpz.Res.Not_found "WorkflowNotFound"
          ("unknown workflow " ^ name))
    names;
  List.filter
    (fun (r : Runner.t) -> names = [] || List.mem r.job.name names)
    p.workflows

let host_of origin =
  match String.index_opt origin ':' with
  | Some i when String.length origin > i + 3 ->
      String.sub origin (i + 3) (String.length origin - i - 3)
  | _ -> origin

let ref_variables ref_ =
  let short prefix =
    String.sub ref_ (String.length prefix)
      (String.length ref_ - String.length prefix)
  in
  if String.starts_with ~prefix:"refs/heads/" ref_ then
    [
      ("TANGLED_REF", ref_);
      ("TANGLED_REF_NAME", short "refs/heads/");
      ("TANGLED_REF_TYPE", "branch");
    ]
  else if String.starts_with ~prefix:"refs/tags/" ref_ then
    [
      ("TANGLED_REF", ref_);
      ("TANGLED_REF_NAME", short "refs/tags/");
      ("TANGLED_REF_TYPE", "tag");
    ]
  else [ ("TANGLED_REF", ref_); ("TANGLED_REF_NAME", ref_) ]

(* The reference spindle's CI variables, for every step of every workflow. *)
let variables (repo : Catalog.repo) ~default_branch ~id ~kind ~trigger
    ~source_did ~commit =
  let opt k = function None | Some "" -> [] | Some v -> [ (k, v) ] in
  let text k = Option.map string (field k trigger) in
  [
    ("CI", "true");
    ("TANGLED_PIPELINE_ID", id);
    ( "TANGLED_PIPELINE_KIND",
      match kind with
      | Job.Push -> "push"
      | Job.Pull_request -> "pull_request"
      | Job.Manual -> "manual" );
    ("TANGLED_PIPELINE_SOURCE", source_did);
    ("TANGLED_REPO_KNOT", host_of repo.knot);
    ("TANGLED_REPO_DID", repo.owner);
    ("TANGLED_REPO_REPO_DID", repo.did);
    ("TANGLED_REPO_URL", repo.source);
    ("TANGLED_SHA", commit);
    ("TANGLED_COMMIT_SHA", commit);
  ]
  @ opt "TANGLED_REPO_NAME" (Some repo.name)
  @ opt "TANGLED_REPO_DEFAULT_BRANCH" default_branch
  @
  match kind with
  | Job.Push -> ref_variables (get "ref" trigger)
  | Job.Manual ->
      Option.fold ~none:[] ~some:ref_variables (text "ref")
      @ List.map
          (fun pair ->
            ( "TANGLED_INPUT_" ^ String.uppercase_ascii (get "key" pair),
              get "value" pair ))
          (Option.fold ~none:[] ~some:list (field "inputs" trigger))
  | Job.Pull_request ->
      Option.fold ~none:[]
        ~some:(fun b -> ref_variables ("refs/heads/" ^ b))
        (text "sourceBranch")
      @ opt "TANGLED_PIPELINE_SOURCE_BRANCH" (text "sourceBranch")
      @ opt "TANGLED_PIPELINE_TARGET_BRANCH" (text "targetBranch")
      @ opt "TANGLED_PR_SOURCE_BRANCH" (text "sourceBranch")
      @ opt "TANGLED_PR_TARGET_BRANCH" (text "targetBranch")
      @ opt "TANGLED_PR_SOURCE_SHA" (Some commit)

let create t ?dedup ?(changed_files = []) ?(default_ref = false)
    ?(blocked = false) ?(untrusted = false) ?principal ~automatic ~actor request
    =
  let repo = managed t (get "repo" request) in
  if not automatic then
    check_actor t repo (Option.value principal ~default:actor);
  let trigger = required "trigger" request in
  let kind = kind trigger in
  if (not automatic) && kind = Job.Push then
    invalid "push requires a knot event";
  let commit =
    sha
      (get
         (match kind with
         | Job.Push -> "newSha"
         | Job.Pull_request -> "sourceSha"
         | Job.Manual -> "commit")
         trigger)
  in
  let ref_ =
    Option.map string
      (field
         (if kind = Job.Pull_request then "sourceBranch" else "ref")
         trigger)
  in
  (match kind with
  | Job.Push ->
      ignore (get "ref" trigger);
      let old = get "oldSha" trigger in
      if not (zero_sha old) then ignore (sha old)
  | Job.Pull_request ->
      ignore (get "targetBranch" trigger);
      Option.iter
        (fun action ->
          if
            not
              (List.mem (string action)
                 [ "opened"; "reopened"; "closed"; "merged"; "synchronize" ])
          then invalid "invalid pull request action")
        (field "action" trigger)
  | Job.Manual ->
      Option.iter
        (fun inputs ->
          List.iter
            (fun pair ->
              ignore (get "key" pair);
              ignore (get "value" pair))
            (list inputs))
        (field "inputs" trigger));
  let source_did =
    match field "sourceRepo" trigger with
    | Some id when string id <> repo.did -> did (string id)
    | _ -> repo.did
  in
  let source =
    if source_did = repo.did then repo.source
    else Catalog.locate t.catalog source_did
  in
  (* Secrets reach only code the repository's own writers put there: not a
     fork's, nor a pull request that needed approval. *)
  let trusted =
    (not blocked) && (not untrusted) && source_did = repo.did
    && (kind <> Job.Pull_request || field "sourceBranch" trigger <> None)
  in
  let context : Job.context =
    {
      repo = repo.did;
      actor;
      commit;
      kind;
      ref_;
      changed_files;
      default_ref;
      request;
    }
  in
  let names = strings "workflows" request in
  let files =
    match t.read_workflows with
    | None -> []
    | Some read -> read ~did:source_did ~commit
  in
  List.iter
    (fun name ->
      if
        not
          (List.exists (fun (j : Job.t) -> j.name = name) t.jobs
          || List.mem_assoc name files)
      then invalid "unknown workflow")
    names;
  let from_repository, errors =
    Workflows.select ~repo:source_did ~commit ~trigger ~names files
  in
  List.iter
    (fun (name, e) ->
      Printf.eprintf "spindle: %s %s/%s: %s\n%!" repo.did Workflows.directory
        name e)
    errors;
  let jobs =
    List.filter
      (fun (job : Job.t) ->
        (names = [] || List.mem job.name names) && job.accepts context)
      t.jobs
    @ from_repository
  in
  Settings.note_owner t.settings ~repo:repo.did ~owner:repo.owner;
  let default_branch =
    try Catalog.default_branch t.catalog repo
    with Eio.Io _ | Network.Unavailable _ | Failure _ | Invalid _ -> None
  in
  Lock.protect t.lock @@ fun () ->
  if not (Catalog.current t.catalog repo) then
    invalid "repository assignment changed during dispatch";
  match Option.bind dedup (Store.get t.store "dispatch") with
  | Some id -> Some id
  | None when jobs = [] -> None
  | None ->
      let active =
        Hashtbl.fold
          (fun _ p n -> if terminal p then n else n + 1)
          t.pipelines 0
      in
      if active >= 32 then raise Capacity;
      let timestamp =
        Int64.of_float (Eio.Time.now t.runner.system#clock *. 1e6)
      in
      let timestamp = Int64.max timestamp (Int64.succ t.last_tid) in
      t.last_tid <- timestamp;
      let id =
        Atp.Tid.to_string (Atp.Tid.of_timestamp_us ~clockid:0 timestamp)
      in
      let created = Runner.now () in
      let metadata =
        obj
          [
            ("pipeline", str id);
            ("actor", str actor);
            ("spindle", str t.hostname);
            ("receivedAt", str created);
            ("changedFiles", arr (List.map str changed_files));
            ("isDefaultRef", bool default_ref);
            ("request", request);
          ]
      in
      let p =
        {
          id;
          repo = repo.did;
          owner = repo.owner;
          source;
          request;
          metadata;
          commit;
          created;
          trusted;
          variables =
            variables repo ~default_branch ~id ~kind ~trigger ~source_did
              ~commit;
          workflows =
            List.map
              (fun job ->
                let run = Runner.v job in
                if blocked then run.status <- "blocked";
                run)
              jobs;
        }
      in
      let puts =
        [
          ("pipeline", id, snapshot t p);
          ("pipeline-view", id, encode (view t p));
        ]
        @ match dedup with None -> [] | Some key -> [ ("dispatch", key, id) ]
      in
      Store.batch t.store ~puts ~deletes:[];
      Hashtbl.add t.pipelines id p;
      start t p;
      Some id

let cancel t ~actor ~repo ~id ~names =
  let canonical = managed t repo in
  check_actor t canonical actor;
  if not (Catalog.current t.catalog canonical) then
    invalid "repository assignment changed during cancellation";
  match find t id with
  | None -> not_found ()
  | Some p ->
      if p.repo <> repo then not_found ();
      List.iter
        (Runner.cancel ~persist:(fun () -> persist t p))
        (select_workflows p names);
      (* A workflow waiting for quota room notices its cancellation. *)
      Eio.Condition.broadcast t.quota_changed

let approve t ~actor ~repo ~id =
  let canonical = managed t repo in
  check_actor t canonical actor;
  match find t id with
  | None -> not_found ()
  | Some p ->
      if p.repo <> repo then not_found ();
      let blocked =
        List.filter (fun (r : Runner.t) -> r.status = "blocked") p.workflows
      in
      if blocked = [] then invalid "pipeline is not blocked";
      Lock.protect t.lock (fun () ->
          if not (Hashtbl.mem t.pipelines p.id) then
            Hashtbl.add t.pipelines p.id p);
      List.iter (fun (r : Runner.t) -> r.status <- "pending") blocked;
      persist t p;
      start t p

let retry t ~principal ~actor ~repo ~id ~names =
  let canonical = managed t repo in
  check_actor t canonical principal;
  match find t id with
  | None -> not_found ()
  | Some p ->
      if p.repo <> repo then not_found ();
      let names =
        match names with
        | [] -> List.map (fun (r : Runner.t) -> r.job.name) p.workflows
        | names ->
            List.map
              (fun (r : Runner.t) -> r.job.name)
              (select_workflows p names)
      in
      let request =
        obj
          [
            ("repo", str repo);
            ("trigger", required "trigger" p.request);
            ("workflows", arr (List.map str names));
          ]
      in
      (* The actor's push access was checked above. The original trigger may be
         a push, which only a verified event may otherwise create. A retried
         untrusted pipeline stays untrusted. *)
      create t ~untrusted:(not p.trusted) ~automatic:true ~actor request

(* [wipe t repo] cancels and deletes every pipeline of [repo]. *)
let wipe t repo =
  let ids =
    Store.fold t.store "pipeline-view" ~init:[]
      ~f:(fun acc (id, raw) ->
        if get "repo" (decode raw) = repo then id :: acc else acc)
      ()
  in
  List.iter
    (fun id ->
      (match Hashtbl.find_opt t.pipelines id with
      | Some p -> List.iter (Runner.cancel ~persist:(fun () -> ())) p.workflows
      | None -> ());
      Lock.protect t.lock (fun () -> Hashtbl.remove t.pipelines id);
      Store.batch t.store ~puts:[]
        ~deletes:[ ("pipeline", id); ("pipeline-view", id) ])
    ids;
  Catalog.forget t.catalog repo
