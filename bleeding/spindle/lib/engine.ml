(* SPDX-License-Identifier: ISC *)
open Json

type pipeline = {
  id : string;
  repo : string;
  source : string;
  request : Jsont.json;
  metadata : Jsont.json;
  commit : string;
  created : string;
  workflows : Runner.t list;
}

type t = {
  store : Store.t;
  catalog : Catalog.t;
  hostname : string;
  jobs : Job.t list;
  runner : Runner.env;
  sw : Eio.Switch.t;
  pipelines : (string, pipeline) Hashtbl.t;
  lock : Eio.Mutex.t;
  mutable last_tid : int64;
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
    @ [ ("definition", definition hostname run.job.name) ])

let view_in hostname p =
  obj
    ([
       ("id", str p.id);
       ("repo", str p.repo);
       ("trigger", required "trigger" p.request);
       ("commit", str p.commit);
       ("createdAt", str p.created);
       ("workflows", arr (List.map (view_run hostname) p.workflows));
     ]
    @
    match field "sourceRepo" (required "trigger" p.request) with
    | None -> []
    | Some value -> [ ("sourceRepo", value) ])

let snapshot t p =
  encode
    (obj
       [
         ("pipeline", view_in t.hostname p);
         ("source", str p.source);
         ("request", p.request);
         ("metadata", p.metadata);
         ("runs", arr (List.map Runner.snapshot p.workflows));
       ])

let persist t p =
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

let start t p =
  List.iter
    (fun workflow ->
      if workflow.Runner.status = "pending" then
        Eio.Fiber.fork ~sw:t.sw (fun () ->
            let input : Runner.input =
              {
                id = p.id;
                repo = p.repo;
                source = p.source;
                commit = p.commit;
                metadata = p.metadata;
              }
            in
            try
              Runner.execute t.runner input
                ~record:(fun event ->
                  Store.append_log t.store ~pipeline:p.id
                    ~workflow:workflow.job.name event)
                ~persist:(fun () -> persist t p)
                workflow
            with exn ->
              Eio.Cancel.protect (fun () ->
                  workflow.status <- "failed";
                  workflow.finished <- Some (Runner.now ());
                  workflow.error <- Some (Printexc.to_string exn);
                  persist t p)))
    p.workflows

let v ~store ~catalog ~hostname ~jobs ~system ~directory ~sw =
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
        List.iter
          (function Job.Command [] -> invalid "empty command" | _ -> ())
          j.steps;
        j.name)
      jobs
  in
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    invalid "duplicate workflow name";
  {
    store;
    catalog;
    hostname;
    jobs;
    sw;
    runner = { Runner.system; directory; slots = Eio.Semaphore.make 2 };
    pipelines = Hashtbl.create 32;
    lock = Eio.Mutex.create ();
    last_tid = 0L;
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
  {
    id;
    repo = get "repo" v;
    source = get "source" data;
    request = required "request" data;
    metadata = required "metadata" data;
    commit = sha (get "commit" v);
    created = get "createdAt" v;
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
        List.iter
          (fun (run : Runner.t) ->
            if
              not
                (String.for_all
                   (function
                     | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
                     | _ -> false)
                   run.job.name)
            then invalid "invalid stored workflow name";
            Eio.Process.run t.runner.system#process_mgr
              [
                "rm";
                "-rf";
                "--";
                Eio.Path.(
                  native_exn
                    (t.runner.directory / (id ^ "." ^ run.job.name ^ ".work")));
              ])
          p.workflows;
        Hashtbl.add t.pipelines id p;
        persist t p;
        start t p))
    ()

let view t p = view_in t.hostname p

let managed t id =
  match Catalog.managed t.catalog id with
  | Some repo -> repo
  | None -> invalid "repository is not assigned to spindle"

let check_actor t repo actor =
  if not (Catalog.authorized t.catalog repo actor) then raise Auth.Rejected

let find t id =
  if not (Atp.Tid.is_valid id) then invalid "pipeline must be a TID";
  match Hashtbl.find_opt t.pipelines id with
  | Some p -> Some p
  | None -> Option.map (restore t id) (Store.get t.store "pipeline" id)

let query t ~repo ~limit ~cursor ~triggers ~commits =
  let seen = Hashtbl.create (List.length commits) in
  let total, count, page =
    Store.fold t.store "pipeline-view" ~descending:true ~init:(0, 0, [])
      ~f:(fun ((total, count, page) as acc) (id, raw) ->
        let p = decode raw in
        let trigger = get "$type" (required "trigger" p) in
        let commit = get "commit" p in
        if
          get "repo" p <> repo
          || (triggers <> [] && not (List.mem trigger triggers))
          || commits <> []
             && ((not (List.mem commit commits)) || Hashtbl.mem seen commit)
        then acc
        else (
          if commits <> [] then Hashtbl.add seen commit ();
          if
            Option.fold ~none:false
              ~some:(fun cursor -> String.compare id cursor >= 0)
              cursor
          then acc
          else if count = limit then (total + 1, count, page)
          else (total + 1, count + 1, p :: page)))
      ()
  in
  let next =
    if total > count then [ ("cursor", str (get "id" (List.hd page))) ] else []
  in
  obj ([ ("total", int total); ("pipelines", arr (List.rev page)) ] @ next)

let select_workflows p names =
  let names = List.sort_uniq String.compare names in
  List.iter
    (fun name ->
      if not (List.exists (fun (r : Runner.t) -> r.job.name = name) p.workflows)
      then invalid "unknown workflow")
    names;
  List.filter
    (fun (r : Runner.t) -> names = [] || List.mem r.job.name names)
    p.workflows

let create t ?dedup ?(changed_files = []) ?(default_ref = false)
    ?(blocked = false) ~automatic ~actor request =
  let repo = managed t (get "repo" request) in
  if not automatic then check_actor t repo actor;
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
  let source =
    match field "sourceRepo" trigger with
    | Some id when string id <> repo.did ->
        Catalog.locate t.catalog (did (string id))
    | _ -> repo.source
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
  List.iter
    (fun name ->
      if not (List.exists (fun (j : Job.t) -> j.name = name) t.jobs) then
        invalid "unknown workflow")
    names;
  let jobs =
    List.filter
      (fun (job : Job.t) ->
        (names = [] || List.mem job.name names) && job.accepts context)
      t.jobs
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
          source;
          request;
          metadata;
          commit;
          created;
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
  | None -> invalid "pipeline not found"
  | Some p ->
      if p.repo <> repo then invalid "pipeline belongs to another repository";
      List.iter
        (Runner.cancel ~persist:(fun () -> persist t p))
        (select_workflows p names)

let approve t ~actor ~repo ~id =
  let canonical = managed t repo in
  check_actor t canonical actor;
  match find t id with
  | None -> invalid "pipeline not found"
  | Some p ->
      if p.repo <> repo then invalid "pipeline belongs to another repository";
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

let retry t ~actor ~repo ~id ~names =
  let canonical = managed t repo in
  check_actor t canonical actor;
  match find t id with
  | None -> invalid "pipeline not found"
  | Some p ->
      if p.repo <> repo then invalid "pipeline belongs to another repository";
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
         a push, which only a verified event may otherwise create. *)
      create t ~automatic:true ~actor request
