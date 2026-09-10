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

let kind trigger =
  match get "$type" trigger with
  | "sh.tangled.ci.trigger#push" -> Job.Push
  | "sh.tangled.ci.trigger#pullRequest" -> Job.Pull_request
  | "sh.tangled.ci.trigger#manual" -> Job.Manual
  | _ -> invalid "unknown trigger type"

let view p =
  obj
    ([
       ("id", str p.id);
       ("repo", str p.repo);
       ("trigger", required "trigger" p.request);
       ("commit", str p.commit);
       ("createdAt", str p.created);
       ("workflows", arr (List.map Runner.view p.workflows));
     ]
    @
    match field "sourceRepo" (required "trigger" p.request) with
    | None -> []
    | Some value -> [ ("sourceRepo", value) ])

let snapshot p =
  encode
    (obj
       [
         ("pipeline", view p);
         ("source", str p.source);
         ("request", p.request);
         ("metadata", p.metadata);
         ("runs", arr (List.map Runner.snapshot p.workflows));
       ])

let persist t p =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      Store.batch t.store
        ~puts:
          [
            ("pipeline", p.id, snapshot p);
            ("pipeline-view", p.id, encode (view p));
          ]
        ~deletes:[];
      if terminal p then Hashtbl.remove t.pipelines p.id)

let start t p =
  List.iter
    (fun workflow ->
      if not (Runner.terminal workflow) then
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

let restore id raw =
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
    workflows = List.map Runner.restore (list (required "runs" data));
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
            List.mem (get "status" w) [ "pending"; "running" ])
      in
      if active then (
        let p = restore id (Option.get (Store.get t.store "pipeline" id)) in
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
  | None -> Option.map (restore id) (Store.get t.store "pipeline" id)

let query t ~repo ~limit ~cursor ~kinds ~commits =
  let seen = Hashtbl.create (List.length commits) in
  let total, count, page =
    Store.fold t.store "pipeline-view" ~descending:true ~init:(0, 0, [])
      ~f:(fun ((total, count, page) as acc) (id, raw) ->
        let p = decode raw in
        let kind =
          match kind (required "trigger" p) with
          | Job.Push -> "push"
          | Job.Manual -> "manual"
          | Job.Pull_request -> "pull_request"
        in
        let commit = get "commit" p in
        if
          get "repo" p <> repo
          || (kinds <> [] && not (List.mem kind kinds))
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

let create t ?dedup ?(changed_files = []) ?(default_ref = false) ~automatic
    ~actor request =
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
         | Job.Manual -> "sha")
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
      ignore (sha (get "oldSha" trigger))
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
    | None -> repo
    | Some id -> Catalog.verified t.catalog (did (string id))
  in
  (* Static mappings may expose private local mirrors. A member's public
     target repository does not authorize reading the operator's mirror. *)
  if source.did <> repo.did && source.knot = "" then check_actor t source actor;
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
  Eio.Mutex.use_rw ~protect:true t.lock @@ fun () ->
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
          source = source.source;
          request;
          metadata;
          commit;
          created;
          workflows = List.map Runner.v jobs;
        }
      in
      let puts =
        [ ("pipeline", id, snapshot p); ("pipeline-view", id, encode (view p)) ]
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
