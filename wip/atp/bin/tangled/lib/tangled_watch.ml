(* SPDX-License-Identifier: ISC *)
module Api = Tangled_api
module Ci = Api.Lex.Ci

type workflow = {
  name : string;
  status : string;
  error : string option;
  started_at : string option;
  finished_at : string option;
}

type pipeline = {
  repo : Api.repository;
  spindle : string;
  id : string;
  commit : string;
  trigger : Jsont.json;
  created_at : string option;
  workflows : workflow list;
}

type change = { workflow : workflow; previous : string option }

type event =
  | Started of pipeline
  | Changed of pipeline * change list
  | Finished of pipeline

let ended = function
  | "success" | "failed" | "timeout" | "cancelled" -> true
  | _ -> false

let finished p =
  p.workflows <> [] && List.for_all (fun w -> ended w.status) p.workflows

let status p =
  let any s = List.exists (fun w -> w.status = s) p.workflows in
  if p.workflows = [] then "pending"
  else
    match
      List.find_opt any
        [ "running"; "pending"; "blocked"; "failed"; "timeout"; "cancelled" ]
    with
    | Some s -> s
    | None -> "success"

let text key json =
  match Tangled_schema.member key json with
  | Some (Jsont.String (s, _)) -> Some s
  | _ -> None

let trigger_kind p =
  match text "$type" p.trigger with
  | Some "org.tangled.event.push" -> "push"
  | Some "org.tangled.event.pullRequest" -> "pull_request"
  | Some "org.tangled.ci.trigger.manual" -> "manual"
  | Some "org.tangled.ci.trigger.schedule" -> "schedule"
  | Some t -> t
  | None -> "unknown"

let trigger_ref p =
  match trigger_kind p with
  | "pull_request" -> text "sourceBranch" p.trigger
  | _ -> text "ref" p.trigger

let pull p =
  if trigger_kind p = "pull_request" then text "pull" p.trigger else None

let web_url ?(appview = "https://tangled.org") ?handle p =
  Printf.sprintf "%s/%s/%s/pipelines/%s" appview
    (Option.value ~default:p.repo.owner handle)
    p.repo.name p.id

let of_lexicon repo ~spindle (p : Ci.Defs.pipeline) =
  {
    repo;
    spindle;
    id = p.id;
    commit = p.commit;
    trigger = p.trigger;
    created_at = p.created_at;
    workflows =
      List.map
        (fun (w : Ci.Defs.workflow_run) ->
          {
            name = Option.value ~default:w.id w.name;
            status = w.status;
            error = w.error;
            started_at = w.started_at;
            finished_at = w.finished_at;
          })
        (Api.workflow_runs p);
  }

type t = {
  api : Api.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  interval : float;
  recent : int;
  known : (string * string, pipeline) Hashtbl.t;
      (* Each repository's pipelines from its last poll, and those being
         followed. *)
  baselined : (string, unit) Hashtbl.t;
      (* Repositories polled at least once, whose finished pipelines from
         before the watcher started are not reported. *)
  mutable resume : (string * string) list;
}

let v ?(interval = 15.) ?(recent = 10) ~clock api =
  {
    api;
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
    interval;
    recent;
    known = Hashtbl.create 64;
    baselined = Hashtbl.create 16;
    resume = [];
  }

let changes ~before p =
  List.filter_map
    (fun w ->
      let previous =
        Option.bind before (fun b ->
            List.find_map
              (fun (x : workflow) ->
                if x.name = w.name then Some x.status else None)
              b.workflows)
      in
      if previous = Some w.status then None else Some { workflow = w; previous })
    p.workflows

(* [observe t ~first p f] reports what changed since [p] was last seen. A
   pipeline first seen in a repository's first poll is reported only if it
   is still running, or if it is being resumed. *)
let observe t ~first p f =
  let key = (p.repo.did, p.id) in
  let before = Hashtbl.find_opt t.known key in
  Hashtbl.replace t.known key p;
  match before with
  | None ->
      let resumed = List.mem key t.resume in
      if resumed then t.resume <- List.filter (( <> ) key) t.resume;
      if (not first) || resumed || not (finished p) then (
        f (Started p);
        if finished p then f (Finished p))
  | Some before ->
      if not (finished before) then (
        let cs = changes ~before:(Some before) p in
        if cs <> [] then f (Changed (p, cs));
        if finished p then f (Finished p))

let poll_repo t (repo : Api.repository) f =
  let spindle = Api.spindle_of t.api repo in
  let first = not (Hashtbl.mem t.baselined repo.did) in
  let page =
    (Api.query_pipelines t.api ~spindle ~repo:repo.did ~limit:t.recent ())
      .pipelines
    |> List.map (fun j ->
        of_lexicon repo ~spindle (Api.decode Ci.Defs.pipeline_jsont j))
  in
  let seen = List.map (fun p -> p.id) page in
  (* Followed pipelines that have fallen off the page are asked for alone. *)
  let missing =
    Hashtbl.fold
      (fun (did, id) p acc ->
        if did = repo.did && (not (finished p)) && not (List.mem id seen) then
          id :: acc
        else acc)
      t.known []
    @ List.filter_map
        (fun (did, id) ->
          if did = repo.did && not (List.mem id seen) then Some id else None)
        t.resume
  in
  let extra =
    List.filter_map
      (fun id ->
        match Api.get_pipeline t.api ~spindle ~repo:repo.did ~pipeline:id with
        | p -> Some (of_lexicon repo ~spindle p)
        | exception (Eio.Cancel.Cancelled _ as e) -> raise e
        | exception _ -> None)
      (List.sort_uniq String.compare missing)
  in
  (* Oldest first, so that events come in the order pipelines started. *)
  List.iter (fun p -> observe t ~first p f) (List.rev page @ extra);
  Hashtbl.replace t.baselined repo.did ();
  (* Finished pipelines that have left the page will not change again. *)
  let keep = seen @ List.map (fun p -> p.id) extra in
  Hashtbl.filter_map_inplace
    (fun (did, id) p ->
      if did = repo.did && finished p && not (List.mem id keep) then None
      else Some p)
    t.known

(* A repository's events are gathered before [f] sees them, so that what [f]
   raises is not taken for the repository's failure. *)
let poll t ~repos ?(on_error = fun _ _ -> ()) f =
  List.iter
    (fun (repo : Api.repository) ->
      let events = ref [] in
      (try poll_repo t repo (fun e -> events := e :: !events) with
      | Eio.Cancel.Cancelled _ as e -> raise e
      | e -> on_error repo e);
      List.iter f (List.rev !events))
    repos

let run t ~repos ?(resume = []) ?on_error f =
  t.resume <- resume;
  let rec loop () =
    poll t ~repos:(repos ()) ?on_error f;
    Eio.Time.sleep t.clock t.interval;
    loop ()
  in
  loop ()
