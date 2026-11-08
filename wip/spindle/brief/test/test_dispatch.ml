(* SPDX-License-Identifier: ISC *)
module Executor = Spindle.Executor
module Job = Spindle.Job

let check name b = if not b then failwith ("check failed: " ^ name)

type seen = {
  mutable steps : (string * string option * Executor.kind) list;
  mutable log : string;
  mutable spawns : (string * (string * string) list * Job.step list) list;
}

let fresh () = { steps = []; log = ""; spawns = [] }

let context seen : Executor.context =
  {
    step =
      (fun ?(kind = `User) ?command name f ->
        seen.steps <- seen.steps @ [ (name, command, kind) ];
        seen.log <- seen.log ^ "## " ^ name ^ "\n";
        f ());
    log = (fun _ text -> seen.log <- seen.log ^ text);
    spawn =
      (fun ?(env = []) name steps ->
        seen.spawns <- seen.spawns @ [ (name, env, steps) ]);
    await_spawned = (fun () -> []);
    spawned = (fun ?since:_ () -> []);
    finished = (fun () -> []);
    queued = (fun f -> f ());
  }

let request ?(env = []) workflow : Executor.request =
  {
    pipeline = "p1";
    workflow;
    repo = "did:web:repo.test";
    source = "https://knot.test/repo";
    commit = String.make 40 'a';
    clone = true;
    metadata = Jsont.Json.null ();
    definition = None;
    yaml = None;
    env;
    variables = [];
    secrets = [];
    steps = [];
    files = { list = (fun _ -> []); read = (fun _ -> None) };
    labels = (fun () -> []);
  }

let within haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  go 0

let report language managers =
  Printf.sprintf
    {|{"version":"0.15.0","path":"/src","languages":[{"name":"%s","category":"language","confidence":"high"}],"package_managers":[%s],"tools":{},"stats":{"duration_ms":1}}|}
    language managers

let opam = {|{"name":"opam","category":"package_manager","confidence":"high"}|}

(* The dispatcher starts the routes a report selects and tags their work. *)
let () =
  let ran = ref [] in
  let route name accepts jobs : Spindle_brief.route =
    {
      name;
      accepts;
      jobs;
      executor =
        (fun ctx r ->
          ran := !ran @ [ (name, r.workflow, r.env) ];
          ctx.spawn "child" [ Job.step "build" "make" ]);
    }
  in
  let routes =
    [
      route "ocaml-ci"
        (fun r -> Brief.package_manager r "opam" <> None)
        [ Job.v "ocaml-ci" [ Job.step "plan" "opam-0install" ] ];
      route "go-ci"
        (fun r -> Brief.language r "Go" <> None)
        [
          Job.v "go-test" [ Job.step "test" "go test ./..." ];
          Job.v "go-vet" [ Job.step "vet" "go vet ./..." ];
        ];
    ]
  in
  let scanned = ref [] in
  let with_report json =
    Spindle_brief.executor
      ~scan:(fun ctx r ->
        scanned := !scanned @ [ r.commit ];
        ctx.log `Stdout "scanning\n";
        json)
      routes
  in
  check "jobs"
    (List.map (fun (j : Job.t) -> (j.name, j.steps)) Spindle_brief.jobs
    = [ ("detect", [ Job.step "detect" "brief -json ." ]) ]);
  (* An OCaml project starts the OCaml route's workflow, tagged with it. *)
  let seen = fresh () in
  with_report (report "OCaml" opam) (context seen) (request "detect");
  check "scanned" (!scanned = [ String.make 40 'a' ]);
  check "steps"
    (seen.steps
    = [
        ("detect", Some "brief -json .", `User);
        ("report", Some "brief -json .", `System);
      ]);
  check "spawn"
    (seen.spawns
    = [
        ( "ocaml-ci",
          [ (Spindle_brief.route_var, "ocaml-ci") ],
          [ Job.step "plan" "opam-0install" ] );
      ]);
  check "scan logged" (within seen.log "## detect\nscanning\n");
  check "summary logged" (within seen.log "Languages:        OCaml\n");
  check "start logged" (within seen.log "Starting ocaml-ci for ocaml-ci.\n");
  check "report logged" (within seen.log "## report\n{\"version\":\"0.15.0\"");
  (* A Go project starts both of its route's workflows. *)
  let seen = fresh () in
  with_report (report "Go" "") (context seen) (request "detect");
  check "go spawns"
    (List.map (fun (n, _, _) -> n) seen.spawns = [ "go-test"; "go-vet" ]);
  (* A project no route tests ends the detect workflow in success. *)
  let seen = fresh () in
  with_report (report "Zig" "") (context seen) (request "detect");
  check "no spawns" (seen.spawns = []);
  check "nothing to do"
    (within seen.log "so there is nothing to do.\n## report\n");
  (* A tagged workflow reaches its route, whose spawns are tagged too. *)
  let seen = fresh () in
  let env = [ (Spindle_brief.route_var, "ocaml-ci"); ("X", "1") ] in
  with_report "" (context seen) (request ~env "ocaml-ci");
  check "forwarded" (!ran = [ ("ocaml-ci", "ocaml-ci", env) ]);
  check "tagged spawn"
    (seen.spawns
    = [
        ( "child",
          [ (Spindle_brief.route_var, "ocaml-ci") ],
          [ Job.step "build" "make" ] );
      ]);
  (* Failures: an unreadable report, no such route, an untagged stranger and
     a repository workflow. *)
  let fails json name request =
    match with_report json (context (fresh ())) request with
    | () -> failwith (name ^ " did not fail")
    | exception Failure m -> m
  in
  check "bad report"
    (String.starts_with ~prefix:"brief's report does not read: "
       (fails "nonsense" "bad report" (request "detect")));
  check "no route"
    (fails "" "no route"
       (request ~env:[ (Spindle_brief.route_var, "rust-ci") ] "x")
    = "this spindle has no route called rust-ci");
  check "stranger"
    (fails "" "stranger" (request "x") = "no route for the workflow x");
  check "repository workflow"
    (String.starts_with ~prefix:"this spindle runs only"
       (fails "" "repository"
          { (request "detect") with definition = Some (Jsont.Json.null ()) }))

let () =
  print_endline "dispatch: detection, routing, tagging and failures passed"
