(* SPDX-License-Identifier: ISC *)

type config = {
  scheduler : Ocluster_client.scheduler;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  opam_repository : Opam_repo.t;
  oxcaml_repository : Opam_repo.t option;
  compilers : string list;
  platforms : Variant.platform list;
}

module Job = Spindle.Job

let variant_var = "OCAMLCI_VARIANT"
let pool_var = "OCAMLCI_POOL"
let jobs = [ Job.v "ocaml-ci" [ Job.step "plan" "opam-0install" ] ]

let logf (ctx : Spindle.Executor.context) fmt =
  Printf.ksprintf (fun s -> ctx.log `Stdout s) fmt

let first_lines n s =
  String.split_on_char '\n' s
  |> List.filteri (fun i _ -> i < n)
  |> List.map (fun l -> "  " ^ l)
  |> String.concat "\n"

(* [snapshots repos f] is [f] applied to each repository's commit and packages
   directory, all held still while [f] runs. *)
let rec snapshots repos f =
  match repos with
  | [] -> f []
  | repo :: rest ->
      Opam_repo.with_snapshot repo (fun ~commit ~packages ->
          snapshots rest (fun l -> f ((repo, commit, packages) :: l)))

let env_of (v : Variant.t) = [ (variant_var, v.name); (pool_var, v.pool) ]

(* The [ocaml-ci] workflow: find the project, solve, spawn the builds. The
   newest OCaml's build carries the builds for the other platforms as further
   steps, which it spawns once it passes. *)
let plan config (ctx : Spindle.Executor.context) (r : Spindle.Executor.request)
    =
  let project = ref None in
  ctx.step ~command:"ls *.opam" "analyse" (fun () ->
      match Analysis.find r.files with
      | None ->
          logf ctx
            "No opam files at the root of the repository, so there is nothing \
             to build.\n"
      | Some a ->
          project := Some a;
          List.iter (fun (f, _) -> logf ctx "%s\n" f) a.opam_files);
  Option.iter
    (fun (a : Analysis.t) ->
      let builds = ref [] and later = ref [] in
      ctx.step ~command:"opam-0install" "solve" (fun () ->
          let repos =
            config.opam_repository :: Option.to_list config.oxcaml_repository
          in
          snapshots repos @@ fun snaps ->
          List.iter
            (fun (repo, commit, _) ->
              logf ctx "%s at %s\n" (Opam_repo.url repo) commit)
            snaps;
          let _, default_commit, default = List.hd snaps in
          let oxcaml = List.nth_opt snaps 1 in
          let solve (v : Variant.t) =
            let repositories =
              (match (v.oxcaml, oxcaml) with
                | true, Some (_, _, packages) -> [ packages ]
                | _ -> [])
              @ [ default ]
            in
            let request =
              {
                Solve.platform = v.vars;
                repositories;
                pins = Analysis.packages a;
                compiler = v.compiler;
                requires = v.requires;
              }
            in
            match Eio_unix.run_in_systhread (fun () -> Solve.solve request) with
            | Ok packages ->
                logf ctx "%s: %d packages\n" v.name (List.length packages);
                Some
                  ( v,
                    Spec.v ~variant:v ~opam_repository:default_commit
                      ?oxcaml_repository:
                        (Option.map
                           (fun (repo, commit, _) ->
                             (Opam_repo.url repo, commit))
                           oxcaml)
                      ~opam_files:(List.map fst a.opam_files)
                      ~packages () )
            | Error why ->
                logf ctx "%s: no solution\n%s\n" v.name (first_lines 20 why);
                None
          in
          let variants =
            Variant.choose ~packages:default config.compilers
            @ match oxcaml with Some _ -> [ Variant.oxcaml ] | None -> []
          in
          builds := List.filter_map solve variants;
          (* [choose] puts the newest OCaml first. *)
          match
            List.find_opt (fun ((v : Variant.t), _) -> not v.oxcaml) !builds
          with
          | None -> ()
          | Some (primary, _) ->
              let others =
                List.filter_map
                  (fun platform ->
                    let v = Variant.v ~platform (Variant.version primary) in
                    if
                      List.exists
                        (fun ((b : Variant.t), _) -> b.name = v.name)
                        !builds
                    then None
                    else Some v)
                  config.platforms
              in
              if others <> [] then logf ctx "Once %s passes:\n" primary.name;
              later := List.filter_map solve others);
      if !builds = [] then failwith "no compiler has a solution";
      let primary =
        List.find_opt (fun ((v : Variant.t), _) -> not v.oxcaml) !builds
      in
      List.iter
        (fun (((v : Variant.t), spec) as build) ->
          let after =
            if Option.map fst primary = Some (fst build) then
              List.map
                (fun ((v : Variant.t), spec) ->
                  Job.step ~env:(env_of v) v.name spec)
                !later
            else []
          in
          ctx.spawn ~env:(env_of v) v.name (Job.step "obuilder" spec :: after))
        !builds)
    !project

type line = Line of string | End

let max_line = 16384
let max_joined = 64

(* An [obuilder] workflow: submit its spec and follow the job. *)
let build config (ctx : Spindle.Executor.context) (r : Spindle.Executor.request)
    =
  let spec, after =
    match r.steps with
    | s :: after -> (s.command, after)
    | [] -> invalid_arg "an OBuilder workflow has no spec"
  in
  let pool =
    match List.assoc_opt pool_var r.env with
    | Some pool -> pool
    | None -> invalid_arg "an OBuilder workflow has no pool"
  in
  let lines = Eio.Stream.create 4096 in
  (* Lines of the job's log, ending with [End]. Only a chunk's own text is
     searched for line ends. *)
  let follow job =
    let partial = Buffer.create 256 in
    Ocluster_client.stream_log job (fun chunk ->
        let start = ref 0 in
        String.iteri
          (fun i c ->
            if c = '\n' then (
              Buffer.add_substring partial chunk !start (i - !start);
              Eio.Stream.add lines (Line (Buffer.contents partial));
              Buffer.clear partial;
              start := i + 1))
          chunk;
        Buffer.add_substring partial chunk !start (String.length chunk - !start);
        if Buffer.length partial >= max_line then (
          Eio.Stream.add lines (Line (Buffer.contents partial));
          Buffer.clear partial));
    if Buffer.length partial > 0 then
      Eio.Stream.add lines (Line (Buffer.contents partial));
    Eio.Stream.add lines End
  in
  (* An operation printed over several lines continues until its brackets
     close, for at most [max_joined] lines, so that output imitating an
     unclosed operation cannot swallow the log. *)
  let rest_of op =
    let b = Buffer.create 256 in
    Buffer.add_string b op;
    let rec go depth n =
      if depth <= 0 || n >= max_joined then Buffer.contents b
      else
        match Eio.Stream.take lines with
        | Line l ->
            let l = String.trim (Obuilder_log.strip_ansi l) in
            Buffer.add_char b ' ';
            Buffer.add_string b l;
            go (depth + Obuilder_log.depth l) (n + 1)
        | End ->
            Eio.Stream.add lines End;
            Buffer.contents b
    in
    go (Obuilder_log.depth op) 0
  in
  (* Each section of the log is a step, until the log ends with the job's
     result. *)
  let rec section ~kind ?command name job =
    let next = ref None in
    ctx.step ~kind ?command name (fun () ->
        let rec loop () =
          match Eio.Stream.take lines with
          | End -> (
              match Ocluster_client.result job with
              | Ok _ -> ()
              | Error message ->
                  ctx.log `Stderr (message ^ "\n");
                  failwith message)
          | Line l -> (
              match Obuilder_log.operation l with
              | Some op -> next := Some (rest_of op)
              | None ->
                  ctx.log `Stdout (l ^ "\n");
                  loop ())
        in
        loop ());
    Option.iter
      (fun op ->
        section ~kind:`User ~command:op (Obuilder_log.step_name op) job)
      !next
  in
  let submission = Ocluster_client.submission config.scheduler in
  Fun.protect ~finally:(fun () -> Ocluster_client.release_submission submission)
  @@ fun () ->
  let ticket =
    Ocluster_client.submit submission ~pool
      ~cache_hint:(r.repo ^ "/" ^ r.workflow)
      ~src:(r.source, [ r.commit ]) spec
  in
  let job = Ocluster_client.job ticket in
  (* A cancelled workflow cancels its job, over the scheduler's connection,
     which outlives the workflow. Releasing a ticket also cancels its job, so
     it is released only once the job has finished. *)
  Fun.protect
    ~finally:(fun () ->
      Ocluster_client.release_job job;
      Ocluster_client.release ticket)
    (fun () ->
      try
        Eio.Fiber.both
          (fun () -> follow job)
          (fun () ->
            section ~kind:`System
              ~command:("ocluster submit --pool " ^ pool)
              "queue" job)
      with Eio.Cancel.Cancelled _ as e ->
        (* A scheduler that stops answering must not hold the workflow, and
           its concurrency slot, forever. *)
        Eio.Cancel.protect (fun () ->
            try
              Eio.Time.Timeout.run_exn
                (Eio.Time.Timeout.seconds config.clock 30.) (fun () ->
                  Ocluster_client.cancel_ticket ticket)
            with _ -> ());
        raise e);
  List.iter
    (fun (s : Job.step) ->
      ctx.spawn ~env:s.env s.name [ Job.step "obuilder" s.command ])
    after

let executor config ctx (r : Spindle.Executor.request) =
  if r.definition <> None then
    failwith "this spindle runs only its own workflows, not the repository's"
  else if List.mem_assoc variant_var r.env then build config ctx r
  else plan config ctx r
