(* SPDX-License-Identifier: ISC *)

type config = {
  scheduler : Ocluster_client.scheduler;
  pool : string;
  platform : Solve.platform;
  opam_repository : Opam_repo.t;
  oxcaml_repository : Opam_repo.t option;
  compilers : string list;
}

module Job = Spindle.Job

let variant_var = "OCAMLCI_VARIANT"
let jobs = [ Job.v "ocaml-ci" [ Job.step "plan" "opam-0install" ] ]

let logf (ctx : Spindle.Executor.context) fmt =
  Printf.ksprintf (fun s -> ctx.log `Stdout s) fmt

let first_lines n s =
  String.split_on_char '\n' s
  |> List.filteri (fun i _ -> i < n)
  |> List.map (fun l -> "  " ^ l)
  |> String.concat "\n"

(* The [ocaml-ci] workflow: find the project, solve, spawn the builds. *)
(* [snapshots repos f] is [f] applied to each repository's commit and packages
   directory, all held still while [f] runs. *)
let rec snapshots repos f =
  match repos with
  | [] -> f []
  | repo :: rest ->
      Opam_repo.with_snapshot repo (fun ~commit ~packages ->
          snapshots rest (fun l -> f ((repo, commit, packages) :: l)))

(* The [ocaml-ci] workflow: find the project, solve, spawn the builds. *)
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
      let builds = ref [] in
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
          let variants =
            Variant.choose ~packages:default config.compilers
            @ match oxcaml with Some _ -> [ Variant.oxcaml ] | None -> []
          in
          List.iter
            (fun (v : Variant.t) ->
              let repositories =
                (match (v.oxcaml, oxcaml) with
                  | true, Some (_, _, packages) -> [ packages ]
                  | _ -> [])
                @ [ default ]
              in
              let request =
                {
                  Solve.platform = config.platform;
                  repositories;
                  pins = Analysis.packages a;
                  compiler = v.compiler;
                }
              in
              match
                Eio_unix.run_in_systhread (fun () -> Solve.solve request)
              with
              | Ok packages ->
                  logf ctx "%s: %d packages\n" v.name (List.length packages);
                  let spec =
                    Spec.v ~variant:v ~opam_repository:default_commit
                      ?oxcaml_repository:
                        (Option.map
                           (fun (repo, commit, _) ->
                             (Opam_repo.url repo, commit))
                           oxcaml)
                      ~opam_files:(List.map fst a.opam_files)
                      ~packages ()
                  in
                  builds := (v, spec) :: !builds
              | Error why ->
                  logf ctx "%s: no solution\n%s\n" v.name (first_lines 20 why))
            variants);
      if !builds = [] then failwith "no compiler has a solution";
      List.iter
        (fun ((v : Variant.t), spec) ->
          ctx.spawn
            ~env:[ (variant_var, v.name) ]
            v.name
            [ Job.step "obuilder" spec ])
        (List.rev !builds))
    !project

type line = Line of string | End

(* An [obuilder] workflow: submit its spec and follow the job. *)
let build config (ctx : Spindle.Executor.context) (r : Spindle.Executor.request)
    =
  let spec =
    match r.steps with
    | s :: _ -> s.command
    | [] -> invalid_arg "an OBuilder workflow has no spec"
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
        Buffer.add_substring partial chunk !start (String.length chunk - !start));
    if Buffer.length partial > 0 then
      Eio.Stream.add lines (Line (Buffer.contents partial));
    Eio.Stream.add lines End
  in
  (* An operation printed over several lines continues until its brackets
     close. *)
  let rec rest_of op =
    if Obuilder_log.depth op <= 0 then op
    else
      match Eio.Stream.take lines with
      | Line l -> rest_of (op ^ " " ^ String.trim (Obuilder_log.strip_ansi l))
      | End ->
          Eio.Stream.add lines End;
          op
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
    Ocluster_client.submit submission ~pool:config.pool
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
              ~command:("ocluster submit --pool " ^ config.pool)
              "queue" job)
      with Eio.Cancel.Cancelled _ as e ->
        Eio.Cancel.protect (fun () ->
            try Ocluster_client.cancel_ticket ticket with _ -> ());
        raise e)

let executor config ctx (r : Spindle.Executor.request) =
  if List.mem_assoc variant_var r.env then build config ctx r
  else plan config ctx r
