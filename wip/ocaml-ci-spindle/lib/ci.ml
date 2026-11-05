(* SPDX-License-Identifier: ISC *)

type config = {
  scheduler : Ocluster_client.scheduler;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  opam_repository : Opam_repo.t;
  oxcaml_repository : Opam_repo.t option;
  remotes : Opam_repo.remotes;
  compilers : string list;
  platforms : Variant.platform list;
  full_platforms : Variant.platform list;
  day10 : Variant.arch list;
  diagnose : Diagnose.t option;
}

let full_label = "ci-full"
let diagnose_label = "ci-diagnose"

module Job = Spindle.Job

let variant_var = "OCAMLCI_VARIANT"
let pool_var = "OCAMLCI_POOL"
let diagnose_var = "OCAMLCI_DIAGNOSE"
let day10_var = "OCAMLCI_DAY10"
let jobs = [ Job.v "ocaml-ci" [ Job.step "plan" "opam-0install" ] ]

let logf (ctx : Spindle.Executor.context) fmt =
  Printf.ksprintf (fun s -> ctx.log `Stdout s) fmt

(* [snapshots repos f] is [f] applied to each repository's commit and packages
   directory, all held still while [f] runs. *)
let rec snapshots repos f =
  match repos with
  | [] -> f []
  | repo :: rest ->
      Opam_repo.with_snapshot repo (fun ~commit ~packages ->
          snapshots rest (fun l -> f ((repo, commit, packages) :: l)))

(* [available ~skip remotes f] is [f] applied to the snapshots of those of
   [remotes] that can be fetched, held still while [f] runs. One that cannot
   is passed to [skip] with the reason and left out. *)
let rec available ~skip remotes f =
  match remotes with
  | [] -> f []
  | repo :: rest -> (
      let entered = ref false in
      match
        Opam_repo.with_snapshot repo (fun ~commit ~packages ->
            entered := true;
            available ~skip rest (fun l -> f ((repo, commit, packages) :: l)))
      with
      | v -> v
      | exception Failure why when not !entered ->
          skip repo why;
          available ~skip rest f)

let env_of (v : Variant.t) =
  [ (variant_var, v.name); (pool_var, v.pool) ]
  @ if v.day10 then [ (day10_var, "1") ] else []

(* A build's first step is its OBuilder spec, or a day10 build's JSON. *)
let build_step (v : Variant.t) spec =
  Job.step (if v.day10 then "day10" else "obuilder") spec

(* The [ocaml-ci] workflow: find the project, solve, spawn the builds. The
   newest OCaml's build carries the builds for the other platforms as further
   steps, which it spawns once it passes. *)
let label = function
  | `Success -> "passed"
  | `Failed -> "FAILED"
  | `Cancelled -> "cancelled"
  | `Timeout -> "timed out"

module P = struct
  type t = Spindle.Executor.progress

  let ended (p : t) =
    match p.status with
    | `Pending | `Running -> None
    | (`Success | `Failed | `Cancelled | `Timeout) as s -> Some s

  (* What a viewer is told about a build's state. *)
  let phase (p : t) =
    match (p.status, p.step) with
    | `Pending, _ -> "queued"
    | `Running, (None | Some "start") -> "starting"
    | `Running, Some step -> "building: " ^ step
    | ((`Success | `Failed | `Cancelled | `Timeout) as s), _ -> label s
end

let max_phase = 100

let clip n s =
  if String.length s <= n then s
  else
    let rec cut i =
      if i > 0 && Char.code s.[i] land 0xc0 = 0x80 then cut (i - 1) else i
    in
    String.sub s 0 (cut (n - 3)) ^ "..."

exception Did_not_pass

(* While builds run, the [ocaml-ci] workflow shows their progress. A step
   waits until a build ends, logging each build's changes as they happen,
   then a step for that build passes or fails with it. *)
let follow config (ctx : Spindle.Executor.context) ~primary ~later =
  let began = Eio.Time.Mono.now config.clock in
  let elapsed () =
    let s =
      Mtime.Span.to_uint64_ns
        (Mtime.span began (Eio.Time.Mono.now config.clock))
      |> Int64.to_int
      |> fun ns -> ns / 1_000_000_000
    in
    if s < 3600 then Printf.sprintf "%2dm%02ds" (s / 60) (s mod 60)
    else Printf.sprintf "%dh%02dm" (s / 3600) (s / 60 mod 60)
  in
  let reported = Hashtbl.create 16 in
  let unreported (now : P.t list) =
    List.filter
      (fun (p : P.t) ->
        P.ended p <> None && not (Hashtbl.mem reported p.workflow))
      now
  in
  let primary_ended (now : P.t list) =
    match primary with
    | None -> None
    | Some (v : Variant.t) ->
        List.find_opt (fun (p : P.t) -> p.workflow = v.name) now
        |> Fun.flip Option.bind P.ended
  in
  (* The later platforms' builds yet to be spawned, none once the newest
     compiler's build has failed. *)
  let planned (now : P.t list) =
    match primary_ended now with
    | Some (`Failed | `Cancelled | `Timeout) -> []
    | Some `Success | None ->
        List.filter
          (fun ((v : Variant.t), _) ->
            not (List.exists (fun (p : P.t) -> p.workflow = v.name) now))
          later
  in
  let after =
    Option.fold ~none:"" ~some:(fun (v : Variant.t) -> v.name) primary
  in
  (* Tangled shows ANSI colours, so an ended build stands out. *)
  let line (p : P.t) =
    let colour =
      match P.ended p with
      | Some `Success -> "\027[32m"
      | Some _ -> "\027[31m"
      | None -> ""
    in
    logf ctx "%s  %-28s %s%s%s\n" (elapsed ()) p.workflow colour
      (clip max_phase (P.phase p))
      (if colour = "" then "" else "\027[0m")
  in
  let report (p : P.t) =
    Hashtbl.replace reported p.workflow ();
    let status = Option.get (P.ended p) in
    let name = p.workflow ^ " " ^ String.lowercase_ascii (label status) in
    try
      ctx.step name (fun () ->
          Option.iter (logf ctx "%s\n") p.error;
          logf ctx "%s after %s.\n" (label status) (String.trim (elapsed ()));
          if status <> `Success then raise Did_not_pass)
    with Did_not_pass -> ()
  in
  let running_of = List.filter (fun p -> P.ended p = None) in
  let rec rounds (now : P.t list) =
    List.iter report (unreported now);
    let running = running_of now in
    if running <> [] then (
      let latest = ref now in
      let name =
        Printf.sprintf "waiting for %d of %d builds" (List.length running)
          (List.length now + List.length (planned now))
      in
      ctx.step ~kind:`System ~command:"await builds" name (fun () ->
          List.iter line running;
          List.iter
            (fun ((v : Variant.t), _) ->
              logf ctx "%s  %-28s planned, once %s passes\n" (elapsed ()) v.name
                after)
            (planned now);
          let rec wait () =
            let before = !latest in
            let next = ctx.spawned ~since:before () in
            latest := next;
            List.iter
              (fun (p : P.t) -> if not (List.mem p before) then line p)
              next;
            (match (primary_ended before, primary_ended next) with
            | None, Some (`Failed | `Cancelled | `Timeout) ->
                List.iter
                  (fun ((v : Variant.t), _) ->
                    logf ctx "%s  %-28s skipped, since %s did not pass\n"
                      (elapsed ()) v.name after)
                  (planned before)
            | _ -> ());
            if unreported next = [] then wait ()
          in
          wait ());
      rounds !latest)
  in
  rounds (ctx.spawned ())

(* The [summary] step reports every build, including those spawned once
   the newest compiler's passed. Failures are handed to a [diagnose]
   workflow when a model is configured. *)
let summary config (ctx : Spindle.Executor.context)
    (r : Spindle.Executor.request) ~primary ~later =
  follow config ctx ~primary ~later;
  ctx.step "summary" (fun () ->
      let outcomes = ctx.await_spawned () in
      List.iter
        (fun (o : Spindle.Executor.outcome) ->
          logf ctx "%-9s  %s%s\n" (label o.status) o.workflow
            (match o.error with
            | Some e when o.status <> `Success -> " (" ^ e ^ ")"
            | _ -> ""))
        outcomes;
      let ran name =
        List.exists
          (fun (o : Spindle.Executor.outcome) -> o.workflow = name)
          outcomes
      in
      (match primary with
      | Some (p : Variant.t) ->
          List.iter
            (fun ((v : Variant.t), _) ->
              if not (ran v.name) then
                logf ctx "%-9s  %s (%s did not pass)\n" "skipped" v.name p.name)
            later
      | None -> ());
      let passed =
        List.filter_map
          (fun (o : Spindle.Executor.outcome) ->
            if o.status = `Success then Some o.workflow else None)
          outcomes
      in
      let failed =
        List.filter
          (fun (o : Spindle.Executor.outcome) -> o.status = `Failed)
          outcomes
      in
      logf ctx "\n%d of %d builds passed.\n" (List.length passed)
        (List.length outcomes);
      if
        failed <> [] && config.diagnose <> None
        && List.mem diagnose_label (r.labels ())
      then (
        ctx.spawn
          ~env:[ (diagnose_var, "1") ]
          "diagnose"
          [ Job.step "diagnose" "chat completion" ];
        logf ctx "The diagnose workflow is asking a model about the failures.\n");
      if List.length passed < List.length outcomes then
        failwith
          (Printf.sprintf "%d of %d builds did not pass"
             (List.length outcomes - List.length passed)
             (List.length outcomes)))

let plan config (ctx : Spindle.Executor.context) (r : Spindle.Executor.request)
    =
  let project = ref None and platforms = ref config.platforms in
  ctx.step ~command:"ls *.opam" "analyse" (fun () ->
      match Analysis.find r.files with
      | None ->
          logf ctx
            "No opam files at the root of the repository, so there is nothing \
             to build.\n"
      | Some a ->
          project := Some a;
          List.iter (fun (f, _) -> logf ctx "%s\n" f) a.opam_files;
          let slow =
            String.concat ", "
              (List.map
                 (fun p ->
                   fst (List.find (fun (_, q) -> q = p) Variant.platforms))
                 config.full_platforms)
          in
          if config.full_platforms <> [] then
            if List.mem full_label (r.labels ()) then (
              platforms := config.platforms @ config.full_platforms;
              logf ctx "Labelled ci:full, so %s are built too.\n" slow)
            else
              logf ctx
                "%s are built only for a pull request a maintainer labels \
                 ci:full.\n"
                (String.capitalize_ascii slow));
  Option.iter
    (fun (a : Analysis.t) ->
      let builds = ref [] and later = ref [] in
      ctx.step ~command:"opam-0install" "solve" (fun () ->
          let base =
            config.opam_repository :: Option.to_list config.oxcaml_repository
          in
          (* The project's own remotes, from [x-opam-repositories]. *)
          let wanted, ignored = Analysis.remotes a in
          List.iter
            (fun (entry, why) ->
              logf ctx "Ignoring x-opam-repositories entry %S: %s.\n" entry why)
            ignored;
          let same url repo =
            let norm u =
              let u = String.lowercase_ascii u in
              let u =
                if String.ends_with ~suffix:"/" u then
                  String.sub u 0 (String.length u - 1)
                else u
              in
              if String.ends_with ~suffix:".git" u then
                String.sub u 0 (String.length u - 4)
              else u
            in
            norm url = norm (Opam_repo.url repo)
          in
          let remotes =
            List.filter_map
              (fun (r : Analysis.remote) ->
                if r.branch = None && List.exists (same r.url) base then None
                else
                  match
                    Opam_repo.remote config.remotes ~url:r.url ~host:r.host
                      ?branch:r.branch ()
                  with
                  | Some t -> Some t
                  | None ->
                      logf ctx
                        "Ignoring x-opam-repositories remote %s: this spindle \
                         holds too many remotes already.\n"
                        r.url;
                      None)
              wanted
          in
          (* Remotes are locked before the shared repositories, in the order
             of their names, so that no two plans wait on each other and a
             slow remote holds up only the plans that name it. *)
          let by_name =
            List.sort
              (fun a b -> String.compare (Opam_repo.name a) (Opam_repo.name b))
              remotes
          in
          let skip repo why =
            logf ctx "Ignoring x-opam-repositories remote %s: %s.\n"
              (Opam_repo.url repo) why;
            Opam_repo.forget config.remotes repo
          in
          available ~skip by_name @@ fun fetched ->
          let fetched =
            List.filter_map
              (fun r -> List.find_opt (fun (q, _, _) -> q == r) fetched)
              remotes
          in
          snapshots base @@ fun base_fetched ->
          let snaps = base_fetched @ fetched in
          logf ctx "Solving with opam-0install against:\n";
          List.iteri
            (fun i (repo, commit, _) ->
              logf ctx "  %s %s (%s)\n"
                (if i < List.length base then Opam_repo.name repo
                 else
                   "x-opam-repositories"
                   ^ match Opam_repo.branch repo with
                     | Some b -> " " ^ b
                     | None -> "")
                (String.sub commit 0 (min 7 (String.length commit)))
                (Opam_repo.url repo))
            snaps;
          logf ctx "\n";
          let n = List.length base in
          let base_snaps = List.filteri (fun i _ -> i < n) snaps
          and remote_snaps = List.filteri (fun i _ -> i >= n) snaps in
          let _, default_commit, default = List.hd base_snaps in
          let oxcaml = List.nth_opt base_snaps 1 in
          (* Remotes are pinned at the commits solved against. day10 takes
             one repository, so a project with remotes builds on OBuilder. *)
          let pinned =
            List.map
              (fun (repo, commit, _) -> (Opam_repo.url repo, commit))
              remote_snaps
          in
          if pinned <> [] && config.day10 <> [] then
            logf ctx
              "Built with OBuilder rather than day10, which takes one opam \
               repository.\n\n";
          let day10 = if pinned = [] then config.day10 else [] in
          let solve (v : Variant.t) =
            let repositories =
              List.map (fun (_, _, packages) -> packages) remote_snaps
              @ (match (v.oxcaml, oxcaml) with
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
            (* What a build would install: the project's direct
               dependencies, at the versions the solve chose. *)
            let installable packages =
              let chosen =
                List.filter_map
                  (fun name ->
                    List.find_map
                      (fun nv ->
                        let p = name ^ "." in
                        if String.starts_with ~prefix:p nv then
                          Some
                            (name ^ " "
                            ^ String.sub nv (String.length p)
                                (String.length nv - String.length p))
                        else None)
                      packages)
                  (Solve.direct (Analysis.packages a))
              in
              logf ctx "✓ %s: installable, %d packages%s\n" v.name
                (List.length packages)
                (if v.day10 then ", built with day10" else "");
              if chosen <> [] then
                let shown = List.filteri (fun i _ -> i < 12) chosen in
                logf ctx "    %s%s\n" (String.concat ", " shown)
                  (match List.length chosen - List.length shown with
                  | 0 -> ""
                  | n -> Printf.sprintf " and %d more" n)
            in
            match Eio_unix.run_in_systhread (fun () -> Solve.solve request) with
            | Ok packages when v.day10 ->
                (* day10 solves again itself, but the solve here says which
                   builds the project can have. *)
                installable packages;
                let d =
                  Spec.day10 ~variant:v
                    ~opam_repository:
                      (Opam_repo.url config.opam_repository, default_commit)
                    ~opam_files:(List.map fst a.opam_files)
                in
                Some
                  ( v,
                    match Jsont_bytesrw.encode_string Spec.day10_jsont d with
                    | Ok s -> s
                    | Error e -> failwith e )
            | Ok packages ->
                installable packages;
                Some
                  ( v,
                    Spec.v ~variant:v ~opam_repository:default_commit
                      ~remotes:pinned
                      ?oxcaml_repository:
                        (Option.map
                           (fun (repo, commit, _) ->
                             (Opam_repo.url repo, commit))
                           oxcaml)
                      ~opam_files:(List.map fst a.opam_files)
                      ~packages () )
            | Error why ->
                logf ctx "✗ %s: not installable, as %s\n" v.name
                  (Solve.explain request why);
                None
          in
          let variants =
            (Variant.choose ~packages:default config.compilers
            @ match oxcaml with Some _ -> [ Variant.oxcaml ] | None -> [])
            |> List.map (Variant.use_day10 ~arches:day10)
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
                    let v =
                      Variant.use_day10 ~arches:day10
                        (Variant.v ~platform (Variant.version primary))
                    in
                    if
                      List.exists
                        (fun ((b : Variant.t), _) -> b.name = v.name)
                        !builds
                    then None
                    else Some v)
                  !platforms
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
          ctx.spawn ~env:(env_of v) v.name (build_step v spec :: after))
        !builds;
      summary config ctx r ~primary:(Option.map fst primary) ~later:!later)
    !project

(* The [diagnose] workflow: one question to the model, with no tools. *)
let diagnose config (ctx : Spindle.Executor.context)
    (r : Spindle.Executor.request) =
  let d =
    match config.diagnose with
    | Some d -> d
    | None -> failwith "no model is configured"
  in
  (* The builds are the pipeline's other workflows, read back from the
     spindle's store, so a diagnosis survives a restart. *)
  let builds =
    List.filter
      (fun (o : Spindle.Executor.outcome) ->
        o.workflow <> "ocaml-ci" && o.workflow <> r.workflow)
      (ctx.finished ())
  in
  let failures =
    List.filter_map
      (fun (o : Spindle.Executor.outcome) ->
        if o.status = `Failed then
          Some { Diagnose.workflow = o.workflow; error = o.error; log = o.log }
        else None)
      builds
  and passed =
    List.filter_map
      (fun (o : Spindle.Executor.outcome) ->
        if o.status = `Success then Some o.workflow else None)
      builds
  in
  if failures = [] then failwith "no build of this pipeline failed";
  let prompt = ref "" in
  ctx.step ~kind:`System ~command:"prepare prompt" "collect logs" (fun () ->
      prompt := Diagnose.prompt ~files:r.files ~passed failures;
      List.iter
        (fun (f : Diagnose.failure) -> logf ctx "%s\n" f.workflow)
        failures;
      logf ctx "%d KiB for %s at %s\n"
        (String.length !prompt / 1024)
        (Diagnose.model d) (Diagnose.url d));
  ctx.step ~command:(Diagnose.model d) "diagnose" (fun () ->
      match Diagnose.ask d ~prompt:!prompt with
      | Ok answer -> logf ctx "%s\n" answer
      | Error why -> failwith why)

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
  let cache_hint = r.repo ^ "/" ^ r.workflow
  and src = (r.source, [ r.commit ]) in
  let ticket =
    if List.mem_assoc day10_var r.env then
      match Jsont_bytesrw.decode_string Spec.day10_jsont spec with
      | Ok d -> Ocluster_client.submit_day10 submission ~pool ~cache_hint ~src d
      | Error e -> invalid_arg ("a day10 workflow's build is unreadable: " ^ e)
    else Ocluster_client.submit submission ~pool ~cache_hint ~src spec
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
        (* The workflow is pending until a worker takes the job from the
           pool's queue, which is all the scheduler says of the queue. *)
        ctx.queued (fun () ->
            match Ocluster_client.started job with
            | Ok () -> ()
            | Error e -> failwith ("OCluster did not start the job: " ^ e));
        Eio.Fiber.both
          (fun () -> follow job)
          (fun () ->
            section ~kind:`System
              ~command:("ocluster submit --pool " ^ pool)
              "start" job)
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
      ctx.spawn ~env:s.env s.name
        [
          Job.step
            (if List.mem_assoc day10_var s.env then "day10" else "obuilder")
            s.command;
        ])
    after

let executor config =
 fun ctx (r : Spindle.Executor.request) ->
  if r.definition <> None then
    failwith "this spindle runs only its own workflows, not the repository's"
  else if List.mem_assoc variant_var r.env then build config ctx r
  else if List.mem_assoc diagnose_var r.env then diagnose config ctx r
  else plan config ctx r
