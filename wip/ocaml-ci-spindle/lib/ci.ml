(* SPDX-License-Identifier: ISC *)

type config = {
  cap : string;
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
      let solved = ref [] in
      ctx.step ~command:"opam-0install" "solve" (fun () ->
          let repos =
            config.opam_repository :: Option.to_list config.oxcaml_repository
          in
          List.iter
            (fun repo ->
              Opam_repo.refresh repo;
              logf ctx "%s at %s\n" (Opam_repo.url repo) (Opam_repo.commit repo))
            repos;
          let default = Opam_repo.packages config.opam_repository in
          let variants =
            Variant.choose ~packages:default config.compilers
            @
            match config.oxcaml_repository with
            | Some _ -> [ Variant.oxcaml ]
            | None -> []
          in
          List.iter
            (fun (v : Variant.t) ->
              let repositories =
                (match (v.oxcaml, config.oxcaml_repository) with
                  | true, Some ox -> [ Opam_repo.packages ox ]
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
                  solved := (v, packages) :: !solved
              | Error why ->
                  logf ctx "%s: no solution\n%s\n" v.name (first_lines 20 why))
            variants);
      if !solved = [] then failwith "no compiler has a solution";
      let oxcaml_repository =
        Option.map
          (fun ox -> (Opam_repo.url ox, Opam_repo.commit ox))
          config.oxcaml_repository
      in
      List.iter
        (fun ((v : Variant.t), packages) ->
          let spec =
            Spec.v ~variant:v
              ~opam_repository:(Opam_repo.commit config.opam_repository)
              ?oxcaml_repository
              ~opam_files:(List.map fst a.opam_files)
              ~packages ()
          in
          ctx.spawn
            ~env:[ (variant_var, v.name) ]
            v.name
            [ Job.step "obuilder" spec ])
        (List.rev !solved))
    !project

type line = Line of string | End

(* An [obuilder] workflow: submit its spec and follow the job. *)
let build config ~net (ctx : Spindle.Executor.context)
    (r : Spindle.Executor.request) =
  let spec =
    match r.steps with
    | s :: _ -> s.command
    | [] -> invalid_arg "an OBuilder workflow has no spec"
  in
  Eio.Switch.run @@ fun sw ->
  let lines = Eio.Stream.create 4096 in
  let ticket = ref None in
  let job () = Option.get !ticket |> Ocluster_client.job in
  let cancel () =
    Option.iter
      (fun t -> try Ocluster_client.cancel_ticket t with _ -> ())
      !ticket
  in
  (* Lines of the job's log, ending with [End]. *)
  let follow job =
    let partial = Buffer.create 256 in
    Ocluster_client.stream_log job (fun chunk ->
        Buffer.add_string partial chunk;
        let text = Buffer.contents partial in
        let parts = String.split_on_char '\n' text in
        let rec push = function
          | [ rest ] ->
              Buffer.clear partial;
              Buffer.add_string partial rest
          | l :: more ->
              Eio.Stream.add lines (Line l);
              push more
          | [] -> ()
        in
        push parts);
    if Buffer.length partial > 0 then
      Eio.Stream.add lines (Line (Buffer.contents partial));
    Eio.Stream.add lines End
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
              | Some op -> next := Some op
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
  let submission = Ocluster_client.connect ~sw net config.cap in
  let t =
    Ocluster_client.submit submission ~pool:config.pool
      ~cache_hint:(r.repo ^ "/" ^ r.workflow)
      ~src:(r.source, [ r.commit ]) spec
  in
  ticket := Some t;
  let job = job () in
  (* A cancelled workflow cancels its job. Releasing a ticket also cancels
     its job, so it is released only once the job has finished. *)
  Fun.protect
    ~finally:(fun () -> Ocluster_client.release t)
    (fun () ->
      try
        Eio.Fiber.both
          (fun () -> follow job)
          (fun () ->
            section ~kind:`System
              ~command:("ocluster submit --pool " ^ config.pool)
              "queue" job)
      with Eio.Cancel.Cancelled _ as e ->
        Eio.Cancel.protect cancel;
        raise e)

let executor ~net config ctx (r : Spindle.Executor.request) =
  if List.mem_assoc variant_var r.env then build config ~net ctx r
  else plan config ctx r
