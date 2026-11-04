(* SPDX-License-Identifier: ISC *)
type files = { list : string -> string list; read : string -> string option }

type request = {
  pipeline : string;
  workflow : string;
  repo : string;
  source : string;
  commit : string;
  clone : bool;
  metadata : Jsont.json;
  definition : Jsont.json option;
  yaml : string option;
  env : (string * string) list;
  variables : (string * string) list;
  secrets : (string * string) list;
  steps : Job.step list;
  files : files;
  labels : unit -> string list;
}

type kind = [ `User | `System ]

type outcome = {
  workflow : string;
  status : [ `Success | `Failed | `Cancelled | `Timeout ];
  error : string option;
  log : string;
}

type progress = {
  workflow : string;
  status : [ `Pending | `Running | `Success | `Failed | `Cancelled | `Timeout ];
  step : string option;
  error : string option;
}

type context = {
  step : ?kind:kind -> ?command:string -> string -> (unit -> unit) -> unit;
  log : [ `Stdout | `Stderr ] -> string -> unit;
  spawn : ?env:(string * string) list -> string -> Job.step list -> unit;
  await_spawned : unit -> outcome list;
  spawned : ?since:progress list -> unit -> progress list;
}

type t = context -> request -> unit

let bindings = function
  | [] -> "  (none)\n"
  | l -> String.concat "" (List.map (fun (k, v) -> "  " ^ k ^ "=" ^ v ^ "\n") l)

let indent text =
  String.split_on_char '\n' text
  |> List.map (fun line -> if line = "" then "" else "  " ^ line)
  |> String.concat "\n"

let describe r =
  let line k v = Printf.sprintf "%-11s %s\n" k v in
  String.concat ""
    [
      line "Pipeline" r.pipeline;
      line "Workflow" r.workflow;
      line "Repository" r.repo;
      line "Source" r.source;
      line "Commit" r.commit;
      line "Clone" (if r.clone then "yes" else "no");
      "Metadata\n";
      indent (Json.encode r.metadata);
      "\nEnvironment\n";
      bindings r.env;
      "CI variables\n";
      bindings r.variables;
      "Secrets\n";
      bindings (List.map (fun (k, _) -> (k, "<secret>")) r.secrets);
      (match r.definition with
      | None -> ""
      | Some d -> "Definition\n" ^ indent (Json.encode d) ^ "\n");
      (match r.yaml with
      | None -> ""
      | Some text ->
          "YAML\n" ^ indent text
          ^ if String.ends_with ~suffix:"\n" text then "" else "\n");
    ]

let print ?(delay = 2.) clock ctx r =
  let text = describe r in
  print_string text;
  flush stdout;
  ctx.step ~kind:`System "describe the request" (fun () -> ctx.log `Stdout text);
  List.iter
    (fun (s : Job.step) ->
      ctx.step ~command:s.command s.name (fun () ->
          ctx.log `Stdout ("Environment\n" ^ bindings s.env)))
    r.steps;
  Eio.Time.sleep clock delay

(* The simulated planner and builds. *)

let compiler_var = "SPINDLE_COMPILER"
let green s = "\027[32m" ^ s ^ "\027[0m"

let solve ?(compilers = [ "ocaml-4.14"; "ocaml-5.5"; "oxcaml-5.2minus39" ])
    clock =
  let random = Random.State.make_self_init () in
  let pause low high =
    Eio.Time.sleep clock (low +. Random.State.float random (high -. low))
  in
  fun ctx r ->
    match List.assoc_opt compiler_var r.env with
    | None ->
        ctx.step ~kind:`System "describe the request" (fun () ->
            ctx.log `Stdout (describe r));
        ctx.step ~command:"opam solve" "solve" (fun () ->
            ctx.log `Stdout
              (Printf.sprintf "Solving %s at %s for %d compilers\n" r.repo
                 r.commit (List.length compilers));
            List.iter
              (fun compiler ->
                pause 0.5 2.;
                ctx.log `Stdout
                  (Printf.sprintf "%s: a solution with %d packages\n" compiler
                     (20 + Random.State.int random 40)))
              compilers);
        List.iter
          (fun compiler ->
            ctx.spawn
              ~env:[ (compiler_var, compiler) ]
              compiler
              [
                Job.step "install dependencies"
                  "opam install --deps-only --with-test .";
                Job.step "build" "dune build";
                Job.step "test" "dune runtest";
              ])
          compilers
    | Some compiler ->
        let output = function
          | "install dependencies" ->
              [
                "The following actions will be performed:";
                "  - install dune  3.20.2";
                "  - install fmt   0.11.0";
                "  - install cmdliner 2.0.0";
                "<><> Processing actions <><><><><><><><><><><><><><><><><><><>";
                "-> installed dune.3.20.2";
                "-> installed fmt.0.11.0";
                "-> installed cmdliner.2.0.0";
                "Done.";
              ]
          | "build" ->
              [
                Printf.sprintf "Building with %s" compiler;
                "      ocamlc lib/.tdmrep.objs/byte/tdmrep.{cmi,cmo,cmt}";
                "    ocamlopt lib/.tdmrep.objs/native/tdmrep.{cmx,o}";
                "    ocamlopt bin/main.exe";
              ]
          | "test" ->
              [
                "Testing `tdmrep'.";
                "This run has ID `" ^ String.uppercase_ascii compiler ^ "'.";
                "  " ^ green "[OK]"
                ^ "          parse          0   simple policy.";
                "  " ^ green "[OK]" ^ "          parse          1   wildcards.";
                "  " ^ green "[OK]" ^ "          encode         0   round trip.";
                "Full test results in `_build/default/test/_build/_tests'.";
                "Test Successful in 0.004s. 3 tests run.";
              ]
          | _ -> [ "nothing to do" ]
        in
        List.iter
          (fun (s : Job.step) ->
            ctx.step ~command:s.command s.name (fun () ->
                List.iter
                  (fun line ->
                    pause 0.1 0.6;
                    ctx.log `Stdout (line ^ "\n"))
                  (output s.name)))
          r.steps
