(* SPDX-License-Identifier: ISC *)
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
}

type context = {
  step : string -> (unit -> unit) -> unit;
  log : [ `Stdout | `Stderr ] -> string -> unit;
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
  ctx.step "describe the request" (fun () -> ctx.log `Stdout text);
  List.iter
    (fun (s : Job.step) ->
      ctx.step s.name (fun () ->
          ctx.log `Stdout
            ("Command\n" ^ indent s.command
            ^ (if String.ends_with ~suffix:"\n" s.command then "" else "\n")
            ^ "Environment\n" ^ bindings s.env)))
    r.steps;
  Eio.Time.sleep clock delay
