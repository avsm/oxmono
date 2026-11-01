(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** yamlcat - parse and reprint YAML files *)

open Cmdliner

type output_format = Yaml | Json | Flow | Debug

let rec json_to_string buf (v : Yamlrw.value) =
  match v with
  | `Null -> Buffer.add_string buf "null"
  | `Bool b -> Buffer.add_string buf (if b then "true" else "false")
  | `Float f ->
      if Float.is_integer f && Float.abs f < 1e15 then
        Buffer.add_string buf (Printf.sprintf "%.0f" f)
      else Buffer.add_string buf (Printf.sprintf "%g" f)
  | `String s -> Buffer.add_string buf (Printf.sprintf "%S" s)
  | `A items ->
      Buffer.add_char buf '[';
      List.iteri
        (fun i item ->
          if i > 0 then Buffer.add_string buf ", ";
          json_to_string buf item)
        items;
      Buffer.add_char buf ']'
  | `O pairs ->
      Buffer.add_char buf '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf (Printf.sprintf "%S: " k);
          json_to_string buf v)
        pairs;
      Buffer.add_char buf '}'

let value_to_json v =
  let buf = Buffer.create 256 in
  json_to_string buf v;
  Buffer.contents buf

let process_string ~format ~resolve_aliases ~max_nodes ~max_depth content =
  try
    (* Always parse as multi-document stream *)
    let documents = Yamlrw.documents_of_string content in

    match format with
    | Yaml ->
        (* Convert through Value to apply tag-based type coercion *)
        let first = ref true in
        List.iter
          (fun (doc : Yamlrw.document) ->
            if not !first then print_string "---\n";
            first := false;
            match doc.root with
            | None -> print_endline ""
            | Some yaml ->
                let value =
                  Yamlrw.to_json ~resolve_aliases ~max_nodes ~max_depth yaml
                in
                print_string (Yamlrw.to_string value))
          documents
    | Flow ->
        (* Convert through Value to apply tag-based type coercion *)
        let first = ref true in
        List.iter
          (fun (doc : Yamlrw.document) ->
            if not !first then print_string "---\n";
            first := false;
            match doc.root with
            | None -> print_endline ""
            | Some yaml ->
                let value =
                  Yamlrw.to_json ~resolve_aliases ~max_nodes ~max_depth yaml
                in
                print_string (Yamlrw.to_string ~layout_style:`Flow value))
          documents
    | Json ->
        let first = ref true in
        List.iter
          (fun (doc : Yamlrw.document) ->
            match doc.root with
            | None -> ()
            | Some yaml ->
                if not !first then print_endline "---";
                first := false;
                let value =
                  Yamlrw.to_json ~resolve_aliases ~max_nodes ~max_depth yaml
                in
                print_endline (value_to_json value))
          documents
    | Debug ->
        List.iteri
          (fun i (doc : Yamlrw.document) ->
            Format.printf "Document %d:@." (i + 1);
            (* Convert back to Document.t for printing *)
            let doc' : Yamlrw.Document.t =
              {
                Yamlrw.Document.version = doc.version;
                Yamlrw.Document.tags = doc.tags;
                Yamlrw.Document.root = (doc.root :> Yamlrw.Yaml.t option);
                Yamlrw.Document.implicit_start = doc.implicit_start;
                Yamlrw.Document.implicit_end = doc.implicit_end;
              }
            in
            Format.printf "%a@." Yamlrw.Document.pp doc')
          documents
  with Yamlrw.Yamlrw_error e ->
    Printf.eprintf "Error: %s\n" (Yamlrw.Error.to_string e);
    exit 1

let process_file ~format ~resolve_aliases ~max_nodes ~max_depth filename =
  let content =
    if filename = "-" then In_channel.input_all In_channel.stdin
    else In_channel.with_open_text filename In_channel.input_all
  in
  process_string ~format ~resolve_aliases ~max_nodes ~max_depth content

let run format _all resolve_aliases max_nodes max_depth files =
  let files = if files = [] then [ "-" ] else files in
  List.iter (process_file ~format ~resolve_aliases ~max_nodes ~max_depth) files;
  `Ok ()

(* Command-line arguments *)

let format_arg =
  let doc = "Output format: yaml (default), json, flow, or debug." in
  let formats =
    [ ("yaml", Yaml); ("json", Json); ("flow", Flow); ("debug", Debug) ]
  in
  Arg.(
    value & opt (enum formats) Yaml & info [ "format"; "f" ] ~docv:"FORMAT" ~doc)

let json_arg =
  let doc = "Output as JSON (shorthand for --format=json)." in
  Arg.(value & flag & info [ "json" ] ~doc)

let flow_arg =
  let doc = "Output in flow style (shorthand for --format=flow)." in
  Arg.(value & flag & info [ "flow" ] ~doc)

let debug_arg =
  let doc = "Output internal representation (shorthand for --format=debug)." in
  Arg.(value & flag & info [ "debug" ] ~doc)

let all_arg =
  let doc = "Output all documents (for multi-document YAML)." in
  Arg.(value & flag & info [ "all"; "a" ] ~doc)

let no_resolve_aliases_arg =
  let doc = "Don't resolve aliases (keep them as references)." in
  Arg.(value & flag & info [ "no-resolve-aliases" ] ~doc)

let max_nodes_arg =
  let doc =
    "Maximum number of nodes during alias expansion (default: 10000000). \
     Protection against billion laughs attack."
  in
  Arg.(
    value
    & opt int Yamlrw.default_max_alias_nodes
    & info [ "max-nodes" ] ~docv:"N" ~doc)

let max_depth_arg =
  let doc =
    "Maximum alias nesting depth (default: 100). Protection against deeply \
     nested alias chains."
  in
  Arg.(
    value
    & opt int Yamlrw.default_max_alias_depth
    & info [ "max-depth" ] ~docv:"N" ~doc)

let files_arg =
  let doc = "YAML file(s) to process. Use '-' for stdin." in
  Arg.(value & pos_all file [] & info [] ~docv:"FILE" ~doc)

let combined_format format json flow debug =
  if json then Json else if flow then Flow else if debug then Debug else format

let term =
  let combine format json flow debug all no_resolve max_nodes max_depth files =
    let format = combined_format format json flow debug in
    let resolve_aliases = not no_resolve in
    run format all resolve_aliases max_nodes max_depth files
  in
  Term.(
    ret
      (const combine $ format_arg $ json_arg $ flow_arg $ debug_arg $ all_arg
     $ no_resolve_aliases_arg $ max_nodes_arg $ max_depth_arg $ files_arg))

let info =
  let doc = "Parse and reprint YAML files" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) parses YAML files and reprints them in various formats. It \
         can be used to validate YAML, convert between styles, or convert to \
         JSON.";
      `S Manpage.s_examples;
      `P "Parse and reprint a YAML file:";
      `Pre "  $(tname) config.yaml";
      `P "Convert YAML to JSON:";
      `Pre "  $(tname) --json config.yaml";
      `P "Process multi-document YAML:";
      `Pre "  $(tname) --all multi.yaml";
      `P "Limit alias expansion (protection against malicious YAML):";
      `Pre "  $(tname) --max-nodes 1000 --max-depth 10 untrusted.yaml";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/avsm/ocaml-yaml/issues";
    ]
  in
  Cmd.info "yamlcat" ~version:"0.1.0" ~doc ~man

let () = exit (Cmd.eval (Cmd.v info term))
