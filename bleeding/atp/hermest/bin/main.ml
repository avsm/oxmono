(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Hermest

(* Verbose mode controlled by ATP_VERBOSE environment variable *)
let verbose = Sys.getenv_opt "ATP_VERBOSE" |> Option.is_some

let log fmt =
  if verbose then Printf.printf fmt else Printf.ifprintf stdout fmt

let dune_file ~name ~public_name =
  let public_name_line =
    match public_name with
    | Some pn -> Printf.sprintf "\n (public_name %s)" pn
    | None -> ""
  in
  Printf.sprintf {|(library
 (name %s)%s
 (libraries atp jsont jsont.bytesrw))|}
    name public_name_line

(* Parse a lexicon file using jsont *)
let parse_file path =
  try
    let content = In_channel.with_open_bin path In_channel.input_all in
    match Jsont_bytesrw.decode_string Lexicon_types.lexicon_doc_jsont content with
    | Ok doc -> Ok doc
    | Error e -> Error ("JSON decode error: " ^ e)
  with
  | Sys_error e -> Error ("File error: " ^ e)
  | e -> Error ("Unexpected error: " ^ Printexc.to_string e)

(* recursively find all json files in a path (file or directory) *)
let find_json_files path =
  let rec aux acc p =
    if Sys.is_directory p then
      Sys.readdir p |> Array.to_list
      (* Sort directory entries for deterministic ordering *)
      |> List.sort String.compare
      |> List.map (Filename.concat p)
      |> List.fold_left aux acc
    else if Filename.check_suffix p ".json" then p :: acc
    else acc
  in
  (* Sort final result for deterministic ordering *)
  aux [] path |> List.sort String.compare

(* generate module structure from lexicons - unified mode *)
let generate ~inputs ~output_dir ~module_name ~public_name =
  (* create output directory *)
  if not (Sys.file_exists output_dir) then Sys.mkdir output_dir 0o755;
  (* find all lexicon files from all inputs *)
  let files = List.concat_map find_json_files inputs in
  log "Found %d lexicon files\n" (List.length files);
  (* parse all files *)
  let lexicons =
    List.filter_map
      (fun path ->
        match parse_file path with
        | Ok doc ->
            log "  Parsed: %s\n" doc.Lexicon_types.id;
            Some doc
        | Error e ->
            Printf.eprintf "  Error parsing %s: %s\n" path e;
            None)
      files
  in
  log "Successfully parsed %d lexicons\n" (List.length lexicons);
  (* Generate unified module with nested structure *)
  let code = Codegen_jsont.gen_unified_module ~module_name lexicons in
  let unified_path =
    Filename.concat output_dir (String.lowercase_ascii module_name ^ ".ml")
  in
  let oc = open_out unified_path in
  output_string oc code;
  close_out oc;
  log "Generated unified module: %s\n" unified_path;
  (* Generate unified interface (.mli) *)
  let interface = Codegen_jsont.gen_unified_interface ~module_name lexicons in
  let interface_path =
    Filename.concat output_dir (String.lowercase_ascii module_name ^ ".mli")
  in
  let oc = open_out interface_path in
  output_string oc interface;
  close_out oc;
  log "Generated unified interface: %s\n" interface_path;
  (* generate dune.inc file (for inclusion via (include dune.inc)) *)
  let dune_path = Filename.concat output_dir "dune.inc" in
  let oc = open_out dune_path in
  let name = String.lowercase_ascii module_name in
  let dune_content = dune_file ~name ~public_name in
  Out_channel.output_string oc dune_content;
  close_out oc;
  log "Generated dune.inc file\n";
  log "Done! Generated unified module from %d lexicons\n" (List.length lexicons)

let inputs =
  let doc = "lexicon files or directories to search recursively for JSON" in
  Cmdliner.Arg.(non_empty & pos_all file [] & info [] ~docv:"INPUT" ~doc)

let output_dir =
  let doc = "output directory for generated code" in
  Cmdliner.Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"DIR" ~doc)

let module_name =
  let doc = "name of the generated module" in
  Cmdliner.Arg.(
    value & opt string "Lexicons"
    & info [ "m"; "module-name" ] ~docv:"NAME" ~doc)

let public_name_arg =
  let doc =
    "Public package name for the dune library (e.g. 'tangled-lexicons')"
  in
  Cmdliner.Arg.(
    value
    & opt (some string) None
    & info [ "p"; "public-name" ] ~docv:"NAME" ~doc)

let generate_cmd =
  let doc = "generate ocaml types from atproto lexicons" in
  let info = Cmdliner.Cmd.info "generate" ~doc in
  let generate' inputs output_dir module_name public_name =
    generate ~inputs ~output_dir ~module_name ~public_name
  in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(
      const generate' $ inputs $ output_dir $ module_name $ public_name_arg)

let main_cmd =
  let doc = "hermest - atproto lexicon code generator (jsont)" in
  let info = Cmdliner.Cmd.info "hermest" ~version:"0.1.0" ~doc in
  Cmdliner.Cmd.group info [ generate_cmd ]

let () = exit (Cmdliner.Cmd.eval main_cmd)
