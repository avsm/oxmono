(** OpenAPI code generator CLI. *)

let setup_logging style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(** Check if a file appears to be YAML based on extension or content *)
let is_yaml_file path content =
  let ext = Filename.extension path |> String.lowercase_ascii in
  ext = ".yaml" || ext = ".yml" ||
  (* Also detect YAML by content if no clear extension *)
  (ext <> ".json" && String.length content > 0 &&
   (content.[0] = '#' || String.sub content 0 (min 7 (String.length content)) = "openapi"))

(** Parse spec file and run action, handling errors uniformly.
    Automatically handles both JSON and YAML formats using jsont codecs. *)
let with_spec spec_path f =
  let spec_content = read_file spec_path in
  let result =
    if is_yaml_file spec_path spec_content then begin
      Logs.info (fun m -> m "Detected YAML format");
      Yamlt.decode_string Openapi.Spec.jsont spec_content
    end else
      Openapi.Spec.of_string spec_content
  in
  match result with
  | Error e ->
      Logs.err (fun m -> m "Failed to parse OpenAPI spec: %s" e);
      1
  | Ok spec -> f spec

let generate_cmd spec_path output_dir package_name include_regen_rule =
  setup_logging None (Some Logs.Info);
  Logs.info (fun m -> m "Reading OpenAPI spec from %s" spec_path);
  with_spec spec_path (fun spec ->
    Logs.info (fun m -> m "Parsed OpenAPI spec: %s v%s"
      spec.info.title spec.info.version);

    let package_name = Option.value package_name
      ~default:(Openapi.Codegen.Name.to_snake_case spec.info.title) in

    (* Use spec_path for dune.inc regeneration rule if requested *)
    let spec_path_for_dune = if include_regen_rule then Some spec_path else None in
    let config = Openapi.Codegen.{ output_dir; package_name; spec_path = spec_path_for_dune } in
    let files = Openapi.Codegen.generate ~config spec in

    (try Unix.mkdir output_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    Openapi.Codegen.write_files ~output_dir files;

    Logs.info (fun m -> m "Generated %d files in %s" (List.length files) output_dir);
    List.iter (fun (name, _) -> Logs.info (fun m -> m "  - %s" name)) files;
    0)

let inspect_cmd spec_path =
  setup_logging None (Some Logs.Info);
  with_spec spec_path (fun spec ->
      Fmt.pr "@[<v>";
      Fmt.pr "OpenAPI Specification@,";
      Fmt.pr "====================@,@,";
      Fmt.pr "Title: %s@," spec.info.title;
      Fmt.pr "Version: %s@," spec.info.version;
      Option.iter (fun d -> Fmt.pr "Description: %s@," d) spec.info.description;
      Fmt.pr "@,";

      Fmt.pr "Servers:@,";
      List.iter (fun (s : Openapi.Spec.server) ->
        Fmt.pr "  - %s@," s.url
      ) spec.servers;
      Fmt.pr "@,";

      Fmt.pr "Paths (%d):@," (List.length spec.paths);
      List.iter (fun (path, _item) ->
        Fmt.pr "  - %s@," path
      ) spec.paths;
      Fmt.pr "@,";

      (match spec.components with
       | Some c ->
           Fmt.pr "Schemas (%d):@," (List.length c.schemas);
           List.iter (fun (name, _) ->
             Fmt.pr "  - %s@," name
           ) c.schemas
       | None -> ());

      Fmt.pr "@]";
      0)

(* Cmdliner setup *)
open Cmdliner

let spec_path =
  let doc = "Path to the OpenAPI specification file (JSON or YAML)." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"SPEC" ~doc)

let output_dir =
  let doc = "Output directory for generated code." in
  Arg.(required & opt (some string) None & info ["o"; "output"] ~docv:"DIR" ~doc)

let package_name =
  let doc = "Package name for generated code (defaults to API title)." in
  Arg.(value & opt (some string) None & info ["n"; "name"] ~docv:"NAME" ~doc)

let include_regen_rule =
  let doc = "Include dune.inc regeneration rule with spec path." in
  Arg.(value & flag & info ["regen"; "include-regen-rule"] ~doc)

let generate_term =
  Term.(const generate_cmd $ spec_path $ output_dir $ package_name $ include_regen_rule)

let generate_info =
  let doc = "Generate OCaml code from an OpenAPI specification." in
  let man = [
    `S Manpage.s_description;
    `P "Generates OCaml types and client code from an OpenAPI 3.x specification.";
    `P "The generated code uses:";
    `I ("$(b,jsont)", "for JSON encoding/decoding");
    `I ("$(b,requests)", "for HTTP client (Eio-based)");
    `I ("$(b,ptime)", "for date-time handling");
    `S Manpage.s_examples;
    `P "Generate client from local spec:";
    `Pre "  openapi generate spec.json -o ./client -n my_api";
    `P "Generate with regeneration rule for dune:";
    `Pre "  openapi generate spec.json -o ./client -n my_api --regen";
  ] in
  Cmd.info "generate" ~doc ~man

let inspect_term =
  Term.(const inspect_cmd $ spec_path)

let inspect_info =
  let doc = "Inspect an OpenAPI specification." in
  Cmd.info "inspect" ~doc

let main_info =
  let doc = "OpenAPI code generator for OCaml." in
  let man = [
    `S Manpage.s_description;
    `P "Generate OCaml API clients from OpenAPI 3.x specifications.";
    `P "Use $(b,generate) to create client code, or $(b,inspect) to view spec details.";
  ] in
  Cmd.info "openapi" ~version:"0.1.0" ~doc ~man

let main_cmd =
  Cmd.group main_info [
    Cmd.v generate_info generate_term;
    Cmd.v inspect_info inspect_term;
  ]

let () = exit (Cmd.eval' main_cmd)
