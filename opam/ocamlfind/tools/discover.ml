module C = Configurator.V1

let () =
  C.main ~name:"findlib" (fun c ->
    let ocaml_stdlib = C.ocaml_config_var_exn c "standard_library" in
    let prefix = try Unix.getenv "FINDLIB_PREFIX" with _ -> ocaml_stdlib in
    let ocaml_ldconf = Filename.concat ocaml_stdlib "ld.conf" in
    let has_autolinking = "true" in
    let system = C.ocaml_config_var_exn c "system" in
    let dll_suffix = C.ocaml_config_var_exn c "ext_dll" in
    let ocaml_version = C.ocaml_config_var_exn c "version" in
    let ocaml_major = int_of_string (String.sub ocaml_version 0 (String.index ocaml_version '.')) in
    let ocaml_has_meta_files = if ocaml_major >= 5 then "true" else "false" in
    (* For duniverse builds, set config_file to "" so findlib doesn't require
       an external config file. Dune handles all package paths directly. *)
    let config_file = "" in
    (* Write findlib_config.ml *)
    C.Flags.write_lines "findlib_config.ml" [ Printf.sprintf {|
let config_file = lazy %S;;
let location = lazy None;;
let ocaml_stdlib = %S;;
let ocaml_ldconf = %S;;
let ocaml_has_autolinking = %s;;
let ocaml_has_meta_files = %s;;
let libexec_name = "stublibs";;
let system = %S;;
let dll_suffix = %S;;
|} config_file ocaml_stdlib ocaml_ldconf has_autolinking ocaml_has_meta_files system dll_suffix ];
    (* Write toplevel script *)
    let topfind = C.Process.run_capture_exn c "cat" ["topfind_rd1.p"] in
    let topfind = Str.(global_replace (regexp_string "@SITELIB@") prefix topfind) in
    C.Flags.write_lines "topfind" [ topfind ];
    (* Write findlib.conf *)
    C.Flags.write_lines "findlib.conf" [
      Printf.sprintf "destdir=%S" prefix;
      Printf.sprintf "path=%S" prefix ]
  )
