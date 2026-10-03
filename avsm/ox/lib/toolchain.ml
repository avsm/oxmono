open Support

type t = {
  prefix : string;
  fingerprint : string;
  packages : OpamPackage.t list;
  tools : string list;
}

let tool_names =
  [
    "ocaml";
    "ocamlc";
    "ocamlc.opt";
    "ocamlopt";
    "ocamlopt.opt";
    "ocamldep";
    "ocamldep.opt";
    "ocamllex";
    "ocamllex.opt";
    "ocamlyacc";
    "ocamlrun";
    "ocamlrund";
    "ocamlruni";
    "ocamlmklib";
    "ocamlmktop";
    "ocamlobjinfo";
    "ocamlobjinfo.opt";
    "ocamldebug";
    "ocamlprof";
    "ocamlcp";
    "ocamloptp";
  ]

let inspect proc prefix =
  let prefix = Unix.realpath prefix in
  let config = capture proc [ prefix / "bin/ocamlc"; "-config" ] in
  if not (List.mem "ox: true" (lines config)) then
    fail "%s is not an OxCaml compiler prefix" prefix;
  let state = prefix / ".opam-switch/switch-state" in
  if not (exists state) then fail "Expected an opam switch at %s" prefix;
  let selections = OpamFile.SwitchSelections.read (opam_file state) in
  let packages =
    OpamPackage.Set.elements selections.sel_compiler
    |> List.filter (fun p ->
           let n = OpamPackage.Name.to_string (OpamPackage.name p) in
           List.mem n
             [
               "ocaml";
               "ocaml-config";
               "ocaml-variants";
               "oxcaml";
               "oxcaml-compiler";
               "base-bigarray";
               "base-domains";
               "base-nnp";
               "base-threads";
               "base-unix";
               "base-effects";
             ]
           || String.starts_with ~prefix:"ocaml-option-" n
           || String.starts_with ~prefix:"ocaml-options-" n)
  in
  if packages = [] then fail "Compiler selection is empty in %s" prefix;
  let tools = List.filter (fun n -> exists (prefix / "bin" / n)) tool_names in
  let metadata =
    List.concat_map
      (fun pkg ->
        let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
        let opam =
          prefix / ".opam-switch/packages" / OpamPackage.to_string pkg / "opam"
        in
        let config = prefix / ".opam-switch/config" / (name ^ ".config") in
        [
          OpamPackage.to_string pkg;
          read opam;
          (if exists config then read config else "");
        ])
      packages
  in
  let artifacts =
    tools |> List.concat_map (fun n -> [ n; hash_file (prefix / "bin" / n) ])
  in
  let stdlib = tree_hash (prefix / "lib/ocaml") in
  let fingerprint =
    hash_fields
      (("ox-host-v2" :: prefix :: config :: stdlib :: metadata) @ artifacts)
  in
  { prefix; fingerprint; packages; tools }

let write_repository_contents t ~guards path =
  let write_package name version text =
    write (path / "packages" / name / (name ^ "." ^ version) / "opam") text
  in
  write (path / "repo") "opam-version: \"2.0\"\n";
  List.iter
    (fun pkg ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
      (* The selected compiler components already exist at a verified prefix.
         These definitions describe supplied artifacts, with no build actions. *)
      let original =
        read_opam
          (t.prefix / ".opam-switch/packages" / OpamPackage.to_string pkg
         / "opam")
      in
      let supplied =
        OpamFile.OPAM.create pkg
        |> OpamFile.OPAM.with_synopsis ("External OxCaml component " ^ name)
        |> OpamFile.OPAM.with_conflicts (OpamFile.OPAM.conflicts original)
        |> OpamFile.OPAM.with_env (OpamFile.OPAM.env original)
      in
      write_package name version (OpamFile.OPAM.write_to_string supplied))
    t.packages;
  let deps =
    List.map
      (fun p ->
        Printf.sprintf "%S {= %S}"
          (OpamPackage.Name.to_string (OpamPackage.name p))
          (OpamPackage.Version.to_string (OpamPackage.version p)))
      t.packages
  in
  let deps = if guards then "\"oxcaml-patch-guards\"" :: deps else deps in
  write_package "ox-host-toolchain" t.fingerprint
    (Printf.sprintf "opam-version: \"2.0\"\nflags: compiler\ndepends: [%s]\n"
       (String.concat "\n" deps))

let copy_tree proc ~src ~dst =
  (* Clone-on-write is optional. The fallback copies rather than hardlinking
     files that package installers may modify. *)
  let copied =
    try
      command proc [ "cp"; "-cR"; src; dst ];
      true
    with Eio.Exn.Io _ -> false
  in
  if not copied then (
    if exists dst then remove_tree dst;
    command proc [ "cp"; "-R"; src; dst ])

let install proc t ~prefix =
  mkdir (prefix / "bin");
  mkdir (prefix / "lib");
  copy_tree proc ~src:(t.prefix / "lib/ocaml") ~dst:(prefix / "lib/ocaml");
  List.iter
    (fun name ->
      let dst = prefix / "bin" / name in
      if not (exists dst) then
        command proc [ "cp"; "-pL"; t.prefix / "bin" / name; dst ])
    t.tools;
  List.iter
    (fun pkg ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let rel = ".opam-switch/config" / (name ^ ".config") in
      let src = t.prefix / rel in
      if exists src then (
        let text = read src in
        let old = t.prefix in
        let n = String.length old in
        let buf = Buffer.create (String.length text) in
        let rec copy i =
          if i < String.length text then
            if i + n <= String.length text && String.sub text i n = old then (
              Buffer.add_string buf prefix;
              copy (i + n))
            else (
              Buffer.add_char buf text.[i];
              copy (i + 1))
        in
        copy 0;
        write (prefix / rel) (Buffer.contents buf)))
    t.packages

let write_repository t ~guards path =
  let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
  if exists tmp then remove_tree tmp;
  Fun.protect
    ~finally:(fun () -> remove_tree tmp)
    (fun () ->
      write_repository_contents t ~guards tmp;
      Unix.rename tmp path)
