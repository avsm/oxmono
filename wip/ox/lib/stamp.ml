open Support
open Sexplib0.Sexp

type package = {
  name : string;
  project : string;
  base : string;
  version : string;
  source_hash : string;
  opam : OpamFile.OPAM.t;
  binaries : string list;
}

type snapshot = { commit : string; source : string; packages : package list }

let sexps s = try Parsexp.Many.parse_string_exn s with _ -> []

let field key fields =
  List.find_map
    (function
      | Sexplib0.Sexp.List (Atom k :: values) when k = key -> Some values
      | _ -> None)
    fields

let atom_field key fields =
  match field key fields with Some [ Atom x ] -> Some x | _ -> None

let project_version text =
  sexps text
  |> List.find_map (function
       | Sexplib0.Sexp.List [ Atom "version"; Atom v ] -> Some v
       | _ -> None)

let source_url root source =
  let s = Option.value source ~default:("file://" ^ root) in
  if String.contains s '#' then
    fail "Source URL must not contain a revision: %s" s;
  if String.starts_with ~prefix:"git+" s then s else "git+" ^ s

let inspect ?(working = false) ?(select = fun _ -> true)
    ?(select_package = fun _ -> true) ?(historical_versions = true) proc ~repo
    ~revision ~source =
  let root = git proc repo [ "rev-parse"; "--show-toplevel" ] in
  if
    (not working) && historical_versions
    && git proc root [ "rev-parse"; "--is-shallow-repository" ] = "true"
  then
    fail
      "Snapshot versioning requires complete Git history. Unshallow %s first."
      root;
  let commit =
    if working then ""
    else git proc root [ "rev-parse"; "--verify"; revision ^ "^{commit}" ]
  in
  let files =
    (if working then
       capture proc
         [
           "git";
           "-C";
           root;
           "ls-files";
           "-z";
           "--cached";
           "--others";
           "--exclude-standard";
         ]
     else
       capture proc
         [ "git"; "-C"; root; "ls-tree"; "-rz"; "--name-only"; commit ])
    |> nul_lines
    |> List.sort_uniq String.compare
    |> List.filter (fun path -> (not working) || exists (root / path))
  in
  let projects =
    files
    |> List.filter_map (fun p ->
           if Filename.basename p = "dune-project" then
             Some (Filename.dirname p)
           else None)
  in
  let project_of path =
    projects
    |> List.filter (fun d ->
           d = "." || path = d || String.starts_with ~prefix:(d ^ "/") path)
    |> List.sort (fun a b -> Int.compare (String.length b) (String.length a))
    |> function
    | p :: _ -> Some p
    | [] -> None
  in
  let blobs = Hashtbl.create 128 in
  if not working then (
    let paths =
      files
      |> List.filter (fun path ->
             (Filename.check_suffix path ".opam"
             || List.mem (Filename.basename path) [ "dune"; "dune-project" ])
             && match project_of path with Some p -> select p | None -> false)
    in
    List.iter
      (fun path ->
        if String.contains path '\n' then
          fail "Unsupported metadata path: %S" path)
      paths;
    let stdin =
      Eio.Flow.string_source
        (String.concat ""
           (List.map (fun path -> commit ^ ":" ^ path ^ "\n") paths))
    in
    Eio.Process.parse_out ~env:(clean_env ()) ~stdin proc
      (fun input ->
        List.iter
          (fun path ->
            let header = Eio.Buf_read.line input in
            let size =
              match String.split_on_char ' ' header with
              | [ _; "blob"; size ] -> int_of_string size
              | _ -> fail "Invalid Git blob response for %s" path
            in
            let contents = Eio.Buf_read.take size input in
            Eio.Buf_read.char '\n' input;
            Hashtbl.add blobs path contents)
          paths)
      [ "git"; "-C"; root; "cat-file"; "--batch" ]);
  let blob path =
    if working then read (root / path) else Hashtbl.find blobs path
  in
  let commit =
    if working then
      hash_fields
        (root
        :: (files
           |> List.filter (fun path ->
                  Filename.check_suffix path ".opam"
                  || List.mem (Filename.basename path)
                       [ "dune"; "dune-project" ])
           |> List.concat_map (fun path -> [ path; blob path ])))
    else commit
  in
  let count =
    if working then "local"
    else if historical_versions then
      git proc root [ "rev-list"; "--count"; commit ]
    else "0"
  in
  let trees = Hashtbl.create 128 in
  if not working then (
    capture proc [ "git"; "-C"; root; "ls-tree"; "-rdz"; commit ]
    |> nul_lines
    |> List.iter (fun entry ->
           match String.index_opt entry '\t' with
           | None -> fail "Invalid Git tree entry"
           | Some tab -> (
               let header = String.sub entry 0 tab in
               let path =
                 String.sub entry (tab + 1) (String.length entry - tab - 1)
               in
               match String.split_on_char ' ' header with
               | [ _; "tree"; hash ] -> Hashtbl.add trees path hash
               | _ -> fail "Invalid Git tree header"));
    if select "." then
      Hashtbl.add trees "." (git proc root [ "rev-parse"; commit ^ "^{tree}" ]));
  let metadata =
    files
    |> List.filter_map (fun path ->
           let project = Filename.dirname path in
           if
             not
               (Filename.check_suffix path ".opam"
               && List.mem project projects && select project
               && select_package
                    (Filename.chop_extension (Filename.basename path)))
           then None
           else if String.trim (blob path) = "" then (
             log "Skipping empty opam placeholder %s" path;
             None)
           else
             let name = Filename.basename path |> Filename.chop_extension in
             if List.mem name [ "ox-local-snapshot"; "oxmono-snapshot" ] then
               fail "Reserved ox package name: %s" name;
             let opam =
               OpamFile.OPAM.read_from_string
                 ~filename:(opam_file (root / path))
                 (blob path)
             in
             let base =
               match OpamFile.OPAM.version_opt opam with
               | Some v -> OpamPackage.Version.to_string v
               | None ->
                   Option.value ~default:"0.0.0"
                     (project_version
                        (blob
                           (if project = "." then "dune-project"
                            else project / "dune-project")))
             in
             let source_hash =
               if working then commit else Hashtbl.find trees project
             in
             let version =
               base ^ "+ox." ^ count ^ "." ^ String.sub commit 0 12
             in
             ignore (OpamPackage.of_string (name ^ "." ^ version));
             Some
               {
                 name;
                 project;
                 base;
                 version;
                 source_hash;
                 opam;
                 binaries = [];
               })
  in
  let seen = Hashtbl.create 64 in
  List.iter
    (fun p ->
      if Hashtbl.mem seen p.name then fail "Duplicate opam package %s" p.name;
      Hashtbl.add seen p.name ())
    metadata;
  let binaries = Hashtbl.create 32 in
  files
  |> List.filter (fun p -> Filename.basename p = "dune")
  |> List.iter (fun path ->
         let owners =
           List.filter (fun p -> Some p.project = project_of path) metadata
         in
         (if owners = [] then [] else sexps (blob path))
         |> List.iter (function
              | Sexplib0.Sexp.List
                  (Atom ("executable" | "executables") :: fields) ->
                  let public =
                    match field "public_names" fields with
                    | Some names ->
                        List.filter_map
                          (function
                            | Atom n when n <> "-" -> Some n | _ -> None)
                          names
                    | None -> Option.to_list (atom_field "public_name" fields)
                  in
                  List.iter
                    (fun bin ->
                      let owner =
                        match atom_field "package" fields with
                        | Some name ->
                            List.find_opt (fun p -> p.name = name) owners
                        | None -> (
                            match owners with
                            | [ p ] -> Some p
                            | _ -> List.find_opt (fun p -> p.name = bin) owners)
                      in
                      Option.iter
                        (fun p ->
                          let old =
                            Option.value
                              (Hashtbl.find_opt binaries p.name)
                              ~default:[]
                          in
                          Hashtbl.replace binaries p.name (bin :: old))
                        owner)
                    public
              | _ -> ()));
  let packages =
    List.map
      (fun p ->
        {
          p with
          binaries =
            Option.value (Hashtbl.find_opt binaries p.name) ~default:[]
            |> List.sort_uniq String.compare;
        })
      metadata
  in
  { commit; source = source_url root source; packages }

let stamped_opam snapshot p =
  let rewrite formula =
    OpamFormula.map
      (fun (name, condition) ->
        match
          List.find_opt
            (fun q -> q.name = OpamPackage.Name.to_string name)
            snapshot.packages
        with
        | None -> OpamFormula.Atom (name, condition)
        | Some q ->
            let condition =
              OpamFormula.map
                (function
                  | OpamTypes.Constraint (op, OpamTypes.FString v)
                    when v = q.base ->
                      OpamFormula.Atom
                        (OpamTypes.Constraint (op, OpamTypes.FString q.version))
                  | atom -> OpamFormula.Atom atom)
                condition
            in
            let exact =
              OpamFormula.Atom
                (OpamTypes.Constraint (`Eq, OpamTypes.FString q.version))
            in
            let condition =
              match condition with
              | OpamFormula.Empty -> exact
              | _ -> OpamFormula.And (condition, exact)
            in
            OpamFormula.Atom (name, condition))
      formula
  in
  let url =
    OpamFile.URL.create
      (OpamUrl.parse (snapshot.source ^ "#" ^ snapshot.commit))
  in
  let url =
    if p.project = "." then url
    else
      OpamFile.URL.with_subpath (OpamFilename.SubPath.of_string p.project) url
  in
  p.opam
  |> OpamFile.OPAM.with_name (OpamPackage.Name.of_string p.name)
  |> OpamFile.OPAM.with_version (OpamPackage.Version.of_string p.version)
  |> OpamFile.OPAM.with_url url
  |> OpamFile.OPAM.with_depends (rewrite (OpamFile.OPAM.depends p.opam))
  |> OpamFile.OPAM.with_pin_depends
       (List.filter
          (fun (pkg, _) ->
            not
              (List.exists
                 (fun q ->
                   q.name = OpamPackage.Name.to_string (OpamPackage.name pkg))
                 snapshot.packages))
          (OpamFile.OPAM.pin_depends p.opam))
  |> OpamFile.OPAM.with_depopts (rewrite (OpamFile.OPAM.depopts p.opam))
  |> OpamFile.OPAM.write_to_string
  |> fun text ->
  text
  ^ Printf.sprintf
      "\n\
       x-ox-commit: %S\n\
       x-ox-project: %S\n\
       x-ox-source-hash: %S\n\
       x-ox-binaries: [%s]\n"
      snapshot.commit p.project p.source_hash
      (String.concat " " (List.map (Printf.sprintf "%S") p.binaries))

let export proc ~repo ~revision ~source ~output =
  if exists output then fail "Output already exists: %s" output;
  let snapshot = inspect proc ~repo ~revision ~source in
  if snapshot.packages = [] then
    fail "No project-root opam files found in %s" repo;
  publish_dir output (fun staging ->
      write (staging / "repo") "opam-version: \"2.0\"\n";
      List.iter
        (fun p ->
          write
            (staging / "packages" / p.name / (p.name ^ "." ^ p.version) / "opam")
            (stamped_opam snapshot p))
        snapshot.packages;
      write (staging / "ox-source")
        (snapshot.source ^ "#" ^ snapshot.commit ^ "\n"));
  snapshot

let working proc ~repo ~data =
  let root = git proc repo [ "rev-parse"; "--show-toplevel" ] in
  let snapshot =
    inspect ~working:true proc ~repo:root ~revision:"HEAD" ~source:None
  in
  let output = data / "workspaces" / snapshot.commit in
  if not (exists output) then
    publish_dir output (fun tmp ->
        write (tmp / "repo") "opam-version: \"2.0\"\n";
        List.iter
          (fun p ->
            write
              (tmp / "packages" / p.name / (p.name ^ "." ^ p.version) / "opam")
              (stamped_opam snapshot p
              ^ Printf.sprintf "x-ox-worktree: %S\n" (root / p.project)))
          snapshot.packages;
        write (tmp / "ox-source") (root ^ "\n");
        write (tmp / "ox-worktree") (root ^ "\n"));
  (root, output, snapshot)
