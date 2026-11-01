open Support

let archive_script =
  {|
import gzip, hashlib, io, os, pathlib, sys, tarfile
root, output, timestamp = pathlib.Path(sys.argv[1]), sys.argv[2], int(sys.argv[3])
paths = sorted(root.rglob('*'), key=lambda p: p.relative_to(root).as_posix())
with open(output, 'wb') as raw:
 with gzip.GzipFile(filename='', mode='wb', fileobj=raw, mtime=0) as gz:
  with tarfile.open(fileobj=gz, mode='w', format=tarfile.PAX_FORMAT) as tar:
   for path in paths:
    name = path.relative_to(root).as_posix()
    if path.is_symlink():
     link = os.readlink(path)
     if os.path.isabs(link) or not path.resolve().is_relative_to(root.resolve()):
      raise ValueError('Escaping symlink: '+name)
    if not (path.is_file() or path.is_dir() or path.is_symlink()):
     raise ValueError('Unsupported archive entry: '+name)
    info = tar.gettarinfo(str(path), arcname=name)
    info.uid = info.gid = 0
    info.uname = info.gname = ''
    info.mtime = timestamp
    info.mode = 0o755 if info.isdir() or (info.mode & 0o111) else 0o644
    info.pax_headers = {}
    if info.isfile():
     with path.open('rb') as f: tar.addfile(info, f)
    else: tar.addfile(info)
|}

let digest path = OpamHash.compute ~kind:`SHA256 path |> OpamHash.to_string
let json_string s = `String s
let compiler = "5.2.0minus39"

let version base revision =
  if revision < 1 then fail "OxCaml revision must be positive";
  let base =
    match OpamStd.String.cut_at base '+' with
    | Some (base, suffix)
      when String.starts_with ~prefix:"ox" suffix
           && String.for_all
                (function '0' .. '9' -> true | _ -> false)
                (String.sub suffix 2 (String.length suffix - 2)) ->
        base
    | _ -> base
  in
  base ^ "+ox" ^ if revision = 1 then "" else string_of_int revision

let exact name version =
  OpamFormula.Atom
    ( OpamPackage.Name.of_string name,
      OpamFormula.Atom (OpamTypes.Constraint (`Eq, OpamTypes.FString version))
    )

let conjunction a b =
  match (a, b) with
  | OpamFormula.Empty, x | x, OpamFormula.Empty -> x
  | _ -> OpamFormula.And (a, b)

let metadata ~maintainers snapshot p =
  let opam = OpamFile.OPAM.read_from_string (Stamp.stamped_opam snapshot p) in
  let opam =
    OpamFile.OPAM.remove_extension opam "x-subpath"
    |> OpamFile.OPAM.with_url_opt None
  in
  let opam =
    if OpamFile.OPAM.maintainer opam = [] then
      OpamFile.OPAM.with_maintainer maintainers opam
    else opam
  in
  let conflicts =
    OpamFormula.map
      (fun (name, condition) ->
        let condition =
          match
            List.find_opt
              (fun q -> q.Stamp.name = OpamPackage.Name.to_string name)
              snapshot.Stamp.packages
          with
          | None -> condition
          | Some q ->
              OpamFormula.map
                (function
                  | OpamTypes.Constraint (op, OpamTypes.FString v)
                    when v = q.base ->
                      OpamFormula.Atom
                        (OpamTypes.Constraint (op, OpamTypes.FString q.version))
                  | atom -> OpamFormula.Atom atom)
                condition
        in
        OpamFormula.Atom (name, condition))
      (OpamFile.OPAM.conflicts opam)
  in
  opam
  |> OpamFile.OPAM.with_depends
       (conjunction
          (OpamFile.OPAM.depends opam)
          (exact "oxcaml-compiler" compiler))
  |> OpamFile.OPAM.with_conflicts conflicts

let freeze_project path packages =
  let file = path / "dune-project" in
  let text = read file in
  let length = String.length text in
  let depth = ref 0 and start = ref 0 and spans = ref [] in
  let rec line_end i =
    if i >= length || text.[i] = '\n' then i else line_end (i + 1)
  in
  let rec quoted i =
    if i >= length then fail "Unterminated string in %s" file
    else
      match text.[i] with
      | '\\' -> quoted (i + 2)
      | '"' -> i + 1
      | _ -> quoted (i + 1)
  in
  let rec scan i =
    if i < length then
      match text.[i] with
      | ';' -> scan (line_end i)
      | '#' when i + 1 < length && text.[i + 1] = ';' ->
          fail "S-expression comments are unsupported in release metadata: %s"
            file
      | '"'
        when i + 2 < length
             && text.[i + 1] = '\\'
             && (text.[i + 2] = '>' || text.[i + 2] = '|') ->
          scan (line_end i)
      | '"' -> scan (quoted (i + 1))
      | '(' ->
          if !depth = 0 then start := i;
          incr depth;
          scan (i + 1)
      | ')' ->
          decr depth;
          if !depth < 0 then fail "Unbalanced project: %s" file;
          if !depth = 0 then spans := (!start, i + 1) :: !spans;
          scan (i + 1)
      | _ -> scan (i + 1)
  in
  scan 0;
  if !depth <> 0 then fail "Unbalanced project: %s" file;
  let whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false in
  let head a b =
    let rec skip i = if i < b && whitespace text.[i] then skip (i + 1) else i in
    let a = skip (a + 1) in
    let rec stop i =
      if i < b && (not (whitespace text.[i])) && text.[i] <> ')' then
        stop (i + 1)
      else i
    in
    String.sub text a (stop a - a)
  in
  (* Preserve Dune's quoted-line strings and unrelated source formatting. *)
  let buffer = Buffer.create length and cursor = ref 0 in
  List.rev !spans
  |> List.iter (fun (a, b) ->
         if List.mem (head a b) [ "version"; "generate_opam_files" ] then (
           Buffer.add_substring buffer text !cursor (a - !cursor);
           cursor := b));
  Buffer.add_substring buffer text !cursor (length - !cursor);
  Buffer.add_string buffer "\n(generate_opam_files false)\n";
  let versions =
    List.map (fun p -> p.Stamp.version) packages
    |> List.sort_uniq String.compare
  in
  (match versions with
  | [ v ] -> Buffer.add_string buffer (Printf.sprintf "(version %S)\n" v)
  | _ -> ());
  write file (Buffer.contents buffer)

let copy_guards repository output packages =
  (* Preserve the upstream guard chain and allow only explicit new versions. *)
  List.iter
    (fun p ->
      let name = "oxcaml-" ^ p.Stamp.name ^ "-patches" in
      let dir = repository / "packages" / name in
      if exists dir then
        List.iter
          (fun nv ->
            let src = dir / nv in
            let dst = output / "packages" / name / nv in
            let opam =
              read_opam ((if exists (dst / "opam") then dst else src) / "opam")
            in
            let deps =
              OpamFormula.map
                (fun (n, condition) ->
                  if OpamPackage.Name.to_string n <> p.name then
                    OpamFormula.Atom (n, condition)
                  else
                    (* Keep dependency filters outside the version disjunction. *)
                    let expanded =
                      OpamFormula.map
                        (function
                          | OpamTypes.Constraint (`Eq, _) as atom ->
                              OpamFormula.Or
                                ( OpamFormula.Atom atom,
                                  OpamFormula.Atom
                                    (OpamTypes.Constraint
                                       (`Eq, OpamTypes.FString p.version)) )
                          | atom -> OpamFormula.Atom atom)
                        condition
                    in
                    OpamFormula.Atom (n, expanded))
                (OpamFile.OPAM.depends opam)
            in
            copy_files ~src ~dst;
            write_opam (dst / "opam") (OpamFile.OPAM.with_depends deps opam))
          (sorted_dir dir))
    packages

let committed_json proc root commit path =
  let files = git proc root [ "ls-tree"; "--name-only"; commit; "--"; path ] in
  if files = "" then `Null
  else
    match
      OpamJson.of_string
        (capture proc [ "git"; "-C"; root; "show"; commit ^ ":" ^ path ])
    with
    | Some json -> json
    | None -> fail "Invalid committed JSON: %s" path

let prepare proc ~repo ~revision ~roots ~owner ~ox_revision =
  let root = git proc repo [ "rev-parse"; "--show-toplevel" ] in
  let commit =
    git proc root [ "rev-parse"; "--verify"; revision ^ "^{commit}" ]
  in
  let selected =
    Layout.select ~working:false ~revision:commit ~shared_only:(owner = None)
      proc ~repo:root ~owner ()
  in
  let release_versions =
    match committed_json proc root commit "release-versions.json" with
    | `Null -> []
    | `O fields when List.assoc_opt "schema_version" fields = Some (`Float 1.)
      -> (
        match List.assoc_opt "projects" fields with
        | Some (`O projects) -> projects
        | _ -> fail "Missing release version projects")
    | _ -> fail "Invalid release-versions.json"
  in
  let all =
    List.map
      (fun p ->
        match List.assoc_opt p.Stamp.project release_versions with
        | None -> p
        | Some (`O fields) -> (
            match List.assoc_opt "base_version" fields with
            | Some (`String base) when base <> "" -> { p with Stamp.base }
            | _ -> fail "Missing release base for %s" p.project)
        | _ -> fail "Invalid release base for %s" p.project)
      selected.snapshot.Stamp.packages
  in
  let roots =
    if roots <> [] then roots
    else
      match owner with
      | Some _ -> selected.roots
      | None ->
          List.filter
            (fun p ->
              String.starts_with ~prefix:"wip/" p.Stamp.project
              || String.starts_with ~prefix:"bleeding/" p.Stamp.project)
            all
          |> List.map (fun p -> p.Stamp.name)
  in
  List.iter
    (fun n ->
      if not (List.exists (fun p -> p.Stamp.name = n) all) then
        fail "Unknown release package %s" n)
    roots;
  let seen = Hashtbl.create 64 in
  let rec visit name =
    if not (Hashtbl.mem seen name) then
      match List.find_opt (fun p -> p.Stamp.name = name) all with
      | None -> ()
      | Some p ->
          Hashtbl.add seen name ();
          List.iter
            (fun q -> if q.Stamp.project = p.project then visit q.name)
            all;
          List.iter
            (fun formula ->
              OpamFormula.fold_left
                (fun () (n, _) -> visit (OpamPackage.Name.to_string n))
                () formula)
            [ OpamFile.OPAM.depends p.opam; OpamFile.OPAM.depopts p.opam ]
  in
  List.iter visit roots;
  let packages =
    all
    |> List.filter (fun p -> Hashtbl.mem seen p.Stamp.name)
    |> List.map (fun p ->
           if List.mem p.Stamp.base [ "0.0.0"; "dev"; "development" ] then
             fail "Declare a release base version for %s in %s/dune-project"
               p.name p.project;
           { p with Stamp.version = version p.Stamp.base ox_revision })
  in
  if packages = [] then fail "No release packages selected";
  let snapshot = { selected.snapshot with Stamp.packages } in
  (root, commit, snapshot)

let guard_lock proc root commit guard_repository =
  let lock = committed_json proc root commit "toolchain.lock.json" in
  (match (lock, guard_repository) with
  | `O fields, Some path ->
      let expected =
        match List.assoc_opt "repositories" fields with
        | Some (`A (`O fields :: _)) -> (
            match List.assoc_opt "revision" fields with
            | Some (`String s) -> s
            | _ -> fail "Missing repository revision")
        | _ -> fail "Missing locked OxCaml repository"
      in
      if git proc path [ "rev-parse"; "HEAD" ] <> expected then
        fail "Guard repository must be checked out at %s" expected;
      if git proc path [ "status"; "--porcelain" ] <> "" then
        fail "Guard repository must have a clean working tree"
  | _ -> ());
  lock

let provenance proc root commit packages =
  let names =
    packages
    |> List.filter_map (fun p ->
           match String.split_on_char '/' p.Stamp.project with
           | "vendor" :: name :: _ -> Some name
           | _ -> None)
    |> List.sort_uniq String.compare
  in
  let entries =
    match committed_json proc root commit "vendor/upstreams.json" with
    | `O fields -> (
        match List.assoc_opt "vendors" fields with
        | Some (`A xs) -> xs
        | _ -> [])
    | _ -> []
  in
  `A
    (List.map
       (fun name ->
         match
           List.find_opt
             (function
               | `O fields -> List.assoc_opt "name" fields = Some (`String name)
               | _ -> false)
             entries
         with
         | Some (`O fields as entry) -> (
             match List.assoc_opt "revision" fields with
             | Some (`String rev) when rev <> "" -> entry
             | _ ->
                 fail "Cannot release vendor %s without its upstream revision"
                   name)
         | _ -> fail "Missing vendor provenance for %s" name)
       names)

let verify _proc ~directory =
  let obj = function
    | `O fields -> fields
    | _ -> fail "Expected manifest object"
  in
  let str fields name =
    match List.assoc_opt name fields with
    | Some (`String s) -> s
    | _ -> fail "Missing manifest field %s" name
  in
  let arr fields name =
    match List.assoc_opt name fields with
    | Some (`A xs) -> xs
    | _ -> fail "Missing manifest array %s" name
  in
  let fields =
    match OpamJson.of_string (read (directory / "manifest.json")) with
    | Some json -> obj json
    | None -> fail "Invalid release manifest"
  in
  if List.assoc_opt "schema_version" fields <> Some (`Float 1.) then
    fail "Unsupported release manifest schema";
  if str fields "compiler" <> compiler then fail "Unsupported release compiler";
  if arr fields "archives" = [] || arr fields "metadata" = [] then
    fail "Empty release manifest";
  let root = Unix.realpath directory in
  let check field record =
    let fields = obj record in
    let path = str fields field in
    if
      (not (Filename.is_relative path))
      || List.exists (( = ) "..") (String.split_on_char '/' path)
    then fail "Unsafe manifest path %s" path;
    let actual = Unix.realpath (directory / path) in
    if not (String.starts_with ~prefix:(root ^ "/") actual) then
      fail "Manifest link escapes release: %s" path;
    if digest (directory / path) <> str fields "checksum" then
      fail "Checksum mismatch: %s" path
  in
  List.iter (check "archive") (arr fields "archives");
  List.iter (check "path") (arr fields "metadata");
  let expected =
    List.map (fun r -> str (obj r) "archive") (arr fields "archives")
    @ List.map (fun r -> str (obj r) "path") (arr fields "metadata")
    |> List.sort String.compare
  in
  let rec files rel =
    sorted_dir (directory / rel)
    |> List.concat_map (fun name ->
           let path = rel / name in
           match (Unix.lstat (directory / path)).Unix.st_kind with
           | Unix.S_DIR -> files path
           | Unix.S_REG -> [ path ]
           | _ -> fail "Unexpected release entry: %s" path)
  in
  let actual =
    files "sources" @ files "opam-repository" |> List.sort String.compare
  in
  if actual <> expected then
    fail "Release contains missing, duplicate or unrecorded files";
  log "Verified release %s" (str fields "commit")

let validate_inputs ~base_url ~guard_repository =
  if guard_repository = None then
    fail
      "Source exports require --guard-repository with the pinned OxCaml overlay";
  Option.iter
    (fun path ->
      if not (exists (path / "repo")) then
        fail "Not an opam guard repository: %s" path)
    guard_repository;
  if
    not
      (String.starts_with ~prefix:"https://" base_url
      || String.starts_with ~prefix:"file://" base_url)
  then fail "Archive base URL must use https:// or file://"

let validate_versions guard_repository packages =
  Option.iter
    (fun path ->
      List.iter
        (fun p ->
          if
            exists
              (path / "packages" / p.Stamp.name
              / (p.name ^ "." ^ p.version)
              / "opam")
          then
            fail
              "Version %s.%s already exists in the upstream overlay; increase \
               --ox-revision"
              p.name p.version)
        packages)
    guard_repository

let validate_source stage packages =
  let root = Unix.realpath stage in
  let rec walk path =
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR -> List.iter (fun name -> walk (path / name)) (sorted_dir path)
    | Unix.S_REG -> ()
    | Unix.S_LNK ->
        let target = Unix.realpath path in
        if
          (not (Filename.is_relative (Unix.readlink path)))
          || not (String.starts_with ~prefix:(root ^ "/") target)
        then fail "Source symlink escapes project: %s" path
    | _ -> fail "Unsupported source entry: %s" path
  in
  walk stage;
  List.iter
    (fun path ->
      if (Unix.lstat (stage / path)).Unix.st_kind <> Unix.S_REG then
        fail "Release metadata must be a regular file: %s" path)
    ("dune-project" :: List.map (fun p -> p.Stamp.name ^ ".opam") packages)

let plan proc ~repo ~revision ~packages ~owner ~ox_revision ~base_url
    ~guard_repository =
  validate_inputs ~base_url ~guard_repository;
  let root, commit, snapshot =
    prepare proc ~repo ~revision ~roots:packages ~owner ~ox_revision
  in
  let _ = guard_lock proc root commit guard_repository in
  let _ = provenance proc root commit snapshot.Stamp.packages in
  validate_versions guard_repository snapshot.packages;
  `O
    [
      ("schema_version", `Float 1.);
      ("repo", `String root);
      ("commit", `String commit);
      ( "packages",
        `A (List.map (fun p -> `String p.Stamp.name) snapshot.packages) );
      ( "versions",
        `A
          (List.map
             (fun p -> `String (p.Stamp.name ^ "." ^ p.version))
             snapshot.packages) );
      ("owner", match owner with None -> `Null | Some s -> `String s);
      ("ox_revision", `Float (float_of_int ox_revision));
      ("base_url", `String base_url);
      ( "guard_repository",
        match guard_repository with
        | None -> `Null
        | Some s -> `String (Unix.realpath s) );
      ( "guard_hash",
        match guard_repository with
        | None -> `Null
        | Some s -> `String (tree_hash s) );
    ]

let export proc ~repo ~revision ~packages:roots ~owner ~ox_revision ~base_url
    ~guard_repository ~previous ~output =
  validate_inputs ~base_url ~guard_repository;
  if exists output then fail "Output already exists: %s" output;
  Option.iter (fun directory -> verify proc ~directory) previous;
  let root, commit, snapshot =
    prepare proc ~repo ~revision ~roots ~owner ~ox_revision
  in
  let maintainers =
    match committed_json proc root commit "release-versions.json" with
    | `O fields -> (
        match List.assoc_opt "maintainers" fields with
        | Some (`A xs) ->
            List.map
              (function
                | `String s -> s | _ -> fail "Invalid release maintainer")
              xs
        | _ -> [ "OxMono release maintainers" ])
    | _ -> [ "OxMono release maintainers" ]
  in
  let lock = guard_lock proc root commit guard_repository in
  let packages = snapshot.Stamp.packages in
  let provenance = provenance proc root commit packages in
  validate_versions guard_repository packages;
  let projects =
    List.map (fun p -> p.Stamp.project) packages
    |> List.sort_uniq String.compare
  in
  let timestamp = git proc root [ "show"; "-s"; "--format=%ct"; commit ] in
  publish_dir output (fun tmp ->
      let overlay = tmp / "opam-repository" in
      write (overlay / "repo") "opam-version: \"2.0\"\n";
      Option.iter
        (fun path ->
          let src = path / "opam-repository" in
          if not (exists (src / "repo")) then
            fail "No previous release repository at %s" path;
          copy_files ~src ~dst:overlay)
        previous;
      let put path opam =
        let text = OpamFile.OPAM.write_to_string opam in
        if exists path && read path <> text then
          fail
            "Package version already exists with different metadata: %s. \
             Increase --ox-revision."
            path;
        write path text
      in
      let policy = "oxmono-snapshot" in
      let cohort =
        hash_fields
          (commit :: List.map (fun p -> p.Stamp.name ^ "." ^ p.version) packages)
      in
      let policy_version = "0+ox." ^ cohort in
      let policy_conflicts =
        List.map
          (fun p -> Printf.sprintf "%S {!= %S}" p.Stamp.name p.version)
          packages
      in
      put
        (overlay / "packages" / policy
        / (policy ^ "." ^ policy_version)
        / "opam")
        (OpamFile.OPAM.read_from_string
           (Printf.sprintf
              "opam-version: \"2.0\"\n\
               synopsis: \"OxMono source cohort\"\n\
               maintainer: [%s]\n\
               depends: [\"oxcaml-compiler\" {= %S}]\n\
               conflicts: [%s]\n"
              (String.concat " " (List.map (Printf.sprintf "%S") maintainers))
              compiler
              (String.concat "\n" policy_conflicts)));
      let records =
        List.map
          (fun project ->
            let unit_packages =
              List.filter (fun p -> p.Stamp.project = project) packages
            in
            let name =
              String.concat "-" (String.split_on_char '/' project)
              ^ "-" ^ hash project
            in
            let tree = git proc root [ "rev-parse"; commit ^ ":" ^ project ] in
            let filename =
              name ^ "-" ^ tree ^ "-" ^ String.sub commit 0 12 ^ "-ox"
              ^ string_of_int ox_revision ^ ".tar.gz"
            in
            let stage = tmp / "staging" / name in
            mkdir stage;
            let raw = tmp / "staging" / (name ^ ".tar") in
            command proc
              [
                "git";
                "-C";
                root;
                "archive";
                "--format=tar";
                "-o";
                raw;
                commit ^ ":" ^ project;
              ];
            command proc [ "tar"; "-xf"; raw; "-C"; stage ];
            validate_source stage unit_packages;
            freeze_project stage unit_packages;
            if not (exists (stage / "dune-workspace")) then
              write (stage / "dune-workspace")
                "(lang dune 3.21)\n\
                 (env (dev (ocamlopt_flags (:standard -zero-alloc-check \
                 none))) (release (ocamlopt_flags (:standard -zero-alloc-check \
                 none))) (release-check (ocamlopt_flags (:standard -O3 \
                 -zero-alloc-check all))))\n";
            List.iter
              (fun p ->
                let opam = metadata ~maintainers snapshot p in
                let opam =
                  OpamFile.OPAM.with_depends
                    (conjunction
                       (OpamFile.OPAM.depends opam)
                       (exact policy policy_version))
                    opam
                in
                (* The archive does not contain its own download checksum. *)
                write_opam
                  (stage / (p.name ^ ".opam"))
                  (OpamFile.OPAM.with_url_opt None opam))
              unit_packages;
            let rel = "sources" / filename in
            mkdir (tmp / "sources");
            command proc
              [ "python3"; "-c"; archive_script; stage; tmp / rel; timestamp ];
            let checksum = digest (tmp / rel) in
            List.iter
              (fun p ->
                let opam = read_opam (stage / (p.Stamp.name ^ ".opam")) in
                let url =
                  OpamFile.URL.create
                    (OpamUrl.parse (String.trim base_url ^ "/" ^ rel))
                  |> OpamFile.URL.with_checksum [ OpamHash.of_string checksum ]
                in
                put
                  (overlay / "packages" / p.name
                  / (p.name ^ "." ^ p.version)
                  / "opam")
                  (OpamFile.OPAM.with_url url opam))
              unit_packages;
            `O
              [
                ("project", json_string project);
                ("archive", json_string rel);
                ("checksum", json_string checksum);
                ( "packages",
                  `A
                    (List.map
                       (fun p -> json_string (p.Stamp.name ^ "." ^ p.version))
                       unit_packages) );
              ])
          projects
      in
      Option.iter
        (fun path -> copy_guards path overlay packages)
        guard_repository;
      remove_tree (tmp / "staging");
      let rec entries dir =
        sorted_dir dir
        |> List.concat_map (fun name ->
               let path = dir / name in
               if Sys.is_directory path then entries path else [ path ])
      in
      let metadata_files =
        entries overlay
        |> List.map (fun path ->
               `O
                 [
                   ( "path",
                     json_string
                       (String.sub path
                          (String.length tmp + 1)
                          (String.length path - String.length tmp - 1)) );
                   ("checksum", json_string (digest path));
                 ])
      in
      write (tmp / "manifest.json")
        (OpamJson.to_string
           (`O
             [
               ("schema_version", `Float 1.);
               ("commit", json_string commit);
               ("compiler", json_string compiler);
               ("export_recipe", `Float 1.);
               ("toolchain_lock", lock);
               ("provenance", provenance);
               ( "guard_hash",
                 match guard_repository with
                 | None -> `Null
                 | Some path -> json_string (tree_hash path) );
               ("archives", `A records);
               ("metadata", `A metadata_files);
             ])))
