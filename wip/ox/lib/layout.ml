open Support

type selection = {
  root : string;
  owner : string option;
  roots : string list;
  snapshot : Stamp.snapshot;
}

let object_file path =
  match OpamJson.of_string (read path) with
  | Some (`O fields) -> fields
  | _ -> fail "Expected a JSON object in %s" path

let strings fields name =
  match List.assoc_opt name fields with
  | None -> []
  | Some (`A xs) ->
      List.map
        (function `String s -> s | _ -> fail "%s must contain strings" name)
        xs
  | _ -> fail "%s must be an array" name

let string fields name =
  match List.assoc_opt name fields with
  | Some (`String s) -> s
  | _ -> fail "Missing string field %s" name

let valid_owner name =
  name <> "" && name <> "." && name <> ".."
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       name

let beneath dir path = path = dir || String.starts_with ~prefix:(dir ^ "/") path

let select ?(working = true) ?(revision = "HEAD") ?(shared_only = false) proc
    ~repo ~owner () =
  let root = git proc repo [ "rev-parse"; "--show-toplevel" ] in
  let contents path =
    if working then read (root / path)
    else capture proc [ "git"; "-C"; root; "show"; revision ^ ":" ^ path ]
  in
  let manifest path =
    match OpamJson.of_string (contents path) with
    | Some (`O fields) -> fields
    | _ -> fail "Invalid manifest %s" path
  in
  let excluded_packages =
    let present =
      if working then exists (root / "workspace.json")
      else
        List.mem "workspace.json"
          (capture proc
             [ "git"; "-C"; root; "ls-tree"; "--name-only"; revision ]
          |> String.split_on_char '\n')
    in
    if not present then []
    else
      let fields = manifest "workspace.json" in
      if
        strings fields "shared_roots" <> [ "wip"; "vendor" ]
        || string fields "user_root" <> "u"
      then fail "Unsupported workspace layout";
      match List.assoc_opt "exclude_packages" fields with
      | Some (`O fields) -> List.map fst fields
      | None -> []
      | _ -> fail "exclude_packages must be an object of reasons"
  in
  let includes, excluded =
    match owner with
    | None -> ([], [])
    | Some owner ->
        if not (valid_owner owner) then fail "Invalid owner %S" owner;
        let fields = manifest ("u" / owner / "user.json") in
        if string fields "owner" <> owner then fail "Owner manifest mismatch";
        let excluded =
          match List.assoc_opt "exclude_projects" fields with
          | None -> []
          | Some (`O fields) -> List.map fst fields
          | _ -> fail "exclude_projects must be an object of reasons"
        in
        (strings fields "include", List.map (fun p -> "u" / owner / p) excluded)
  in
  let allowed project =
    project <> "."
    && (not (List.exists (fun p -> beneath p project) excluded))
    && (beneath "wip" project || beneath "bleeding" project
      || beneath "vendor" project
       ||
       match owner with
       | None -> (not shared_only) && String.starts_with ~prefix:"u/" project
       | Some owner -> beneath ("u" / owner) project)
  in
  let snapshot =
    Stamp.inspect ~working ~select:allowed
      ~select_package:(fun name -> not (List.mem name excluded_packages))
      ~historical_versions:working proc ~repo:root ~revision ~source:None
  in
  let packages = snapshot.Stamp.packages in
  let roots =
    match owner with
    | None -> List.map (fun p -> p.Stamp.name) packages
    | Some owner ->
        includes
        @ (packages
          |> List.filter (fun p -> beneath ("u" / owner) p.Stamp.project)
          |> List.map (fun p -> p.Stamp.name))
  in
  let roots = List.sort_uniq String.compare roots in
  List.iter
    (fun n ->
      if not (List.exists (fun p -> p.Stamp.name = n) packages) then
        fail "Unknown selected package %s" n)
    roots;
  let declared =
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
         [ "git"; "-C"; root; "ls-tree"; "-rz"; "--name-only"; revision ])
    |> nul_lines
    |> List.filter (fun path ->
           Filename.check_suffix path ".opam" && beneath "u" path)
  in
  let forbidden name =
    List.exists
      (fun path ->
        Filename.basename path = name ^ ".opam"
        && not (allowed (Filename.dirname path)))
      declared
  in
  let selected = Hashtbl.create 64 in
  let rec visit name =
    if not (Hashtbl.mem selected name) then
      match List.find_opt (fun p -> p.Stamp.name = name) packages with
      | None ->
          if forbidden name then
            fail "Dependency on excluded owner package %s" name
      | Some p ->
          Hashtbl.add selected name ();
          (* Dune projects are source units; retain their sibling packages. *)
          List.iter
            (fun q -> if q.Stamp.project = p.project then visit q.name)
            packages;
          List.iter
            (fun (n, _) ->
              let name = OpamPackage.Name.to_string n in
              (match List.find_opt (fun q -> q.Stamp.name = name) packages with
              | Some q
                when (not (beneath "u" p.project)) && beneath "u" q.project ->
                  fail "Shared package %s depends on owner package %s" p.name
                    name
              | _ -> ());
              visit name)
            (OpamFormula.fold_left
               (fun xs atom -> atom :: xs)
               []
               (OpamFile.OPAM.depends p.opam)
            @ OpamFormula.fold_left
                (fun xs atom -> atom :: xs)
                []
                (OpamFile.OPAM.depopts p.opam))
  in
  List.iter visit roots;
  let packages =
    List.filter (fun p -> Hashtbl.mem selected p.Stamp.name) packages
  in
  let binaries = Hashtbl.create 32 in
  List.iter
    (fun p ->
      List.iter
        (fun bin ->
          match Hashtbl.find_opt binaries bin with
          | Some other when other <> p.Stamp.name ->
              fail "Duplicate binary %s (%s and %s)" bin other p.name
          | _ -> Hashtbl.replace binaries bin p.Stamp.name)
        p.Stamp.binaries)
    packages;
  { root; owner; roots; snapshot = { snapshot with packages } }

let json t =
  let strings xs = `A (List.map (fun s -> `String s) xs) in
  `O
    [
      ("owner", match t.owner with None -> `Null | Some s -> `String s);
      ("roots", strings t.roots);
      ( "packages",
        `A
          (List.map
             (fun p ->
               `O
                 [
                   ("name", `String p.Stamp.name);
                   ("project", `String p.project);
                   ("base", `String p.base);
                   ("binaries", strings p.binaries);
                 ])
             t.snapshot.packages) );
    ]

let stage proc t ~output =
  if exists output then fail "Output already exists: %s" output;
  let projects =
    List.map (fun p -> p.Stamp.project) t.snapshot.packages
    |> List.sort_uniq String.compare
  in
  let files =
    capture proc
      [
        "git";
        "-C";
        t.root;
        "ls-files";
        "-z";
        "--cached";
        "--others";
        "--exclude-standard";
      ]
    |> nul_lines
    |> List.sort_uniq String.compare
  in
  let roots =
    [
      ".gitignore";
      "dune";
      "dune-project";
      "dune-workspace";
      "workspace.json";
      "toolchain.lock.json";
    ]
  in
  publish_dir output (fun tmp ->
      List.iter
        (fun path ->
          if
            (List.mem path roots
            || List.exists (fun p -> beneath p path) projects)
            && exists (t.root / path)
          then (
            let src = t.root / path and dst = tmp / path in
            let st = Unix.lstat src in
            mkdir (Filename.dirname dst);
            match st.Unix.st_kind with
            | Unix.S_REG ->
                write dst (read src);
                Unix.chmod dst st.st_perm
            | Unix.S_LNK ->
                let target = Unix.realpath src in
                if
                  not
                    (List.exists
                       (fun p -> beneath (t.root / p) target)
                       projects)
                then fail "Symlink escapes selected projects: %s" path;
                let link = Unix.readlink src in
                if not (Filename.is_relative link) then
                  fail "Absolute symlink: %s" path;
                Unix.symlink link dst
            | _ -> fail "Unsupported source file: %s" path))
        files;
      Option.iter
        (fun owner ->
          let path = "u" / owner / "user.json" in
          write (tmp / path) (read (t.root / path)))
        t.owner;
      write (tmp / "ox-selection.json") (OpamJson.to_string (json t)))

let locked_config ~repo (config : Runner.config) =
  let absolute path =
    if Filename.is_relative path then Sys.getcwd () / path else path
  in
  let config =
    {
      config with
      cache = absolute config.cache;
      data = absolute config.data;
      overlays =
        List.map
          (fun path -> if exists path then absolute path else path)
          config.overlays;
      repositories =
        List.map
          (fun path -> if exists path then absolute path else path)
          config.repositories;
    }
  in
  let path = repo / "toolchain.lock.json" in
  if not (exists path) then config
  else
    let fields = object_file path in
    let compiler = string fields "compiler" in
    if compiler <> "oxcaml-compiler.5.2.0minus39" then
      fail "Unsupported compiler lock: %s" compiler;
    let repositories =
      match List.assoc_opt "repositories" fields with
      | Some (`A xs) ->
          List.map
            (function
              | `O fields ->
                  let rev = string fields "revision" in
                  if
                    String.length rev <> 40
                    || not
                         (String.for_all
                            (function
                              | '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
                            rev)
                  then fail "Repository revision must be a full commit";
                  string fields "url" ^ "#" ^ rev
              | _ -> fail "Invalid repository lock")
            xs
      | _ -> fail "Missing repositories in toolchain lock"
    in
    {
      config with
      toolchain = compiler;
      repositories =
        (if config.repositories = [] then repositories else config.repositories);
    }

let init proc ~repo ~owner =
  if not (valid_owner owner) then fail "Invalid owner %S" owner;
  let root = git proc repo [ "rev-parse"; "--show-toplevel" ] in
  let output = root / "u" / owner in
  if exists output then fail "Owner already exists: %s" owner;
  let substitute text =
    let token = "@OWNER@" in
    let buffer = Buffer.create (String.length text) in
    let rec loop i =
      if i < String.length text then
        if
          i + String.length token <= String.length text
          && String.sub text i (String.length token) = token
        then (
          Buffer.add_string buffer owner;
          loop (i + String.length token))
        else (
          Buffer.add_char buffer text.[i];
          loop (i + 1))
    in
    loop 0;
    Buffer.contents buffer
  in
  publish_dir output (fun tmp ->
      List.iter
        (fun file ->
          write (tmp / file)
            (substitute (read (root / "tools/user-template" / file))))
        [ "Dockerfile"; "Dockerfile.dockerignore" ];
      write (tmp / "user.json")
        (OpamJson.to_string
           (`O
             [
               ("schema_version", `Float 1.);
               ("owner", `String owner);
               ("include", `A [ `String "ox" ]);
               ("exclude_projects", `O []);
             ]));
      let name = "hello-" ^ String.lowercase_ascii owner in
      write
        (tmp / "hello/dune-project")
        (Printf.sprintf "(lang dune 3.21)\n(name %s)\n(version 0.1.0)\n" name);
      write (tmp / "hello/dune")
        (Printf.sprintf "(executable (name main) (public_name %s))\n" name);
      write (tmp / "hello/main.ml")
        (Printf.sprintf "let () = print_endline %S\n" ("Hello from " ^ owner));
      write
        (tmp / "hello" / (name ^ ".opam"))
        (Printf.sprintf
           "opam-version: \"2.0\"\n\
            synopsis: %S\n\
            depends: [\"ocaml\" \"dune\" {>= \"3.21\"}]\n\
            build: [[\"dune\" \"build\" \"-p\" name \"-j\" jobs \"@install\"]]\n"
           ("Example application for " ^ owner)))
