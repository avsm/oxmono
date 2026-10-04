open Support

let name p = OpamPackage.Name.to_string (OpamPackage.name p.Solve.id)
let version p = OpamPackage.Version.to_string (OpamPackage.version p.Solve.id)

let path_var ~prefix ~name ~qualified = function
  | "prefix" -> Some prefix
  | "lib" -> Some (if qualified then prefix / "lib" / name else prefix / "lib")
  | ("share" | "etc" | "doc") as sub ->
      Some (if qualified then prefix / sub / name else prefix / sub)
  | ("bin" | "sbin" | "man") as sub -> Some (prefix / sub)
  | "stublibs" -> Some (prefix / "lib/stublibs")
  | "toplevel" -> Some (prefix / "lib/toplevel")
  | _ -> None

let resolver ?(test = false) ?config_var ~solution ~installed ~prefix ~build_dir
    ~jobs p =
  let configs = Hashtbl.create 16 in
  let config pkg var =
    let conf =
      match Hashtbl.find_opt configs pkg with
      | Some c -> c
      | None ->
          let path = prefix / ".ox/config" / (pkg ^ ".config") in
          let c =
            if exists path then Some (OpamFile.Dot_config.read (opam_file path))
            else None
          in
          Hashtbl.add configs pkg c;
          c
    in
    Option.bind conf (fun c ->
        OpamFile.Dot_config.variable c (OpamVariable.of_string var))
  in
  fun full ->
    let scope = OpamVariable.Full.scope full in
    let var = OpamVariable.Full.variable full |> OpamVariable.to_string in
    let pkg, qualified =
      match scope with
      | Global -> (name p, false)
      | Self -> (name p, true)
      | Package n -> (OpamPackage.Name.to_string n, true)
    in
    let target =
      List.find_opt (fun q -> name q = pkg) solution.Solve.packages
    in
    let present = List.mem pkg installed in
    let str v = Some (OpamTypes.S v) and bool v = Some (OpamTypes.B v) in
    let package_value () =
      match var with
      | "name" -> str pkg
      | "version" -> Option.bind target (fun q -> str (version q))
      | "installed" -> bool present
      | "enable" -> str (if present then "enable" else "disable")
      | "pinned" | "dev" -> bool false
      | "build" -> str build_dir
      | "opamfile" -> str (p.directory / "opam")
      | "depends" ->
          str
            (Solve.dependencies solution p
            |> List.map (fun q -> OpamPackage.to_string q.Solve.id)
            |> String.concat " ")
      | _ -> (
          match config pkg var with
          | Some _ as x -> x
          | None
            when pkg = "ocaml"
                 && List.mem var [ "native"; "native-tools"; "native-dynlink" ]
            ->
              bool true
          | None -> (
              match path_var ~prefix ~name:pkg ~qualified var with
              | Some path -> str path
              | None -> Option.bind config_var (fun f -> f pkg var)))
    in
    if qualified then package_value ()
    else
      match var with
      | "jobs" -> str (string_of_int jobs)
      | "make" -> str "make"
      | "exe" -> str ""
      | "with-test" -> bool test
      | "with-doc" | "with-dev-setup" -> bool false
      | "build" | "post" -> if var = "build" then str build_dir else bool false
      | "root" -> str prefix
      | _ -> (
          match Solve.platform_value solution.platform var with
          | Some _ as x -> x
          | None -> package_value ())

let system_path () =
  getenv "PATH" "/usr/bin:/bin"
  |> String.split_on_char ':'
  |> List.filter (fun p ->
         p <> "" && not (exists (Filename.dirname p / ".opam-switch")))
  |> String.concat ":"

let environment ~prefix =
  let vars =
    [
      ("PATH", (prefix / "bin") ^ ":" ^ system_path ());
      ("OCAMLPATH", prefix / "lib");
      ("OCAMLFIND_DESTDIR", prefix / "lib");
      ("OCAMLFIND_LDCONF", "ignore");
      ("OCAMLTOP_INCLUDE_PATH", prefix / "lib/toplevel");
      ("OCAML_TOPLEVEL_PATH", prefix / "lib/toplevel");
      ( "CAML_LD_LIBRARY_PATH",
        (prefix / "lib/stublibs") ^ ":" ^ (prefix / "lib/ocaml/stublibs") );
      ("CDPATH", "");
      ("MAKEFLAGS", "");
      ("MAKELEVEL", "");
    ]
  in
  let vars =
    if exists (prefix / "lib/ocaml/stdlib.cmi") then
      ("OCAMLLIB", prefix / "lib/ocaml") :: vars
    else vars
  in
  let allowed =
    [
      "HOME";
      "TMPDIR";
      "TMP";
      "TEMP";
      "LANG";
      "LC_ALL";
      "PATH";
      "CC";
      "CXX";
      "CFLAGS";
      "CXXFLAGS";
      "CPPFLAGS";
      "LDFLAGS";
      "LIBRARY_PATH";
      "CPATH";
      "C_INCLUDE_PATH";
      "CPLUS_INCLUDE_PATH";
      "PKG_CONFIG_PATH";
      "PKG_CONFIG_LIBDIR";
      "SDKROOT";
      "MACOSX_DEPLOYMENT_TARGET";
      "OCAMLPARAM";
    ]
  in
  let env =
    clean_env () |> Array.to_list
    |> List.filter (fun s ->
           List.exists (fun k -> String.starts_with ~prefix:(k ^ "=") s) allowed)
    |> Array.of_list
  in
  replace_env env vars

let apply_env resolve env updates =
  List.fold_left
    (fun env
         (update :
           ( OpamTypes.spf_unresolved,
             OpamTypes.euok_writeable )
           OpamTypes.env_update) ->
      let value =
        OpamFilter.expand_string resolve update.OpamTypes.envu_value
      in
      let old =
        List.assoc_opt update.envu_var (env_bindings env)
        |> Option.value ~default:""
      in
      let join a b = if a = "" then b else if b = "" then a else a ^ ":" ^ b in
      let value =
        match update.envu_op with
        | OpamTypes.Eq -> value
        | PlusEq | ColonEq | EqPlusEq -> join value old
        | EqPlus | EqColon -> join old value
      in
      replace_env env [ (update.envu_var, value) ])
    env updates

let package_environment ?base_env ?config_var ~solution ~installed ~prefix
    ~build_dir ~jobs () =
  List.fold_left
    (fun env q ->
      if List.mem (name q) installed then
        apply_env
          (resolver ?config_var ~solution ~installed ~prefix ~build_dir ~jobs q)
          env
          (OpamFile.OPAM.env q.Solve.opam)
      else env)
    (Option.value base_env ~default:(environment ~prefix))
    solution.Solve.packages

let build_environment ?(test = false) ?base_env ?config_var ~solution ~installed
    ~prefix ~build_dir ~jobs p =
  let env =
    package_environment ?base_env ?config_var ~solution ~installed ~prefix
      ~build_dir ~jobs ()
  in
  let env =
    apply_env
      (resolver ~test ?config_var ~solution ~installed ~prefix ~build_dir ~jobs
         p)
      env
      (OpamFile.OPAM.build_env p.Solve.opam)
  in
  replace_env env
    [
      ("OPAM_PACKAGE_NAME", name p);
      ("OPAM_PACKAGE_VERSION", version p);
      ("OPAMCLI", "2.0");
    ]

let shell commands =
  commands
  |> List.map (fun args -> String.concat " " (List.map Filename.quote args))
  |> String.concat "\n"

let runtime_environment ~solution ~prefix ~jobs =
  let installed = List.map name solution.Solve.packages in
  package_environment ~solution ~installed ~prefix ~build_dir:prefix ~jobs ()

let compile ?(test = false) ?base_env ?config_var ~solution ~installed ~jobs p
    ~prefix ~build_dir (node : D10ir.Plan.node) =
  let resolve =
    resolver ~test ?config_var ~solution ~installed ~prefix ~build_dir ~jobs p
  in
  let env =
    build_environment ~test ?base_env ?config_var ~solution ~installed ~prefix
      ~build_dir ~jobs p
  in
  let substs =
    OpamFile.OPAM.substs p.opam
    |> List.map (fun b -> Source.safe_relative (OpamFilename.Base.to_string b))
  in
  let patches =
    OpamFile.OPAM.patches p.opam
    |> List.filter_map (fun (file, condition) ->
           if OpamFilter.opt_eval_to_bool resolve condition then
             Some
               [
                 "patch";
                 "-p1";
                 "-i";
                 Source.safe_relative (OpamFilename.Base.to_string file);
               ]
           else None)
  in
  let commands =
    patches
    @ OpamFilter.commands resolve
        (OpamFile.OPAM.build p.opam
        @ (if test then OpamFile.OPAM.run_test p.opam else [])
        @ OpamFile.OPAM.install p.opam)
  in
  let conf_name = name p ^ ".config" in
  let config_dir = prefix / ".ox/config" in
  let script =
    shell commands ^ "\nif test -f " ^ Filename.quote conf_name ^ "; then\n"
    ^ shell
        [
          [ "mkdir"; "-p"; config_dir ];
          [ "cp"; conf_name; config_dir / conf_name ];
        ]
    ^ "\nfi\n"
  in
  { node with script; env = Array.to_list env; substs }

let prepare ?(test = false) ~solution ~installed ~jobs p ~prefix ~build_dir node
    =
  let node =
    compile ~test ~solution ~installed ~jobs p ~prefix ~build_dir node
  in
  let resolve =
    resolver ~test ~solution ~installed ~prefix ~build_dir ~jobs p
  in
  List.iter
    (fun base ->
      OpamFilter.expand_interpolations_in_file_full resolve
        ~src:(OpamFilename.raw (build_dir / (base ^ ".in")))
        ~dst:(OpamFilename.raw (build_dir / base)))
    node.substs;
  { node with substs = [] }

let export ~solution ~installed ~jobs ~source_dir p node =
  let prefix = node.D10ir.Plan.prefix in
  let build_dir = prefix / ".build" in
  let base_env =
    [|
      "PATH=" ^ prefix ^ "/bin:/usr/local/bin:/usr/bin:/bin";
      "OCAMLPATH=" ^ prefix ^ "/lib";
      "OCAMLFIND_DESTDIR=" ^ prefix ^ "/lib";
      "CAML_LD_LIBRARY_PATH=" ^ prefix ^ "/lib/stublibs:" ^ prefix
      ^ "/lib/ocaml/stublibs";
      "OCAMLFIND_LDCONF=ignore";
    |]
  in
  let bindings = Hashtbl.create 8 in
  let config_var pkg variable =
    if List.exists (fun p -> name p = pkg) solution.Solve.packages then (
      let token = "D10_CONFIG_" ^ hash_fields [ pkg; variable ] in
      Hashtbl.replace bindings token (".ox/config/" ^ pkg ^ ".config", variable);
      Some (OpamTypes.S token))
    else None
  in
  let p = { p with Solve.directory = prefix / ".opam-dir" } in
  let resolve =
    resolver ~config_var ~solution ~installed ~prefix ~build_dir ~jobs p
  in
  let static_resolve =
    resolver ~solution ~installed ~prefix ~build_dir ~jobs p
  in
  let check_filter = function
    | None -> ()
    | Some f ->
        List.iter
          (fun v ->
            if static_resolve v = None then
              fail "Cannot export a filter depending on generated variable %s"
                (OpamVariable.Full.to_string v))
          (OpamFilter.variables f)
  in
  (* OpamFilter.commands substitutes undefined variables with empty strings.
     A standalone export must reject those while the user can fix the recipe. *)
  List.iter
    (fun (args, condition) ->
      check_filter condition;
      if OpamFilter.opt_eval_to_bool resolve condition then
        List.iter
          (fun (arg, condition) ->
            check_filter condition;
            if OpamFilter.opt_eval_to_bool resolve condition then
              match arg with
              | OpamTypes.CString s ->
                  ignore (OpamFilter.expand_string resolve s)
              | CIdent s ->
                  ignore
                    (OpamFilter.ident_value resolve
                       (OpamFilter.ident_of_string s)))
          args)
    (OpamFile.OPAM.build p.opam @ OpamFile.OPAM.install p.opam);
  List.iter
    (fun (_, filter) -> check_filter filter)
    (OpamFile.OPAM.patches p.opam);
  let node =
    compile ~base_env ~config_var ~solution ~installed ~jobs p ~prefix
      ~build_dir node
  in
  List.iter
    (fun base ->
      let input = source_dir / (base ^ ".in") in
      let output = input ^ ".ox-subst" in
      OpamFilter.expand_interpolations_in_file_full resolve
        ~src:(OpamFilename.raw input) ~dst:(OpamFilename.raw output);
      Unix.rename output input)
    node.substs;
  let bindings =
    Hashtbl.fold
      (fun token (file, variable) xs -> (token, file, variable) :: xs)
      bindings []
    |> List.sort compare
  in
  (node, bindings)
