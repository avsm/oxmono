(* Repository context adapted from oi ca8c59ff, ISC. *)
open Support

module Dir_context = struct
  type rejection = User_constraint of OpamFormula.atom

  let with_dir path fn =
    let ch = Unix.opendir path in
    Fun.protect ~finally:(fun () -> Unix.closedir ch) (fun () -> fn ch)

  let list_dir path =
    let rec aux acc ch =
      match Unix.readdir ch with
      | name when name.[0] <> '.' -> aux (name :: acc) ch
      | _ -> aux acc ch
      | exception End_of_file -> acc
    in
    with_dir path (aux [])

  type t = {
    env : string -> OpamVariable.variable_contents option;
    packages_dirs : string list;
    pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
    constraints : OpamFormula.version_constraint OpamTypes.name_map;
    test : OpamPackage.Name.Set.t;
    doc : OpamPackage.Name.Set.t;
    prefer_oldest : bool;
  }

  let load t pkg =
    let { OpamPackage.name; version = _ } = pkg in
    let raw =
      match OpamPackage.Name.Map.find_opt name t.pins with
      | Some (_, opam) -> opam
      | None ->
          List.find_map
            (fun packages_dir ->
              let opam =
                packages_dir
                / OpamPackage.Name.to_string name
                / OpamPackage.to_string pkg / "opam"
              in
              if Sys.file_exists opam then Some opam else None)
            t.packages_dirs
          |> Option.get |> OpamFilename.raw |> OpamFile.make
          |> OpamFile.OPAM.read
    in
    raw

  let user_restrictions t name =
    OpamPackage.Name.Map.find_opt name t.constraints

  let dev = OpamPackage.Version.of_string "dev"

  let env t pkg v =
    if
      List.mem v
        (List.map OpamVariable.Full.of_string
           [ "build"; "post"; "with-test"; "with-doc"; "with-dev-setup"; "dev" ])
    then None
    else
      match OpamVariable.Full.to_string v with
      | "version" ->
          Some
            (OpamTypes.S
               (OpamPackage.Version.to_string (OpamPackage.version pkg)))
      | x -> t.env x

  let filter_deps t pkg f =
    let dev = OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
    let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
    let doc = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.doc in
    f
    |> OpamFilter.partial_filter_formula (env t pkg)
    |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc ~dev
         ~dev_setup:false ~default:false

  let version_compare t (v1, v1_avoid, _) (v2, v2_avoid, _) =
    match (v1_avoid, v2_avoid) with
    | true, true | false, false ->
        if t.prefer_oldest then OpamPackage.Version.compare v1 v2
        else OpamPackage.Version.compare v2 v1
    | true, false -> 1
    | false, true -> -1

  let version_dir_exists t ~name dir =
    List.exists
      (fun packages_dir ->
        Sys.file_exists
          (packages_dir / OpamPackage.Name.to_string name / dir / "opam"))
      t.packages_dirs

  let version_of_dir t ~name dir =
    match OpamPackage.of_string_opt dir with
    | Some pkg when version_dir_exists t ~name dir ->
        Some (OpamPackage.version pkg)
    | _ -> None

  let load_candidate t name v =
    let pkg = OpamPackage.create name v in
    let opam = load t pkg in
    let avoid = OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam in
    let available = OpamFile.OPAM.available opam in
    if OpamFilter.eval_to_bool ~default:false (env t pkg) available then
      Some (v, avoid, opam)
    else None

  let apply_user_constraint name user_constraints (v, _, opam) =
    match user_constraints with
    | Some test
      when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) ->
        (v, Error (User_constraint (name, Some test)))
    | _ -> (v, Ok opam)

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.pins with
    | Some (version, opam) -> [ (version, Ok opam) ]
    | None ->
        let versions =
          List.concat_map
            (fun packages_dir ->
              try packages_dir / OpamPackage.Name.to_string name |> list_dir
              with Unix.Unix_error (Unix.ENOENT, _, _) -> [])
            t.packages_dirs
          |> List.sort_uniq compare
        in
        let user_constraints = user_restrictions t name in
        let raw =
          versions
          |> List.filter_map (version_of_dir t ~name)
          |> List.filter_map (load_candidate t name)
        in
        let filtered =
          if List.for_all (fun (_, avoid, _) -> avoid) raw then [] else raw
        in
        filtered
        |> List.sort (version_compare t)
        |> List.map (apply_user_constraint name user_constraints)

  let pp_rejection f = function
    | User_constraint x ->
        Fmt.pf f "Rejected by user-specified constraint %s"
          (OpamFormula.string_of_atom x)

  let create ?(prefer_oldest = false) ?(test = OpamPackage.Name.Set.empty)
      ?(doc = OpamPackage.Name.Set.empty) ?(pins = OpamPackage.Name.Map.empty)
      ~constraints ~env packages_dirs =
    { env; packages_dirs; pins; constraints; test; doc; prefer_oldest }
end

module Inst = Opam_0install.Solver.Make (Dir_context)

type package = {
  id : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  directory : string;
}

type t = { packages : package list; platform : Osrel.t }

let platform_value p = function
  | "os" -> Some (OpamTypes.S (Osrel.OS.to_string p.Osrel.os))
  | "os-family" -> Some (OpamTypes.S p.os.family)
  | "os-distribution" -> Some (OpamTypes.S (Osrel.OS.kind_to_string p.os.kind))
  | "os-version" -> Some (OpamTypes.S p.os.version)
  | "arch" -> Some (OpamTypes.S (Osrel.Arch.to_string p.arch))
  | "opam-version" -> Some (OpamTypes.S "2.5.2")
  | "ocaml:native" -> Some (OpamTypes.B true)
  | _ -> None

let dependency_formula platform p ~post =
  let env v =
    match OpamVariable.Full.to_string v with
    | "version" ->
        Some
          (OpamTypes.S
             (OpamPackage.Version.to_string (OpamPackage.version p.id)))
    | "build" | "post" | "with-test" | "with-doc" | "dev" | "with-dev-setup" ->
        None
    | v -> platform_value platform v
  in
  OpamFilter.partial_filter_formula env (OpamFile.OPAM.depends p.opam)
  |> OpamFilter.filter_deps ~build:true ~post ~test:false ~doc:false ~dev:false
       ~dev_setup:false ~default:false

let dependencies t p =
  let formula = dependency_formula t.platform p ~post:false in
  let selected name constraint_ =
    List.find_opt
      (fun q ->
        OpamPackage.Name.equal name (OpamPackage.name q.id)
        && OpamFormula.check_version_formula constraint_
             (OpamPackage.version q.id))
      t.packages
  in
  let rec chosen = function
    | OpamFormula.Empty -> Some []
    | Atom (name, c) -> Option.map (fun p -> [ p ]) (selected name c)
    | Block f -> chosen f
    | And (a, b) -> (
        match (chosen a, chosen b) with
        | Some a, Some b -> Some (a @ b)
        | _ -> None)
    | Or (a, b) -> ( match chosen a with Some _ as x -> x | None -> chosen b)
  in
  let required =
    match chosen formula with
    | Some deps -> deps
    | None ->
        fail "Unsatisfied build dependencies for %s"
          (OpamPackage.to_string p.id)
  in
  let optional =
    OpamFormula.atoms
      (dependency_formula t.platform
         {
           p with
           opam =
             OpamFile.OPAM.with_depends (OpamFile.OPAM.depopts p.opam) p.opam;
         }
         ~post:false)
    |> List.filter_map (fun (n, _) ->
           List.find_opt
             (fun q -> OpamPackage.Name.equal n (OpamPackage.name q.id))
             t.packages)
  in
  List.sort_uniq (fun a b -> OpamPackage.compare a.id b.id) (required @ optional)

let run ~platform ~repos roots =
  let constraints, names =
    List.fold_left
      (fun (cs, ns) root ->
        let n, c = OpamFormula.atom_of_string root in
        let cs =
          match c with
          | None -> cs
          | Some c -> (
              match OpamPackage.Name.Map.find_opt n cs with
              | Some old when old <> c ->
                  fail "Conflicting root constraints for %s"
                    (OpamPackage.Name.to_string n)
              | _ -> OpamPackage.Name.Map.add n c cs)
        in
        (cs, n :: ns))
      (OpamPackage.Name.Map.empty, [])
      roots
  in
  let packages_dirs =
    List.map (fun r -> r.Repository.path / "packages") repos
  in
  let ctx =
    Dir_context.create ~constraints ~env:(platform_value platform) packages_dirs
  in
  let ids =
    match Inst.solve ctx names with
    | Ok sels -> Inst.packages_of_result sels
    | Error e -> fail "%s" (Inst.diagnostics e)
  in
  let packages =
    List.map
      (fun id ->
        let directory =
          List.find_map
            (fun root ->
              let p =
                root
                / OpamPackage.Name.to_string (OpamPackage.name id)
                / OpamPackage.to_string id
              in
              if exists (p / "opam") then Some p else None)
            packages_dirs
          |> Option.get
        in
        let opam =
          read_opam (directory / "opam")
          |> OpamFile.OPAM.with_name (OpamPackage.name id)
          |> OpamFile.OPAM.with_version (OpamPackage.version id)
        in
        if OpamFile.OPAM.pin_depends opam <> [] then
          fail
            "External pin-depends in %s requires a pinned --overlay definition"
            (OpamPackage.to_string id);
        { id; opam; directory })
      ids
  in
  let t = { packages; platform } in
  let visiting = Hashtbl.create 64 and done_ = Hashtbl.create 64 in
  let ordered = ref [] in
  let rec visit p =
    let key = OpamPackage.to_string p.id in
    if Hashtbl.mem visiting key then fail "Build dependency cycle at %s" key;
    if not (Hashtbl.mem done_ key) then (
      Hashtbl.add visiting key ();
      List.iter visit (dependencies t p);
      Hashtbl.remove visiting key;
      Hashtbl.add done_ key ();
      ordered := p :: !ordered)
  in
  List.iter visit packages;
  { t with packages = List.rev !ordered }
