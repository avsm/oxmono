(* Repository context adapted from oi ca8c59ff, ISC. *)
open Support

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

let package_env platform id v =
  match OpamVariable.Full.to_string v with
  | "version" ->
      Some
        (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version id)))
  | "build" | "post" | "with-test" | "with-doc" | "dev" | "with-dev-setup" ->
      None
  | v -> platform_value platform v

let dependency_formula platform id ~post formula =
  OpamFilter.partial_filter_formula (package_env platform id) formula
  |> OpamFilter.filter_deps ~build:true ~post ~test:false ~doc:false ~dev:false
       ~dev_setup:false ~default:false

module Dir_context = struct
  type rejection = User_constraint of OpamFormula.atom

  type t = {
    platform : Osrel.t;
    packages_dirs : string list;
    constraints : OpamFormula.version_constraint OpamTypes.name_map;
  }

  let directory t id =
    List.find_map
      (fun root ->
        let path =
          root
          / OpamPackage.Name.to_string (OpamPackage.name id)
          / OpamPackage.to_string id
        in
        if exists (path / "opam") then Some path else None)
      t.packages_dirs

  let user_restrictions t name =
    OpamPackage.Name.Map.find_opt name t.constraints

  let filter_deps t pkg = dependency_formula t.platform pkg ~post:true

  let version_compare (v1, avoid1, _) (v2, avoid2, _) =
    match Bool.compare avoid1 avoid2 with
    | 0 -> OpamPackage.Version.compare v2 v1
    | n -> n

  let candidates t name =
    let versions =
      List.concat_map
        (fun root ->
          let dir = root / OpamPackage.Name.to_string name in
          if exists dir then sorted_dir dir else [])
        t.packages_dirs
      |> List.sort_uniq String.compare
    in
    let raw =
      versions
      |> List.filter_map (fun nv ->
             match OpamPackage.of_string_opt nv with
             | Some id when OpamPackage.Name.equal (OpamPackage.name id) name ->
                 Option.bind (directory t id) (fun dir ->
                     let opam = read_opam (dir / "opam") in
                     if
                       OpamFilter.eval_to_bool ~default:false
                         (package_env t.platform id)
                         (OpamFile.OPAM.available opam)
                     then
                       Some
                         ( OpamPackage.version id,
                           OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam,
                           opam )
                     else None)
             | _ -> None)
    in
    let raw =
      if List.for_all (fun (_, avoid, _) -> avoid) raw then [] else raw
    in
    raw |> List.sort version_compare
    |> List.map (fun (v, _, opam) ->
           match user_restrictions t name with
           | Some test
             when not
                    (OpamFormula.check_version_formula (OpamFormula.Atom test) v)
             ->
               (v, Error (User_constraint (name, Some test)))
           | _ -> (v, Ok opam))

  let pp_rejection f = function
    | User_constraint x ->
        Fmt.pf f "Rejected by user-specified constraint %s"
          (OpamFormula.string_of_atom x)
end

module Inst = Opam_0install.Solver.Make (Dir_context)

let dependencies t p =
  let formula =
    dependency_formula t.platform p.id ~post:false
      (OpamFile.OPAM.depends p.opam)
  in
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
      (dependency_formula t.platform p.id ~post:false
         (OpamFile.OPAM.depopts p.opam))
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
  let ctx : Dir_context.t = { constraints; platform; packages_dirs } in
  let ids =
    match Inst.solve ctx names with
    | Ok sels -> Inst.packages_of_result sels
    | Error e -> fail "%s" (Inst.diagnostics e)
  in
  let packages =
    List.map
      (fun id ->
        let directory = Option.get (Dir_context.directory ctx id) in
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
