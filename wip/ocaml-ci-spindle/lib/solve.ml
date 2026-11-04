(* SPDX-License-Identifier: ISC *)
(* Repository context adapted from wip/ox/lib/solve.ml, from oi ca8c59ff. *)

type platform = {
  os : string;
  os_family : string;
  os_distribution : string;
  os_version : string;
  arch : string;
  opam_version : string;
}

type request = {
  platform : platform;
  repositories : string list;
  pins : (string * string) list;
  compiler : string * string;
  requires : string list;
}

let ( / ) = Filename.concat
let exists path = Sys.file_exists path

let sorted_dir dir =
  match Sys.readdir dir with
  | entries ->
      let l = Array.to_list entries in
      List.sort String.compare l
  | exception Sys_error _ -> []

let read_opam path =
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string path))

let platform_value p = function
  | "os" -> Some (OpamTypes.S p.os)
  | "os-family" -> Some (OpamTypes.S p.os_family)
  | "os-distribution" -> Some (OpamTypes.S p.os_distribution)
  | "os-version" -> Some (OpamTypes.S p.os_version)
  | "arch" -> Some (OpamTypes.S p.arch)
  | "opam-version" -> Some (OpamTypes.S p.opam_version)
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

module Context = struct
  type rejection = User_constraint of OpamFormula.atom | Unavailable

  type t = {
    platform : platform;
    repositories : string list;
    pins : OpamFile.OPAM.t OpamPackage.Name.Map.t;
    constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  }

  let dev = OpamPackage.Version.of_string "dev"

  let directory t id =
    List.find_map
      (fun root ->
        let path =
          root
          / OpamPackage.Name.to_string (OpamPackage.name id)
          / OpamPackage.to_string id
        in
        if exists (path / "opam") then Some path else None)
      t.repositories

  let user_restrictions t name =
    OpamPackage.Name.Map.find_opt name t.constraints

  (* The project's own packages are tested, so their test dependencies are
     needed. Others are installed without theirs. *)
  let filter_deps t pkg formula =
    let test = OpamPackage.Name.Map.mem (OpamPackage.name pkg) t.pins in
    OpamFilter.partial_filter_formula (package_env t.platform pkg) formula
    |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev:false
         ~dev_setup:false ~default:false

  let available t id opam =
    OpamFilter.eval_to_bool ~default:false
      (package_env t.platform id)
      (OpamFile.OPAM.available opam)

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.pins with
    | Some opam -> [ (dev, Ok opam) ]
    | None ->
        let versions =
          List.concat_map
            (fun root -> sorted_dir (root / OpamPackage.Name.to_string name))
            t.repositories
          |> List.sort_uniq String.compare
          |> List.filter_map (fun nv ->
              match OpamPackage.of_string_opt nv with
              | Some id when OpamPackage.Name.equal (OpamPackage.name id) name
                ->
                  Some id
              | _ -> None)
          |> List.sort (fun a b ->
              OpamPackage.Version.compare (OpamPackage.version b)
                (OpamPackage.version a))
        in
        (* Versions flagged [avoid-version], such as previews, come after the
           rest, as opam orders them. *)
        List.filter_map
          (fun id ->
            Option.map
              (fun dir ->
                let v = OpamPackage.version id in
                let opam = read_opam (dir / "opam") in
                let avoid = OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam in
                if not (available t id opam) then (avoid, (v, Error Unavailable))
                else
                  match user_restrictions t name with
                  | Some c
                    when not
                           (OpamFormula.check_version_formula
                              (OpamFormula.Atom c) v) ->
                      (avoid, (v, Error (User_constraint (name, Some c))))
                  | _ -> (avoid, (v, Ok opam)))
              (directory t id))
          versions
        |> List.stable_sort (fun (a, _) (b, _) -> Bool.compare a b)
        |> List.map snd

  let pp_rejection f = function
    | User_constraint x ->
        Fmt.pf f "Rejected by the compiler constraint %s"
          (OpamFormula.string_of_atom x)
    | Unavailable -> Fmt.string f "Unavailable on this platform"
end

module Solver = Opam_0install.Solver.Make (Context)

let solve r =
  match
    let pins =
      List.fold_left
        (fun m (name, text) ->
          let opam =
            OpamFile.OPAM.read_from_string text
            |> OpamFile.OPAM.with_name (OpamPackage.Name.of_string name)
            |> OpamFile.OPAM.with_version Context.dev
          in
          OpamPackage.Name.Map.add (OpamPackage.Name.of_string name) opam m)
        OpamPackage.Name.Map.empty r.pins
    in
    let compiler, version = r.compiler in
    let compiler = OpamPackage.Name.of_string compiler in
    let constraints =
      OpamPackage.Name.Map.singleton compiler
        (`Eq, OpamPackage.Version.of_string version)
    in
    let ctx =
      {
        Context.platform = r.platform;
        repositories = r.repositories;
        pins;
        constraints;
      }
    in
    let roots =
      compiler
      :: List.map OpamPackage.Name.of_string (r.requires @ List.map fst r.pins)
    in
    match Solver.solve ctx roots with
    | Ok selections ->
        Ok
          (Solver.packages_of_result selections
          |> List.map OpamPackage.to_string
          |> List.sort String.compare)
    | Error d -> Error (Solver.diagnostics d)
  with
  | result -> result
  | exception (Failure m | Invalid_argument m) -> Error m
  | exception OpamPp.Bad_format (_, m) -> Error ("bad opam file: " ^ m)
  | exception ((Out_of_memory | Stack_overflow) as e) -> raise e
  | exception e -> Error ("bad opam file: " ^ Printexc.to_string e)

(* A project package's dependencies, with its test ones, as the solve reads
   them. *)
let depends text =
  OpamFile.OPAM.read_from_string text
  |> OpamFile.OPAM.depends
  |> OpamFilter.filter_deps ~build:true ~post:false ~test:true ~doc:false
       ~dev:false ~default:false

let direct pins =
  let own = List.map fst pins in
  List.concat_map
    (fun (_, text) ->
      match depends text with
      | f ->
          List.map
            (fun (n, _) -> OpamPackage.Name.to_string n)
            (OpamFormula.atoms f)
      | exception _ -> [])
    pins
  |> List.filter (fun n -> not (List.mem n own))
  |> List.sort_uniq String.compare

(* The first reason the solver gives for each package it could not choose,
   from lines such as [- ocaml-compiler -> (problem)] and those indented
   beneath them. *)
let problems diagnostics =
  let lines = List.map String.trim (String.split_on_char '\n' diagnostics) in
  let rec go acc = function
    | [] -> List.rev acc
    | l :: rest
      when String.starts_with ~prefix:"- " l
           && String.ends_with ~suffix:"-> (problem)" l ->
        let name =
          String.trim
            (String.sub l 2
               (String.length l - 2 - String.length "-> (problem)"))
        in
        let rec reason = function
          | r :: rest when r = "" || r = "Rejected candidates:" -> reason rest
          | r :: _ when not (String.starts_with ~prefix:"- " r) -> Some r
          | _ -> None
        in
        go
          (match reason rest with Some r -> (name, r) :: acc | None -> acc)
          rest
    | _ :: rest -> go acc rest
  in
  go [] lines

let explain r diagnostics =
  let cname, cver = r.compiler in
  let compiler =
    OpamPackage.create
      (OpamPackage.Name.of_string cname)
      (OpamPackage.Version.of_string cver)
  in
  (* The commonest reason is the project's own bound on the compiler. *)
  let bound =
    List.find_map
      (fun (name, text) ->
        match depends text with
        | f ->
            List.find_map
              (fun ((n, _) as atom) ->
                if
                  OpamPackage.Name.to_string n = cname
                  && not (OpamFormula.check atom compiler)
                then
                  let bound =
                    match atom with
                    | n, Some (op, v) ->
                        Printf.sprintf "%s %s %s"
                          (OpamPackage.Name.to_string n)
                          (match op with
                          | `Eq -> "="
                          | `Neq -> "!="
                          | `Geq -> ">="
                          | `Gt -> ">"
                          | `Leq -> "<="
                          | `Lt -> "<")
                          (OpamPackage.Version.to_string v)
                    | _ -> OpamFormula.short_string_of_atom atom
                  in
                  Some (Printf.sprintf "%s requires %s" name bound)
                else None)
              (OpamFormula.atoms f)
        | exception _ -> None)
      r.pins
  in
  match bound with
  | Some why -> why
  | None -> (
      match problems diagnostics with
      | [] -> (
          match
            List.filter (( <> ) "") (String.split_on_char '\n' diagnostics)
          with
          | first :: _ -> String.trim first
          | [] -> "no solution")
      | ps ->
          String.concat "; "
            (List.map
               (fun (name, why) -> name ^ ": " ^ why)
               (List.filteri (fun i _ -> i < 3) ps)))

let versions packages name =
  sorted_dir (packages / name)
  |> List.filter_map (fun nv ->
      match OpamPackage.of_string_opt nv with
      | Some id
        when OpamPackage.Name.to_string (OpamPackage.name id) = name
             && not
                  (String.contains
                     (OpamPackage.Version.to_string (OpamPackage.version id))
                     '~') ->
          Some (OpamPackage.version id)
      | _ -> None)
  |> List.sort (fun a b -> OpamPackage.Version.compare b a)
  |> List.map OpamPackage.Version.to_string
