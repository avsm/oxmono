open Support

type t = { name : string; path : string; digest : string }

let defaults =
  [
    "git+https://github.com/oxcaml/opam-repository.git";
    "git+https://github.com/ocaml/opam-repository.git";
  ]

let prepare proc ~data ~refresh source =
  let path, digest =
    if exists source then (
      let path = Unix.realpath source in
      if not (Sys.is_directory path && exists (path / "repo")) then
        fail "Not an opam repository: %s" source;
      (path, tree_hash path))
    else
      let url =
        if String.starts_with ~prefix:"git+" source then
          String.sub source 4 (String.length source - 4)
        else source
      in
      let key = hash source in
      let path = data / "repositories" / key in
      if not (exists path) then (
        mkdir (Filename.dirname path);
        let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
        if exists tmp then remove_tree tmp;
        log "Cloning repository %s" url;
        Fun.protect
          ~finally:(fun () -> if exists tmp then remove_tree tmp)
          (fun () ->
            command proc [ "git"; "clone"; "--depth=1"; "--"; url; tmp ];
            if not (exists (tmp / "repo")) then
              fail "No opam repo file in %s" url;
            Unix.rename tmp path));
      if refresh then refresh_checkout proc path;
      if git proc path [ "status"; "--porcelain" ] <> "" then
        fail "Cached repository has local changes: %s. Use --overlay for edits."
          path;
      (path, git proc path [ "rev-parse"; "HEAD" ])
  in
  {
    name = "repo-" ^ String.sub (hash_fields [ source; digest ]) 0 20;
    path;
    digest;
  }

let binaries opam =
  match
    OpamStd.String.Map.find_opt "x-ox-binaries" (OpamFile.OPAM.extensions opam)
  with
  | Some { OpamParserTypes.FullPos.pelem = List { pelem = values; _ }; _ } ->
      List.filter_map
        (function
          | {
              OpamParserTypes.FullPos.pelem = OpamParserTypes.FullPos.String x;
              _;
            } ->
              Some x
          | _ -> None)
        values
  | _ -> []

let snapshot_root repos atom =
  let name, constraint_ = OpamFormula.atom_of_string atom in
  match constraint_ with
  | Some _ -> atom
  | None ->
      let name_s = OpamPackage.Name.to_string name in
      let rec search = function
        | [] -> atom
        | r :: rest -> (
            let dir = r.path / "packages" / name_s in
            let versions =
              if exists dir then
                sorted_dir dir
                |> List.filter_map (fun nv ->
                       let path = dir / nv / "opam" in
                       if not (exists path) then None
                       else
                         let opam = read_opam path in
                         if
                           OpamStd.String.Map.mem "x-ox-commit"
                             (OpamFile.OPAM.extensions opam)
                         then Some (OpamPackage.of_string nv)
                         else None)
              else []
            in
            match List.sort (fun a b -> OpamPackage.compare b a) versions with
            | p :: _ -> OpamPackage.to_string p
            | [] -> search rest)
      in
      search repos

let resolve_binary repos target with_packages =
  let binary, roots =
    if with_packages <> [] then (target, with_packages)
    else
      let name, _ = OpamFormula.atom_of_string target in
      let name_s = OpamPackage.Name.to_string name in
      let package_exists =
        List.exists (fun r -> exists (r.path / "packages" / name_s)) repos
      in
      if package_exists then (name_s, [ target ])
      else
        let seen = Hashtbl.create 64 in
        let owners = ref [] in
        List.iter
          (fun r ->
            let packages = r.path / "packages" in
            if exists packages then
              List.iter
                (fun name ->
                  let dir = packages / name in
                  if Sys.is_directory dir then
                    List.iter
                      (fun nv ->
                        if not (Hashtbl.mem seen nv) then (
                          Hashtbl.add seen nv ();
                          let path = dir / nv / "opam" in
                          if
                            exists path
                            && List.mem target (binaries (read_opam path))
                          then owners := name :: !owners))
                      (sorted_dir dir))
                (sorted_dir packages))
          repos;
        match List.sort_uniq String.compare !owners with
        | [ name ] -> (target, [ name ])
        | [] ->
            fail "No package or binary mapping for %s. Use --with PACKAGE."
              target
        | names ->
            fail "Binary %s is ambiguous (%s). Use --with PACKAGE." target
              (String.concat ", " names)
  in
  (binary, List.map (snapshot_root repos) roots)

let constrain ~data repos =
  let available =
    List.exists
      (fun r -> exists (r.path / "packages/oxcaml-patch-guards"))
      repos
  in
  let locals =
    repos
    |> List.filter (fun r -> exists (r.path / "ox-source"))
    |> List.concat_map (fun r -> sorted_dir (r.path / "packages"))
    |> List.sort_uniq String.compare
  in
  if locals = [] then (available, [], repos)
  else
    let digest =
      hash_fields
        (("ox-constraints-v3" :: locals) @ List.map (fun r -> r.digest) repos)
    in
    let path = data / "guards" / digest in
    if not (exists path) then (
      let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
      if exists tmp then remove_tree tmp;
      Fun.protect
        ~finally:(fun () -> remove_tree tmp)
        (fun () ->
          write (tmp / "repo") "opam-version: \"2.0\"\n";
          let conflicts =
            List.map
              (fun name ->
                let pkg = OpamPackage.of_string (snapshot_root repos name) in
                Printf.sprintf "%S {!= %S}" name
                  (OpamPackage.Version.to_string (OpamPackage.version pkg)))
              locals
          in
          write
            (tmp / "packages/ox-local-snapshot"
            / ("ox-local-snapshot." ^ digest)
            / "opam")
            ("opam-version: \"2.0\"\nconflicts: ["
            ^ String.concat "\n" conflicts
            ^ "]\n");
          let seen = Hashtbl.create 64 in
          let external_only formula =
            OpamFormula.map
              (fun ((name, _) as atom) ->
                if List.mem (OpamPackage.Name.to_string name) locals then
                  OpamFormula.Empty
                else OpamFormula.Atom atom)
              formula
          in
          List.iter
            (fun r ->
              let packages = r.path / "packages" in
              if exists packages then
                sorted_dir packages
                |> List.iter (fun name ->
                       if String.starts_with ~prefix:"oxcaml-" name then
                         sorted_dir (packages / name)
                         |> List.iter (fun nv ->
                                let is_guard =
                                  name = "oxcaml-patch-guards"
                                  || Filename.check_suffix name "-patches"
                                  || Filename.check_suffix nv ".guard"
                                in
                                let src = packages / name / nv / "opam" in
                                if
                                  is_guard
                                  && (not (Hashtbl.mem seen nv))
                                  && exists src
                                then (
                                  Hashtbl.add seen nv ();
                                  let original = read_opam src in
                                  let opam =
                                    original
                                    |> OpamFile.OPAM.with_depends
                                         (external_only
                                            (OpamFile.OPAM.depends original))
                                    |> OpamFile.OPAM.with_conflicts
                                         (external_only
                                            (OpamFile.OPAM.conflicts original))
                                  in
                                  let dst = tmp / "packages" / name / nv in
                                  copy_files ~src:(Filename.dirname src) ~dst;
                                  write_opam (dst / "opam") opam))))
            repos;
          Unix.rename tmp path));
    ( available,
      [ "ox-local-snapshot." ^ digest ],
      { name = "guards-" ^ String.sub digest 0 20; path; digest } :: repos )
