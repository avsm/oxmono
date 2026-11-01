open Support

let safe_relative path =
  if
    (not (Filename.is_relative path))
    || List.exists (( = ) "..") (String.split_on_char '/' path)
  then fail "Source path escapes its package: %s" path;
  path

let mutable_url url =
  let source = OpamFile.URL.url url in
  match source.OpamUrl.backend with
  | `git -> (
      match source.hash with
      | Some h
        when String.length h = 40
             && String.for_all
                  (function
                    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
                  h ->
          false
      | _ -> true)
  | _ -> OpamFile.URL.checksum url = []

let download ~refresh proc (d10 : D10.Config.t) url =
  let checksums = OpamFile.URL.checksum url in
  let sources = OpamFile.URL.url url :: OpamFile.URL.mirrors url in
  let key =
    hash_fields
      (List.map OpamUrl.to_string sources
      @ List.map OpamHash.to_string checksums)
  in
  let dst = Eio.Path.native_exn d10.root / "sources/downloads" / key in
  let valid p = exists p && List.for_all (OpamHash.check_file p) checksums in
  if exists dst && ((refresh && mutable_url url) || not (valid dst)) then
    Unix.unlink dst;
  if not (exists dst) then (
    mkdir (Filename.dirname dst);
    let tmp = dst ^ ".tmp" in
    let rec fetch = function
      | [] ->
          fail "Could not fetch verified source %s"
            (OpamUrl.to_string (List.hd sources))
      | source :: rest ->
          remove_tree tmp;
          let ok =
            if source.OpamUrl.transport = "file" then
              let path = source.path in
              if exists path then (
                command proc [ "cp"; path; tmp ];
                true)
              else false
            else
              D10.Sysops.Http.fetch d10.sys ~url:(OpamUrl.to_string source)
                ~dst:Eio.Path.(d10.fs / tmp)
          in
          if ok && valid tmp then Unix.rename tmp dst else fetch rest
    in
    Fun.protect ~finally:(fun () -> remove_tree tmp) (fun () -> fetch sources));
  dst

let tree ~refresh proc d10 url ~dst =
  let source = OpamFile.URL.url url in
  match source.OpamUrl.backend with
  | `git ->
      let raw = git_url (OpamUrl.to_string { source with hash = None }) in
      let mirror =
        Eio.Path.native_exn d10.D10.Config.root / "sources/git" / hash raw
      in
      if not (exists mirror) then
        publish_dir mirror (fun tmp ->
            command proc [ "git"; "clone"; "--mirror"; "--"; raw; tmp ]);
      if refresh && mutable_url url then
        command proc [ "git"; "-C"; mirror; "fetch"; "origin" ];
      let revision = Option.value source.hash ~default:"HEAD" in
      let resolve () =
        git proc mirror [ "rev-parse"; "--verify"; revision ^ "^{commit}" ]
      in
      let commit =
        try resolve ()
        with Eio.Exn.Io _ ->
          command proc [ "git"; "-C"; mirror; "fetch"; "origin" ];
          resolve ()
      in
      let tar = dst ^ ".git.tar" in
      Fun.protect
        ~finally:(fun () -> remove_tree tar)
        (fun () ->
          command proc
            [
              "git"; "-C"; mirror; "archive"; "--format=tar"; "-o"; tar; commit;
            ];
          command proc [ "tar"; "-xf"; tar; "-C"; dst ]);
      (* Submodules need their pinned commits, not the branch tips. *)
      if exists (dst / ".gitmodules") then
        fail "Git submodules require an explicit source archive: %s" raw
  | `http | `rsync ->
      let archive = download ~refresh proc d10 url in
      let listing =
        capture proc [ "tar"; "-tf"; archive ]
        |> lines
        |> List.map (fun p ->
               if String.starts_with ~prefix:"./" p then
                 String.sub p 2 (String.length p - 2)
               else p)
      in
      List.iter (fun p -> ignore (safe_relative p)) listing;
      let components =
        listing
        |> List.filter_map (fun p ->
               match String.split_on_char '/' p with
               | name :: _ when name <> "" -> Some name
               | _ -> None)
        |> List.sort_uniq String.compare
      in
      let strip =
        match components with
        | [ root ]
          when List.for_all
                 (fun p -> p = "" || String.starts_with ~prefix:(root ^ "/") p)
                 listing ->
            "1"
        | _ -> "0"
      in
      command proc
        [ "tar"; "-xf"; archive; "-C"; dst; "--strip-components"; strip ]
  | _ -> fail "Unsupported source backend for %s" (OpamUrl.to_string source)

let worktree_files proc dir =
  capture proc
    [
      "git";
      "-C";
      dir;
      "ls-files";
      "-z";
      "--cached";
      "--others";
      "--exclude-standard";
      "--";
      ".";
    ]
  |> nul_lines
  |> List.sort_uniq String.compare
  |> List.filter (fun rel -> exists (dir / rel))

let worktree opam =
  match
    OpamStd.String.Map.find_opt "x-ox-worktree" (OpamFile.OPAM.extensions opam)
  with
  | Some { OpamParserTypes.FullPos.pelem = String path; _ } -> Some path
  | _ -> None

let prepare ~refresh proc (d10 : D10.Config.t) p =
  let local =
    Option.map
      (fun path -> (path, worktree_files proc path))
      (worktree p.Solve.opam)
  in
  let local_key =
    match local with
    | None -> [ "ox-source-v2" ]
    | Some (path, files) ->
        let contents rel =
          let path = path / rel in
          let stat = Unix.lstat path in
          let digest =
            match stat.Unix.st_kind with
            | Unix.S_REG -> hash_file path
            | Unix.S_LNK -> Unix.readlink path
            | _ -> fail "Unsupported working-tree source: %s" path
          in
          [ rel; digest; string_of_int stat.Unix.st_perm ]
        in
        [
          "ox-worktree-source-v1"; hash_fields (List.concat_map contents files);
        ]
  in
  let key =
    hash_fields
      (local_key
      @ [ OpamFile.OPAM.write_to_string p.Solve.opam; tree_hash p.directory ])
  in
  let root = Eio.Path.native_exn d10.root / "sources/prepared" / key in
  let marker = root / ".source-hash" in
  let urls =
    Option.to_list (OpamFile.OPAM.url p.opam)
    @ List.map snd (OpamFile.OPAM.extra_sources p.opam)
  in
  if refresh && List.exists mutable_url urls then remove_tree root;
  if not (exists marker) then (
    remove_tree root;
    mkdir root;
    let unpack = root / "unpack" in
    mkdir unpack;
    (match local with
    | None ->
        Option.iter
          (fun url -> tree ~refresh proc d10 url ~dst:unpack)
          (OpamFile.OPAM.url p.opam)
    | Some (path, files) ->
        List.iter
          (fun rel ->
            ignore (safe_relative rel);
            mkdir (Filename.dirname (unpack / rel));
            command proc [ "cp"; "-pP"; path / rel; unpack / rel ])
          files);
    let subdir =
      match
        if local <> None then None
        else Option.bind (OpamFile.OPAM.url p.opam) OpamFile.URL.subpath
      with
      | None -> unpack
      | Some sub -> unpack / safe_relative (OpamFilename.SubPath.to_string sub)
    in
    if not (exists subdir) then fail "Source subdirectory missing: %s" subdir;
    let src = root / "tree" in
    if subdir = unpack then Unix.rename unpack src
    else (
      command proc [ "cp"; "-R"; subdir; src ];
      remove_tree unpack);
    let files = p.directory / "files" in
    OpamFile.OPAM.extra_files p.opam
    |> Option.value ~default:[]
    |> List.iter (fun (base, checksum) ->
           let path =
             files / safe_relative (OpamFilename.Base.to_string base)
           in
           if not (exists path && OpamHash.check_file path checksum) then
             fail "Repository file failed checksum verification: %s" path);
    if exists files then
      List.iter
        (fun n -> copy_files ~src:(files / n) ~dst:(src / n))
        (sorted_dir files);
    List.iter
      (fun (base, url) ->
        let rel = safe_relative (OpamFilename.Base.to_string base) in
        let file = download ~refresh proc d10 url in
        mkdir (Filename.dirname (src / rel));
        command proc [ "cp"; file; src / rel ])
      (OpamFile.OPAM.extra_sources p.opam);
    atomic_write marker (tree_hash src));
  (root / "tree", read marker)
