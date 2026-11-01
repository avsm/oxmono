[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

(* -- Assembly ------------------------------------------------------------ *)

let native p = Eio.Path.native_exn p

let assemble (c : Config.t) ~layer_hashes ~dst =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  List.iter
    (fun hash -> Layer.restore c ~hash ~prefix:(native dst))
    layer_hashes

let path (c : Config.t) ~hash = native c.root / "prefixes" / c.os_key / hash
let marker ~fs prefix = Eio.Path.(fs / prefix / ".ready")

let ready ~fs ~key prefix =
  let file = marker ~fs prefix in
  Sysops.file_exists file && Eio.Path.load file = key

let mark_ready ~fs ~key prefix =
  let file = marker ~fs prefix in
  let tmp = Eio.Path.(fs / prefix / ".ready.tmp") in
  Eio.Path.save ~create:(`Or_truncate 0o600) tmp key;
  Eio.Path.rename tmp file

let prepare (c : Config.t) ~layer_hashes ~dst =
  Eio.Path.rmtree ~missing_ok:true dst;
  assemble c ~layer_hashes ~dst;
  (* Layer restoration rewrites metadata at its final path. Detach the
     resulting hardlinks before any installer can mutate cached files. *)
  let detached = Eio.Path.(c.fs / (native_exn dst ^ ".copy")) in
  Eio.Path.rmtree ~missing_ok:true detached;
  Sysops.copy_tree c.sys ~src:dst ~dst:detached;
  Eio.Path.rmtree dst;
  Eio.Path.rename detached dst;
  List.iter
    (fun sub ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(dst / sub))
    [ "bin"; "lib"; "share"; "etc"; "doc"; "man"; "sbin" ]

let ensure (c : Config.t) ~key ~layer_hashes ~dst =
  if not (ready ~fs:c.fs ~key (native dst)) then (
    prepare c ~layer_hashes ~dst;
    mark_ready ~fs:c.fs ~key (native dst))

let closure c hashes =
  let states = Hashtbl.create 32 in
  let result = ref [] in
  let rec visit hash =
    match Hashtbl.find_opt states hash with
    | Some `Done -> ()
    | Some `Active -> Fmt.failwith "Layer dependency cycle at %s" hash
    | None ->
        Hashtbl.add states hash `Active;
        (match Layer.load_meta (Layer.json_path c ~hash) with
        | Some { exit_status = 0; hashes; _ } -> List.iter visit hashes
        | _ -> Fmt.failwith "Missing day10 layer %s" hash);
        Hashtbl.replace states hash `Done;
        result := hash :: !result
  in
  List.iter visit hashes;
  List.rev !result

let restore (c : Config.t) ~hash =
  let layers = closure c [ hash ] in
  List.iter
    (fun hash ->
      let dst = path c ~hash in
      if not (ready ~fs:c.fs ~key:hash dst) then
        ensure c ~key:hash ~layer_hashes:(closure c [ hash ])
          ~dst:Eio.Path.(c.fs / dst))
    layers

let solve_hash hashes =
  (* Overlay order affects the resulting files. Keep it in the cache key. *)
  Digest.string (String.concat "\n" ("ordered-v1" :: hashes)) |> Digest.to_hex

let assemble_cached (c : Config.t) ~layer_hashes =
  let hash = solve_hash layer_hashes in
  let dst = path c ~hash in
  ensure c ~key:hash ~layer_hashes ~dst:Eio.Path.(c.fs / dst);
  dst

(* -- Prefix diff --------------------------------------------------------- *)

type entry = File of int * string | Link of string
type snapshot = (string, entry) Hashtbl.t

let snapshot ~fs prefix =
  let root = native Eio.Path.(fs / prefix) in
  let entries = Hashtbl.create 4096 in
  let rec walk rel =
    let path = if rel = "" then root else root / rel in
    let st = Unix.lstat path in
    match st.Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun n -> walk (if rel = "" then n else rel / n))
    | Unix.S_REG ->
        let hash = OpamHash.compute ~kind:`SHA256 path |> OpamHash.contents in
        Hashtbl.add entries rel (File (st.st_perm, hash))
    | Unix.S_LNK -> Hashtbl.add entries rel (Link (Unix.readlink path))
    | _ -> Fmt.failwith "Unsupported installed file: %s" path
  in
  walk "";
  Hashtbl.remove entries ".ready";
  entries

let diff ~fs ~prefix ~before =
  let after = snapshot ~fs prefix in
  Hashtbl.iter
    (fun path _ ->
      if not (Hashtbl.mem after path) then
        Fmt.failwith "Package deleted a dependency file: %s" path)
    before;
  Hashtbl.fold
    (fun path entry acc ->
      if Hashtbl.find_opt before path = Some entry then acc
      else (path, prefix / path) :: acc)
    after []
  |> List.sort compare
