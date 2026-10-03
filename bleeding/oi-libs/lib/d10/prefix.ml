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

let solve_hash hashes =
  let sorted = List.sort String.compare hashes in
  Digest.string (String.concat "\n" sorted) |> Digest.to_hex

(* Cross-process serialisation comes from {!Oi.Lock.acquire_global} in
   {!Cmd.Harness.bootstrap}; in-process callers go through the d10ir
   scheduler. The [.ready] marker is still the commit point so a
   crashed-mid-assembly prefix dir is re-assembled by the next run. *)
let assemble_cached (c : Config.t) ~layer_hashes =
  let hash = solve_hash layer_hashes in
  let dir = Eio.Path.(c.root / "prefixes" / c.os_key / hash) in
  let ready = Eio.Path.(dir / ".ready") in
  if Sysops.file_exists ready then native dir
  else begin
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir;
    assemble c ~layer_hashes ~dst:dir;
    Eio.Path.save ~create:(`Or_truncate 0o644) ready "";
    native dir
  end

(* -- Prefix diff --------------------------------------------------------- *)

type snapshot = (string, float) Hashtbl.t

let snapshot ~fs prefix =
  let tbl = Hashtbl.create 4096 in
  let base = Eio.Path.(fs / prefix) in
  let record_entry ~rel ~(st : Eio.File.Stat.t) ~scan =
    match st.kind with
    | `Regular_file | `Symbolic_link -> Hashtbl.replace tbl rel st.mtime
    | `Directory -> scan rel
    | _ -> ()
  in
  let process_name ~rel_dir ~dir ~scan name =
    let rel = if rel_dir = "" then name else rel_dir / name in
    try
      let st = Eio.Path.stat ~follow:false Eio.Path.(dir / name) in
      record_entry ~rel ~st ~scan
    with Eio.Exn.Io _ -> ()
  in
  let rec scan rel_dir =
    let dir = if rel_dir = "" then base else Eio.Path.(base / rel_dir) in
    if Sysops.file_exists dir then
      List.iter (process_name ~rel_dir ~dir ~scan) (Eio.Path.read_dir dir)
  in
  scan "";
  tbl

let diff ~fs ~prefix ~(before : snapshot) =
  let results = ref [] in
  let base = Eio.Path.(fs / prefix) in
  let regular_is_new rel (st : Eio.File.Stat.t) =
    match Hashtbl.find_opt before rel with
    | None -> true
    | Some old_mtime -> st.mtime > old_mtime
  in
  let record_entry ~rel ~abs ~(st : Eio.File.Stat.t) ~scan =
    match st.kind with
    | `Regular_file | `Symbolic_link ->
        if regular_is_new rel st then results := (rel, abs) :: !results
    | `Directory -> scan rel
    | _ -> ()
  in
  let process_name ~rel_dir ~dir ~scan name =
    let rel = if rel_dir = "" then name else rel_dir / name in
    let abs = prefix / if rel_dir = "" then name else rel_dir / name in
    try
      let st = Eio.Path.stat ~follow:false Eio.Path.(dir / name) in
      record_entry ~rel ~abs ~st ~scan
    with Eio.Exn.Io _ -> ()
  in
  let rec scan rel_dir =
    let dir = if rel_dir = "" then base else Eio.Path.(base / rel_dir) in
    if Sysops.file_exists dir then
      List.iter (process_name ~rel_dir ~dir ~scan) (Eio.Path.read_dir dir)
  in
  scan "";
  !results
