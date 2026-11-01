let err_str fmt = Fmt.kstr (fun s -> Error s) fmt

type package = { name : string; version : string }
type overlay = { handle : string; version : string }

type node = {
  package : package;
  layer_hash : Layer_hash.t;
  dep_layer_hashes : Layer_hash.t list;
  archive : Archive.t;
  script : string;
  env : string list;
  depexts : string list;
  prefix : string;
  substs : string list;
  subst_vars : string list;
  overlay : overlay option;
  opam_file_sha256 : string;
}

type toolchain = { name : string; base_layer : Layer_hash.t }

type metadata = {
  oi_version : string;
  generated_at : float;
  cli_invocation : string list;
}

type mount = {
  name : string;
  source : string;
  target : string;
  mode : [ `Ro | `Rw ];
  env : string list;
}

type t = {
  schema_version : int;
  os_key : string;
  toolchain : toolchain;
  archive_root : string;
  nodes : node list;
  roots : Layer_hash.t list;
  mounts : mount list;
  external_layers : Layer_hash.t list;
  metadata : metadata;
}

let current_schema_version = 1

(* Codecs. *)

let layer_hash_jsont : Layer_hash.t Jsont.t =
  Jsont.map ~kind:"layer_hash" ~dec:Layer_hash.of_string
    ~enc:Layer_hash.to_string Jsont.string

let package_jsont : package Jsont.t =
  let open Jsont in
  Object.map ~kind:"package" (fun name version : package -> { name; version })
  |> Object.mem "name" string ~enc:(fun (p : package) -> p.name)
  |> Object.mem "version" string ~enc:(fun (p : package) -> p.version)
  |> Object.finish

let overlay_jsont : overlay Jsont.t =
  let open Jsont in
  Object.map ~kind:"overlay" (fun handle version : overlay ->
      { handle; version })
  |> Object.mem "handle" string ~enc:(fun (o : overlay) -> o.handle)
  |> Object.mem "version" string
       ~dec_absent:(fun () -> "")
       ~enc:(fun (o : overlay) -> o.version)
  |> Object.finish

let archive_jsont : Archive.t Jsont.t =
  let open Jsont in
  Object.map ~kind:"archive" (fun path sha256 strip_components : Archive.t ->
      { path; sha256; strip_components })
  |> Object.mem "path" string ~enc:(fun (a : Archive.t) -> a.path)
  |> Object.mem "sha256" string ~enc:(fun (a : Archive.t) -> a.sha256)
  |> Object.mem "strip_components" int
       ~dec_absent:(fun () -> 1)
       ~enc:(fun (a : Archive.t) -> a.strip_components)
  |> Object.finish

type node_t = node

let node_jsont : node Jsont.t =
  let open Jsont in
  Object.map ~kind:"node"
    (fun
      package
      layer_hash
      dep_layer_hashes
      archive
      script
      env
      depexts
      prefix
      substs
      subst_vars
      overlay
      opam_file_sha256
      :
      node_t
    ->
      {
        package;
        layer_hash;
        dep_layer_hashes;
        archive;
        script;
        env;
        depexts;
        prefix;
        substs;
        subst_vars;
        overlay;
        opam_file_sha256;
      })
  |> Object.mem "package" package_jsont ~enc:(fun (n : node_t) -> n.package)
  |> Object.mem "layer_hash" layer_hash_jsont ~enc:(fun (n : node_t) ->
         n.layer_hash)
  |> Object.mem "dep_layer_hashes" (list layer_hash_jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (n : node_t) -> n.dep_layer_hashes)
  |> Object.mem "archive" archive_jsont ~enc:(fun (n : node_t) -> n.archive)
  |> Object.mem "script" string ~enc:(fun (n : node_t) -> n.script)
  |> Object.mem "env" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (n : node_t) -> n.env)
  |> Object.mem "depexts" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (n : node_t) -> n.depexts)
  |> Object.mem "prefix" string ~enc:(fun (n : node_t) -> n.prefix)
  |> Object.mem "substs" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (n : node_t) -> n.substs)
  |> Object.mem "subst_vars" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (n : node_t) -> n.subst_vars)
  |> Object.opt_mem "overlay" overlay_jsont ~enc:(fun (n : node_t) -> n.overlay)
  |> Object.mem "opam_file_sha256" string
       ~dec_absent:(fun () -> "")
       ~enc:(fun (n : node_t) -> n.opam_file_sha256)
  |> Object.finish

let toolchain_jsont : toolchain Jsont.t =
  let open Jsont in
  Object.map ~kind:"toolchain" (fun name base_layer : toolchain ->
      { name; base_layer })
  |> Object.mem "name" string ~enc:(fun (t : toolchain) -> t.name)
  |> Object.mem "base_layer" layer_hash_jsont ~enc:(fun (t : toolchain) ->
         t.base_layer)
  |> Object.finish

let mode_jsont : [ `Ro | `Rw ] Jsont.t =
  let dec = function
    | "ro" -> `Ro
    | "rw" -> `Rw
    | s -> failwith ("invalid mount mode: " ^ s)
  in
  let enc = function `Ro -> "ro" | `Rw -> "rw" in
  Jsont.map ~kind:"mount_mode" ~dec ~enc Jsont.string

let mount_jsont : mount Jsont.t =
  let open Jsont in
  Object.map ~kind:"mount" (fun name source target mode env : mount ->
      { name; source; target; mode; env })
  |> Object.mem "name" string ~enc:(fun (m : mount) -> m.name)
  |> Object.mem "source" string ~enc:(fun (m : mount) -> m.source)
  |> Object.mem "target" string ~enc:(fun (m : mount) -> m.target)
  |> Object.mem "mode" mode_jsont
       ~dec_absent:(fun () -> `Rw)
       ~enc:(fun (m : mount) -> m.mode)
  |> Object.mem "env" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (m : mount) -> m.env)
  |> Object.finish

let metadata_jsont : metadata Jsont.t =
  let open Jsont in
  Object.map ~kind:"metadata"
    (fun oi_version generated_at cli_invocation : metadata ->
      { oi_version; generated_at; cli_invocation })
  |> Object.mem "oi_version" string
       ~dec_absent:(fun () -> "")
       ~enc:(fun (m : metadata) -> m.oi_version)
  |> Object.mem "generated_at" number
       ~dec_absent:(fun () -> 0.0)
       ~enc:(fun (m : metadata) -> m.generated_at)
  |> Object.mem "cli_invocation" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (m : metadata) -> m.cli_invocation)
  |> Object.finish

(* Local alias kept because [t] inside [let open Jsont] resolves to [Jsont.t]. *)
type local = t

let codec : t Jsont.t =
  let open Jsont in
  Object.map ~kind:"d10ir-plan"
    (fun
      schema_version
      os_key
      toolchain
      archive_root
      nodes
      roots
      mounts
      external_layers
      metadata
      :
      local
    ->
      {
        schema_version;
        os_key;
        toolchain;
        archive_root;
        nodes;
        roots;
        mounts;
        external_layers;
        metadata;
      })
  |> Object.mem "schema_version" int ~enc:(fun (p : local) -> p.schema_version)
  |> Object.mem "os_key" string ~enc:(fun (p : local) -> p.os_key)
  |> Object.mem "toolchain" toolchain_jsont ~enc:(fun (p : local) ->
         p.toolchain)
  |> Object.mem "archive_root" string
       ~dec_absent:(fun () -> "archives")
       ~enc:(fun (p : local) -> p.archive_root)
  |> Object.mem "nodes" (list node_jsont) ~enc:(fun (p : local) -> p.nodes)
  |> Object.mem "roots" (list layer_hash_jsont) ~enc:(fun (p : local) ->
         p.roots)
  |> Object.mem "mounts" (list mount_jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (p : local) -> p.mounts)
  |> Object.mem "external_layers" (list layer_hash_jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (p : local) -> p.external_layers)
  |> Object.mem "metadata" metadata_jsont
       ~dec_absent:(fun () ->
         { oi_version = ""; generated_at = 0.0; cli_invocation = [] })
       ~enc:(fun (p : local) -> p.metadata)
  |> Object.finish

let to_string t =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec t with
  | Ok s -> s
  | Error e -> Fmt.failwith "d10ir plan encode: %s" e

let pp ppf t =
  Fmt.pf ppf "@[<h>plan v%d %s toolchain=%s roots=%d nodes=%d@]"
    t.schema_version t.os_key t.toolchain.name (List.length t.roots)
    (List.length t.nodes)

let of_string s =
  match Jsont_bytesrw.decode_string ~locs:true ~file:"<plan.json>" codec s with
  | Ok t -> Ok t
  | Error e -> Error e

let encode_node n =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent node_jsont n with
  | Ok s -> s
  | Error e -> Fmt.failwith "d10ir node encode: %s" e

let decode_node s =
  Jsont_bytesrw.decode_string ~locs:true ~file:"<recipe.json>" node_jsont s

let save path t =
  let dir = Eio.Path.split path |> Option.map fst in
  Option.iter (Eio.Path.mkdirs ~exists_ok:true ~perm:0o755) dir;
  Eio.Path.save ~create:(`Or_truncate 0o644) path (to_string t)

let load path =
  try
    let s = Eio.Path.load path in
    let file =
      match Eio.Path.native path with Some s -> s | None -> "<plan.json>"
    in
    match Jsont_bytesrw.decode_string ~locs:true ~file codec s with
    | Ok t -> Ok t
    | Error e -> Error e
  with Eio.Exn.Io _ as e -> err_str "%a" Eio.Exn.pp e

(* Validation *)

type validate_error =
  | Schema_mismatch of { found : int; expected : int }
  | Cycle of Layer_hash.t list list
  | Unsatisfiable_dep of { node : Layer_hash.t; missing_dep : Layer_hash.t }
  | Duplicate_layer of Layer_hash.t
  | Archive_missing of { node : Layer_hash.t; path : string }
  | Archive_sha_mismatch of {
      node : Layer_hash.t;
      path : string;
      expected : string;
      actual : string;
    }

let pp_validate_error ppf = function
  | Schema_mismatch { found; expected } ->
      Fmt.pf ppf "schema_version=%d, expected=%d" found expected
  | Cycle cycles ->
      Fmt.pf ppf "@[<v>dependency cycle(s):@,%a@]"
        (Fmt.list ~sep:Fmt.cut (fun ppf c ->
             Fmt.pf ppf "  %a → %a"
               (Fmt.list ~sep:(Fmt.any " → ") Layer_hash.pp)
               c Layer_hash.pp (List.hd c)))
        cycles
  | Unsatisfiable_dep { node; missing_dep } ->
      Fmt.pf ppf "node %a needs %a, which is not in the plan and not in d10"
        Layer_hash.pp node Layer_hash.pp missing_dep
  | Duplicate_layer h ->
      Fmt.pf ppf "duplicate layer_hash %a in plan" Layer_hash.pp h
  | Archive_missing { node; path } ->
      Fmt.pf ppf "node %a references archive %s but the file is missing"
        Layer_hash.pp node path
  | Archive_sha_mismatch { node; path; expected; actual } ->
      Fmt.pf ppf "node %a archive %s sha256 mismatch (expected %s, got %s)"
        Layer_hash.pp node path expected actual

let producers_table t =
  let h = Hashtbl.create (List.length t.nodes) in
  List.iter (fun n -> Hashtbl.replace h n.layer_hash n) t.nodes;
  h

(* Dedupe a list-of-lists by a string key, preserving first-seen
   order. Each merge field (nodes, roots, mounts, external
   layers) uses the same shape with a different key function. *)
let merge_dedupe_by ~size ~key plans field =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create size in
  let keep x =
    let k = key x in
    if Hashtbl.mem seen k then false
    else (
      Hashtbl.add seen k ();
      true)
  in
  List.concat_map (fun p -> List.filter keep (field p)) plans

(* Find the first plan that disagrees with [first] on a non-batchable
   field. Returns [None] if all plans are mutually batchable, otherwise
   the offending plan. *)
let merge_find_invalid ~first rest =
  List.find_opt
    (fun p ->
      p.schema_version <> first.schema_version
      || p.os_key <> first.os_key
      || p.archive_root <> first.archive_root
      || p.toolchain.name <> first.toolchain.name)
    rest

let merge_mismatch_error ~first p =
  let mismatch field =
    Fmt.str "merge: plans disagree on %s — cannot batch them" field
  in
  if p.schema_version <> first.schema_version then mismatch "schema_version"
  else if p.os_key <> first.os_key then mismatch "os_key"
  else if p.archive_root <> first.archive_root then mismatch "archive_root"
  else mismatch "toolchain.name"

let merge_combine ~first plans =
  let lh x = Layer_hash.to_string x in
  let nodes =
    merge_dedupe_by ~size:256
      ~key:(fun n -> lh n.layer_hash)
      plans
      (fun p -> p.nodes)
  in
  let roots = merge_dedupe_by ~size:32 ~key:lh plans (fun p -> p.roots) in
  let mounts =
    merge_dedupe_by ~size:8
      ~key:(fun (m : mount) -> m.name)
      plans
      (fun p -> p.mounts)
  in
  let external_layers =
    merge_dedupe_by ~size:8 ~key:lh plans (fun p -> p.external_layers)
  in
  let metadata =
    {
      oi_version = first.metadata.oi_version;
      generated_at =
        List.fold_left (fun acc p -> max acc p.metadata.generated_at) 0. plans;
      cli_invocation =
        List.concat_map (fun p -> p.metadata.cli_invocation) plans;
    }
  in
  {
    schema_version = first.schema_version;
    os_key = first.os_key;
    toolchain = first.toolchain;
    archive_root = first.archive_root;
    nodes;
    roots;
    mounts;
    external_layers;
    metadata;
  }

(* The base_layer hash can legitimately differ across groups —
   it's the recipe's [packages[0].layer_hash], which depends on
   topological order and that group's specific dep closure. We
   only require [toolchain.name] to match: same logical
   toolchain ⇒ batchable. *)
let merge = function
  | [] -> Error "merge: empty plan list"
  | first :: rest -> (
      match merge_find_invalid ~first rest with
      | Some p -> Error (merge_mismatch_error ~first p)
      | None -> Ok (merge_combine ~first (first :: rest)))

(* Tarjan SCC over the nodes-by-layer-hash graph; returns SCCs of size > 1
   (true cycles) and self-loops. *)

type tarjan_state = {
  index : (Layer_hash.t, int) Hashtbl.t;
  lowlink : (Layer_hash.t, int) Hashtbl.t;
  on_stack : (Layer_hash.t, bool) Hashtbl.t;
  stack : node Stack.t;
  counter : int ref;
  sccs : Layer_hash.t list list ref;
}

let new_tarjan_state () =
  {
    index = Hashtbl.create 64;
    lowlink = Hashtbl.create 64;
    on_stack = Hashtbl.create 64;
    stack = Stack.create ();
    counter = ref 0;
    sccs = ref [];
  }

let tarjan_pop_scc state (v : node) =
  let scc = ref [] in
  let stop = ref false in
  while not !stop do
    let w = Stack.pop state.stack in
    Hashtbl.remove state.on_stack w.layer_hash;
    scc := w.layer_hash :: !scc;
    if Layer_hash.equal w.layer_hash v.layer_hash then stop := true
  done;
  !scc

let cycles_in (nodes : node list) : Layer_hash.t list list =
  let producers = Hashtbl.create (List.length nodes) in
  List.iter (fun n -> Hashtbl.replace producers n.layer_hash n) nodes;
  let succ n =
    List.filter (fun h -> Hashtbl.mem producers h) n.dep_layer_hashes
    |> List.map (fun h -> Hashtbl.find producers h)
  in
  let state = new_tarjan_state () in
  let is_cycle_scc v scc =
    List.length scc > 1
    ||
    let v_succs = List.map (fun n -> n.layer_hash) (succ v) in
    List.exists (Layer_hash.equal v.layer_hash) v_succs
  in
  let rec strongconnect (v : node) =
    Hashtbl.replace state.index v.layer_hash !(state.counter);
    Hashtbl.replace state.lowlink v.layer_hash !(state.counter);
    incr state.counter;
    Stack.push v state.stack;
    Hashtbl.replace state.on_stack v.layer_hash true;
    List.iter (visit_succ v) (succ v);
    if
      Hashtbl.find state.lowlink v.layer_hash
      = Hashtbl.find state.index v.layer_hash
    then
      let scc = tarjan_pop_scc state v in
      if is_cycle_scc v scc then state.sccs := scc :: !(state.sccs)
  and visit_succ (v : node) (w : node) =
    if not (Hashtbl.mem state.index w.layer_hash) then (
      strongconnect w;
      let lv = Hashtbl.find state.lowlink v.layer_hash in
      let lw = Hashtbl.find state.lowlink w.layer_hash in
      Hashtbl.replace state.lowlink v.layer_hash (min lv lw))
    else if Hashtbl.mem state.on_stack w.layer_hash then
      let lv = Hashtbl.find state.lowlink v.layer_hash in
      let iw = Hashtbl.find state.index w.layer_hash in
      Hashtbl.replace state.lowlink v.layer_hash (min lv iw)
  in
  List.iter
    (fun n ->
      if not (Hashtbl.mem state.index n.layer_hash) then strongconnect n)
    nodes;
  List.rev !(state.sccs)

let sha256_of_file ~fs:_ path =
  OpamHash.contents (OpamHash.compute ~kind:`SHA256 path)

(* Build a [layer_hash → node] producer map; returns [Some n] for the first
   duplicate, [None] if all hashes are unique. *)
let duplicate_node nodes =
  let producers = Hashtbl.create (List.length nodes) in
  let dup =
    List.find_opt
      (fun n ->
        if Hashtbl.mem producers n.layer_hash then true
        else (
          Hashtbl.add producers n.layer_hash n;
          false))
      nodes
  in
  (producers, dup)

let external_layer_set external_layers =
  let h = Hashtbl.create (List.length external_layers) in
  List.iter
    (fun lh -> Hashtbl.add h (Layer_hash.to_string lh) ())
    external_layers;
  h

let dep_in_d10 ~d10 h =
  match d10 with
  | None -> false
  | Some c ->
      D10.Layer.exists c ~hash:(Layer_hash.to_string h)
      && D10.Layer.succeeded c ~hash:(Layer_hash.to_string h)

let unsat_dep_for ~producers ~external_set ~d10 n h =
  if Hashtbl.mem producers h then None
  else if Hashtbl.mem external_set (Layer_hash.to_string h) then None
  else if dep_in_d10 ~d10 h then None
  else Some (Unsatisfiable_dep { node = n.layer_hash; missing_dep = h })

let find_unsat_dep ~producers ~external_set ~d10 nodes =
  List.find_map
    (fun n ->
      List.find_map
        (unsat_dep_for ~producers ~external_set ~d10 n)
        n.dep_layer_hashes)
    nodes

let absolute_archive_path ~plan_dir ~archive_root path =
  let rel_or_abs =
    if Filename.is_relative path then Filename.concat archive_root path
    else path
  in
  if Filename.is_relative rel_or_abs then Filename.concat plan_dir rel_or_abs
  else rel_or_abs

(* Skip the archive check for nodes whose layer is already succeeded in d10:
   the build path's [needs_archive] predicate doesn't fetch in that case, so
   requiring the archive here would reject a perfectly buildable plan
   whenever [d10ir-archives/<sha>.tar.zst] has been pruned. *)
let archive_err_for ~d10 ~fs ~plan_dir ~archive_root n =
  let already_built =
    match d10 with
    | None -> false
    | Some c -> D10.Layer.succeeded c ~hash:(Layer_hash.to_string n.layer_hash)
  in
  if already_built then None
  else
    let abs = absolute_archive_path ~plan_dir ~archive_root n.archive.path in
    if not (Sys.file_exists abs) then
      Some (Archive_missing { node = n.layer_hash; path = abs })
    else
      let actual = sha256_of_file ~fs abs in
      if String.equal actual n.archive.sha256 then None
      else
        Some
          (Archive_sha_mismatch
             {
               node = n.layer_hash;
               path = abs;
               expected = n.archive.sha256;
               actual;
             })

let find_archive_err ~d10 ~fs ~plan_dir ~archive_root nodes =
  List.find_map (archive_err_for ~d10 ~fs ~plan_dir ~archive_root) nodes

let check_schema t =
  if t.schema_version = current_schema_version then Ok ()
  else
    Error
      (Schema_mismatch
         { found = t.schema_version; expected = current_schema_version })

let check_no_cycle nodes =
  match cycles_in nodes with
  | _ :: _ as cycles -> Error (Cycle cycles)
  | [] -> Ok ()

let opt_to_err = function Some err -> Error err | None -> Ok ()

let validate ?d10 ~fs ~plan_dir t =
  let ( let* ) = Result.bind in
  let* () = check_schema t in
  let producers, dup = duplicate_node t.nodes in
  let* () =
    match dup with
    | Some n -> Error (Duplicate_layer n.layer_hash)
    | None -> Ok ()
  in
  let* () = check_no_cycle t.nodes in
  let external_set = external_layer_set t.external_layers in
  let* () = opt_to_err (find_unsat_dep ~producers ~external_set ~d10 t.nodes) in
  opt_to_err
    (find_archive_err ~d10 ~fs ~plan_dir ~archive_root:t.archive_root t.nodes)
