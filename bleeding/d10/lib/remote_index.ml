[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Fetches the published [<base>/<os_key>/index.json] (and, for binary
   lookups, [<base>/<os_key>/index-full.json]) and projects each layer
   row into the [Layer.remote_index] / {!index_full} maps that
   [oi run] / [oi build] / [oi run BARE_BINARY] consume.

   Both files are produced by a server-side Clickhouse query that scans
   the bucket's per-layer [<hash>.json] manifest sidecars — see
   [foo.sql] in the repo root. There is no fallback to the legacy
   [index.db]: the streaming-upload pipeline never publishes one, so
   buckets we read from in practice will only ever carry the JSON. *)

let log_src = Logs.Src.create "d10.remote_index"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Wire types + Jsont codecs ----------------------------------------- *)

type layer_min = {
  hash : string;
  tarball_sha256 : string;
  tarball_size : int64;
}

type index_min = {
  schema : int;
  os_key : string;
  generated_at : string;
  n_layers : int;
  layers : layer_min list;
}

let layer_min_codec : layer_min Jsont.t =
  let open Jsont in
  Object.map ~kind:"layer_min" (fun hash tarball_sha256 tarball_size ->
      { hash; tarball_sha256; tarball_size })
  |> Object.mem "hash" string ~enc:(fun l -> l.hash)
  |> Object.mem "tarball_sha256" string ~enc:(fun l -> l.tarball_sha256)
  |> Object.mem "tarball_size" int64 ~enc:(fun l -> l.tarball_size)
  |> Object.finish

let index_min_codec : index_min Jsont.t =
  let open Jsont in
  Object.map ~kind:"index_min"
    (fun schema os_key generated_at n_layers layers ->
      { schema; os_key; generated_at; n_layers; layers })
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "os_key" string ~enc:(fun r -> r.os_key)
  |> Object.mem "generated_at" string ~enc:(fun r -> r.generated_at)
  |> Object.mem "n_layers" int ~enc:(fun r -> r.n_layers)
  |> Object.mem "layers" (list layer_min_codec) ~enc:(fun r -> r.layers)
  |> Object.finish

type dep = { name : string; version : string; hash : string }

type findlib_entry = {
  package_dir : string;
  findlib_pkg : string;
  archive : string;
}

type layer_full = {
  hash : string;
  package_name : string;
  package_ver : string;
  exit_status : int;
  created : float;
  overlay_handle : string option;
  overlay_version : string option;
  tarball_sha256 : string option;
  tarball_size : int64 option;
  deps : dep list;
  binaries : string list;
  findlib : findlib_entry list;
}

type index_full = {
  schema : int;
  os_key : string;
  arch : string;
  distro : string;
  os_version : string;
  os : string;
  generated_at : string;
  n_layers : int;
  layers : layer_full list;
}

let dep_codec : dep Jsont.t =
  let open Jsont in
  Object.map ~kind:"dep" (fun name version hash -> { name; version; hash })
  |> Object.mem "name" string ~enc:(fun (d : dep) -> d.name)
  |> Object.mem "version" string ~enc:(fun (d : dep) -> d.version)
  |> Object.mem "hash" string ~enc:(fun (d : dep) -> d.hash)
  |> Object.finish

let findlib_codec : findlib_entry Jsont.t =
  let open Jsont in
  Object.map ~kind:"findlib_entry" (fun package_dir findlib_pkg archive ->
      { package_dir; findlib_pkg; archive })
  |> Object.mem "package_dir" string ~enc:(fun (f : findlib_entry) ->
         f.package_dir)
  |> Object.mem "findlib_pkg" string ~enc:(fun (f : findlib_entry) ->
         f.findlib_pkg)
  |> Object.mem "archive" string
       ~dec_absent:(fun () -> "")
       ~enc:(fun (f : findlib_entry) -> f.archive)
  |> Object.finish

let layer_full_codec : layer_full Jsont.t =
  let open Jsont in
  Object.map ~kind:"layer_full"
    (fun
      hash
      package_name
      package_ver
      exit_status
      created
      overlay_handle
      overlay_version
      tarball_sha256
      tarball_size
      deps
      binaries
      findlib
    ->
      {
        hash;
        package_name;
        package_ver;
        exit_status;
        created;
        overlay_handle;
        overlay_version;
        tarball_sha256;
        tarball_size;
        deps;
        binaries;
        findlib;
      })
  |> Object.mem "hash" string ~enc:(fun l -> l.hash)
  |> Object.mem "package_name" string ~enc:(fun l -> l.package_name)
  |> Object.mem "package_ver" string ~enc:(fun l -> l.package_ver)
  |> Object.mem "exit_status" int ~enc:(fun l -> l.exit_status)
  |> Object.mem "created" number ~enc:(fun l -> l.created)
  (* These come from a server-side Clickhouse query that emits SQL [NULL] as
     JSON [null] (not an absent member), so decode must tolerate an explicit
     [null] as well as absence — [opt_mem] only handles absence and would fail
     the whole index decode on [null]. [enc_omit] keeps the wire form
     unchanged (member omitted when [None]). *)
  |> Object.mem "overlay_handle" (option string)
       ~dec_absent:(fun () -> None)
       ~enc_omit:Option.is_none
       ~enc:(fun l -> l.overlay_handle)
  |> Object.mem "overlay_version" (option string)
       ~dec_absent:(fun () -> None)
       ~enc_omit:Option.is_none
       ~enc:(fun l -> l.overlay_version)
  |> Object.mem "tarball_sha256" (option string)
       ~dec_absent:(fun () -> None)
       ~enc_omit:Option.is_none
       ~enc:(fun l -> l.tarball_sha256)
  |> Object.mem "tarball_size" (option int64)
       ~dec_absent:(fun () -> None)
       ~enc_omit:Option.is_none
       ~enc:(fun l -> l.tarball_size)
  |> Object.mem "deps" (list dep_codec)
       ~dec_absent:(fun () -> [])
       ~enc:(fun l -> l.deps)
  |> Object.mem "binaries" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun l -> l.binaries)
  |> Object.mem "findlib" (list findlib_codec)
       ~dec_absent:(fun () -> [])
       ~enc:(fun l -> l.findlib)
  |> Object.finish

let index_full_codec : index_full Jsont.t =
  let open Jsont in
  Object.map ~kind:"index_full"
    (fun schema os_key arch distro os_version os generated_at n_layers layers ->
      {
        schema;
        os_key;
        arch;
        distro;
        os_version;
        os;
        generated_at;
        n_layers;
        layers;
      })
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "os_key" string ~enc:(fun r -> r.os_key)
  |> Object.mem "arch" string ~enc:(fun r -> r.arch)
  |> Object.mem "distro" string ~enc:(fun r -> r.distro)
  |> Object.mem "os_version" string ~enc:(fun r -> r.os_version)
  |> Object.mem "os" string ~enc:(fun r -> r.os)
  |> Object.mem "generated_at" string ~enc:(fun r -> r.generated_at)
  |> Object.mem "n_layers" int ~enc:(fun r -> r.n_layers)
  |> Object.mem "layers" (list layer_full_codec) ~enc:(fun r -> r.layers)
  |> Object.finish

(* -- HTTP fetch + decode ----------------------------------------------- *)

let trim_trailing_slash s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let url_for ~remote ~os_key ~basename =
  match remote with
  | `Http_remote base ->
      Fmt.str "%s/%s/%s" (trim_trailing_slash base) os_key basename

(* Download a JSON file into a temp path under the os_key's layer dir,
   decode via [codec], then unlink the temp. Returns [None] on any
   HTTP, read, or decode failure (with a warn log so the user sees
   "registry index unreachable" instead of silently building from
   source). *)
let download_and_decode (c : Config.t) ~session ~url codec =
  let os_layer_dir = Eio.Path.(c.root / "layers" / c.os_key) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_layer_dir;
  let tmp = Eio.Path.(os_layer_dir / ".remote-index.json.tmp") in
  let result =
    if not (Sysops.Http.fetch_session session ~url ~dst:tmp) then (
      Log.warn (fun m -> m "Could not fetch %s" url);
      None)
    else
      let path = Eio.Path.native_exn tmp in
      try
        let s = In_channel.with_open_text path In_channel.input_all in
        match Jsont_bytesrw.decode_string ~file:path codec s with
        | Ok r -> Some r
        | Error e ->
            Log.warn (fun m -> m "Decode %s: %s" url e);
            None
      with Sys_error _ | End_of_file ->
        Log.warn (fun m -> m "Could not read %s after fetch" url);
        None
  in
  (try Eio.Path.unlink tmp with Eio.Exn.Io _ -> ());
  result

(* Process-wide memo so [oi build --all] (which fires one solve per
   group) hits the network once per (registry, os_key) and reuses the
   parsed result for every later group. *)
let min_cache : (string, Layer.remote_index) Hashtbl.t = Hashtbl.create 4
let full_cache : (string, index_full) Hashtbl.t = Hashtbl.create 4

let populate_min_table (idx : Layer.remote_index) (parsed : index_min) =
  List.iter
    (fun (l : layer_min) ->
      Hashtbl.replace idx l.hash
        { Layer.sha256 = l.tarball_sha256; size = l.tarball_size })
    parsed.layers;
  Log.info (fun m ->
      m "Loaded %d tarball entries from index.json (%s)" (Hashtbl.length idx)
        parsed.os_key)

let fetch (c : Config.t) ~session ~remote =
  let url = url_for ~remote ~os_key:c.os_key ~basename:"index.json" in
  match Hashtbl.find_opt min_cache url with
  | Some cached -> cached
  | None ->
      let idx : Layer.remote_index = Hashtbl.create 64 in
      (match download_and_decode c ~session ~url index_min_codec with
      | Some parsed -> populate_min_table idx parsed
      | None -> ());
      Hashtbl.replace min_cache url idx;
      idx

let fetch_full (c : Config.t) ~session ~remote =
  let url = url_for ~remote ~os_key:c.os_key ~basename:"index-full.json" in
  match Hashtbl.find_opt full_cache url with
  | Some cached -> Some cached
  | None -> (
      match download_and_decode c ~session ~url index_full_codec with
      | Some parsed ->
          Log.info (fun m ->
              m "Loaded %d layers from index-full.json (%s)" parsed.n_layers
                parsed.os_key);
          Hashtbl.replace full_cache url parsed;
          Some parsed
      | None -> None)
