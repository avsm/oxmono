(** zarr-meta -- Zarr v3 metadata viewer.

    Reads and displays zarr.json metadata from local directories or remote
    URLs (via the requests library).  Supports listing the hierarchy,
    showing array metadata, and dumping raw JSON.

    Examples:

    {v
    zarr-meta show ./my-store/data
    zarr-meta show https://dl2.geotessera.org/zarr/v1/utm31_2024.zarr/
    zarr-meta tree ./my-store
    zarr-meta json ./my-store/data
    v} *)

open Cmdliner

(* ---------- helpers ------------------------------------------------- *)

(** Determine whether a path looks like a URL (http:// or https://). *)
let is_url s =
  String.length s > 8
  && (String.sub s 0 7 = "http://" || String.sub s 0 8 = "https://")

(** Normalise a path so it doesn't end with '/'. *)
let strip_trailing_slash s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

(** Fetch a file as a string from a remote URL or local filesystem.
    Returns [None] on 404 or missing file. *)
let fetch_string ~session path key =
  if is_url path then begin
    let url = strip_trailing_slash path ^ "/" ^ key in
    try
      let resp = Requests.get session url in
      if Requests.Response.ok resp then
        Some (Requests.Response.text resp)
      else None
    with _ -> None
  end else begin
    let full = Filename.concat path key in
    if Sys.file_exists full then
      Some (In_channel.with_open_text full In_channel.input_all)
    else None
  end

(** Fetch a URL directly (for directory listings). *)
let fetch_url ~session url =
  match Requests.get session url with
  | resp when Requests.Response.ok resp ->
    Some (Requests.Response.text resp)
  | _ -> None
  | exception _ -> None

(* ---------- pretty-printing ----------------------------------------- *)

let pp_dtype fmt (d : Zarr.Dtype.t) =
  Format.fprintf fmt "%s" (Zarr.Dtype.to_string d)

let pp_int_array fmt a =
  Format.fprintf fmt "[";
  Array.iteri (fun i v ->
    if i > 0 then Format.fprintf fmt ", ";
    Format.fprintf fmt "%d" v
  ) a;
  Format.fprintf fmt "]"

let pp_fill_value fmt fv =
  let s = Zarr.Json_util.to_string ~format:Jsont.Minify
            (Zarr.Fill.to_json fv) in
  Format.fprintf fmt "%s" s

let pp_codec_spec (s : Zarr.Codec.codec_spec) =
  match s with
  | Bytes { endian } ->
    let e = match endian with Some Zarr.Dtype.Big -> "big" | _ -> "little" in
    Printf.sprintf "bytes(%s)" e
  | Transpose { order } ->
    Printf.sprintf "transpose(%s)"
      (String.concat "," (Array.to_list (Array.map string_of_int order)))
  | Gzip { level } -> Printf.sprintf "gzip(%d)" level
  | Zstd { level; checksum } ->
    Printf.sprintf "zstd(%d%s)" level (if checksum then ",crc" else "")
  | Crc32c -> "crc32c"
  | Sharding { chunk_shape; _ } ->
    Printf.sprintf "sharding(%s)"
      (String.concat "x" (Array.to_list (Array.map string_of_int chunk_shape)))
  | Extension { name; _ } -> name

let pp_codecs fmt specs =
  Format.fprintf fmt "%s"
    (String.concat " -> " (List.map pp_codec_spec specs))

let pp_dim_names fmt = function
  | None -> Format.fprintf fmt "(none)"
  | Some names ->
    Format.fprintf fmt "[";
    Array.iteri (fun i n ->
      if i > 0 then Format.fprintf fmt ", ";
      match n with
      | Some s -> Format.fprintf fmt "%S" s
      | None -> Format.fprintf fmt "null"
    ) names;
    Format.fprintf fmt "]"

let pp_chunk_key_encoding fmt (enc : Zarr.Chunk_key.encoding) =
  match enc with
  | Default { separator = Slash } -> Format.fprintf fmt "default(/)"
  | Default { separator = Dot } -> Format.fprintf fmt "default(.)"
  | V2 { separator = Slash } -> Format.fprintf fmt "v2(/)"
  | V2 { separator = Dot } -> Format.fprintf fmt "v2(.)"

let pp_size fmt bytes =
  if bytes < 1_048_576 then
    Format.fprintf fmt "%d bytes" bytes
  else if bytes < 1_073_741_824 then
    Format.fprintf fmt "%.1f MiB" (Float.of_int bytes /. 1_048_576.0)
  else
    Format.fprintf fmt "%.2f GiB" (Float.of_int bytes /. 1_073_741_824.0)

let pp_array_metadata fmt (m : Zarr.Metadata.array_metadata) =
  let numel = Array.fold_left ( * ) 1 m.shape in
  let raw_bytes = numel * Zarr.Dtype.byte_size m.data_type in
  Format.fprintf fmt "@[<v 0>\
    @,  data_type   : %a\
    @,  shape       : %a\
    @,  chunks      : %a\
    @,  fill_value  : %a\
    @,  codecs      : %a\
    @,  chunk_key   : %a\
    @,  dim_names   : %a\
    @,  elements    : %d\
    @,  raw_size    : %a\
    @]"
    pp_dtype m.data_type
    pp_int_array m.shape
    pp_int_array (Zarr.Chunk_grid.chunk_shape m.chunk_grid)
    pp_fill_value m.fill_value
    pp_codecs m.codecs
    pp_chunk_key_encoding m.chunk_key_encoding
    pp_dim_names m.dimension_names
    numel
    pp_size raw_bytes

(** Try to parse array metadata; if codec parsing fails (e.g. unregistered
    extension codec like blosc), fall back to extracting fields from raw JSON
    so we can still display something useful. *)
let pp_array_metadata_or_raw fmt json_str =
  try
    let m = Zarr.Metadata.array_of_json json_str in
    pp_array_metadata fmt m
  with _ ->
    (* Fallback: extract what we can from raw JSON *)
    let json = Zarr.Json_util.of_string json_str in
    let shape_json = Zarr.Json_util.(member "shape" json |> to_list_opt) in
    let dtype_str = Zarr.Json_util.(member "data_type" json |> to_string_opt) in
    let codecs_json = Zarr.Json_util.(member "codecs" json |> to_list_opt) in
    Format.fprintf fmt "@[<v 0>";
    (match dtype_str with
     | Some d -> Format.fprintf fmt "@,  data_type   : %s" d
     | None -> ());
    (match shape_json with
     | Some dims ->
       let shape = List.filter_map Zarr.Json_util.to_int_opt dims in
       Format.fprintf fmt "@,  shape       : [%s]"
         (String.concat ", " (List.map string_of_int shape))
     | None -> ());
    (match codecs_json with
     | Some codecs ->
       let names = List.filter_map (fun c ->
         Zarr.Json_util.(member "name" c |> to_string_opt)
       ) codecs in
       Format.fprintf fmt "@,  codecs      : %s" (String.concat " -> " names)
     | None -> ());
    Format.fprintf fmt "@]"

(** Extract dtype, shape, and codec summary from raw JSON for tree view. *)
let pp_array_summary_raw fmt json_str =
  try
    let m = Zarr.Metadata.array_of_json json_str in
    Format.fprintf fmt "  %a %a  %a"
      pp_dtype m.data_type
      pp_int_array m.shape
      pp_codecs m.codecs
  with _ ->
    let json = Zarr.Json_util.of_string json_str in
    let dtype_str = Zarr.Json_util.(member "data_type" json |> to_string_opt) in
    let shape_json = Zarr.Json_util.(member "shape" json |> to_list_opt) in
    let codecs_json = Zarr.Json_util.(member "codecs" json |> to_list_opt) in
    (match dtype_str with Some d -> Format.fprintf fmt "  %s" d | None -> ());
    (match shape_json with
     | Some dims ->
       let shape = List.filter_map Zarr.Json_util.to_int_opt dims in
       Format.fprintf fmt " [%s]"
         (String.concat ", " (List.map string_of_int shape))
     | None -> ());
    (match codecs_json with
     | Some codecs ->
       let names = List.filter_map (fun c ->
         Zarr.Json_util.(member "name" c |> to_string_opt)
       ) codecs in
       Format.fprintf fmt "  %s" (String.concat " -> " names)
     | None -> ())

(* ---------- commands ------------------------------------------------ *)

(** [show] -- display formatted array or group metadata. *)
let run_show ~session path =
  match fetch_string ~session path "zarr.json" with
  | None ->
    Format.eprintf "Error: no zarr.json found at %s@." path;
    1
  | Some json_str ->
    let json = Zarr.Json_util.of_string json_str in
    let node_type =
      Zarr.Json_util.(member "node_type" json |> to_string_opt) in
    (match node_type with
     | Some "array" ->
       Format.printf "Array: %s%a@." path
         pp_array_metadata_or_raw json_str;
       0
     | Some "group" ->
       let attrs = Zarr.Json_util.member "attributes" json in
       Format.printf "Group: %s@." path;
       if not (Zarr.Json_util.is_null attrs) then
         Format.printf "  attributes: %s@."
           (Zarr.Json_util.to_string_pretty attrs);
       0
     | _ ->
       Format.eprintf "Error: unrecognised node_type in %s@." path;
       1)

(** [json] -- dump raw zarr.json with indentation. *)
let run_json ~session path =
  match fetch_string ~session path "zarr.json" with
  | None ->
    Format.eprintf "Error: no zarr.json found at %s@." path;
    1
  | Some json_str ->
    let json = Zarr.Json_util.of_string json_str in
    Format.printf "%s@." (Zarr.Json_util.to_string_pretty json);
    0

(** Recursively walk a local directory for zarr.json files. *)
let walk_local dir =
  let results = ref [] in
  let rec go prefix =
    let full_dir = if prefix = "" then dir else Filename.concat dir prefix in
    let meta = Filename.concat full_dir "zarr.json" in
    if Sys.file_exists meta then begin
      let json_str = In_channel.with_open_text meta In_channel.input_all in
      (try
         let json = Zarr.Json_util.of_string json_str in
         let nt = Zarr.Json_util.(member "node_type" json |> to_string_opt) in
         let path = if prefix = "" then "/" else "/" ^ prefix in
         results := (path, nt, Some json_str) :: !results
       with _ -> ())
    end;
    if Sys.file_exists full_dir && Sys.is_directory full_dir then
      Sys.readdir full_dir |> Array.iter (fun entry ->
        let child = if prefix = "" then entry
          else Filename.concat prefix entry in
        let child_full = Filename.concat dir child in
        if Sys.is_directory child_full then go child
      )
  in
  go "";
  List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) !results

(** Extract directory names from an HTML directory listing.

    Many HTTP servers (Caddy, nginx, Apache) serve an HTML page listing
    directory contents when you GET a path ending with [/].  We look for
    [href="NAME/"] links that point to subdirectories.  Simple scan --
    no regex library needed. *)
let extract_directory_links html =
  let pattern = {|href="|} in
  let plen = String.length pattern in
  let hlen = String.length html in
  let results = ref [] in
  let mutable i = 0 in
  while i < hlen - plen do
    if String.sub html i plen = pattern then begin
      let start = i + plen in
      (* Scan to the closing quote *)
      let mutable j = start in
      while j < hlen && html.[j] <> '"' do j <- j + 1 done;
      if j > start && html.[j - 1] = '/' then begin
        let raw = String.sub html start (j - 1 - start) in
        (* Strip leading "./" that Caddy-style listings use *)
        let name =
          if String.length raw > 2 && raw.[0] = '.' && raw.[1] = '/'
          then String.sub raw 2 (String.length raw - 2)
          else raw
        in
        (* Skip . , .. , and paths with / (relative links to parents) *)
        if name <> "" && name <> "." && name <> ".."
           && not (String.contains name '/') then
          results := name :: !results
      end;
      i <- j + 1
    end else
      i <- i + 1
  done;
  List.rev !results

(** Walk a remote zarr store by probing for zarr.json files and
    recursively listing directories via HTML directory listings. *)
let walk_remote ~session base =
  let base = strip_trailing_slash base in
  let results = ref [] in
  let rec go url prefix =
    (* Try zarr.json at this level *)
    (try
       match fetch_string ~session url "zarr.json" with
       | None -> ()
       | Some json_str ->
         (try
            let json = Zarr.Json_util.of_string json_str in
            let nt = Zarr.Json_util.(member "node_type" json |> to_string_opt) in
            let path = if prefix = "" then "/" else "/" ^ prefix in
            results := (path, nt, Some json_str) :: !results
          with _ -> ())
     with _ -> ());
    (* Enumerate children via HTML directory listing *)
    let children =
      match fetch_url ~session (url ^ "/") with
      | None -> []
      | Some html -> extract_directory_links html
      | exception _ -> []
    in
    List.iter (fun child ->
      (* Skip chunk data directories (e.g. "c", "0", "1", ...) *)
      let is_chunk_dir =
        child = "c"
        || (String.length child > 0
            && child.[0] >= '0' && child.[0] <= '9'
            && (try ignore (int_of_string child); true with _ -> false))
      in
      if not is_chunk_dir then begin
        let child_url = url ^ "/" ^ child in
        let child_prefix = if prefix = "" then child
          else prefix ^ "/" ^ child in
        (try go child_url child_prefix with _ -> ())
      end
    ) children
  in
  (try go base "" with _ -> ());
  List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) !results

(** [tree] -- show the hierarchy of arrays and groups. *)
let run_tree ~session path =
  let nodes =
    if is_url path then walk_remote ~session path
    else walk_local path
  in
  if nodes = [] then begin
    Format.eprintf "Error: no zarr nodes found at %s@." path;
    1
  end else begin
    List.iter (fun (node_path, nt, json_str_opt) ->
      let kind = match nt with
        | Some "array" -> "[array]"
        | Some "group" -> "[group]"
        | _ -> "[?]" in
      Format.printf "%-40s %s" node_path kind;
      (match nt, json_str_opt with
       | Some "array", Some json_str ->
         pp_array_summary_raw Format.std_formatter json_str
       | _ -> ());
      Format.printf "@."
    ) nodes;
    0
  end

(* ---------- cmdliner definitions ------------------------------------ *)

type action =
  | Show of string
  | Json of string
  | Tree of string

let path_arg =
  let doc = "Path to a zarr store or node. May be a local directory path or \
             an HTTP(S) URL." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)

let show_cmd_t =
  let info = Cmd.info "show"
    ~doc:"Display formatted metadata for a zarr array or group."
    ~man:[
      `S Manpage.s_description;
      `P "Read the $(b,zarr.json) metadata at $(i,PATH) and display it in a \
          human-readable format.  Works with both local directory paths and \
          remote HTTP(S) URLs.";
      `S Manpage.s_examples;
      `Pre "  zarr-meta show ./my-store/data";
      `Pre "  zarr-meta show https://example.com/zarr/data";
    ] in
  Cmd.v info Term.(const (fun p -> Show p) $ path_arg)

let json_cmd_t =
  let info = Cmd.info "json"
    ~doc:"Dump raw zarr.json with pretty indentation."
    ~man:[
      `S Manpage.s_description;
      `P "Fetch and pretty-print the raw $(b,zarr.json) document at $(i,PATH). \
          Useful for debugging or piping into other tools.";
      `S Manpage.s_examples;
      `Pre "  zarr-meta json ./my-store/data | jq .codecs";
    ] in
  Cmd.v info Term.(const (fun p -> Json p) $ path_arg)

let tree_cmd_t =
  let info = Cmd.info "tree"
    ~doc:"Show the hierarchy of arrays and groups in a zarr store."
    ~man:[
      `S Manpage.s_description;
      `P "Walk the zarr store at $(i,PATH) and list all arrays and groups \
          with a one-line summary of their metadata.  Local stores are \
          enumerated by directory traversal; remote stores discover children \
          via HTTP directory listings.";
      `S Manpage.s_examples;
      `Pre "  zarr-meta tree ./my-store";
      `Pre "  zarr-meta tree https://example.com/zarr/store";
    ] in
  Cmd.v info Term.(const (fun p -> Tree p) $ path_arg)

let main_cmd =
  let info = Cmd.info "zarr-meta" ~version:"0.1.0"
    ~doc:"Zarr v3 metadata viewer"
    ~man:[
      `S Manpage.s_description;
      `P "$(tname) inspects Zarr v3 store metadata.  It reads \
          $(b,zarr.json) documents from local directories or remote HTTP(S) \
          URLs and displays them in human-readable or JSON format.";
      `S Manpage.s_examples;
      `Pre "  # Show array metadata";
      `Pre "  zarr-meta show ./my-store/data";
      `Pre "";
      `Pre "  # Show hierarchy";
      `Pre "  zarr-meta tree ./my-store";
      `Pre "";
      `Pre "  # Dump raw JSON from remote store";
      `Pre "  zarr-meta json https://example.com/zarr/data";
      `S Manpage.s_see_also;
      `P "Zarr v3.1 specification: \
          https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html";
    ] in
  Cmd.group info [show_cmd_t; json_cmd_t; tree_cmd_t]

(* ---------- entry point --------------------------------------------- *)

let () =
  match Cmd.eval_value main_cmd with
  | Ok (`Ok action) ->
    let exit_code = ref 1 in
    (try
       Eio_main.run @@ fun env ->
       Eio.Switch.run @@ fun sw ->
       let session = Requests.create ~sw env in
       exit_code :=
         (match action with
          | Show path -> run_show ~session path
          | Json path -> run_json ~session path
          | Tree path -> run_tree ~session path)
     with
     | Eio.Cancel.Cancelled _ -> ()
     | exn ->
       Format.eprintf "Error: %s@." (Printexc.to_string exn));
    exit !exit_code
  | Ok `Help | Ok `Version -> exit 0
  | Error _ -> exit 2
