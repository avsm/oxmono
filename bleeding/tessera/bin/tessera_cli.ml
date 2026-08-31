(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The [tessera] command: read a Tessera embedding store from a URL or a
   directory. Every subcommand does its work in one plain function that
   takes already parsed arguments, and the cmdliner terms below do
   nothing but assemble those arguments. *)

open Cmdliner

let version = "0.1.0"

(* {1 Failures} *)

(* A store failure, a bad argument or an I/O error is a one line message
   on stderr and exit 1. A backtrace tells a user of a command line tool
   nothing it can act on. *)
let exit_failure = 1

let one_line s =
  String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) s

let report m =
  prerr_string ("tessera: " ^ one_line m ^ "\n");
  exit_failure

let guard f =
  try f () with
  | Zarrz.Error.E e -> report (Zarrz.Error.to_string e)
  | Invalid_argument m -> report m
  | Sys_error m -> report m
  | Eio.Io _ as e -> report (Printexc.to_string e)
  | Failure m -> report m

(* {1 The store} *)

(* [--store] is a URL or a directory, told apart by the scheme, since
   those are the two backends a reader has. *)
let is_url s =
  String.starts_with ~prefix:"https://" s
  || String.starts_with ~prefix:"http://" s

let rec chop_slash s =
  let n = String.length s in
  if n > 1 && s.[n - 1] = '/' then chop_slash (String.sub s 0 (n - 1)) else s

(* [run spec f] opens the store [spec] names and hands it to [f], under
   the one guard and the one switch every subcommand needs. *)
let run spec f =
  guard @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let store =
    if is_url spec then
      let client = Fetch_curl.std ~sw env in
      Zarrz_fetch.store ~base_url:(chop_slash spec) client
    else Zarrz_eio.store Eio.Path.(Eio.Stdenv.fs env / spec)
  in
  f (Tessera.of_store store)

(* {1 Printing} *)

(* One field a line, the name padded past the longest of them so the
   values line up whatever the subcommand. *)
let field name fmt = Printf.printf ("%-11s" ^^ fmt ^^ "\n") name

let status_name = function
  | Tessera.Valid -> "valid"
  | Water -> "water"
  | Nodata -> "nodata"
  | Outside -> "outside"

let pp_transform t =
  let a = Tessera.Affine.to_spatial t in
  String.concat " " (Array.to_list (Array.map (Printf.sprintf "%.10g") a))

(* The elements of a float32 slab, as doubles. *)
let f32 s =
  Bigarray.reshape_1
    (Zarrz.Slab.to_genarray s Bigarray.float32)
    (Zarrz.Slab.num_elements s)

let dims s =
  Bigarray.Genarray.dims (Zarrz.Slab.to_genarray s Bigarray.float32)

let pp_shape s =
  String.concat " x " (Array.to_list (Array.map string_of_int (dims s)))

(* The header and then the elements, so a large region is never copied
   into a string of its own. *)
let write_npy path slab =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      output_string oc (Tessera.Npy.header slab);
      let b = Zarrz.Slab.bigstring slab in
      let n = Base_bigstring.length b in
      let step = 65536 in
      let rec go pos =
        if pos < n then begin
          let len = min step (n - pos) in
          output_string oc
            (Base_bigstring.to_string (Base_bigstring.sub b ~pos ~len));
          go (pos + len)
        end
      in
      go 0)

(* {1 Commands} *)

let info_cmd store =
  run store @@ fun t ->
  let g = Tessera.geoemb t in
  let zones = Tessera.zones t in
  let head = List.filteri (fun i _ -> i < 8) zones in
  field "store" "%s" store;
  field "model" "%s" g.Zarrz_geoemb.model;
  field "type" "%s"
    (match g.Zarrz_geoemb.kind with
    | Zarrz_geoemb.Pixel -> "pixel"
    | Zarrz_geoemb.Chip -> "chip");
  field "dimensions" "%d" g.Zarrz_geoemb.dimensions;
  field "build" "%s"
    (Option.value ~default:"unknown" g.Zarrz_geoemb.build_version);
  field "years" "%s"
    (String.concat " " (List.map string_of_int (Tessera.years t)));
  field "zones" "%d%s" (List.length zones)
    (match head with
    | [] -> ""
    | l ->
        Printf.sprintf " (%s%s)"
          (String.concat " " (List.map (Printf.sprintf "utm%02d") l))
          (if List.length zones > List.length l then " ..." else ""));
  0

let probe_cmd store lon lat year cross_zone search_px =
  run store @@ fun t ->
  let v, st = Tessera.probe t ~lon ~lat ~year ~cross_zone ~search_px () in
  field "point" "%.6f %.6f" lon lat;
  field "zone" "utm%02d" (Tessera.Zone.for_lon lon);
  field "status" "%s" (status_name st);
  (match v with
  | None -> ()
  | Some v ->
      field "bands" "%d" (Array.length v);
      let n = min 8 (Array.length v) in
      field "values" "%s"
        (String.concat " "
           (List.init n (fun i -> Printf.sprintf "%.6g" v.(i))));
      if Array.length v > n then field "" "%s" "...");
  if st = Tessera.Valid then 0 else exit_failure

let region_cmd store min_lon min_lat max_lon max_lat year out =
  run store @@ fun t ->
  let r =
    Tessera.read_region t ~bbox:(min_lon, min_lat, max_lon, max_lat) ~year
  in
  write_npy out r.Tessera.Dataset.Region.data;
  field "wrote" "%s" out;
  field "shape" "%s" (pp_shape r.Tessera.Dataset.Region.data);
  field "transform" "%s" (pp_transform r.Tessera.Dataset.Region.transform);
  field "crs" "EPSG:%d" r.Tessera.Dataset.Region.epsg;
  0

let patch_cmd store lon lat year size out =
  run store @@ fun t ->
  let p = Tessera.read_patch t ~lon ~lat ~year ~size_px:size in
  write_npy out p.Tessera.Patch.data;
  let v = f32 p.Tessera.Patch.data in
  let d = dims p.Tessera.Patch.data in
  let pixels = d.(0) * d.(1) and bands = d.(2) in
  (* A pixel counts as covered when any band is finite, which is how the
     patch marks one it found nothing for. *)
  let covered = ref 0 in
  for i = 0 to pixels - 1 do
    let live = ref false in
    for b = 0 to bands - 1 do
      if Float.is_finite (Bigarray.Array1.get v ((i * bands) + b)) then
        live := true
    done;
    if !live then incr covered
  done;
  field "wrote" "%s" out;
  field "shape" "%s" (pp_shape p.Tessera.Patch.data);
  field "transform" "%s" (pp_transform p.Tessera.Patch.transform);
  field "crs" "%s" (Tessera.Patch.crs_name p.Tessera.Patch.crs);
  field "covered" "%d of %d pixels" !covered pixels;
  0

(* {1 Arguments} *)

let store_t =
  let doc =
    "Store to read. An $(b,https://) or $(b,http://) value is fetched \
     over HTTP, anything else is a local directory."
  in
  let env = Cmd.Env.info "TESSERA_STORE" in
  Arg.(
    value & opt string Tessera.url
    & info [ "store" ] ~env ~docv:"URL_OR_DIR" ~doc)

let year_t =
  let doc =
    "Embedding year. $(b,tessera info) lists the years a store holds."
  in
  Arg.(required & opt (some int) None & info [ "year" ] ~docv:"YEAR" ~doc)

let out_t =
  let doc = "Write the array to $(docv) in NumPy $(b,.npy) format." in
  Arg.(required & opt (some string) None & info [ "o"; "output" ]
       ~docv:"FILE" ~doc)

let lon_t =
  let doc = "Longitude of the point, WGS84 degrees east." in
  Arg.(required & pos 0 (some float) None & info [] ~docv:"LON" ~doc)

let lat_t =
  let doc = "Latitude of the point, WGS84 degrees north." in
  Arg.(required & pos 1 (some float) None & info [] ~docv:"LAT" ~doc)

let cross_zone_t =
  let doc =
    "Do not fall back to the neighbouring UTM zone for a point near a \
     seam. The fallback is on by default."
  in
  Arg.(value & vflag true [ (false, info [ "no-cross-zone" ] ~doc) ])

let search_px_t =
  let doc =
    "Radius in pixels of the window searched for a written pixel when \
     the point's own is unwritten. $(b,0) disables the search. Water is \
     never repaired whatever this is."
  in
  Arg.(value & opt int 1 & info [ "search-px" ] ~docv:"N" ~doc)

let size_t =
  let doc = "Patch width and height in pixels." in
  Arg.(required & opt (some int) None & info [ "size" ] ~docv:"N" ~doc)

let bbox_t n name doc =
  Arg.(required & pos n (some float) None & info [] ~docv:name ~doc)

(* {1 Manuals} *)

let exits =
  Cmd.Exit.info exit_failure
    ~doc:
      "on a store, projection or output failure, and for $(b,probe) on \
       any status but $(b,valid)."
  :: Cmd.Exit.defaults

(* A western or southern coordinate starts with a dash, which the parser
   would take for an option name. Every manual that has a positional
   coordinate says how to pass one. *)
let negative_man =
  `P
    "A negative coordinate looks like an option, so put $(b,--) before \
     the positional arguments and every option before it, as in \
     $(b,tessera probe --year 2024 -- -3.44 56.19)."

let common_man =
  [
    `S Manpage.s_common_options;
    `S Manpage.s_environment;
    `S Manpage.s_exit_status;
    `S Manpage.s_bugs;
    `P "Report issues at $(b,https://github.com/geo-embeddings).";
  ]

let info_cmd_t =
  let doc = "Describe the store." in
  let man =
    `S Manpage.s_description
    :: `P
         "Prints the model the embeddings came from, the length of one \
          vector, the version of the software that built the store, the \
          years it covers and the UTM zones it holds."
    :: common_man
  in
  Cmd.v
    (Cmd.info "info" ~doc ~man ~exits)
    Term.(const info_cmd $ store_t)

let probe_cmd_t =
  let doc = "Sample the embedding at a point." in
  let man =
    `S Manpage.s_description
    :: `P
         "Prints the outcome of the read and, when there is a vector, \
          its length and its first eight values."
    :: `P
         "The outcome is $(b,valid) for a vector, $(b,water) for a pixel \
          the producer marked as having no embedding, $(b,nodata) for a \
          neighbourhood no tile ever wrote, and $(b,outside) when no \
          zone's grid reaches the point. Only $(b,valid) exits 0."
    :: negative_man
    :: `S Manpage.s_examples
    :: `Pre "  tessera probe 0.0918 52.2109 --year 2024"
    :: `Pre "  tessera probe --year 2024 -- -3.44 56.19"
    :: common_man
  in
  Cmd.v
    (Cmd.info "probe" ~doc ~man ~exits)
    Term.(
      const probe_cmd $ store_t $ lon_t $ lat_t $ year_t $ cross_zone_t
      $ search_px_t)

let region_cmd_t =
  let doc = "Write every pixel inside a bounding box." in
  let man =
    `S Manpage.s_description
    :: `P
         "Reads the pixels of a WGS84 bounding box on their native grid, \
          unresampled, and writes them as a $(b,(h, w, bands)) float32 \
          NumPy array. A pixel with no embedding is a row of $(b,NaN)."
    :: `P
         "The zone holding the centre of the box serves the whole \
          request, so a box spanning a UTM seam is short on the far side \
          of it. Use $(b,patch) for a square that crosses a seam."
    :: negative_man
    :: `S Manpage.s_examples
    :: `Pre
         "  tessera region 0.08 52.20 0.10 52.22 --year 2024 -o out.npy"
    :: common_man
  in
  Cmd.v
    (Cmd.info "region" ~doc ~man ~exits)
    Term.(
      const region_cmd $ store_t
      $ bbox_t 0 "MINLON" "Western edge of the box, WGS84 degrees east."
      $ bbox_t 1 "MINLAT" "Southern edge of the box, WGS84 degrees north."
      $ bbox_t 2 "MAXLON" "Eastern edge of the box, WGS84 degrees east."
      $ bbox_t 3 "MAXLAT" "Northern edge of the box, WGS84 degrees north."
      $ year_t $ out_t)

let patch_cmd_t =
  let doc = "Write a fixed-size square patch centred on a point." in
  let man =
    `S Manpage.s_description
    :: `P
         "Writes exactly $(b,(N, N, bands)) float32 with the point on \
          the centre pixel, as a NumPy array. A pixel the store holds \
          nothing for is a row of $(b,NaN)."
    :: `P
         "A patch inside one UTM zone keeps that zone's grid and its \
          EPSG code. One crossing a seam is merged onto a transverse \
          Mercator grid centred on the patch, whose proj string is \
          printed as the CRS."
    :: negative_man
    :: `S Manpage.s_examples
    :: `Pre "  tessera patch 0.0918 52.2109 --size 32 --year 2024 -o p.npy"
    :: common_man
  in
  Cmd.v
    (Cmd.info "patch" ~doc ~man ~exits)
    Term.(const patch_cmd $ store_t $ lon_t $ lat_t $ year_t $ size_t $ out_t)

let main =
  let doc = "Read Tessera geospatial embeddings." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Reads a Tessera embedding store: one Zarr V3 group per UTM \
         zone, holding a vector per 10 metre pixel per year. Points come \
         back dequantised on the store's own grid, never resampled.";
      `P
        "Every subcommand takes $(b,--store), which defaults to the \
         public store over HTTP. Pass a directory to read a local copy.";
      `S Manpage.s_common_options;
      `S Manpage.s_environment;
      `S Manpage.s_exit_status;
    ]
  in
  Cmd.group
    (Cmd.info "tessera" ~version ~doc ~man ~exits)
    [ info_cmd_t; probe_cmd_t; region_cmd_t; patch_cmd_t ]

let () =
  match Cmd.eval_value main with
  | Ok (`Ok code) -> exit code
  | Ok (`Help | `Version) -> exit Cmd.Exit.ok
  | Error _ -> exit Cmd.Exit.cli_error
