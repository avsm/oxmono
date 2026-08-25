(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module A1 = Bigarray.Array1
module Arr = Zarrz.Arr
module Dtype = Zarrz.Dtype
module Slab = Zarrz.Slab
module Subset = Zarrz.Subset

type status = Valid | Water | Nodata | Outside

module Region = struct
  type t = { data : Slab.t; transform : Affine.t; epsg : int }
end

(* The published store shards at 4096 and chunks inside a shard at 32,
   so a 32 aligned window is the smallest read that decodes nothing it
   does not use. *)
let tile_px = 32

type array_kind = Emb | Scale

type t = {
  zone : int;
  epsg : int;
  transform : Affine.t;
  px : float;
  height : int;
  width : int;
  bands : int;
  crs : Crs.t;
  emb : Arr.t;
  scales : Arr.t;
  time : Arr.t;
  mutable years : int list option;
  cache : (array_kind * int * int * int, Slab.t) Lru.t;
}

let err fmt =
  Format.kasprintf (fun m -> Zarrz.Error.raise_ (Zarrz.Error.Metadata m)) fmt

(* {1 Views over a slab} *)

(* A [float32] bigarray presents its elements as OCaml doubles, widening
   on a read and rounding to nearest even on a write. The rounding is
   what the dequantiser needs: the exact product of two float32 values
   fits a double, so a double multiply stored back through this view is
   the float32 product, with no double rounding. *)
let f32 s = Bigarray.reshape_1 (Slab.to_genarray s Bigarray.float32)
    (Slab.num_elements s)

let i8 s = Bigarray.reshape_1 (Slab.to_genarray s Bigarray.int8_signed)
    (Slab.num_elements s)

let i32 s = Bigarray.reshape_1 (Slab.to_genarray s Bigarray.int32)
    (Slab.num_elements s)

(* Rounding a double to float32 the same way the store would. *)
let to_f32 x = Int32.float_of_bits (Int32.bits_of_float x)

(* {1 Opening} *)

let group_name zone = Printf.sprintf "utm%02d" zone
let group_path zone = "/" ^ group_name zone

let attr_mems ~name = function
  | Some (Jsont.Object (o, _)) -> o
  | Some _ -> err "%s: attributes are not a JSON object" name
  | None -> err "%s: group has no attributes" name

let epsg_of_mems ~name mems =
  match Jsont.Json.find_mem "proj:code" mems with
  | Some (_, Jsont.String (s, _)) -> (
      match String.index_opt s ':' with
      | Some i -> (
          let code = String.sub s (i + 1) (String.length s - i - 1) in
          match int_of_string_opt code with
          | Some c -> c
          | None -> err "%s: proj:code %S has no numeric code" name s)
      | None -> err "%s: proj:code %S is not an authority code" name s)
  | Some _ -> err "%s: proj:code is not a string" name
  | None -> err "%s: group has no proj:code" name

let transform_of_mems ~name mems =
  match Jsont.Json.find_mem "spatial:transform" mems with
  | Some (_, Jsont.Array (l, _)) ->
      let n = List.length l in
      if n <> 6 then
        err "%s: spatial:transform has %d elements, not 6" name n;
      Affine.of_spatial
        (Array.of_list
           (List.map
              (function
                | Jsont.Number (f, _) -> f
                | _ -> err "%s: spatial:transform holds a non-number" name)
              l))
  | Some _ -> err "%s: spatial:transform is not an array" name
  | None -> err "%s: group has no spatial:transform" name

let check_dtype ~name array a want =
  let got = Arr.dtype a in
  if not (Dtype.equal got want) then
    err "%s/%s: data type is %s, not %s" name array (Dtype.name got)
      (Dtype.name want)

let open_ ?(cache_capacity = 256) ?consolidated store ~zone =
  let name = group_name zone in
  let path = group_path zone in
  let inlined sub =
    match consolidated with
    | None -> None
    | Some c ->
        Consolidated.node c (if sub = "" then name else name ^ "/" ^ sub)
  in
  let gmeta =
    match inlined "" with
    | Some j -> (
        match Zarrz.Metadata.group_of_json j with
        | Ok m -> m
        | Error m -> err "%s: consolidated metadata: %s" name m)
    | None -> Zarrz.Group.metadata (Zarrz.Group.open_ store ~path)
  in
  let mems = attr_mems ~name gmeta.group_attributes in
  let epsg = epsg_of_mems ~name mems in
  (* Both hemispheres are filed under the northern code, so a zone whose
     group says otherwise is not the grid {!crs} would project onto and
     every read off it would be silently displaced. *)
  if epsg <> Zone.canonical_epsg zone then
    err "%s: proj:code is EPSG:%d, not the canonical EPSG:%d" name epsg
      (Zone.canonical_epsg zone);
  let transform = transform_of_mems ~name mems in
  let arr sub =
    let path = path ^ "/" ^ sub in
    match inlined sub with
    | Some j -> Arr.of_json store ~path j
    | None -> Arr.open_ store ~path
  in
  let emb = arr "embeddings" and scales = arr "scales" and time = arr "time" in
  check_dtype ~name "embeddings" emb Dtype.Int8;
  check_dtype ~name "scales" scales Dtype.Float32;
  check_dtype ~name "time" time Dtype.Int32;
  let es = Arr.shape emb and ss = Arr.shape scales and ts = Arr.shape time in
  if Array.length es <> 4 then
    err "%s/embeddings: rank %d, not 4" name (Array.length es);
  if Array.length ss <> 3 then
    err "%s/scales: rank %d, not 3" name (Array.length ss);
  if Array.length ts <> 1 then err "%s/time: rank %d, not 1" name
      (Array.length ts);
  let bands = es.(1) and height = es.(2) and width = es.(3) in
  if ss.(0) <> es.(0) || ss.(1) <> height || ss.(2) <> width then
    err "%s: scales are %dx%dx%d against embeddings %dx%dx%d" name ss.(0)
      ss.(1) ss.(2) es.(0) height width;
  if ts.(0) <> es.(0) then
    err "%s: time has %d values against %d embedding times" name ts.(0) es.(0);
  (* An empty dimension would make every pixel index of the grid out of
     range, so refuse the zone rather than read past a chunk later. *)
  if es.(0) < 1 || bands < 1 || height < 1 || width < 1 then
    err "%s/embeddings: shape %dx%dx%dx%d has an empty dimension" name es.(0)
      bands height width;
  {
    zone;
    epsg;
    transform;
    px = Float.abs transform.Affine.a;
    height;
    width;
    bands;
    crs = Crs.utm_north ~zone;
    emb;
    scales;
    time;
    years = None;
    cache = Lru.create ~capacity:cache_capacity;
  }

(* {1 Properties} *)

let zone t = t.zone
let epsg t = t.epsg
let transform t = t.transform
let shape t = (t.height, t.width)
let bands t = t.bands
let pixel_size t = t.px
let crs t = t.crs
let proj t ~lon ~lat = Crs.forward t.crs ~lon ~lat

let years t =
  match t.years with
  | Some y -> y
  | None ->
      let n = (Arr.shape t.time).(0) in
      let s = Arr.read t.time { Subset.start = [: 0 :]; shape = [: n :] } in
      let v = i32 s in
      let y = List.init n (fun i -> Int32.to_int (A1.get v i)) in
      t.years <- Some y;
      y

let time_index t year =
  let ys = years t in
  let rec go i = function
    | [] ->
        invalid_arg
          (Printf.sprintf "Tessera.Dataset: year %d is not one of %s" year
             (String.concat ", " (List.map string_of_int ys)))
    | y :: tl -> if y = year then i else go (i + 1) tl
  in
  go 0 ys

(* {1 Tiles} *)

(* The tile a pixel falls in, read whole and kept. An edge tile is
   clipped to the grid rather than padded, so the subset never runs off
   the array and the slab is exactly what was asked for. *)
let tile t kind ti ty tx =
  match Lru.find_opt t.cache (kind, ti, ty, tx) with
  | Some s -> s
  | None ->
      let y0 = ty * tile_px and x0 = tx * tile_px in
      let th = min tile_px (t.height - y0) in
      let tw = min tile_px (t.width - x0) in
      let s =
        match kind with
        | Scale ->
            Arr.read t.scales
              { Subset.start = [: ti; y0; x0 :]; shape = [: 1; th; tw :] }
        | Emb ->
            Arr.read t.emb
              {
                Subset.start = [: ti; 0; y0; x0 :];
                shape = [: 1; t.bands; th; tw :];
              }
      in
      Lru.add t.cache (kind, ti, ty, tx) s;
      s

let scale_at t ~ti ~row ~col =
  let ty = row / tile_px and tx = col / tile_px in
  let s = tile t Scale ti ty tx in
  let tw = min tile_px (t.width - (tx * tile_px)) in
  A1.get (f32 s) (((row - (ty * tile_px)) * tw) + (col - (tx * tile_px)))

(* One pixel's whole vector, dequantised. Taken in one pass so that the
   tile and its view are found once rather than once per band. *)
let emb_column t ~ti ~row ~col ~scale =
  let ty = row / tile_px and tx = col / tile_px in
  let s = tile t Emb ti ty tx in
  let th = min tile_px (t.height - (ty * tile_px)) in
  let tw = min tile_px (t.width - (tx * tile_px)) in
  let v = i8 s in
  let off = ((row - (ty * tile_px)) * tw) + (col - (tx * tile_px)) in
  Array.init t.bands (fun b ->
      to_f32 (float_of_int (A1.get v ((b * th * tw) + off)) *. scale))

(* {1 Point reads} *)

(* [numpy.argmin] over the ascending pixel centres picks the nearest
   index and, on an exact tie, the lower one. [Float.round] rounds a tie
   away from zero instead, so the boundary between two pixels would go
   the other way. [ceil (f -. 0.5)] is the tie-to-lower nearest, and
   clamping in float keeps an infinite or unrepresentable index out of
   [int_of_float]. *)
let nearest f n =
  let g = Float.ceil (f -. 0.5) in
  if Float.is_nan g || g <= 0. then 0
  else if g >= float_of_int (n - 1) then n - 1
  else int_of_float g

let probe t ~e ~n ~year ?(search_px = 1) () =
  let col = nearest (Affine.col_of_x t.transform ~x:e) t.width in
  let row = nearest (Affine.row_of_y t.transform ~y:n) t.height in
  if
    Float.abs (Affine.x_of_col t.transform ~col:(float_of_int col) -. e) > t.px
    || Float.abs (Affine.y_of_row t.transform ~row:(float_of_int row) -. n)
       > t.px
  then (None, Outside)
  else
    let ti = time_index t year in
    let r = max 0 search_px in
    let x0 = max 0 (col - r) and x1 = min t.width (col + r + 1) in
    let y0 = max 0 (row - r) and y1 = min t.height (row + r + 1) in
    let wh = y1 - y0 and ww = x1 - x0 in
    let win =
      Array.init (wh * ww) (fun k ->
          scale_at t ~ti ~row:(y0 + (k / ww)) ~col:(x0 + (k mod ww)))
    in
    let ci = row - y0 and cj = col - x0 in
    let centre = win.((ci * ww) + cj) in
    if Float.is_nan centre then (None, Water)
    else
      let best =
        if Float.is_finite centre then Some (ci, cj)
        else begin
          (* Row-major scan with a strict improvement, which is where
             [numpy.argmin] over the nonzero indices lands too. *)
          let best = ref None and dist = ref max_int in
          for i = 0 to wh - 1 do
            for j = 0 to ww - 1 do
              if Float.is_finite win.((i * ww) + j) then begin
                let d = ((i - ci) * (i - ci)) + ((j - cj) * (j - cj)) in
                if d < !dist then begin
                  dist := d;
                  best := Some (i, j)
                end
              end
            done
          done;
          !best
        end
      in
      match best with
      | None -> (None, Nodata)
      | Some (bi, bj) ->
          let scale = win.((bi * ww) + bj) in
          let row = y0 + bi and col = x0 + bj in
          (Some (emb_column t ~ti ~row ~col ~scale), Valid)

let sample t ~e ~n ~year ?search_px () =
  fst (probe t ~e ~n ~year ?search_px ())

(* {1 Region reads} *)

(* The first index whose pixel centre is at or past [f], clamped to
   [0 .. n]: [n] itself means the box starts past the last pixel and the
   window is empty. *)
let first_index f n =
  let g = Float.ceil f in
  if Float.is_nan g || g <= 0. then 0
  else if g >= float_of_int n then n
  else int_of_float g

(* The last index whose pixel centre is at or before [f], clamped to
   [-1 .. n - 1]. *)
let last_index f n =
  let g = Float.floor f in
  if Float.is_nan g || g <= -1. then -1
  else if g >= float_of_int (n - 1) then n - 1
  else int_of_float g

let read_region t ~e_min ~e_max ~n_min ~n_max ~year =
  let e_min, e_max = (Float.min e_min e_max, Float.max e_min e_max) in
  let n_min, n_max = (Float.min n_min n_max, Float.max n_min n_max) in
  let ti = time_index t year in
  let a = t.transform in
  let col0 = first_index (Affine.col_of_x a ~x:e_min) t.width in
  let col1 = last_index (Affine.col_of_x a ~x:e_max) t.width in
  (* Rows run north to south, so the northern bound picks the first. *)
  let row0 = first_index (Affine.row_of_y a ~y:n_max) t.height in
  let row1 = last_index (Affine.row_of_y a ~y:n_min) t.height in
  let w = max 0 (col1 - col0 + 1) and h = max 0 (row1 - row0 + 1) in
  let out = Slab.create Dtype.Float32 [: h; w; t.bands :] in
  if h > 0 && w > 0 then begin
    let emb =
      Arr.read t.emb
        {
          Subset.start = [: ti; 0; row0; col0 :];
          shape = [: 1; t.bands; h; w :];
        }
    in
    let scl =
      Arr.read t.scales
        { Subset.start = [: ti; row0; col0 :]; shape = [: 1; h; w :] }
    in
    let ov = f32 out and ev = i8 emb and sv = f32 scl in
    (* One fused pass: the transpose from (band, y, x) to (y, x, band)
       and the scale multiply cost the same walk. *)
    for r = 0 to h - 1 do
      for c = 0 to w - 1 do
        let s = A1.get sv ((r * w) + c) in
        let dst = ((r * w) + c) * t.bands in
        if Float.is_finite s then
          for b = 0 to t.bands - 1 do
            A1.set ov (dst + b)
              (float_of_int (A1.get ev ((b * h * w) + (r * w) + c)) *. s)
          done
        else
          for b = 0 to t.bands - 1 do
            A1.set ov (dst + b) Float.nan
          done
      done
    done
  end;
  let transform =
    {
      a with
      Affine.c = a.Affine.c +. (float_of_int col0 *. a.Affine.a);
      f = a.Affine.f +. (float_of_int row0 *. a.Affine.e);
    }
  in
  { Region.data = out; transform; epsg = t.epsg }
