(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module A1 = Bigarray.Array1
module Dtype = Zarrz.Dtype
module Iarray = Stdlib_stable.Iarray
module Slab = Zarrz.Slab

type crs = [ `Epsg of int | `Proj of string ]
type t = { data : Slab.t; transform : Affine.t; crs : crs }

let crs_name = function
  | `Epsg c -> Printf.sprintf "EPSG:%d" c
  | `Proj s -> s

(* {1 Slab helpers} *)

(* The element type must be spelt out wherever a view crosses a function
   boundary. A view left polymorphic goes through the generic bigarray
   accessor, a C call per element. *)
type f32v = (float, Bigarray.float32_elt, Bigarray.c_layout) A1.t

let f32 s : f32v =
  Bigarray.reshape_1 (Slab.to_genarray s Bigarray.float32) (Slab.num_elements s)

(* The height and width of a rank 3 slab. *)
let dims2 s =
  let d = Slab.shape s in
  (Iarray.get d 0, Iarray.get d 1)

let nan_slab shape =
  let s = Slab.create Dtype.Float32 shape in
  let v = f32 s in
  for i = 0 to Slab.num_elements s - 1 do
    A1.unsafe_set v i Float.nan
  done;
  s

(* One pixel's vector moved between two float32 views. The accessors are
   unchecked: every caller derives both offsets from the shapes of the
   slabs the views came from. *)
let[@zero_alloc] blit ~(src : f32v) ~src_off ~(dst : f32v) ~dst_off ~bands =
  for b = 0 to bands - 1 do
    A1.unsafe_set dst (dst_off + b) (A1.unsafe_get src (src_off + b))
  done

(* {1 Index arithmetic} *)

(* The nearest pixel centre, ties to the lower index and clamped to the
   grid, as [Dataset.probe] takes it. The reference reaches this pixel
   through an [xarray] nearest selection, which breaks an exact tie the
   other way. A tie is a point exactly on a pixel boundary. *)
let nearest f n =
  let g = Float.ceil (f -. 0.5) in
  if Float.is_nan g || g <= 0. then 0
  else if g >= float_of_int (n - 1) then n - 1
  else int_of_float g

(* The pixel containing [f], a fractional index measured at pixel
   centres, or [-1] when that pixel is outside [0 .. n - 1]. This is
   where a nearest-neighbour reprojection reads from: the source pixel
   the destination centre falls in, never a clamped edge one. *)
let containing f n =
  let g = Float.floor (f +. 0.5) in
  if Float.is_nan g || g < 0. || g >= float_of_int n then -1
  else int_of_float g

(* {1 The native path} *)

(* One zone, sliced off its own grid. The target grid is the zone's own,
   translated to start at [(col0, row0)], so a target pixel is a source
   pixel and nothing is resampled. The reference builds the same grid by
   reindexing onto the pixel centres around the point with a tolerance
   of half a pixel: since both grids share a pixel size, that tolerance
   only ever rejects a column or row off the grid, which is the [NaN]
   padding here. *)
let native d ~ce ~cn ~year ~size_px =
  let px = Dataset.pixel_size d in
  let tr = Dataset.transform d in
  let h, w = Dataset.shape d in
  let bands = Dataset.bands d in
  let col_near = nearest (Affine.col_of_x tr ~x:ce) w in
  let row_near = nearest (Affine.row_of_y tr ~y:cn) h in
  let col0 = col_near - (size_px / 2) and row0 = row_near - (size_px / 2) in
  let out = nan_slab [: size_px; size_px; bands :] in
  let ov = f32 out in
  let c_lo = max 0 col0 and c_hi = min (w - 1) (col0 + size_px - 1) in
  let r_lo = max 0 row0 and r_hi = min (h - 1) (row0 + size_px - 1) in
  if c_lo <= c_hi && r_lo <= r_hi then begin
    (* [Dataset.read_region] selects on pixel centres, so the bounds are
       nudged a quarter pixel outwards: the coordinate round trip
       through the affine is not exact at the store's eastings, and a
       bound landing a hair inside a centre would drop that column. *)
    let q = 0.25 *. px in
    let reg =
      Dataset.read_region d
        ~e_min:(Affine.x_of_col tr ~col:(float_of_int c_lo) -. q)
        ~e_max:(Affine.x_of_col tr ~col:(float_of_int c_hi) +. q)
        ~n_min:(Affine.y_of_row tr ~row:(float_of_int r_hi) -. q)
        ~n_max:(Affine.y_of_row tr ~row:(float_of_int r_lo) +. q)
        ~year
    in
    let rh, rw = dims2 reg.Dataset.Region.data in
    let rv = f32 reg.Dataset.Region.data in
    (* Where the window actually landed, read back from its transform
       rather than assumed from the bounds asked for. *)
    let rt = reg.Dataset.Region.transform in
    let off x0 x step = int_of_float (Float.round ((x -. x0) /. step)) in
    let dc = off tr.Affine.c rt.Affine.c tr.Affine.a in
    let dr = off tr.Affine.f rt.Affine.f tr.Affine.e in
    (* Window index plus this offset is patch index. Where the two
       overlap is settled once per axis rather than tested per pixel. *)
    let ro = dr - row0 and co = dc - col0 in
    let r0 = max 0 (-ro) and r1 = min (rh - 1) (size_px - 1 - ro) in
    let c0 = max 0 (-co) and c1 = min (rw - 1) (size_px - 1 - co) in
    for r = r0 to r1 do
      let src_row = r * rw and dst_row = (r + ro) * size_px in
      for c = c0 to c1 do
        blit ~src:rv
          ~src_off:((src_row + c) * bands)
          ~dst:ov
          ~dst_off:((dst_row + c + co) * bands)
          ~bands
      done
    done
  end;
  let transform =
    {
      tr with
      Affine.c = tr.Affine.c +. (float_of_int col0 *. tr.Affine.a);
      f = tr.Affine.f +. (float_of_int row0 *. tr.Affine.e);
    }
  in
  { data = out; transform; crs = `Epsg (Dataset.epsg d) }

(* {1 The merged path} *)

(* Every zone's native pixels relocated onto one patch-centred grid. *)
let merged zone_of ~centre ~zones ~lon ~lat ~year ~size_px =
  let px = Dataset.pixel_size centre in
  let bands = Dataset.bands centre in
  let target = Crs.patch ~lon ~lat in
  let ce, cn = Crs.forward target ~lon ~lat in
  (* The point lands on the centre of pixel [size_px / 2]. *)
  let off = (float_of_int (size_px / 2) +. 0.5) *. px in
  let ox = ce -. off and oy = cn +. off in
  let transform = { Affine.a = px; b = 0.; c = ox; d = 0.; e = -.px; f = oy } in
  let side = float_of_int size_px *. px in
  (* A straight edge of the patch curves in a zone's grid, so the four
     corners alone under-cover the middle of an edge. The reference
     densifies each edge to 33 points, and the window a zone is read
     over is the box those points fall in. *)
  let outline =
    List.concat_map
      (fun i ->
        let s = side *. float_of_int i /. 32. in
        [
          (ox +. s, oy);
          (ox +. s, oy -. side);
          (ox, oy -. s);
          (ox +. side, oy -. s);
        ])
      (List.init 33 Fun.id)
  in
  let outline_ll = List.map (fun (e, n) -> Crs.inverse target ~e ~n) outline in
  let n = size_px * size_px in
  (* One inverse projection per output pixel, kept: every zone needs the
     same longitudes and latitudes to project forward into its grid. *)
  let plon = Array.make n 0. and plat = Array.make n 0. in
  let owner = Array.make n 0 in
  for r = 0 to size_px - 1 do
    let y = oy -. ((float_of_int r +. 0.5) *. px) in
    for c = 0 to size_px - 1 do
      let x = ox +. ((float_of_int c +. 0.5) *. px) in
      let lo, la = Crs.inverse target ~e:x ~n:y in
      let i = (r * size_px) + c in
      plon.(i) <- lo;
      plat.(i) <- la;
      owner.(i) <- Zone.for_lon lo
    done
  done;
  let out = nan_slab [: size_px; size_px; bands :] in
  let spare = nan_slab [: size_px; size_px; bands :] in
  let ov = f32 out and sv = f32 spare in
  let owned = Bytes.make n '\000' and spared = Bytes.make n '\000' in
  List.iter
    (fun z ->
      match zone_of z with
      | None -> ()
      | Some d ->
          let zc = Dataset.crs d in
          let e_min = ref Float.infinity and e_max = ref Float.neg_infinity in
          let n_min = ref Float.infinity and n_max = ref Float.neg_infinity in
          List.iter
            (fun (lon, lat) ->
              let e, nn = Crs.forward zc ~lon ~lat in
              if e < !e_min then e_min := e;
              if e > !e_max then e_max := e;
              if nn < !n_min then n_min := nn;
              if nn > !n_max then n_max := nn)
            outline_ll;
          let pad = 2. *. px in
          let reg =
            Dataset.read_region d ~e_min:(!e_min -. pad) ~e_max:(!e_max +. pad)
              ~n_min:(!n_min -. pad) ~n_max:(!n_max +. pad) ~year
          in
          let rh, rw = dims2 reg.Dataset.Region.data in
          if rh > 0 && rw > 0 then begin
            let rv = f32 reg.Dataset.Region.data in
            let rt = reg.Dataset.Region.transform in
            for i = 0 to n - 1 do
              let e, nn = Crs.forward zc ~lon:plon.(i) ~lat:plat.(i) in
              let c = containing (Affine.col_of_x rt ~x:e) rw in
              let r = containing (Affine.row_of_y rt ~y:nn) rh in
              if c >= 0 && r >= 0 then begin
                let src = ((r * rw) + c) * bands in
                (* A pixel with no data is a row of [NaN], which the
                   reference drops the same way. One finite band settles
                   it, so the walk stops there. *)
                let mutable live = false in
                let mutable b = 0 in
                while (not live) && b < bands do
                  if Float.is_finite (A1.unsafe_get rv (src + b)) then
                    live <- true;
                  b <- b + 1
                done;
                if live then
                  if owner.(i) = z then begin
                    blit ~src:rv ~src_off:src ~dst:ov
                      ~dst_off:(i * bands) ~bands;
                    Bytes.unsafe_set owned i '\001'
                  end
                  else if Bytes.unsafe_get spared i = '\000' then begin
                    blit ~src:rv ~src_off:src ~dst:sv
                      ~dst_off:(i * bands) ~bands;
                    Bytes.unsafe_set spared i '\001'
                  end
              end
            done
          end)
    zones;
  (* A pixel its owner had nothing for takes whatever a neighbour
     relocated onto it. *)
  for i = 0 to n - 1 do
    if Bytes.unsafe_get owned i = '\000' && Bytes.unsafe_get spared i = '\001'
    then
      blit ~src:sv ~src_off:(i * bands) ~dst:ov ~dst_off:(i * bands) ~bands
  done;
  { data = out; transform; crs = `Proj (Crs.name target) }

(* {1 Entry point} *)

let read ~zone ~lon ~lat ~year ~size_px =
  if size_px <= 0 then
    invalid_arg
      (Printf.sprintf "Tessera.read_patch: size_px must be positive, got %d"
         size_px);
  let z0 = Zone.for_lon lon in
  let centre =
    match zone z0 with
    | Some d -> d
    | None ->
        Zarrz.Error.raise_
          (Zarrz.Error.Store
             (Printf.sprintf "utm%02d: no such zone in the store" z0))
  in
  let px = Dataset.pixel_size centre in
  let half = float_of_int size_px *. px /. 2. in
  let ce, cn = Dataset.proj centre ~lon ~lat in
  let zc = Dataset.crs centre in
  (* The corners of the patch as the centre zone would lay it out, back
     in longitude: what they span decides which path runs. *)
  let corner_lons =
    List.concat_map
      (fun dx ->
        List.map
          (fun dy -> fst (Crs.inverse zc ~e:(ce +. dx) ~n:(cn +. dy)))
          [ -.half; half ])
      [ -.half; half ]
  in
  match Zone.spanned corner_lons ~centre_lon:lon with
  | [ _ ] -> native centre ~ce ~cn ~year ~size_px
  | zones -> merged zone ~centre ~zones ~lon ~lat ~year ~size_px
