(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Affine = Affine
module Consolidated = Consolidated
module Crs = Crs
module Dataset = Dataset
module Zone = Zone

type status = Dataset.status = Valid | Water | Nodata | Outside

let url = "https://data.source.coop/tessera/tessera/zarr/v1"

type t = {
  store : Zarrz.Store.t;
  geoemb : Zarrz_geoemb.t;
  consolidated : Consolidated.t option;
  datasets : (int, Dataset.t option) Hashtbl.t;
  mutable zone_list : int list option;
  mutable years : int list option;
}

let err fmt =
  Format.kasprintf (fun m -> Zarrz.Error.raise_ (Zarrz.Error.Metadata m)) fmt

let of_store store =
  let g = Zarrz.Group.open_ store ~path:"/" in
  let geoemb =
    match Zarrz.Group.attributes g with
    | None -> err "root group has no attributes"
    | Some j -> (
        match Zarrz_geoemb.of_attributes j with
        | None -> err "root group does not declare the geoembeddings convention"
        | Some (Error m) -> err "root attributes: %s" m
        | Some (Ok v) -> v)
  in
  {
    store;
    geoemb;
    consolidated = Consolidated.of_group (Zarrz.Group.metadata g);
    datasets = Hashtbl.create 8;
    zone_list = None;
    years = None;
  }

let store t = t.store
let geoemb t = t.geoemb
let consolidated t = t.consolidated

(* Presence is asked of the store rather than inferred from a failed
   open: the store answers [None] for a key it does not hold and raises
   for anything else, so a store that is merely broken is not reported
   as sixty missing zones. *)
let present t z =
  match t.consolidated with
  | Some c -> Consolidated.node c (Printf.sprintf "utm%02d" z) <> None
  | None ->
      t.store.Zarrz.Store.get ~key:(Printf.sprintf "utm%02d/zarr.json" z)
      <> None

let zone_opt t z =
  match Hashtbl.find_opt t.datasets z with
  | Some d -> d
  | None ->
      let d =
        if not (present t z) then None
        else Some (Dataset.open_ ?consolidated:t.consolidated t.store ~zone:z)
      in
      Hashtbl.replace t.datasets z d;
      d

let zone t z =
  match zone_opt t z with
  | Some d -> d
  | None ->
      Zarrz.Error.raise_
        (Zarrz.Error.Store
           (Printf.sprintf "utm%02d: no such zone in the store" z))

let zones t =
  match t.zone_list with
  | Some l -> l
  | None ->
      let l =
        match t.consolidated with
        | Some c -> Consolidated.zones c
        | None -> List.filter (present t) (List.init 60 (fun i -> i + 1))
      in
      t.zone_list <- Some l;
      l

let years t =
  match t.years with
  | Some y -> y
  | None ->
      let rec go = function
        | [] -> []
        | z :: tl -> (
            match zone_opt t z with
            | Some d -> Dataset.years d
            | None -> go tl)
      in
      let y = go (zones t) in
      t.years <- Some y;
      y

(* {1 Point queries} *)

let candidates ~cross_zone lon =
  let first = Zone.for_lon lon in
  if cross_zone then
    first :: List.filter (fun z -> z <> first) (Zone.seam_neighbours lon)
  else [ first ]

let probe t ~lon ~lat ~year ?(cross_zone = true) ?(search_px = 1) () =
  let water = ref false and nodata = ref false in
  let rec go = function
    | [] ->
        if !water then (None, Water)
        else if !nodata then (None, Nodata)
        else (None, Outside)
    | z :: tl -> (
        match zone_opt t z with
        | None -> go tl
        | Some d -> (
            let e, n = Dataset.proj d ~lon ~lat in
            match Dataset.probe d ~e ~n ~year ~search_px () with
            | (Some _, Valid) as r -> r
            | _, Water ->
                water := true;
                go tl
            | _, Nodata ->
                nodata := true;
                go tl
            | _, _ -> go tl))
  in
  go (candidates ~cross_zone lon)

let sample t ~lon ~lat ~year ?cross_zone ?search_px () =
  fst (probe t ~lon ~lat ~year ?cross_zone ?search_px ())

let sample_points t pts ~year ?cross_zone ?search_px () =
  let n = Array.length pts in
  let order = Array.init n Fun.id in
  (* Visiting a zone's points together keeps its tile cache warm. The
     sort is stable, so points of one zone keep their input order and
     the walk stays close to the caller's own locality. *)
  Array.stable_sort
    (fun i j ->
      Int.compare
        (Zone.for_lon (fst pts.(i)))
        (Zone.for_lon (fst pts.(j))))
    order;
  let out = Array.make n None in
  Array.iter
    (fun i ->
      let lon, lat = pts.(i) in
      out.(i) <- sample t ~lon ~lat ~year ?cross_zone ?search_px ())
    order;
  out

(* {1 Region queries} *)

let read_region t ~bbox ~year =
  let min_lon, min_lat, max_lon, max_lat = bbox in
  let d = zone t (Zone.for_lon ((min_lon +. max_lon) /. 2.)) in
  (* Python projects the north-west and south-east corners alone and
     takes their extent. A lon/lat box is not axis aligned in UTM, so
     the other two corners can fall marginally outside that extent and
     the window can be a pixel short on one side. Matched here on
     purpose: the two implementations must select the same pixels. *)
  let e_nw, n_nw = Dataset.proj d ~lon:min_lon ~lat:max_lat in
  let e_se, n_se = Dataset.proj d ~lon:max_lon ~lat:min_lat in
  Dataset.read_region d
    ~e_min:(Float.min e_nw e_se)
    ~e_max:(Float.max e_nw e_se)
    ~n_min:(Float.min n_nw n_se)
    ~n_max:(Float.max n_nw n_se)
    ~year
