(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { name : string; mutable tr : Proj.Transformation.t option }

let name t = t.name

let fail t msg =
  invalid_arg (Printf.sprintf "Crs %s: %s" t.name msg)

(* PROJ rejects a bad target only when the transformation is built, and
   an EPSG code needs the on-disk database, so neither error can be
   raised by the constructors. Build on first use and keep the result:
   the bindings hold a C object that costs a database read. *)
let transformation t =
  match t.tr with
  | Some tr -> tr
  | None ->
      let tr =
        match Proj.Transformation.of_string ~src:"EPSG:4326" t.name with
        | tr -> Proj.Transformation.normalize_for_visualization tr
        | exception Failure msg -> fail t msg
      in
      t.tr <- Some tr;
      tr

let utm_north ~zone =
  if zone < 1 || zone > 60 then
    invalid_arg
      (Printf.sprintf "Crs.utm_north: zone %d is outside 1..60" zone);
  { name = Printf.sprintf "EPSG:%d" (32600 + zone); tr = None }

let patch ~lon ~lat =
  let y0 = if lat >= 0. then 0 else 10000000 in
  let name =
    Printf.sprintf
      "+proj=tmerc +lat_0=0 +lon_0=%.8f +k=0.9996 +x_0=500000 +y_0=%d \
       +datum=WGS84 +units=m +no_defs"
      lon y0
  in
  { name; tr = None }

let project t ~direction ~x ~y =
  let tr = transformation t in
  let c = Proj.Coord.make ~x ~y ~z:0. ~t:0. in
  match Proj.Transformation.transform ~direction tr c with
  | r -> (Proj.Coord.x r, Proj.Coord.y r)
  | exception Failure msg -> fail t msg

let forward t ~lon ~lat = project t ~direction:Proj.Forward ~x:lon ~y:lat
let inverse t ~e ~n = project t ~direction:Proj.Inverse ~x:e ~y:n
