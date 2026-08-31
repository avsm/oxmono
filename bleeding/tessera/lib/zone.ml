(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let for_lon lon =
  let z = int_of_float (Float.floor ((lon +. 180.) /. 6.)) + 1 in
  max 1 (min 60 z)

let canonical_epsg z = 32600 + z
let centre_lon z = -180. +. ((float_of_int z -. 0.5) *. 6.)
let seam_degrees = 0.1

(* Python's [%] on floats always returns a value with the sign of the
   divisor, while [Float.rem] takes the sign of the dividend. Negative
   longitudes reach the seam test through here, so fold the remainder
   back into [0, 6) to keep the two implementations in step. *)
let seam_frac lon =
  let r = Float.rem (lon +. 180.) 6. in
  if r < 0. then r +. 6. else r

let seam_neighbours lon =
  let z = for_lon lon in
  let frac = seam_frac lon in
  let west =
    if frac <= seam_degrees then [ (if z = 1 then 60 else z - 1) ] else []
  in
  let east =
    if frac >= 6. -. seam_degrees then [ (if z = 60 then 1 else z + 1) ]
    else []
  in
  west @ east

(* Zone numbers are a ring of 60, so differences are taken modulo 60 and
   the half above 30 is read as a westward offset. OCaml's [mod] keeps
   the sign of the dividend, hence the extra fold. *)
let ring_mod n = ((n mod 60) + 60) mod 60

let spanned lons ~centre_lon =
  match lons with
  | [] -> invalid_arg "Zone.spanned: no longitudes"
  | first :: _ ->
      let zc = for_lon centre_lon in
      let offset z =
        let d = ring_mod (z - zc) in
        if d > 30 then d - 60 else d
      in
      let offs = List.map (fun lon -> offset (for_lon lon)) lons in
      let lo = List.fold_left min (offset (for_lon first)) offs in
      let hi = List.fold_left max (offset (for_lon first)) offs in
      List.init (hi - lo + 1) (fun i -> ring_mod (zc - 1 + lo + i) + 1)
