(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  a : float;
  b : float;
  c : float;
  d : float;
  e : float;
  f : float;
}

let of_spatial v =
  if Array.length v <> 6 then
    invalid_arg
      (Printf.sprintf "Affine.of_spatial: expected 6 elements, got %d"
         (Array.length v));
  {
    a = v.(0);
    b = v.(1);
    c = v.(2);
    d = v.(3);
    e = v.(4);
    f = v.(5);
  }

let to_spatial t = [| t.a; t.b; t.c; t.d; t.e; t.f |]

let apply t ~col ~row =
  (t.c +. (t.a *. col) +. (t.b *. row), t.f +. (t.d *. col) +. (t.e *. row))

let invert t =
  let det = (t.a *. t.e) -. (t.b *. t.d) in
  if det = 0. || not (Float.is_finite det) then
    invalid_arg "Affine.invert: singular transform";
  {
    a = t.e /. det;
    b = -.t.b /. det;
    c = ((t.b *. t.f) -. (t.e *. t.c)) /. det;
    d = -.t.d /. det;
    e = t.a /. det;
    f = ((t.d *. t.c) -. (t.a *. t.f)) /. det;
  }

let col_of_x t ~x = ((x -. t.c) /. t.a) -. 0.5
let row_of_y t ~y = ((y -. t.f) /. t.e) -. 0.5
let x_of_col t ~col = t.c +. ((col +. 0.5) *. t.a)
let y_of_row t ~row = t.f +. ((row +. 0.5) *. t.e)

let equal x y =
  Float.equal x.a y.a && Float.equal x.b y.b && Float.equal x.c y.c
  && Float.equal x.d y.d && Float.equal x.e y.e && Float.equal x.f y.f

let pp ppf t =
  Format.fprintf ppf "@[<1>[%g;@ %g;@ %g;@ %g;@ %g;@ %g]@]" t.a t.b t.c t.d
    t.e t.f
