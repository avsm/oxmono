open Brr

let proj4 = Jv.get (Window.to_jv G.window) "proj4"

type ctx = unit

let ctx () = ()

module CRS = struct
  type t = Jv.t

  let of_string ?ctx:_ s = Jv.new' proj4 [| Jv.of_string s |]
  let of_wkt ?options:_ ?ctx:_ s = Jv.new' proj4 [| Jv.of_string s |]

  let name v =
    let oproj = Jv.get v "oProj" in
    let name = Jv.find oproj "name" in
    Option.map Jv.to_string name
end

type direction = Forward | Inverse | Ident

module Transformation = struct
  type t = Jv.t

  let normalize_for_visualization ?ctx:_ v = v

  let of_string ?area:_ ?ctx:_ ~src dst =
    Jv.new' proj4 [| Jv.of_string src; Jv.of_string dst |]

  let of_crs ?area:_ ?options:_ ?ctx:_ ~src dst = Jv.new' proj4 [| src; dst |]

  let transform ?(direction = Forward) transform coord1 =
    match direction with
    | Forward -> Jv.call transform "forward" [| coord1 |]
    | Inverse -> Jv.call transform "inverse" [| coord1 |]
    | Ident -> coord1
end

type area = unit

module Coord = struct
  type t = Jv.t

  let make ~x ~y ~z ~t =
    let arr = Jv.Jarray.create 4 in
    Jv.Jarray.set arr 0 (Jv.of_float x);
    Jv.Jarray.set arr 1 (Jv.of_float y);
    Jv.Jarray.set arr 2 (Jv.of_float z);
    Jv.Jarray.set arr 3 (Jv.of_float t);
    Jv.call proj4 "toPoint" [| arr |]

  let v jv =
    let x = Jv.Float.get jv "x" in
    let y = Jv.Float.get jv "y" in
    let z = Jv.Float.get jv "z" in
    let m = Jv.Float.get jv "m" in
    [| x; y; z; m |]

  let x jv = Jv.Float.get jv "x"
  let y jv = Jv.Float.get jv "y"
end
