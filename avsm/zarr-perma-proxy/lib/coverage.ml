(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { start : int; stop : int }

let v start stop =
  if start < 0 || stop < start then invalid_arg "Coverage: invalid interval";
  { start; stop }

let merge ranges r =
  if r.start = r.stop then ranges else
  let rec loop acc r = function
    | [] -> List.rev (r :: acc)
    | x :: xs when x.stop < r.start -> loop (x :: acc) r xs
    | x :: xs when r.stop < x.start -> List.rev_append acc (r :: x :: xs)
    | x :: xs -> loop acc (v (min x.start r.start) (max x.stop r.stop)) xs
  in
  loop [] r ranges

let missing ranges r =
  let rec loop pos acc = function
    | _ when pos >= r.stop -> List.rev acc
    | [] -> List.rev (v pos r.stop :: acc)
    | x :: xs when x.stop <= pos -> loop pos acc xs
    | x :: _ when x.start >= r.stop -> List.rev (v pos r.stop :: acc)
    | x :: xs ->
        let acc = if pos < x.start then v pos x.start :: acc else acc in
        loop (max pos x.stop) acc xs
  in
  loop r.start [] ranges

let covers ranges r = missing ranges r = []

let offset_jsont =
  let limit = min 9007199254740991. (float_of_int max_int) in
  let valid n = Float.is_finite n && Float.is_integer n
      && n >= 0. && n <= limit in
  Jsont.map ~kind:"byte offset"
    ~dec:(fun n ->
      if not (valid n) then
        Jsont.Error.msg Jsont.Meta.none "Expected an exact nonnegative byte offset";
      int_of_float n)
    ~enc:(fun n ->
      let f = float_of_int n in
      if n < 0 || not (valid f) then
        Jsont.Error.msg Jsont.Meta.none "Byte offset exceeds JSON's exact range";
      f)
    Jsont.number

let validate r =
  if r.stop < r.start then
    Jsont.Error.msg Jsont.Meta.none "Interval stop precedes its start"

let jsont =
  let open Jsont.Object in
  map ~kind:"byte interval" (fun start stop -> { start; stop })
  |> mem "start" offset_jsont ~enc:(fun r -> r.start)
  |> mem "stop" offset_jsont ~enc:(fun r -> r.stop)
  |> error_unknown
  |> finish
  |> Jsont.iter ~dec:validate ~enc:validate
