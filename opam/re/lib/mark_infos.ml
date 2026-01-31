open Import

type t = int iarray

let make marks =
  let len = 1 + List.fold_left ~f:(fun ma (i, _) -> max ma i) ~init:(-1) marks in
  let t = Array.make len (-1) in
  List.iter ~f:(fun (i, v) -> t.(i) <- v) marks;
  Iarray.unsafe_of_array__promise_no_mutation t
;;

let test t i = if 2 * i >= Iarray.length t then false else t.:(2 * i) <> -1

module Offset = struct
  type t = int

  let is_present t = t >= 0
  let get_no_check t = t
end

let start_offset t i =
  let start_i = 2 * i in
  if start_i + 1 >= Iarray.length t then -1 else t.:(start_i)
;;

let stop_offset t i =
  let stop_i = (2 * i) + 1 in
  if stop_i >= Iarray.length t then -1 else t.:(stop_i)
;;

let offset t i =
  let start_i = 2 * i in
  let stop_i = start_i + 1 in
  if stop_i >= Iarray.length t
  then None
  else (
    let start = t.:(start_i) in
    if start = -1
    then None
    else (
      let stop = t.:(stop_i) in
      Some (start, stop)))
;;

let iteri t ~f =
  for i = 0 to (Iarray.length t / 2) - 1 do
    let idx = 2 * i in
    let start = t.:(idx) in
    if start <> -1
    then (
      let stop = t.:(idx + 1) in
      f i start stop)
  done
;;
