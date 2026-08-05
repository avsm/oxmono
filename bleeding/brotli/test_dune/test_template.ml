open Base

(* Test creating local unboxed arrays using template instantiation *)
let sum_with_local_buffer n =
  (* Stack-allocated unboxed int64 array *)
  let local_ buffer : int64# array = (Array.init [@kind bits64]) n ~f:(fun i -> 
    Int64_u.of_int i) in
  let mutable total = #0L in
  for i = 0 to Array.length buffer - 1 do
    total <- Int64_u.add total (Array.unsafe_get buffer i)
  done;
  Int64_u.to_int64 total

(* Test with nativeint# - stack allocated *)
let sum_native n =
  let local_ buffer : nativeint# array = (Array.init [@kind word]) n ~f:(fun i ->
    Nativeint_u.of_int i) in
  let mutable total = #0n in
  for i = 0 to Array.length buffer - 1 do
    total <- Nativeint_u.add total (Array.unsafe_get buffer i)
  done;
  Nativeint_u.to_int_trunc total

(* Test Array.make with kind annotation *)
let make_nativeint_array size init =
  (Array.create [@kind word]) ~len:size init

let () = 
  Stdlib.Printf.printf "sum_with_local_buffer 10: %Ld\n" (sum_with_local_buffer 10);
  Stdlib.Printf.printf "sum_native 10: %d\n" (sum_native 10);
  let arr = make_nativeint_array 5 #42n in
  Stdlib.Printf.printf "arr length: %d\n" (Array.length arr)
