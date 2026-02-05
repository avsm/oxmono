(* Test ppx_template with kind annotations for unboxed arrays *)
module Nu = Stdlib_upstream_compatible.Nativeint_u
module I64 = Stdlib_upstream_compatible.Int64_u

(* Create a nativeint# array using kind annotation - heap allocated *)
let make_nativeint_array size init : nativeint# array =
  (Core.Array.init [@kind word]) size ~f:(fun _ -> init)

(* Create an int64# array - heap allocated *)  
let make_int64_array size init : int64# array =
  (Core.Array.init [@kind bits64]) size ~f:(fun _ -> init)

(* Stack-allocated local unboxed array for temporary computation *)
let sum_with_local_buffer (data : int array) : int =
  let n = Core.Array.length data in
  (* Stack-allocated nativeint# array *)
  let local_ buffer : nativeint# array = 
    (Core.Array.init [@kind word]) n ~f:(fun i -> Nu.of_int data.(i)) in
  let mutable total = #0n in
  for i = 0 to n - 1 do
    total <- Nu.add total (Core.Array.unsafe_get buffer i)
  done;
  Nu.to_int total

(* Stack-allocated histogram using local unboxed array *)
let histogram_local (data : bytes) : int =
  let local_ counts : nativeint# array = (Core.Array.init [@kind word]) 256 ~f:(fun _ -> #0n) in
  for i = 0 to Bytes.length data - 1 do
    let idx = Char.code (Bytes.unsafe_get data i) in
    let cur = Core.Array.unsafe_get counts idx in
    Core.Array.unsafe_set counts idx (Nu.add cur #1n)
  done;
  (* Return count at position 'a' for testing *)
  Nu.to_int (Core.Array.unsafe_get counts (Char.code 'a'))

let () = 
  (* Test heap-allocated unboxed arrays *)
  let arr1 = make_nativeint_array 5 #42n in
  Stdlib.Printf.printf "nativeint# array length: %d\n" (Core.Array.length arr1);
  
  let arr2 = make_int64_array 5 #100L in
  Stdlib.Printf.printf "int64# array length: %d\n" (Core.Array.length arr2);
  
  (* Test local buffer sum *)
  let data = [| 1; 2; 3; 4; 5 |] in
  Stdlib.Printf.printf "sum_with_local_buffer: %d (expected 15)\n" (sum_with_local_buffer data);
  
  (* Test local histogram *)
  let text = Bytes.of_string "abracadabra" in
  Stdlib.Printf.printf "count of 'a': %d (expected 5)\n" (histogram_local text)
