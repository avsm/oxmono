(* Test using Int64_u.Array for unboxed arrays *)

(* Create an int64# array using Int64_u.Array *)
let int64_arr : int64# array =
  Int64_u.Array.create_uninitialized ~len:64

(* Initialize the array *)
let () =
  for i = 0 to 63 do
    Int64_u.Array.set int64_arr i (Int64_u.of_int i)
  done

let () =
  Int64_u.Array.set int64_arr 0 #42L;
  let v = Int64_u.Array.get int64_arr 0 in
  Printf.printf "int64# array test: %Ld\n" (Int64_u.to_int64 v);

  (* Verify the value *)
  assert (Int64_u.to_int64 v = 42L);
  Printf.printf "Test passed!\n"
