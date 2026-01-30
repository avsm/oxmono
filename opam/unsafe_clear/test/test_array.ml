open! Core
open Unsafe_clear

let%expect_test "clear pointer (value)" =
  let arr = [| 100 |] in
  print_s [%sexp (arr : int array)];
  [%expect {| (100) |}];
  Array.unsafe_clear_if_pointer arr 0;
  print_s [%sexp (arr : int array)];
  [%expect {| (100) |}];
  let x = Some (Sys.opaque_identity 0) in
  Stdlib.Gc.finalise (fun _ -> print_endline "x out of scope") x;
  let arr = [| x |] in
  Stdlib.Gc.full_major ();
  Stdlib.Gc.full_major ();
  [%expect {| |}];
  Array.unsafe_clear_if_pointer arr 0;
  Stdlib.Gc.full_major ();
  Stdlib.Gc.full_major ();
  [%expect {| x out of scope |}]
;;

let%expect_test "clear pointer (immediate64)" =
  let arr = [| 100 |] in
  print_s [%sexp (arr : int array)];
  [%expect {| (100) |}];
  (Array.unsafe_clear_if_pointer [@kind immediate64]) arr 0;
  print_s [%sexp (arr : int array)];
  [%expect {| (100) |}];
  (* [immediate] version skips checking whether this is actually a pointer *)
  let arr = [| Some 0 |] in
  print_s [%sexp (arr : int option array)];
  [%expect {| ((0)) |}];
  let magic_array : int array = Obj.magic arr in
  (Array.unsafe_clear_if_pointer [@kind immediate]) magic_array 0;
  print_s [%sexp (arr : int option array)];
  [%expect {| ((0)) |}]
;;

let%expect_test "clear pointer (value_or_null & value_or_null)" =
  let arr = [| #(1, 2) |] in
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int * int))];
  [%expect {| (1 2) |}];
  (Array.unsafe_clear_if_pointer [@kind value_or_null & value_or_null]) arr 0;
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int * int))];
  [%expect {| (1 2) |}];
  let x = Some (Sys.opaque_identity 2) in
  Stdlib.Gc.finalise (fun _ -> print_endline "x out of scope") x;
  let y = #(1, x) in
  let arr = [| y |] in
  Stdlib.Gc.full_major ();
  Stdlib.Gc.full_major ();
  [%expect {| |}];
  (Array.unsafe_clear_if_pointer [@kind value_or_null & value_or_null]) arr 0;
  Stdlib.Gc.full_major ();
  Stdlib.Gc.full_major ();
  [%expect {| x out of scope |}];
  (* non-pointer part also cleared *)
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int * int option))];
  [%expect {| (0 ()) |}];
  (* works with [Null] *)
  let arr = [| #(1, Null) |] in
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int * int or_null))];
  [%expect {| (1 ()) |}];
  (Array.unsafe_clear_if_pointer [@kind value_or_null & value_or_null]) arr 0;
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int * int or_null))];
  [%expect {| (0 (0)) |}]
;;

let%expect_test "clear pointer (immediate64 & immediate64)" =
  let arr = [| #(1, 2) |] in
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int * int))];
  [%expect {| (1 2) |}];
  (Array.unsafe_clear_if_pointer [@kind immediate64 & immediate64]) arr 0;
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int * int))];
  [%expect {| (1 2) |}];
  (* [immediate] version skips checking whether this is actually a pointer *)
  let arr = [| #(Some 0, Some 1) |] in
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int option * int option))];
  [%expect {| ((0) (1)) |}];
  let magic_array = (Obj.magic arr : #(int * int) array) in
  (Array.unsafe_clear_if_pointer [@kind immediate64 & immediate64]) magic_array 0;
  print_s [%sexp (Core.Array.unsafe_get arr 0 : #(int option * int option))];
  [%expect {| ((0) (1)) |}]
;;
