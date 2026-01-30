open! Base
open Basement

let%test_unit "pattern-matching [Null]" =
  let x = Or_null_shim.Null in
  match x with
  | Or_null_shim.Null -> ()
  | Or_null_shim.This _ -> assert false
;;

let%test_unit "pattern-matching [This]" =
  let y = Or_null_shim.This 3 in
  match y with
  | Or_null_shim.This 3 -> ()
  | _ -> assert false
;;

external int_as_pointer : int -> int or_null = "%int_as_pointer"

let%test_unit "[Null] is represented as [0] pointer" =
  let n = int_as_pointer 0 in
  match n with
  | Or_null_shim.Null -> ()
  | _ -> assert false
;;

external int_as_int : int -> int or_null = "%opaque"

let%test_unit "[This x] and [x] share representation" =
  let m = int_as_int 5 in
  match m with
  | Or_null_shim.This 5 -> ()
  | Or_null_shim.This _ -> assert false
  | Or_null_shim.Null -> assert false
;;

let%test_unit "pattern-matching tuples containing [or_null]" =
  let x = Or_null_shim.Null, Or_null_shim.This "bar" in
  match x with
  | Or_null_shim.Null, Or_null_shim.This "foo" -> assert false
  | Or_null_shim.Null, Or_null_shim.This "bar" -> ()
  | _, Or_null_shim.This "bar" -> assert false
  | Or_null_shim.Null, _ -> assert false
  | _, _ -> assert false
;;

let%test_unit "functions" =
  let y a () = Or_null_shim.This a in
  let d = y 5 in
  match d () with
  | Or_null_shim.This 5 -> ()
  | _ -> assert false
;;

external to_bytes
  : ('a : value_or_null).
  'a -> int list -> bytes
  = "caml_output_value_to_bytes"

external from_bytes_unsafe
  : ('a : value_or_null).
  bytes -> int -> 'a
  = "caml_input_value_from_bytes"

let%test_unit ("marshaling [This]" [@tags "no-wasm"]) =
  let z = to_bytes (Or_null_shim.This "foo") [] in
  match from_bytes_unsafe z 0 with
  | Or_null_shim.This "foo" -> ()
  | Or_null_shim.This _ -> assert false
  | Or_null_shim.Null -> assert false
;;

let%test_unit ("marshaling [Null]" [@tags "no-wasm"]) =
  let w = to_bytes Or_null_shim.Null [] in
  match from_bytes_unsafe w 0 with
  | Or_null_shim.Null -> ()
  | Or_null_shim.This _ -> assert false
;;

external evil : 'a or_null -> 'a = "%opaque"

let%test_unit "[This x] and [x] share representation" =
  let e' = evil (Or_null_shim.This 4) in
  match e' with
  | 4 -> ()
  | _ -> assert false
;;

let%test_unit "Trying to create [This Or_null_shim.Null] results in [Null]" =
  let e = Or_null_shim.This (evil Or_null_shim.Null) in
  match e with
  | Or_null_shim.Null -> ()
  | Or_null_shim.This _ -> assert false
;;

let%test_unit "functions and pattern-matching" =
  let f a () =
    match a with
    | Or_null_shim.This x -> x ^ "bar"
    | Or_null_shim.Null -> "foo"
  in
  let g = f (Or_null_shim.This "xxx") in
  (match g () with
   | "xxxbar" -> ()
   | _ -> assert false);
  let h = f Or_null_shim.Null in
  match h () with
  | "foo" -> ()
  | _ -> assert false
;;

type 'a nref = { mutable v : 'a or_null }

let%test_unit "references containing [or_null]" =
  let x : string nref = { v = Or_null_shim.Null } in
  (match x.v with
   | Or_null_shim.Null -> ()
   | _ -> assert false);
  x.v <- Or_null_shim.This "foo";
  (match x.v with
   | Or_null_shim.This "foo" -> ()
   | _ -> assert false);
  x.v <- Or_null_shim.Null;
  match x.v with
  | Or_null_shim.Null -> ()
  | _ -> assert false
;;

external equal : ('a : value_or_null). 'a -> 'a -> bool = "%equal"
external compare : ('a : value_or_null). 'a -> 'a -> int = "%compare"

let%test_unit "equal" =
  assert (equal Or_null_shim.Null Or_null_shim.Null);
  assert (equal (Or_null_shim.This 4) (Or_null_shim.This 4));
  assert (not (equal Or_null_shim.Null (Or_null_shim.This 4)));
  assert (not (equal (Or_null_shim.This 8) Or_null_shim.Null));
  assert (not (equal (Or_null_shim.This 4) (Or_null_shim.This 5)))
;;

let%test_unit "compare" =
  assert (compare Or_null_shim.Null Or_null_shim.Null = 0);
  assert (compare (Or_null_shim.This 4) (Or_null_shim.This 4) = 0);
  assert (compare Or_null_shim.Null (Or_null_shim.This 4) < 0);
  assert (compare (Or_null_shim.This 8) Or_null_shim.Null > 0);
  assert (compare (Or_null_shim.This 4) (Or_null_shim.This 5) < 0);
  assert (compare (Or_null_shim.This "abc") (Or_null_shim.This "xyz") <> 0);
  assert (compare (Or_null_shim.This "xyz") (Or_null_shim.This "xyz") = 0)
;;

let%test_unit "obj_tag" = assert (Stdlib.Obj.tag (evil Or_null_shim.Null) = 1010)

external is_null : ('a : value_or_null). 'a -> bool = "%is_null"

let%test_unit "is_null" =
  assert (is_null Or_null_shim.Null);
  assert (not (is_null 4));
  assert (not (is_null "String"));
  assert (not (is_null (Or_null_shim.This 0)))
;;
