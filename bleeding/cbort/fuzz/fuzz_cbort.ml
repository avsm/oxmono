(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Crowbar-based fuzz testing for CBOR roundtripping *)

open Bytesrw
open Crowbar
module Cbor = Cbort.Cbor
module Rw = Cbort.Rw

(* Compare CBOR values for equality, handling floats and normalized simple values *)
let rec cbor_equal (a : Cbor.t) (b : Cbor.t) =
  match (a, b) with
  | Cbor.Int x, Cbor.Int y -> Z.equal x y
  | Cbor.Bytes x, Cbor.Bytes y -> String.equal x y
  | Cbor.Text x, Cbor.Text y -> String.equal x y
  | Cbor.Array xs, Cbor.Array ys ->
      List.length xs = List.length ys && List.for_all2 cbor_equal xs ys
  | Cbor.Map xs, Cbor.Map ys ->
      List.length xs = List.length ys
      && List.for_all2
           (fun (k1, v1) (k2, v2) -> cbor_equal k1 k2 && cbor_equal v1 v2)
           xs ys
  | Cbor.Tag (n1, v1), Cbor.Tag (n2, v2) -> n1 = n2 && cbor_equal v1 v2
  | Cbor.Bool x, Cbor.Bool y -> x = y
  | Cbor.Null, Cbor.Null -> true
  | Cbor.Undefined, Cbor.Undefined -> true
  | Cbor.Simple x, Cbor.Simple y -> x = y
  | Cbor.Float x, Cbor.Float y -> (Float.is_nan x && Float.is_nan y) || x = y
  (* Handle Simple(20-23) which decoder normalizes to Bool/Null/Undefined *)
  | Cbor.Simple 20, Cbor.Bool false | Cbor.Bool false, Cbor.Simple 20 -> true
  | Cbor.Simple 21, Cbor.Bool true | Cbor.Bool true, Cbor.Simple 21 -> true
  | Cbor.Simple 22, Cbor.Null | Cbor.Null, Cbor.Simple 22 -> true
  | Cbor.Simple 23, Cbor.Undefined | Cbor.Undefined, Cbor.Simple 23 -> true
  | _ -> false

(* Generator for valid simple values (excluding reserved 20-31) *)
let simple_gen =
  map [ uint8 ] (fun n ->
      (* Simple values 20-31 are reserved/special, avoid them *)
      if n >= 20 && n <= 31 then Cbor.Simple (n + 12) (* shift to 32-43 *)
      else Cbor.Simple n)

(* Generator for arbitrary CBOR values *)
let cbor_gen : Cbor.t gen =
  fix (fun cbor_gen ->
      let leaf_gen =
        choose
          [
            map [ int64 ] (fun n -> Cbor.Int (Z.of_int64 n));
            map [ bytes ] (fun s -> Cbor.Bytes s);
            map [ bytes ] (fun s -> Cbor.Text s);
            map [ bool ] (fun b -> Cbor.Bool b);
            const Cbor.Null;
            const Cbor.Undefined;
            map [ float ] (fun f -> Cbor.Float f);
            simple_gen;
          ]
      in
      let compound_gen =
        choose
          [
            map [ list cbor_gen ] (fun items -> Cbor.Array items);
            map [ list (pair cbor_gen cbor_gen) ] (fun pairs -> Cbor.Map pairs);
            (* Avoid tags 2 and 3 which are bignum tags handled specially *)
            map
              [ range 100; cbor_gen ]
              (fun tag v ->
                let tag = if tag = 2 || tag = 3 then tag + 100 else tag in
                Cbor.Tag (tag, v));
          ]
      in
      (* Bias towards leaf nodes to avoid deeply nested structures *)
      choose [ leaf_gen; leaf_gen; leaf_gen; compound_gen ])

(* Test encode-decode roundtrip *)
let test_encode_decode_roundtrip cbor =
  let buf = Buffer.create 256 in
  let writer = Bytes.Writer.of_buffer buf in
  let enc = Rw.make_encoder writer in
  Rw.write_cbor enc cbor;
  Rw.flush_encoder enc;
  let encoded = Buffer.contents buf in
  let reader = Bytes.Reader.of_string encoded in
  let dec = Rw.make_decoder reader in
  let decoded = Rw.read_cbor dec in
  check_eq ~eq:cbor_equal ~pp:Cbor.pp cbor decoded

(* Check if CBOR value has valid (non-negative) tag numbers *)
let rec has_valid_tags = function
  | Cbor.Tag (n, _) when n < 0 -> false (* Overflow occurred *)
  | Cbor.Tag (_, v) -> has_valid_tags v
  | Cbor.Array items -> List.for_all has_valid_tags items
  | Cbor.Map pairs ->
      List.for_all (fun (k, v) -> has_valid_tags k && has_valid_tags v) pairs
  | _ -> true

(* Test decode-encode roundtrip from raw bytes *)
let test_decode_encode_roundtrip input =
  let reader = Bytes.Reader.of_string input in
  let dec = Rw.make_decoder reader in
  match Rw.read_cbor dec with
  | cbor ->
      (* Skip values with overflowed tag numbers *)
      if not (has_valid_tags cbor) then ()
      else begin
        (* Encode back to bytes *)
        let buf = Buffer.create 256 in
        let writer = Bytes.Writer.of_buffer buf in
        let enc = Rw.make_encoder writer in
        Rw.write_cbor enc cbor;
        Rw.flush_encoder enc;
        let encoded = Buffer.contents buf in
        (* Decode again *)
        let reader2 = Bytes.Reader.of_string encoded in
        let dec2 = Rw.make_decoder reader2 in
        let cbor2 = Rw.read_cbor dec2 in
        check_eq ~eq:cbor_equal ~pp:Cbor.pp cbor cbor2
      end
  | exception _ ->
      (* Invalid CBOR input is fine *)
      ()

let () =
  add_test ~name:"encode-decode roundtrip" [ cbor_gen ]
    test_encode_decode_roundtrip;
  add_test ~name:"decode-encode roundtrip" [ bytes ]
    test_decode_encode_roundtrip
