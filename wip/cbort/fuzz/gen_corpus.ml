(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Generate seed corpus from hex strings *)

let hex_decode s =
  let len = String.length s in
  if len mod 2 <> 0 then failwith "Invalid hex string";
  let out = Bytes.create (len / 2) in
  for i = 0 to (len / 2) - 1 do
    let hi =
      match s.[i * 2] with
      | '0' .. '9' as c -> Char.code c - Char.code '0'
      | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
      | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
      | _ -> failwith "Invalid hex character"
    in
    let lo =
      match s.[(i * 2) + 1] with
      | '0' .. '9' as c -> Char.code c - Char.code '0'
      | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
      | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
      | _ -> failwith "Invalid hex character"
    in
    Bytes.set out i (Char.chr ((hi lsl 4) lor lo))
  done;
  Bytes.unsafe_to_string out

(* RFC 8949 Appendix A test vectors - hex encoded *)
let seeds =
  [
    "00";
    (* 0 *)
    "01";
    (* 1 *)
    "17";
    (* 23 *)
    "1818";
    (* 24 *)
    "1903e8";
    (* 1000 *)
    "1a000f4240";
    (* 1000000 *)
    "1b000000e8d4a51000";
    (* 1000000000000 *)
    "1bffffffffffffffff";
    (* max uint64 *)
    "c249010000000000000000";
    (* bignum 2^64 *)
    "20";
    (* -1 *)
    "3863";
    (* -100 *)
    "3bffffffffffffffff";
    (* -2^64 *)
    "c349010000000000000000";
    (* negative bignum *)
    "f90000";
    (* 0.0 half *)
    "f93c00";
    (* 1.0 half *)
    "fb3ff199999999999a";
    (* 1.1 double *)
    "f97c00";
    (* Infinity half *)
    "f97e00";
    (* NaN half *)
    "f4";
    (* false *)
    "f5";
    (* true *)
    "f6";
    (* null *)
    "f7";
    (* undefined *)
    "40";
    (* empty bytes *)
    "4401020304";
    (* bytes *)
    "60";
    (* empty text *)
    "6161";
    (* "a" *)
    "6449455446";
    (* "IETF" *)
    "80";
    (* empty array *)
    "83010203";
    (* [1,2,3] *)
    "8301820203820405";
    (* [1,[2,3],[4,5]] *)
    "a0";
    (* empty map *)
    "a201020304";
    (* {1:2,3:4} *)
    "a26161016162820203";
    (* {"a":1,"b":[2,3]} *)
    "c074323031332d30332d32315432303a30343a30305a";
    (* tagged date *)
    "d74401020304";
    (* tagged bytes *)
  ]

let () =
  (* Create corpus directory if it doesn't exist *)
  (try Unix.mkdir "corpus" 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iteri
    (fun i hex ->
      let data = hex_decode hex in
      let filename = Printf.sprintf "corpus/seed_%03d" i in
      let oc = open_out_bin filename in
      output_string oc data;
      close_out oc;
      Printf.printf "Created %s (%d bytes)\n" filename (String.length data))
    seeds
