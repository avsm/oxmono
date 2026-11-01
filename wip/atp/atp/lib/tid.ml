(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string (* 13 character TID string *)

(* Base32-like charset for TID encoding *)
let charset = "234567abcdefghijklmnopqrstuvwxyz"

type error =
  [ `Tid_invalid_length of int
  | `Tid_invalid_format of string
  | `Tid_timestamp_out_of_range
  | `Tid_clockid_out_of_range of int ]

let pp_error ppf = function
  | `Tid_invalid_length n -> Fmt.pf ppf "invalid TID length: %d (expected 13)" n
  | `Tid_invalid_format s -> Fmt.pf ppf "invalid TID format: %s" s
  | `Tid_timestamp_out_of_range ->
      Fmt.string ppf "TID timestamp out of range [0, 2^53)"
  | `Tid_clockid_out_of_range n ->
      Fmt.pf ppf "TID clockid out of range: %d (expected 0-1023)" n

type Eio.Exn.err += E of error

let () = Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)
let raise_error e = Eio_error.raise_ (E e)

(* Base32-like encoding *)
let s32_encode n =
  let rec encode acc n =
    if Int64.compare n 0L <= 0 then acc
    else
      let idx = Int64.to_int (Int64.rem n 32L) in
      let c = String.make 1 charset.[idx] in
      encode (c ^ acc) (Int64.unsigned_div n 32L)
  in
  if Int64.compare n 0L = 0 then "2" else encode "" n

let s32_decode s =
  let rec decode acc i =
    if i >= String.length s then acc
    else
      let c = s.[i] in
      match String.index_opt charset c with
      | None -> raise_error (`Tid_invalid_format s)
      | Some idx ->
          let acc = Int64.add (Int64.mul acc 32L) (Int64.of_int idx) in
          decode acc (i + 1)
  in
  decode 0L 0

(* Pad string with '2' (represents 0) on the left *)
let pad_left s len =
  let slen = String.length s in
  if slen >= len then s else String.make (len - slen) '2' ^ s

(* Validation *)
let is_valid_char c = String.contains charset c

let is_valid s =
  String.length s = 13
  && String.for_all is_valid_char s
  &&
  (* First character must be in valid range for timestamp *)
  let first = s.[0] in
  first >= '2' && first <= 'j'

let of_string s =
  try
    let len = String.length s in
    if len <> 13 then raise_error (`Tid_invalid_length len);
    if not (is_valid s) then raise_error (`Tid_invalid_format s);
    s
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "parsing TID from %S" s

let of_string_opt s = if is_valid s then Some s else None
let to_string t = t

(* Creation *)
let of_timestamp_us ?(clockid = Random.int 1024) timestamp =
  if
    Int64.compare timestamp 0L < 0
    || Int64.compare timestamp (Int64.shift_left 1L 53) >= 0
  then raise_error `Tid_timestamp_out_of_range;
  if clockid < 0 || clockid > 1023 then
    raise_error (`Tid_clockid_out_of_range clockid);
  let ts_part = pad_left (s32_encode timestamp) 11 in
  let clk_part = pad_left (s32_encode (Int64.of_int clockid)) 2 in
  ts_part ^ clk_part

let of_timestamp_ms ?(clockid = Random.int 1024) timestamp =
  let us =
    Int64.add (Int64.mul timestamp 1000L) (Int64.of_int (Random.int 1000))
  in
  of_timestamp_us ~clockid us

let now (clock : _ Eio.Time.clock) =
  let time = Eio.Time.now clock in
  let us = Int64.of_float (time *. 1_000_000.0) in
  of_timestamp_us us

let create () =
  let time = Unix.gettimeofday () in
  let us = Int64.of_float (time *. 1_000_000.0) in
  of_timestamp_us us

(* Accessors *)
let to_timestamp_us t =
  let ts_str = String.sub t 0 11 in
  let clk_str = String.sub t 11 2 in
  let timestamp = s32_decode ts_str in
  let clockid = Int64.to_int (s32_decode clk_str) in
  (timestamp, clockid)

let to_timestamp_ms t =
  let timestamp, clockid = to_timestamp_us t in
  (Int64.div timestamp 1000L, clockid)

let timestamp_us t = fst (to_timestamp_us t)
let clock_id t = snd (to_timestamp_us t)

(* Comparison *)
let equal = String.equal
let compare = String.compare
let hash = Hashtbl.hash

(* Pretty printing *)
let pp ppf t = Fmt.string ppf t
