(* test_tchar.ml - exhaustive check of the vectorised token-character scan.

   {!Httpz.Buf_read.skip_token} classifies bytes with two [pshufb] nibble
   lookups (see [scan.simd.ml]). The random corpus in [test_scan.ml] draws
   from eighteen bytes, which cannot show that all 256 code points are
   classified the same way as the scalar table. This checks every byte value,
   at every alignment within a vector block, against {!Scan_portable}. *)

open Base

module I16 = Stdlib_stable.Int16_u
module Char_u = Stdlib_stable.Char_u

let[@inline always] i16 x = I16.of_int x
let failures = ref 0

let check name cond detail =
  if not cond
  then begin
    Int.incr failures;
    if !failures <= 20 then Stdio.printf "FAIL [%s] %s\n" name (detail ())
  end
;;

(* The RFC 7230 [tchar] set, spelled out independently of the implementation
   so this is a genuine differential reference rather than a restatement. *)
let ref_is_tchar c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' -> true
  | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false
;;

let ref_skip_token buf ~pos ~limit =
  let p = ref pos in
  while !p < limit && ref_is_tchar (Bytes.unsafe_get buf !p) do
    Int.incr p
  done;
  !p
;;

(* Every byte value, placed at every offset in [0, 32), inside a run of token
   characters long enough to force the vector path. A wrong classification of
   byte [c] shows up as a token that ends in the wrong place. *)
let test_all_bytes () =
  let cap = 96 in
  let buf = Bytes.make cap 'a' in
  for c = 0 to 255 do
    for pos = 0 to 31 do
      Bytes.fill buf ~pos:0 ~len:cap 'a';
      Bytes.unsafe_set buf pos (Char.of_int_exn c);
      let expect = ref_skip_token buf ~pos:0 ~limit:cap in
      let got = Httpz.Buf_read.skip_token buf ~pos:0 ~limit:cap in
      check
        "skip_token/byte"
        (got = expect)
        (fun () -> Printf.sprintf "byte=0x%02x pos=%d got=%d expect=%d" c pos got expect)
    done
  done
;;

(* Runs of every length from 0 to 40, so the boundary between the vector body
   and the scalar tail is crossed at every alignment. *)
let test_run_lengths () =
  let cap = 96 in
  let buf = Bytes.make cap '\000' in
  for start = 0 to 20 do
    for run = 0 to 40 do
      Bytes.fill buf ~pos:0 ~len:cap ':';
      Bytes.fill buf ~pos:start ~len:run 'x';
      let expect = ref_skip_token buf ~pos:start ~limit:cap in
      let got = Httpz.Buf_read.skip_token buf ~pos:start ~limit:cap in
      check
        "skip_token/run"
        (got = expect)
        (fun () -> Printf.sprintf "start=%d run=%d got=%d expect=%d" start run got expect)
    done
  done
;;

(* A vector load must never look past [limit], even when the bytes beyond it
   are token characters that would extend the run. *)
let test_limit_respected () =
  let cap = 96 in
  let buf = Bytes.make cap 'a' in
  for limit = 0 to cap do
    let got = Httpz.Buf_read.skip_token buf ~pos:0 ~limit in
    check
      "skip_token/limit"
      (got = limit)
      (fun () -> Printf.sprintf "limit=%d got=%d" limit got)
  done
;;

(* {!Httpz.Span.split_on_char} drives query-parameter lookup and now scans
   eight bytes at a time. The SWAR "haszero" trick marks a byte whose XOR with
   the needle is 0x01 when it follows a genuine match, so needles adjacent to
   0x01 in the corpus below are the interesting cases. *)
let split_alphabet =
  [| '&'; '='; '?'; 'a'; '\000'; '\001'; '\002'; '\255'; '%'; '+'; '/' |]
;;

let ref_find_char buf ~off ~len c =
  let i = ref 0 in
  let found = ref (-1) in
  while !found < 0 && !i < len do
    if Char.equal (Bytes.unsafe_get buf (off + !i)) c then found := !i else Int.incr i
  done;
  !found
;;

let test_split_on_char rng =
  let cap = 256 in
  let buf = Bytes.make cap '\000' in
  let n = Array.length split_alphabet in
  for _ = 1 to 200_000 do
    let off = Random.State.int rng 24 in
    let len = Random.State.int rng 100 in
    for i = 0 to cap - 1 do
      Bytes.unsafe_set buf i split_alphabet.(Random.State.int rng n)
    done;
    let needle = split_alphabet.(Random.State.int rng n) in
    let sp = Httpz.Span.make ~off:(i16 off) ~len:(i16 len) in
    let #(before, after) = Httpz.Span.split_on_char buf sp (Char_u.of_char needle) in
    let expect = ref_find_char buf ~off ~len needle in
    let dump () =
      Printf.sprintf
        "off=%d len=%d needle=%C %S"
        off
        len
        needle
        (Bytes.To_string.sub buf ~pos:off ~len)
    in
    if expect < 0
    then begin
      (* No match: everything lands in [before], [after] is empty at the end. *)
      check "split/nomatch_before" (Httpz.Span.len before = len) dump;
      check "split/nomatch_after" (Httpz.Span.len after = 0) dump
    end
    else begin
      check "split/before_len" (Httpz.Span.len before = expect) dump;
      check "split/before_off" (Httpz.Span.off before = off) dump;
      check "split/after_off" (Httpz.Span.off after = off + expect + 1) dump;
      check "split/after_len" (Httpz.Span.len after = len - expect - 1) dump
    end
  done
;;

let () =
  test_all_bytes ();
  test_run_lengths ();
  test_limit_respected ();
  test_split_on_char (Random.State.make [| 20260805 |]);
  if !failures > 0
  then begin
    Stdio.printf "%d differential failures\n" !failures;
    Stdlib.exit 1
  end;
  Stdio.printf
    "test_tchar: 256 byte values classified correctly; split_on_char matches \
     reference\n"
;;
