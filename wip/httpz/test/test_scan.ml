(* Differential tests for scans used in HTTP framing decisions.

   The corpus is biased towards the bytes that drive the branchy paths: CR, LF and SP;
   0x0C and 0x21, which are the neighbours that make the SWAR "haszero" trick report false
   positives; ASCII case pairs; the token characters either side of the tchar boundary;
   and high bytes, which must force the byte-wise fallback in [Httpz.Span.equal_caseless]. *)

open Base
module I16 = Stdlib_stable.Int16_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x
let failures = ref 0

let check name cond detail =
  if not cond
  then (
    Int.incr failures;
    if !failures <= 10 then Stdio.printf "FAIL [%s] %s\n" name (detail ()))
;;

let ref_find_cr buf ~pos ~limit =
  let mutable p = pos in
  while p < limit && Char.( <> ) (Bytes.unsafe_get buf p) '\r' do
    p <- p + 1
  done;
  p
;;

let ref_find_sp_or_cr buf ~pos ~limit =
  let mutable p = pos in
  let mutable stop = false in
  while (not stop) && p < limit do
    let c = Bytes.unsafe_get buf p in
    if Char.( = ) c ' ' || Char.( = ) c '\r' then stop <- true else p <- p + 1
  done;
  p
;;

let ref_is_tchar c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' -> true
  | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false
;;

let ref_skip_token buf ~pos ~limit =
  let mutable p = pos in
  while p < limit && ref_is_tchar (Bytes.unsafe_get buf p) do
    p <- p + 1
  done;
  p
;;

let ref_find_crlf buf ~pos ~len : #(int * bool) =
  if len - pos < 2
  then #(-1, false)
  else (
    let mutable p = pos in
    let mutable found_crlf = false in
    let mutable found_bare_cr = false in
    let mutable stop = false in
    while (not stop) && p < len do
      if Char.( = ) (Bytes.unsafe_get buf p) '\r'
      then
        if p + 1 >= len
        then (
          found_bare_cr <- true;
          stop <- true)
        else if Char.( = ) (Bytes.unsafe_get buf (p + 1)) '\n'
        then (
          found_crlf <- true;
          stop <- true)
        else (
          found_bare_cr <- true;
          p <- p + 1)
      else if Char.( = ) (Bytes.unsafe_get buf p) '\n'
      then (
        found_bare_cr <- true;
        p <- p + 1)
      else p <- p + 1
    done;
    #((if found_crlf then p else -1), found_bare_cr))
;;

let ref_equal buf ~off ~len s =
  String.length s = len
  &&
  let mutable i = 0 in
  let mutable eq = true in
  while eq && i < len do
    if Char.( <> ) (Bytes.unsafe_get buf (off + i)) (String.unsafe_get s i)
    then eq <- false
    else i <- i + 1
  done;
  eq
;;

let ref_equal_caseless buf ~off ~len s =
  String.length s = len
  &&
  let mutable i = 0 in
  let mutable eq = true in
  while eq && i < len do
    let b1 = Char.to_int (Bytes.unsafe_get buf (off + i)) in
    let b2 = Char.to_int (String.unsafe_get s i) in
    let lo = if b1 >= 65 && b1 <= 90 then b1 + 32 else b1 in
    if lo <> b2 then eq <- false else i <- i + 1
  done;
  eq
;;

let scan_alphabet =
  [| '\r'
   ; '\n'
   ; ' '
   ; '\t'
   ; '\012'
   ; '!'
   ; ':'
   ; 'a'
   ; 'Z'
   ; '9'
   ; '^'
   ; '~'
   ; '@'
   ; '('
   ; '-'
   ; '\000'
   ; '\128'
   ; '\255'
  |]
;;

let test_scans rng buf ~rounds =
  let cap = Bytes.length buf in
  let n = Array.length scan_alphabet in
  for _ = 1 to rounds do
    let len = 2 + Random.State.int rng 140 in
    for i = 0 to len - 1 do
      Bytes.unsafe_set buf i scan_alphabet.(Random.State.int rng n)
    done;
    (* Poison the bytes past [len]: a word load that reads beyond the declared length
       would then return a wrong index. *)
    for i = len to Int.min (len + 32) cap - 1 do
      Bytes.unsafe_set buf i (if Random.State.bool rng then '\r' else ' ')
    done;
    let pos = Random.State.int rng len in
    let dump () =
      Printf.sprintf "pos=%d len=%d %S" pos len (Bytes.To_string.sub buf ~pos:0 ~len)
    in
    let a = Httpz.Scan.find_cr buf ~pos ~limit:len in
    check "find_cr" (a = ref_find_cr buf ~pos ~limit:len) dump;
    let a = Httpz.Scan.find_sp_or_cr buf ~pos ~limit:len in
    check "find_sp_or_cr" (a = ref_find_sp_or_cr buf ~pos ~limit:len) dump;
    let a = Httpz.Buf_read.skip_token buf ~pos ~limit:len in
    check "skip_token" (a = ref_skip_token buf ~pos ~limit:len) dump;
    let #(ap, ab) =
      Httpz.Buf_read.find_crlf_check_bare_cr buf ~pos:(i16 pos) ~len:(i16 len)
    in
    let #(bp, bb) = ref_find_crlf buf ~pos ~len in
    check "find_crlf_check_bare_cr" (to_int ap = bp && Bool.( = ) ab bb) dump
  done
;;

let cmp_alphabet =
  [| 'a'; 'A'; 'z'; 'Z'; '0'; '-'; '_'; '@'; '['; '`'; '{'; '\128'; '\255' |]
;;

let test_comparisons rng buf ~rounds =
  let n = Array.length cmp_alphabet in
  for _ = 1 to rounds do
    let slen = Random.State.int rng 34 in
    let s = String.init slen ~f:(fun _ -> cmp_alphabet.(Random.State.int rng n)) in
    let off = Random.State.int rng 64 in
    (* Usually seed the buffer from [s] with random case flips so that matches are common;
       otherwise use unrelated bytes. *)
    let seeded = Random.State.int rng 4 <> 0 in
    for i = 0 to slen - 1 do
      let c =
        if not seeded
        then cmp_alphabet.(Random.State.int rng n)
        else (
          let c = s.[i] in
          if Random.State.bool rng
          then c
          else if Char.is_lowercase c
          then Char.uppercase c
          else Char.lowercase c)
      in
      Bytes.unsafe_set buf (off + i) c
    done;
    (* Vary the declared span length so the length guard is exercised too. *)
    let len = if Random.State.int rng 8 = 0 then Random.State.int rng 34 else slen in
    let sp = Httpz.Span.make ~off:(i16 off) ~len:(i16 len) in
    let dump () =
      Printf.sprintf "lit=%S span=%S" s (Bytes.To_string.sub buf ~pos:off ~len)
    in
    check
      "Span.equal"
      (Bool.( = ) (Httpz.Span.equal buf sp s) (ref_equal buf ~off ~len s))
      dump;
    check
      "Span.equal_caseless"
      (Bool.( = )
         (Httpz.Span.equal_caseless buf sp s)
         (ref_equal_caseless buf ~off ~len s))
      dump
  done
;;

let () =
  let rng = Random.State.make [| 20260804 |] in
  let buf = Bytes.make 4096 '\000' in
  test_scans rng buf ~rounds:100_000;
  test_comparisons rng buf ~rounds:100_000;
  if !failures > 0
  then (
    Stdio.printf "%d differential failures\n" !failures;
    Stdlib.exit 1);
  Stdio.printf "test_scan: all differential checks passed\n"
;;
