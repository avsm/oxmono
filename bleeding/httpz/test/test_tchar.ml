(* Exhaustive differential checks for token and delimiter scans. *)

open Base
module I16 = Stdlib_stable.Int16_u
module Char_u = Stdlib_stable.Char_u

let[@inline always] i16 x = I16.of_int x
let failures = ref 0

let check name cond detail =
  if not cond
  then (
    Int.incr failures;
    if !failures <= 20 then Stdio.printf "FAIL [%s] %s\n" name (detail ()))
;;

(* The RFC 9110 [tchar] set, spelled out independently of the implementation so this is a
   genuine differential reference rather than a restatement. *)
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

(* Every byte value appears at every offset in [0, 32) inside a long token run. *)
let test_all_bytes () =
  let cap = 96 in
  let buf = Bytes.make cap 'a' in
  for c = 0 to 255 do
    for pos = 0 to 31 do
      Bytes.fill buf ~pos:0 ~len:cap 'a';
      Bytes.unsafe_set buf pos (Char.of_int_exn c);
      let expect = ref_skip_token buf ~pos:0 ~limit:cap in
      let got = Httpz.Buf_read.skip_token buf ~pos:0 ~limit:cap in
      check "skip_token/byte" (got = expect) (fun () ->
        Printf.sprintf "byte=0x%02x pos=%d got=%d expect=%d" c pos got expect)
    done
  done
;;

(* Runs from length 0 through 40 exercise each word boundary and alignment. *)
let test_run_lengths () =
  let cap = 96 in
  let buf = Bytes.make cap '\000' in
  for start = 0 to 20 do
    for run = 0 to 40 do
      Bytes.fill buf ~pos:0 ~len:cap ':';
      Bytes.fill buf ~pos:start ~len:run 'x';
      let expect = ref_skip_token buf ~pos:start ~limit:cap in
      let got = Httpz.Buf_read.skip_token buf ~pos:start ~limit:cap in
      check "skip_token/run" (got = expect) (fun () ->
        Printf.sprintf "start=%d run=%d got=%d expect=%d" start run got expect)
    done
  done
;;

(* A word load must not let token characters beyond [limit] extend the run. *)
let test_limit_respected () =
  let cap = 96 in
  let buf = Bytes.make cap 'a' in
  for limit = 0 to cap do
    let got = Httpz.Buf_read.skip_token buf ~pos:0 ~limit in
    check "skip_token/limit" (got = limit) (fun () ->
      Printf.sprintf "limit=%d got=%d" limit got)
  done
;;

let ref_is_field_value c =
  let code = Char.to_int c in
  code = 0x09 || (code >= 0x20 && code <> 0x7f)
;;

let ref_is_qdtext c =
  let code = Char.to_int c in
  code = 0x09
  || code = 0x20
  || code = 0x21
  || (code >= 0x23 && code <= 0x5b)
  || (code >= 0x5d && code <= 0x7e)
  || code >= 0x80
;;

let test_field_byte_classes () =
  for code = 0 to 255 do
    let c = Char.of_int_exn code in
    let detail () = Printf.sprintf "byte=0x%02x" code in
    check
      "field-value-byte"
      (Bool.equal (Httpz.Buf_read.is_field_value_char (Char_u.of_char c)) (ref_is_field_value c))
      detail;
    check
      "quoted-pair-byte"
      (Bool.equal (Httpz.Buf_read.is_quoted_pair_char (Char_u.of_char c)) (ref_is_field_value c))
      detail;
    check
      "qdtext-byte"
      (Bool.equal (Httpz.Buf_read.is_qdtext_char (Char_u.of_char c)) (ref_is_qdtext c))
      detail
  done
;;

let test_header_syntax () =
  let module S = Httpz.Header.Syntax in
  check "token" (S.is_token "keep-alive") (fun () -> "ordinary token rejected");
  check "token-empty" (not (S.is_token "")) (fun () -> "empty token accepted");
  check "token-sub" (S.is_token_sub "xkeep-alive!" ~pos:1 ~len:10) (fun () ->
    "valid interior token rejected");
  check "token-sub-delimiter" (not (S.is_token_sub "xa/by" ~pos:1 ~len:3)) (fun () ->
    "slash accepted in token slice");
  List.iter
    [ -1, 1; 0, -1; 3, 2; Int.max_value, 1; 1, Int.max_value ]
    ~f:(fun (pos, len) ->
      check "token-sub-bounds" (not (S.is_token_sub "abc" ~pos ~len)) (fun () ->
        Printf.sprintf "pos=%d len=%d" pos len));
  check
    "field-value"
    (S.is_field_value "visible\t\255" && not (S.is_field_value "bad\r"))
    (fun () -> "owned field value classification failed");
  let surrounded = "x\"a\\\"b\"y" in
  check
    "quoted-sub"
    (S.is_quoted_string_sub surrounded ~pos:1 ~len:6)
    (fun () -> "valid interior quoted string rejected");
  List.iter
    [ ""; "\""; "\"a\"b\""; "\"a\\\rb\""; "\"a\\\127b\""; "\"a\\\"" ]
    ~f:(fun value ->
      check
        "quoted-invalid"
        (not (S.is_quoted_string_sub value ~pos:0 ~len:(String.length value)))
        (fun () -> Printf.sprintf "%S" value));
  List.iter
    [ -1, 2; 0, -1; 2, 2; Int.max_value, 2; 1, Int.max_value ]
    ~f:(fun (pos, len) ->
      check
        "quoted-sub-bounds"
        (not (S.is_quoted_string_sub "\"a\"" ~pos ~len))
        (fun () -> Printf.sprintf "pos=%d len=%d" pos len));
  let raw = "a\t\255\"\\z" in
  let quoted = S.quote_string raw in
  check "quoted-roundtrip" (Option.equal String.equal (S.unquote_string quoted) (Some raw))
    (fun () -> Printf.sprintf "raw=%S quoted=%S" raw quoted);
  check "strict-unquote" (Option.is_none (S.unquote_string "\"a\rb\"")) (fun () ->
    "strict unquote accepted CR");
  let rejected_control =
    try
      ignore (S.quote_string "a\000b");
      false
    with Invalid_argument _ -> true
  in
  check "quote-control" rejected_control (fun () -> "quote_string accepted NUL")
;;

let test_qvalues () =
  let q = Httpz.Header.Syntax.qvalue_sub in
  List.iter
    [ "0", 0; "0.", 0; "0.1", 100; "0.01", 10; "0.001", 1
    ; "0.999", 999; "1", 1000; "1.", 1000; "1.0", 1000; "1.000", 1000 ]
    ~f:(fun (value, expect) ->
      let got = q value ~pos:0 ~len:(String.length value) in
      check "qvalue-valid" (got = expect) (fun () ->
        Printf.sprintf "%S got=%d expect=%d" value got expect));
  let surrounded = "x0.123y" in
  check "qvalue-sub" (q surrounded ~pos:1 ~len:5 = 123) (fun () ->
    "interior qvalue slice rejected");
  List.iter
    [ ""; "00"; "2"; "1.001"; "0.0000"; "0.a"; ".5"; "+0.5"; "-0.5" ]
    ~f:(fun value ->
      let got = q value ~pos:0 ~len:(String.length value) in
      check "qvalue-invalid" (got = -1) (fun () ->
        Printf.sprintf "%S got=%d" value got));
  List.iter
    [ -1, 1; 0, -1; 3, 1; Int.max_value, 1; 1, Int.max_value ]
    ~f:(fun (pos, len) ->
      let got = q "0.5" ~pos ~len in
      check "qvalue-bounds" (got = -1) (fun () ->
        Printf.sprintf "pos=%d len=%d got=%d" pos len got))
;;

let test_token_lists () =
  let valid value =
    let buf = Bytes.of_string value in
    let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 (Bytes.length buf)) in
    Httpz.Span.token_list_valid buf span
  in
  List.iter [ ""; " , \t,,"; "Upgrade"; " , keep-alive,\tUpgrade,,"; "a,b,c" ] ~f:(fun value ->
    check "token-list-valid" (valid value) (fun () -> Printf.sprintf "%S" value));
  List.iter [ "Upgrade,bad/value"; "Upgrade, bad value" ] ~f:(fun value ->
    check "token-list-invalid" (not (valid value)) (fun () -> Printf.sprintf "%S" value))
;;

(* [Httpz.Span.split_on_char] scans eight bytes at a time. The SWAR "haszero" trick marks
   a byte whose XOR with the needle is 0x01 when it follows a genuine match, so needles
   adjacent to 0x01 in the corpus below are the interesting cases. *)
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
    then (
      check "split/nomatch_before" (Httpz.Span.len before = len) dump;
      check "split/nomatch_after" (Httpz.Span.len after = 0) dump)
    else (
      check "split/before_len" (Httpz.Span.len before = expect) dump;
      check "split/before_off" (Httpz.Span.off before = off) dump;
      check "split/after_off" (Httpz.Span.off after = off + expect + 1) dump;
      check "split/after_len" (Httpz.Span.len after = len - expect - 1) dump)
  done
;;

let () =
  test_all_bytes ();
  test_run_lengths ();
  test_limit_respected ();
  test_field_byte_classes ();
  test_header_syntax ();
  test_qvalues ();
  test_token_lists ();
  test_split_on_char (Random.State.make [| 20260805 |]);
  if !failures > 0
  then (
    Stdio.printf "%d differential failures\n" !failures;
    Stdlib.exit 1);
  Stdio.printf
    "test_tchar: byte classes, field slices, qvalues, token lists, and split scans pass\n"
;;
