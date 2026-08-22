(* [bytesrw] is vendored under vendor/ and patched, so that its [Slice]
   operations borrow their argument rather than requiring a heap value, and so
   that [equal] and [compare] read the whole slice. Dune skips aliases under a
   vendored directory, so the vendored copy's own tests never run. This one
   does, and it fails if a re-vendor drops either patch.

   Arod does not call bytesrw itself. It reaches this copy through jsont, and
   the copy exists because vendoring jsont drags its one dependency in. The
   guard lives here because this is where the vendored tree is tested. *)

open Bytesrw

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* Upstream 0.3.0 stops its comparison loop at [i < len - 1], so it never reads
   the last byte of two slices of equal length: [equal] answers true for "a"
   and "b", and for any two strings differing only in their final byte. The
   vendored copy reads to [i <= len - 1]. This differential is what pins the
   fix, so it must compare against the string the slice was built from rather
   than against bytesrw itself. *)
let slice = Bytes.Slice.of_string

let agrees a b =
  let by_slice = Bytes.Slice.equal (slice a) (slice b) in
  let by_string = String.equal a b in
  by_slice = by_string

let sign n = compare n 0

let orders a b =
  let by_slice = sign (Bytes.Slice.compare (slice a) (slice b)) in
  (* [Slice.compare] orders on length first and only then on content, so the
     oracle has to do the same. It is not [String.compare]. *)
  let by_string =
    match compare (String.length a) (String.length b) with
    | 0 -> sign (String.compare a b)
    | c -> sign c
  in
  by_slice = by_string

let () =
  check "a one byte slice compares its only byte" (agrees "a" "b");
  check "two slices differing in the last byte are not equal" (agrees "abc" "abd");
  check "and are ordered by it" (orders "abc" "abd");
  check "two equal slices are equal" (agrees "abc" "abc");
  check "a difference in the first byte is still found" (agrees "abc" "zbc");
  check "as is one in the middle" (agrees "abc" "azc");
  check "slices of different length are ordered by length" (orders "ab" "abc")

(* [of_string] refuses a zero-length slice, which is [of_string_or_eod]'s job,
   so the corpus above has no empty case and [eod] is checked separately. *)

(* Every ASCII pair of length one and two, which is where the off-by-one bit.
   65536 pairs is cheap and leaves no room for the loop bound to be wrong. *)
let () =
  let n = ref 0 in
  for i = 0 to 255 do
    for j = 0 to 255 do
      let a = String.make 1 (Char.chr i) and b = String.make 1 (Char.chr j) in
      if not (agrees a b && orders a b) then (
        prerr_endline (Printf.sprintf "FAIL: single byte %d vs %d" i j);
        exit 1);
      incr n
    done
  done;
  for i = 0 to 255 do
    let a = "x" ^ String.make 1 (Char.chr i) and b = "xy" in
    if not (agrees a b && orders a b) then (
      prerr_endline (Printf.sprintf "FAIL: trailing byte %d" i);
      exit 1);
    incr n
  done;
  checks := !checks + 1;
  Printf.printf "test_bytesrw: %d exhaustive pairs agree\n" !n

(* The [local_] bindings are the point. They bind at the local mode, and a
   local value is refused where a global one is expected, so none of these
   compile if a re-vendor drops the [@ local] from the [Slice] accessors.
   Nothing here is ascribed [@ portable]: this copy carries no portability
   annotation, and the reasons are in vendor/bytesrw/README.md. *)
let borrowed s =
  let local_ sl = Bytes.Slice.of_string s in
  Bytes.Slice.length sl + Bytes.Slice.first sl + Bytes.Slice.last sl

let borrowed_pair s =
  let local_ a = Bytes.Slice.of_string s in
  let local_ b = Bytes.Slice.of_string s in
  Bytes.Slice.equal a b && Bytes.Slice.compare a b = 0

let borrowed_eod () =
  let local_ e = Bytes.Slice.eod in
  let r = Bytes.Slice.is_eod e in
  r

let borrowed_string s =
  let local_ sl = Bytes.Slice.of_string s in
  let r = Bytes.Slice.to_string sl in
  r

let () =
  check "a borrowed slice answers its offsets" (borrowed "abc" = 3 + 0 + 2);
  check "a borrowed pair compares" (borrowed_pair "abc");
  check "eod is recognised through a borrow" (borrowed_eod ());
  check "a borrowed slice copies out" (String.equal (borrowed_string "abc") "abc")

(* The formatters had the same shape of error. Upstream tests the head cut with
   [len - 1 > max] and the empty case with [max < 0], both of which forget that
   a slice may start away from zero, so a truncated slice with [first > 0]
   printed without the ellipsis that says it was truncated. The vendored copy
   compares against [first + len - 1] and [first]. Only the third line below
   differs from upstream: it prints no ellipsis there. *)
let pp_slice ~first ~length =
  let b = Bytes.of_string "0123456789abcdef" in
  let s = Bytes.Slice.make b ~first ~length in
  Format.asprintf "%a" (Bytes.Slice.pp' ~head:4 ~hex:true ()) s

let () =
  check "a whole slice longer than the head is marked truncated"
    (String.equal (pp_slice ~first:0 ~length:16)
       "[0000;0015] len:0016 x30313233\xe2\x80\xa6");
  check "a truncated slice that starts away from zero is marked too"
    (String.equal (pp_slice ~first:8 ~length:8)
       "[0008;0015] len:0008 x38396162\xe2\x80\xa6");
  check "a slice that fits in the head is not marked"
    (String.equal (pp_slice ~first:12 ~length:4)
       "[0012;0015] len:0004 x63646566");
  check "nor is a one byte slice at the end"
    (String.equal (pp_slice ~first:15 ~length:1) "[0015;0015] len:0001 x66");
  check "nor a short slice in the middle"
    (String.equal (pp_slice ~first:4 ~length:2) "[0004;0005] len:0002 x3435")

let () = Printf.printf "test_bytesrw: %d checks ok\n" !checks
