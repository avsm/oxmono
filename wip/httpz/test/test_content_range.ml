let checks = ref 0

let fail label message =
  Printf.eprintf "FAIL [%s] %s\n" label message;
  exit 1

let check_kind label expected value =
  incr checks;
  let got = Httpz.Range.Content.kind ~unit:"bytes" value in
  if got <> expected then fail label (Printf.sprintf "unexpected classification for %S" value)

let check_bounds label expected ~range ~complete_length =
  incr checks;
  let got = Httpz.Range.Content.valid_bounds ~range ~complete_length in
  if got <> expected then fail label "unexpected semantic-bounds result"

let () =
  let open Httpz.Range.Content in
  List.iter
    (check_kind "satisfied" Satisfied)
    [ "bytes 0-499/1234";
      "bytes 0-499/*";
      "bytes 0-0/1";
      "bytes 500-1233/1234";
      "BYTES 0-1/2";
      " \tbytes 0-1/2\t ";
      "bytes 000-000/001";
      "bytes 0005-0009/0010" ];
  List.iter
    (check_kind "unsatisfied" Unsatisfied)
    [ "bytes */0"; "bytes */1234"; "bytes */000" ];
  incr checks;
  if Httpz.Range.Content.kind ~unit:"ByTeS" "bytes 0-1/2" <> Satisfied then
    fail "unit-case" "unit argument was not compared case-insensitively";
  incr checks;
  if Httpz.Range.Content.kind ~unit:"items" "items 0-1/2" <> Satisfied then
    fail "unit-name" "valid extension range unit was rejected";

  let huge_last = String.make 4096 '9' in
  let huge_total = "1" ^ String.make 4096 '0' in
  check_kind "large-satisfied" Satisfied
    ("bytes 0-" ^ huge_last ^ "/" ^ huge_total);
  check_kind "large-unsatisfied" Unsatisfied ("bytes */" ^ huge_total);
  check_kind "large-equal-total" Invalid
    ("bytes 0-" ^ huge_last ^ "/" ^ huge_last);

  List.iter
    (check_kind "malformed" Invalid)
    [ "";
      "bytes";
      "bytes ";
      "items 0-1/2";
      "bytes\t0-1/2";
      "bytes  0-1/2";
      "bytes 0 -1/2";
      "bytes 0- 1/2";
      "bytes 0-1 /2";
      "bytes 0-1/ 2";
      "bytes 0-1/2 x";
      "bytes 0-1/2\r";
      "bytes */*";
      "bytes */";
      "bytes * /1";
      "bytes -1/2";
      "bytes 0-/2";
      "bytes 0-1/";
      "bytes +0-1/2";
      "bytes 0-1/+2";
      "bytes 0-1/2, bytes 3-4/5";
      "bytes 0010-0009/0011";
      "bytes 0-00010/00010";
      "bytes 0-0/0" ];

  check_bounds "unsatisfied-zero" true ~range:None ~complete_length:(Some 0L);
  check_bounds "unknown-everything" false ~range:None ~complete_length:None;
  check_bounds "negative-total" false ~range:None ~complete_length:(Some (-1L));
  check_bounds "unknown-total" true ~range:(Some (0L, 0L)) ~complete_length:None;
  check_bounds "negative-first" false ~range:(Some (-1L, 0L)) ~complete_length:None;
  check_bounds "negative-last" false ~range:(Some (0L, -1L)) ~complete_length:None;
  check_bounds "reverse" false ~range:(Some (2L, 1L)) ~complete_length:(Some 3L);
  check_bounds "inside-total" true ~range:(Some (0L, 1L)) ~complete_length:(Some 2L);
  check_bounds "equal-total" false ~range:(Some (0L, 2L)) ~complete_length:(Some 2L);
  check_bounds "zero-total" false ~range:(Some (0L, 0L)) ~complete_length:(Some 0L);
  check_bounds "int64-limit" true
    ~range:(Some (0L, Int64.pred Int64.max_int))
    ~complete_length:(Some Int64.max_int);
  check_bounds "int64-limit-equal" false
    ~range:(Some (0L, Int64.max_int)) ~complete_length:(Some Int64.max_int);
  Printf.printf "test_content_range: %d checks passed\n" !checks
