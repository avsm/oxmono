(* Comprehensive tests for Tomlt codecs *)

open Tomlt

(* Helper to encode TOML to string via writer *)
let toml_to_string value =
  let buf = Buffer.create 256 in
  Tomlt_bytesrw.to_writer (Bytesrw.Bytes.Writer.of_buffer buf) value;
  Buffer.contents buf

(* ============================================================================
   Test Helpers
   ============================================================================ *)

(* Decode a value from "value = X" TOML *)
let check_decode_ok name codec input expected =
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  let actual = decode codec value in
  match actual with
  | Ok v when v = expected -> ()
  | Ok _ ->
      Alcotest.failf "%s: decode returned unexpected value" name
  | Error e ->
      Alcotest.failf "%s: decode failed: %s" name (Toml.Error.to_string e)

(* Check that decode fails *)
let check_decode_error name codec input =
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode codec value with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s: expected decode error but succeeded" name

(* Decode from a table (for table codecs) *)
let check_decode_table_ok name codec input expected =
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  let actual = decode codec value in
  match actual with
  | Ok v when v = expected -> ()
  | Ok _ ->
      Alcotest.failf "%s: decode returned unexpected value" name
  | Error e ->
      Alcotest.failf "%s: decode failed: %s" name (Toml.Error.to_string e)

(* Check table decode error *)
let check_decode_table_error name codec input =
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode codec value with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s: expected decode error but succeeded" name

(* Roundtrip test *)
let check_roundtrip name codec value =
  let toml = encode codec value in
  match decode codec toml with
  | Ok v when v = value -> ()
  | Ok _ ->
      Alcotest.failf "%s: roundtrip mismatch, got different value" name
  | Error e ->
      Alcotest.failf "%s: roundtrip decode failed: %s" name (Toml.Error.to_string e)


(* ============================================================================
   Datetime Type Tests
   ============================================================================ *)

(* ---- Tz tests ---- *)

let test_tz_utc () =
  Alcotest.(check string) "utc to_string" "Z" (Tz.to_string Tz.utc);
  Alcotest.(check bool) "utc equal" true (Tz.equal Tz.utc Tz.utc);
  match Tz.of_string "Z" with
  | Ok tz -> Alcotest.(check bool) "parse Z" true (Tz.equal tz Tz.utc)
  | Error e -> Alcotest.failf "failed to parse Z: %s" e

let test_tz_offset () =
  let tz_pos = Tz.offset ~hours:5 ~minutes:30 in
  Alcotest.(check string) "positive offset" "+05:30" (Tz.to_string tz_pos);

  let tz_neg = Tz.offset ~hours:(-8) ~minutes:0 in
  Alcotest.(check string) "negative offset" "-08:00" (Tz.to_string tz_neg);

  let tz_zero = Tz.offset ~hours:0 ~minutes:0 in
  Alcotest.(check string) "zero offset" "+00:00" (Tz.to_string tz_zero)

let test_tz_parse () =
  (match Tz.of_string "+05:30" with
  | Ok tz -> Alcotest.(check string) "parse +05:30" "+05:30" (Tz.to_string tz)
  | Error e -> Alcotest.failf "failed to parse +05:30: %s" e);

  (match Tz.of_string "-08:00" with
  | Ok tz -> Alcotest.(check string) "parse -08:00" "-08:00" (Tz.to_string tz)
  | Error e -> Alcotest.failf "failed to parse -08:00: %s" e);

  (match Tz.of_string "z" with
  | Ok tz -> Alcotest.(check bool) "parse lowercase z" true (Tz.equal tz Tz.utc)
  | Error e -> Alcotest.failf "failed to parse z: %s" e)

let test_tz_compare () =
  let tz1 = Tz.offset ~hours:5 ~minutes:0 in
  let tz2 = Tz.offset ~hours:6 ~minutes:0 in
  Alcotest.(check int) "compare less" (-1) (Int.compare (Tz.compare tz1 tz2) 0);
  Alcotest.(check int) "compare greater" 1 (Int.compare (Tz.compare tz2 tz1) 0);
  Alcotest.(check int) "compare equal" 0 (Tz.compare tz1 tz1);
  Alcotest.(check int) "utc < offset" (-1) (Int.compare (Tz.compare Tz.utc tz1) 0)

(* ---- Date tests ---- *)

let test_date_basic () =
  let d = Date.make ~year:2024 ~month:6 ~day:15 in
  Alcotest.(check string) "to_string" "2024-06-15" (Date.to_string d);
  Alcotest.(check int) "year" 2024 d.year;
  Alcotest.(check int) "month" 6 d.month;
  Alcotest.(check int) "day" 15 d.day

let test_date_equal () =
  let d1 = Date.make ~year:2024 ~month:6 ~day:15 in
  let d2 = Date.make ~year:2024 ~month:6 ~day:15 in
  let d3 = Date.make ~year:2024 ~month:6 ~day:16 in
  Alcotest.(check bool) "equal same" true (Date.equal d1 d2);
  Alcotest.(check bool) "not equal diff day" false (Date.equal d1 d3)

let test_date_compare () =
  let d1 = Date.make ~year:2024 ~month:6 ~day:15 in
  let d2 = Date.make ~year:2024 ~month:6 ~day:16 in
  let d3 = Date.make ~year:2024 ~month:7 ~day:1 in
  let d4 = Date.make ~year:2025 ~month:1 ~day:1 in
  Alcotest.(check int) "compare day" (-1) (Int.compare (Date.compare d1 d2) 0);
  Alcotest.(check int) "compare month" (-1) (Int.compare (Date.compare d1 d3) 0);
  Alcotest.(check int) "compare year" (-1) (Int.compare (Date.compare d1 d4) 0)

let test_date_parse () =
  (match Date.of_string "2024-06-15" with
  | Ok d ->
      Alcotest.(check int) "year" 2024 d.year;
      Alcotest.(check int) "month" 6 d.month;
      Alcotest.(check int) "day" 15 d.day
  | Error e -> Alcotest.failf "parse failed: %s" e);

  (match Date.of_string "1979-05-27" with
  | Ok d -> Alcotest.(check string) "roundtrip" "1979-05-27" (Date.to_string d)
  | Error e -> Alcotest.failf "parse failed: %s" e)

let test_date_edge_cases () =
  (* First day of year *)
  let d1 = Date.make ~year:2024 ~month:1 ~day:1 in
  Alcotest.(check string) "jan 1" "2024-01-01" (Date.to_string d1);

  (* Last day of year *)
  let d2 = Date.make ~year:2024 ~month:12 ~day:31 in
  Alcotest.(check string) "dec 31" "2024-12-31" (Date.to_string d2);

  (* Leading zeros in year *)
  let d3 = Date.make ~year:99 ~month:1 ~day:1 in
  Alcotest.(check string) "year 99" "0099-01-01" (Date.to_string d3)

(* ---- Time tests ---- *)

let test_time_basic () =
  let t = Time.make ~hour:14 ~minute:30 ~second:45 () in
  Alcotest.(check string) "to_string" "14:30:45" (Time.to_string t);
  Alcotest.(check int) "hour" 14 t.hour;
  Alcotest.(check int) "minute" 30 t.minute;
  Alcotest.(check int) "second" 45 t.second;
  Alcotest.(check (float 0.001)) "frac" 0.0 t.frac

let test_time_fractional () =
  let t1 = Time.make ~hour:14 ~minute:30 ~second:45 ~frac:0.123 () in
  Alcotest.(check string) "frac 3 digits" "14:30:45.123" (Time.to_string t1);

  let t2 = Time.make ~hour:0 ~minute:0 ~second:0 ~frac:0.123456789 () in
  Alcotest.(check string) "frac 9 digits" "00:00:00.123456789" (Time.to_string t2);

  let t3 = Time.make ~hour:12 ~minute:0 ~second:0 ~frac:0.1 () in
  Alcotest.(check string) "frac 1 digit" "12:00:00.1" (Time.to_string t3)

let test_time_equal () =
  let t1 = Time.make ~hour:14 ~minute:30 ~second:45 () in
  let t2 = Time.make ~hour:14 ~minute:30 ~second:45 () in
  let t3 = Time.make ~hour:14 ~minute:30 ~second:46 () in
  Alcotest.(check bool) "equal same" true (Time.equal t1 t2);
  Alcotest.(check bool) "not equal" false (Time.equal t1 t3)

let test_time_compare () =
  let t1 = Time.make ~hour:14 ~minute:30 ~second:45 () in
  let t2 = Time.make ~hour:14 ~minute:30 ~second:46 () in
  let t3 = Time.make ~hour:14 ~minute:31 ~second:0 () in
  let t4 = Time.make ~hour:15 ~minute:0 ~second:0 () in
  Alcotest.(check int) "compare second" (-1) (Int.compare (Time.compare t1 t2) 0);
  Alcotest.(check int) "compare minute" (-1) (Int.compare (Time.compare t1 t3) 0);
  Alcotest.(check int) "compare hour" (-1) (Int.compare (Time.compare t1 t4) 0)

let test_time_parse () =
  (match Time.of_string "14:30:45" with
  | Ok t ->
      Alcotest.(check int) "hour" 14 t.hour;
      Alcotest.(check int) "minute" 30 t.minute;
      Alcotest.(check int) "second" 45 t.second
  | Error e -> Alcotest.failf "parse failed: %s" e);

  (match Time.of_string "00:00:00.123456" with
  | Ok t ->
      Alcotest.(check (float 0.000001)) "frac" 0.123456 t.frac
  | Error e -> Alcotest.failf "parse failed: %s" e)

let test_time_edge_cases () =
  let t1 = Time.make ~hour:0 ~minute:0 ~second:0 () in
  Alcotest.(check string) "midnight" "00:00:00" (Time.to_string t1);

  let t2 = Time.make ~hour:23 ~minute:59 ~second:59 () in
  Alcotest.(check string) "end of day" "23:59:59" (Time.to_string t2)

(* ---- Datetime tests ---- *)

let test_datetime_basic () =
  let dt = Datetime.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:15)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ())
    ~tz:Tz.utc
  in
  Alcotest.(check string) "to_string" "2024-06-15T14:30:00Z" (Datetime.to_string dt)

let test_datetime_with_offset () =
  let dt = Datetime.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:15)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ())
    ~tz:(Tz.offset ~hours:5 ~minutes:30)
  in
  Alcotest.(check string) "with offset" "2024-06-15T14:30:00+05:30" (Datetime.to_string dt)

let test_datetime_with_frac () =
  let dt = Datetime.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:15)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ~frac:0.123456 ())
    ~tz:Tz.utc
  in
  Alcotest.(check string) "with frac" "2024-06-15T14:30:00.123456Z" (Datetime.to_string dt)

let test_datetime_parse () =
  (match Datetime.of_string "2024-06-15T14:30:00Z" with
  | Ok dt ->
      Alcotest.(check int) "year" 2024 dt.date.year;
      Alcotest.(check int) "hour" 14 dt.time.hour;
      Alcotest.(check bool) "tz" true (Tz.equal dt.tz Tz.utc)
  | Error e -> Alcotest.failf "parse failed: %s" e);

  (match Datetime.of_string "1979-05-27T07:32:00-08:00" with
  | Ok dt ->
      Alcotest.(check int) "year" 1979 dt.date.year;
      Alcotest.(check string) "tz" "-08:00" (Tz.to_string dt.tz)
  | Error e -> Alcotest.failf "parse failed: %s" e)

let test_datetime_equal_compare () =
  let dt1 = Datetime.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:15)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ())
    ~tz:Tz.utc in
  let dt2 = Datetime.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:15)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ())
    ~tz:Tz.utc in
  let dt3 = Datetime.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:16)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ())
    ~tz:Tz.utc in
  Alcotest.(check bool) "equal same" true (Datetime.equal dt1 dt2);
  Alcotest.(check bool) "not equal" false (Datetime.equal dt1 dt3);
  Alcotest.(check int) "compare" (-1) (Int.compare (Datetime.compare dt1 dt3) 0)

(* ---- Datetime_local tests ---- *)

let test_datetime_local_basic () =
  let dt = Datetime_local.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:15)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ())
  in
  Alcotest.(check string) "to_string" "2024-06-15T14:30:00" (Datetime_local.to_string dt)

let test_datetime_local_parse () =
  match Datetime_local.of_string "2024-06-15T14:30:00" with
  | Ok dt ->
      Alcotest.(check int) "year" 2024 dt.date.year;
      Alcotest.(check int) "hour" 14 dt.time.hour
  | Error e -> Alcotest.failf "parse failed: %s" e

let test_datetime_local_equal_compare () =
  let dt1 = Datetime_local.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:15)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ()) in
  let dt2 = Datetime_local.make
    ~date:(Date.make ~year:2024 ~month:6 ~day:15)
    ~time:(Time.make ~hour:14 ~minute:30 ~second:0 ()) in
  Alcotest.(check bool) "equal" true (Datetime_local.equal dt1 dt2);
  Alcotest.(check int) "compare" 0 (Datetime_local.compare dt1 dt2)

(* ============================================================================
   Base Codec Tests
   ============================================================================ *)

(* ---- Bool codec ---- *)

let test_bool_codec () =
  check_decode_ok "true" bool "value = true" true;
  check_decode_ok "false" bool "value = false" false

let test_bool_roundtrip () =
  check_roundtrip "true roundtrip" bool true;
  check_roundtrip "false roundtrip" bool false

let test_bool_type_error () =
  check_decode_error "string not bool" bool {|value = "true"|}

(* ---- Int codec ---- *)

let test_int_codec () =
  check_decode_ok "positive" int "value = 42" 42;
  check_decode_ok "negative" int "value = -17" (-17);
  check_decode_ok "zero" int "value = 0" 0;
  check_decode_ok "large" int "value = 1000000" 1000000

let test_int_formats () =
  check_decode_ok "hex" int "value = 0xDEADBEEF" 0xDEADBEEF;
  check_decode_ok "octal" int "value = 0o755" 0o755;
  check_decode_ok "binary" int "value = 0b11010110" 0b11010110;
  check_decode_ok "underscore" int "value = 1_000_000" 1_000_000

let test_int_roundtrip () =
  check_roundtrip "positive" int 42;
  check_roundtrip "negative" int (-17);
  check_roundtrip "zero" int 0

let test_int_type_error () =
  check_decode_error "float not int" int "value = 3.14";
  check_decode_error "string not int" int {|value = "42"|}

(* ---- Int32 codec ---- *)

let test_int32_codec () =
  check_decode_ok "positive" int32 "value = 42" 42l;
  check_decode_ok "negative" int32 "value = -17" (-17l);
  check_decode_ok "max" int32 "value = 2147483647" Int32.max_int;
  check_decode_ok "min" int32 "value = -2147483648" Int32.min_int

let test_int32_roundtrip () =
  check_roundtrip "positive" int32 42l;
  check_roundtrip "max" int32 Int32.max_int;
  check_roundtrip "min" int32 Int32.min_int

(* ---- Int64 codec ---- *)

let test_int64_codec () =
  check_decode_ok "positive" int64 "value = 42" 42L;
  check_decode_ok "large" int64 "value = 9223372036854775807" Int64.max_int;
  check_decode_ok "large neg" int64 "value = -9223372036854775808" Int64.min_int

let test_int64_roundtrip () =
  check_roundtrip "positive" int64 42L;
  check_roundtrip "max" int64 Int64.max_int;
  check_roundtrip "min" int64 Int64.min_int

(* ---- Float codec ---- *)

let test_float_codec () =
  check_decode_ok "positive" float "value = 3.14" 3.14;
  check_decode_ok "negative" float "value = -2.5" (-2.5);
  check_decode_ok "zero" float "value = 0.0" 0.0;
  check_decode_ok "exponent" float "value = 5e+22" 5e+22;
  check_decode_ok "neg exponent" float "value = 1e-10" 1e-10

let test_float_special () =
  check_decode_ok "inf" float "value = inf" Float.infinity;
  check_decode_ok "neg inf" float "value = -inf" Float.neg_infinity;
  check_decode_ok "pos inf" float "value = +inf" Float.infinity;
  (* nan requires special handling since nan <> nan *)
  let toml = Tomlt_bytesrw.parse "value = nan" in
  let value = Toml.find "value" toml in
  match decode float value with
  | Ok f when Float.is_nan f -> ()
  | Ok _ -> Alcotest.fail "expected nan"
  | Error e -> Alcotest.failf "decode failed: %s" (Toml.Error.to_string e)

let test_float_roundtrip () =
  check_roundtrip "positive" float 3.14;
  check_roundtrip "negative" float (-2.5);
  check_roundtrip "zero" float 0.0

let test_float_type_error () =
  check_decode_error "int not float" float "value = 42";
  check_decode_error "string not float" float {|value = "3.14"|}

(* ---- Number codec ---- *)

let test_number_codec () =
  check_decode_ok "float" number "value = 3.14" 3.14;
  check_decode_ok "int as float" number "value = 42" 42.0;
  check_decode_ok "negative int" number "value = -17" (-17.0)

let test_number_type_error () =
  check_decode_error "string not number" number {|value = "42"|}

(* ---- String codec ---- *)

let test_string_codec () =
  check_decode_ok "basic" string {|value = "hello"|} "hello";
  check_decode_ok "empty" string {|value = ""|} "";
  check_decode_ok "unicode" string {|value = "hello \u0048\u0065\u006C\u006C\u006F"|} "hello Hello"

let test_string_escapes () =
  check_decode_ok "newline" string {|value = "line1\nline2"|} "line1\nline2";
  check_decode_ok "tab" string {|value = "col1\tcol2"|} "col1\tcol2";
  check_decode_ok "quote" string {|value = "say \"hello\""|} {|say "hello"|};
  check_decode_ok "backslash" string {|value = "path\\to\\file"|} "path\\to\\file"

let test_string_multiline () =
  check_decode_ok "multiline" string {|value = """
hello
world"""|} "hello\nworld";
  check_decode_ok "literal" string "value = 'C:\\path\\to\\file'" "C:\\path\\to\\file"

let test_string_roundtrip () =
  check_roundtrip "basic" string "hello";
  check_roundtrip "empty" string "";
  check_roundtrip "unicode" string "Hello, \xE4\xB8\x96\xE7\x95\x8C!"

let test_string_type_error () =
  check_decode_error "int not string" string "value = 42";
  check_decode_error "bool not string" string "value = true"

(* ============================================================================
   Ptime Codec Tests
   ============================================================================ *)

(* ---- Ptime codecs ---- *)

let ptime_testable =
  let pp fmt t = Format.fprintf fmt "%s" (Ptime.to_rfc3339 ~tz_offset_s:0 t) in
  Alcotest.testable pp Ptime.equal

let ptime_date_testable =
  let pp fmt (y, m, d) = Format.fprintf fmt "%04d-%02d-%02d" y m d in
  let eq (y1, m1, d1) (y2, m2, d2) = y1 = y2 && m1 = m2 && d1 = d2 in
  Alcotest.testable pp eq

let ptime_span_testable =
  let pp fmt span = Format.fprintf fmt "%f" (Ptime.Span.to_float_s span) in
  let eq a b = Float.abs (Ptime.Span.to_float_s a -. Ptime.Span.to_float_s b) < 0.001 in
  Alcotest.testable pp eq

let test_ptime_codec () =
  let input = "value = 2024-06-15T14:30:00Z" in
  let expected = match Ptime.of_date_time ((2024, 6, 15), ((14, 30, 0), 0)) with
    | Some t -> t | None -> failwith "invalid test datetime" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime ()) value with
  | Ok v -> Alcotest.(check ptime_testable) "ptime" expected v
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_codec_offset () =
  (* Test parsing datetime with offset and verify UTC conversion *)
  let input = "value = 1979-05-27T00:32:00-07:00" in
  (* UTC time should be 1979-05-27T07:32:00Z *)
  let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> failwith "invalid test datetime" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime ()) value with
  | Ok v -> Alcotest.(check ptime_testable) "ptime with offset" expected v
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_codec_roundtrip () =
  let original = match Ptime.of_date_time ((2024, 12, 19), ((15, 30, 45), 0)) with
    | Some t -> t | None -> failwith "invalid test datetime" in
  let toml = encode (ptime ()) original in
  match decode (ptime ()) toml with
  | Ok v -> Alcotest.(check ptime_testable) "roundtrip" original v
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_codec_optional_seconds () =
  (* TOML 1.1 allows optional seconds *)
  let input = "value = 1979-05-27T07:32Z" in
  let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> failwith "invalid test datetime" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime ()) value with
  | Ok v -> Alcotest.(check ptime_testable) "optional seconds" expected v
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_opt_codec () =
  (* ptime_opt only accepts offset datetimes *)
  let input = "value = 1979-05-27T07:32:00Z" in
  let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> failwith "invalid test datetime" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime_opt ()) value with
  | Ok t -> Alcotest.(check ptime_testable) "ptime_opt" expected t
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_opt_rejects_local () =
  (* ptime_opt should reject local datetime *)
  let input = "value = 1979-05-27T07:32:00" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime_opt ()) value with
  | Ok _ -> Alcotest.fail "expected error for local datetime"
  | Error _ -> ()

let test_ptime_span_codec () =
  let input = "value = 14:30:45" in
  let expected = match Ptime.Span.of_float_s (14.0 *. 3600.0 +. 30.0 *. 60.0 +. 45.0) with
    | Some s -> s | None -> failwith "invalid span" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode ptime_span value with
  | Ok span -> Alcotest.(check ptime_span_testable) "span" expected span
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_span_roundtrip () =
  let original = match Ptime.Span.of_float_s (7.0 *. 3600.0 +. 32.0 *. 60.0) with
    | Some s -> s | None -> failwith "invalid span" in
  let toml = encode ptime_span original in
  match decode ptime_span toml with
  | Ok v -> Alcotest.(check ptime_span_testable) "roundtrip" original v
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_date_codec () =
  let input = "value = 1979-05-27" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode ptime_date value with
  | Ok date -> Alcotest.(check ptime_date_testable) "date" (1979, 5, 27) date
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_date_roundtrip () =
  let original = (2024, 12, 19) in
  let toml = encode ptime_date original in
  match decode ptime_date toml with
  | Ok v -> Alcotest.(check ptime_date_testable) "roundtrip" original v
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_local_datetime () =
  (* The new ptime () codec accepts local datetime and uses provided tz *)
  let input = "value = 1979-05-27T07:32:00" in
  let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> failwith "invalid test datetime" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime ~tz_offset_s:0 ()) value with
  | Ok v -> Alcotest.(check ptime_testable) "local datetime" expected v
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_date_as_ptime () =
  (* The new ptime () codec accepts date and assumes midnight *)
  let input = "value = 1979-05-27" in
  let expected = match Ptime.of_date_time ((1979, 5, 27), ((0, 0, 0), 0)) with
    | Some t -> t | None -> failwith "invalid test datetime" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime ~tz_offset_s:0 ()) value with
  | Ok v -> Alcotest.(check ptime_testable) "date as ptime" expected v
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

(* ---- Unified ptime_full codec ---- *)

let ptime_full_testable =
  Alcotest.testable Toml.pp_ptime_datetime (fun a b ->
    match a, b with
    | `Datetime (t1, tz1), `Datetime (t2, tz2) ->
        Ptime.equal t1 t2 && tz1 = tz2
    | `Datetime_local t1, `Datetime_local t2 ->
        Ptime.equal t1 t2
    | `Date d1, `Date d2 -> d1 = d2
    | `Time t1, `Time t2 -> t1 = t2
    | _ -> false)

let test_ptime_full_offset () =
  let input = "value = 1979-05-27T07:32:00Z" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime_full ()) value with
  | Ok (`Datetime (ptime, Some 0)) ->
      let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
        | Some t -> t | None -> failwith "invalid datetime" in
      Alcotest.(check ptime_testable) "ptime" expected ptime
  | Ok other -> Alcotest.failf "expected `Datetime, got %a" Toml.pp_ptime_datetime other
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_full_local_datetime () =
  let input = "value = 1979-05-27T07:32:00" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime_full ~tz_offset_s:0 ()) value with
  | Ok (`Datetime_local ptime) ->
      let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
        | Some t -> t | None -> failwith "invalid datetime" in
      Alcotest.(check ptime_testable) "ptime" expected ptime
  | Ok other -> Alcotest.failf "expected `Datetime_local, got %a" Toml.pp_ptime_datetime other
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_full_date () =
  let input = "value = 1979-05-27" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime_full ()) value with
  | Ok (`Date (y, m, d)) ->
      Alcotest.(check int) "year" 1979 y;
      Alcotest.(check int) "month" 5 m;
      Alcotest.(check int) "day" 27 d
  | Ok other -> Alcotest.failf "expected `Date, got %a" Toml.pp_ptime_datetime other
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_full_time () =
  let input = "value = 07:32:00" in
  let toml = Tomlt_bytesrw.parse input in
  let value = Toml.find "value" toml in
  match decode (ptime_full ()) value with
  | Ok (`Time (h, m, s, ns)) ->
      Alcotest.(check int) "hour" 7 h;
      Alcotest.(check int) "minute" 32 m;
      Alcotest.(check int) "second" 0 s;
      Alcotest.(check int) "nanoseconds" 0 ns
  | Ok other -> Alcotest.failf "expected `Time, got %a" Toml.pp_ptime_datetime other
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

let test_ptime_full_roundtrip () =
  let original : Toml.ptime_datetime = `Datetime (
    (match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
     | Some t -> t | None -> failwith "invalid datetime"),
    Some 0
  ) in
  let toml = encode (ptime_full ()) original in
  match decode (ptime_full ()) toml with
  | Ok result -> Alcotest.(check ptime_full_testable) "roundtrip" original result
  | Error e -> Alcotest.fail (Toml.Error.to_string e)

(* ============================================================================
   Combinator Tests
   ============================================================================ *)

(* ---- Map combinator ---- *)

let uppercase_string =
  map string ~dec:String.uppercase_ascii ~enc:String.lowercase_ascii

let test_map_combinator () =
  check_decode_ok "uppercase" uppercase_string {|value = "hello"|} "HELLO"

let test_map_roundtrip () =
  check_roundtrip "map roundtrip" uppercase_string "HELLO"

let doubled_int =
  map int ~dec:(fun x -> x * 2) ~enc:(fun x -> x / 2)

let test_map_int () =
  check_decode_ok "doubled" doubled_int "value = 21" 42;
  check_roundtrip "doubled roundtrip" doubled_int 42

(* ---- Const combinator ---- *)

let test_const () =
  let c = const "default_value" in
  check_decode_ok "const ignores input" c "value = 42" "default_value";
  check_decode_ok "const ignores string" c {|value = "ignored"|} "default_value"

(* ---- Enum combinator ---- *)

type level = Debug | Info | Warn | Error

let level_codec =
  enum [
    "debug", Debug;
    "info", Info;
    "warn", Warn;
    "error", Error;
  ]

let test_enum () =
  check_decode_ok "debug" level_codec {|value = "debug"|} Debug;
  check_decode_ok "info" level_codec {|value = "info"|} Info;
  check_decode_ok "warn" level_codec {|value = "warn"|} Warn;
  check_decode_ok "error" level_codec {|value = "error"|} Error

let test_enum_roundtrip () =
  check_roundtrip "debug" level_codec Debug;
  check_roundtrip "error" level_codec Error

let test_enum_unknown () =
  check_decode_error "unknown value" level_codec {|value = "trace"|}

let test_enum_type_error () =
  check_decode_error "not string" level_codec "value = 42"

(* ---- Option combinator ---- *)

let test_option_codec () =
  let opt_int = option int in
  check_decode_ok "some" opt_int "value = 42" (Some 42)

let test_option_roundtrip () =
  let opt_str = option string in
  check_roundtrip "some string" opt_str (Some "hello")

(* ---- Result combinator ---- *)

let string_or_int_codec : (string, int) result t = result ~ok:string ~error:int

let test_result_codec () =
  check_decode_ok "ok string" string_or_int_codec {|value = "hello"|} (Ok "hello");
  check_decode_ok "error int" string_or_int_codec "value = 42" (Error 42)

let test_result_roundtrip () =
  check_roundtrip "ok" string_or_int_codec (Ok "hello");
  check_roundtrip "error" string_or_int_codec (Error 42)

(* ---- Recursive codec ---- *)

(* Simple recursive structure for testing rec' *)
type nested_list = {
  value : int;
  next : nested_list option;
}

let rec nested_list_codec = lazy (
  Table.(
    obj (fun value next -> { value; next })
    |> mem "value" int ~enc:(fun n -> n.value)
    |> opt_mem "next" (rec' nested_list_codec) ~enc:(fun n -> n.next)
    |> finish
  )
)

let test_recursive_codec () =
  let input = {|
    [value]
    value = 1

    [value.next]
    value = 2

    [value.next.next]
    value = 3
  |} in
  let expected = {
    value = 1;
    next = Some {
      value = 2;
      next = Some { value = 3; next = None }
    }
  } in
  check_decode_table_ok "nested list" (rec' nested_list_codec) input expected

(* ============================================================================
   Array Codec Tests
   ============================================================================ *)

let test_list_codec () =
  check_decode_ok "int list" (list int) "value = [1, 2, 3]" [1; 2; 3];
  check_decode_ok "empty list" (list int) "value = []" [];
  check_decode_ok "string list" (list string) {|value = ["a", "b", "c"]|} ["a"; "b"; "c"]

let test_list_roundtrip () =
  check_roundtrip "int list" (list int) [1; 2; 3];
  check_roundtrip "empty" (list int) [];
  check_roundtrip "strings" (list string) ["hello"; "world"]

let test_array_codec () =
  check_decode_ok "int array" (array int) "value = [1, 2, 3]" [|1; 2; 3|];
  check_decode_ok "empty array" (array int) "value = []" [||]

let test_array_roundtrip () =
  check_roundtrip "int array" (array int) [|1; 2; 3|];
  check_roundtrip "empty" (array int) [||]

let test_nested_list () =
  let nested = list (list int) in
  check_decode_ok "nested" nested "value = [[1, 2], [3, 4], [5]]" [[1; 2]; [3; 4]; [5]];
  check_roundtrip "nested roundtrip" nested [[1; 2]; [3; 4]]

let test_list_of_tables () =
  let point_codec = Table.(
    obj (fun x y -> (x, y))
    |> mem "x" int ~enc:fst
    |> mem "y" int ~enc:snd
    |> finish
  ) in
  let points_codec = list point_codec in
  let input = {|value = [{x = 1, y = 2}, {x = 3, y = 4}]|} in
  check_decode_ok "list of inline tables" points_codec input [(1, 2); (3, 4)]

let test_list_type_error () =
  check_decode_error "not array" (list int) "value = 42";
  check_decode_error "mixed types" (list int) {|value = [1, "two", 3]|}

(* ============================================================================
   Table Codec Tests
   ============================================================================ *)

(* ---- Basic table ---- *)

type point = { x : int; y : int }

let point_codec =
  Table.(
    obj (fun x y -> { x; y })
    |> mem "x" int ~enc:(fun p -> p.x)
    |> mem "y" int ~enc:(fun p -> p.y)
    |> finish
  )

let test_table_codec () =
  let input = {|
    [value]
    x = 10
    y = 20
  |} in
  check_decode_table_ok "point" point_codec input { x = 10; y = 20 }

let test_table_roundtrip () =
  check_roundtrip "point roundtrip" point_codec { x = 5; y = 15 }

let test_table_missing_member () =
  let input = {|
    [value]
    x = 10
  |} in
  check_decode_table_error "missing y" point_codec input

let test_table_type_error () =
  check_decode_error "not table" point_codec "value = 42"

(* ---- Optional members ---- *)

type config = {
  name : string;
  debug : bool;
  timeout : int option;
}

let config_codec =
  Table.(
    obj (fun name debug timeout -> { name; debug; timeout })
    |> mem "name" string ~enc:(fun c -> c.name)
    |> mem "debug" bool ~enc:(fun c -> c.debug) ~dec_absent:false
    |> opt_mem "timeout" int ~enc:(fun c -> c.timeout)
    |> finish
  )

let test_optional_members () =
  let input1 = {|
    [value]
    name = "test"
    debug = true
    timeout = 30
  |} in
  check_decode_table_ok "with all" config_codec input1
    { name = "test"; debug = true; timeout = Some 30 };

  let input2 = {|
    [value]
    name = "test"
  |} in
  check_decode_table_ok "with defaults" config_codec input2
    { name = "test"; debug = false; timeout = None }

let test_optional_roundtrip () =
  let c1 = { name = "app"; debug = true; timeout = Some 60 } in
  check_roundtrip "with timeout" config_codec c1;

  let c2 = { name = "app"; debug = false; timeout = None } in
  check_roundtrip "without timeout" config_codec c2

let test_opt_mem_omits_none () =
  let c = { name = "app"; debug = false; timeout = None } in
  let toml = encode config_codec c in
  (* Just verify encoding doesn't crash *)
  let _ = toml_to_string toml in
  (* Verify None is not encoded *)
  match Toml.find_opt "timeout" toml with
  | None -> ()
  | Some _ -> Alcotest.fail "timeout should not be encoded when None"

(* ---- enc_omit ---- *)

type with_omit = {
  always : string;
  maybe : string;
}

let with_omit_codec =
  Table.(
    obj (fun always maybe -> { always; maybe })
    |> mem "always" string ~enc:(fun r -> r.always)
    |> mem "maybe" string ~enc:(fun r -> r.maybe)
        ~dec_absent:"" ~enc_omit:(fun s -> String.length s = 0)
    |> finish
  )

let test_enc_omit () =
  let r1 = { always = "hello"; maybe = "world" } in
  let toml1 = encode with_omit_codec r1 in
  (match Toml.find_opt "maybe" toml1 with
  | Some _ -> ()
  | None -> Alcotest.fail "maybe should be encoded when non-empty");

  let r2 = { always = "hello"; maybe = "" } in
  let toml2 = encode with_omit_codec r2 in
  (match Toml.find_opt "maybe" toml2 with
  | None -> ()
  | Some _ -> Alcotest.fail "maybe should be omitted when empty")

(* ---- Nested tables ---- *)

type server = {
  host : string;
  port : int;
}

type app_config = {
  title : string;
  server : server;
}

let server_codec =
  Table.(
    obj (fun host port -> { host; port })
    |> mem "host" string ~enc:(fun s -> s.host)
    |> mem "port" int ~enc:(fun s -> s.port)
    |> finish
  )

let app_config_codec =
  Table.(
    obj (fun title server -> { title; server })
    |> mem "title" string ~enc:(fun c -> c.title)
    |> mem "server" server_codec ~enc:(fun c -> c.server)
    |> finish
  )

let test_nested_tables () =
  let input = {|
    [value]
    title = "My App"

    [value.server]
    host = "localhost"
    port = 8080
  |} in
  check_decode_table_ok "nested" app_config_codec input
    { title = "My App"; server = { host = "localhost"; port = 8080 } }

let test_nested_roundtrip () =
  let config = {
    title = "Production";
    server = { host = "0.0.0.0"; port = 443 };
  } in
  check_roundtrip "nested roundtrip" app_config_codec config

(* ---- Deeply nested tables ---- *)

type deep = {
  a : int;
  inner : deep option;
}

let rec deep_codec = lazy (
  Table.(
    obj (fun a inner -> { a; inner })
    |> mem "a" int ~enc:(fun d -> d.a)
    |> opt_mem "inner" (rec' deep_codec) ~enc:(fun d -> d.inner)
    |> finish
  )
)

let test_deeply_nested () =
  let input = {|
    [value]
    a = 1

    [value.inner]
    a = 2

    [value.inner.inner]
    a = 3
  |} in
  let expected = {
    a = 1;
    inner = Some {
      a = 2;
      inner = Some { a = 3; inner = None }
    }
  } in
  check_decode_table_ok "deep" (rec' deep_codec) input expected

(* ---- Unknown member handling ---- *)

type strict_config = {
  name : string;
}

let strict_config_codec =
  Table.(
    obj (fun name -> { name })
    |> mem "name" string ~enc:(fun c -> c.name)
    |> error_unknown
    |> finish
  )

let test_error_unknown () =
  let input1 = {|
    [value]
    name = "test"
  |} in
  check_decode_table_ok "known only" strict_config_codec input1 { name = "test" };

  (* error_unknown raises an exception for unknown members *)
  let input2 = {|
    [value]
    name = "test"
    extra = 42
  |} in
  let toml = Tomlt_bytesrw.parse input2 in
  let value_toml = Toml.find "value" toml in
  try
    let _ = decode strict_config_codec value_toml in
    Alcotest.fail "expected exception for unknown member"
  with Toml.Error.Error _ -> ()

type extensible_config = {
  name : string;
  extras : (string * Toml.t) list;
}

let extensible_config_codec =
  Table.(
    obj (fun name extras -> { name; extras })
    |> mem "name" string ~enc:(fun c -> c.name)
    |> keep_unknown (Mems.assoc value) ~enc:(fun c -> c.extras)
    |> finish
  )

let test_keep_unknown () =
  let input = {|
    [value]
    name = "test"
    extra1 = 42
    extra2 = "hello"
  |} in
  let toml = Tomlt_bytesrw.parse input in
  let value_toml = Toml.find "value" toml in
  match decode extensible_config_codec value_toml with
  | Ok c ->
      Alcotest.(check string) "name" "test" c.name;
      Alcotest.(check int) "extras count" 2 (List.length c.extras);
      (* Check extras contains the unknown members *)
      let has_extra1 = List.exists (fun (k, _) -> k = "extra1") c.extras in
      let has_extra2 = List.exists (fun (k, _) -> k = "extra2") c.extras in
      Alcotest.(check bool) "has extra1" true has_extra1;
      Alcotest.(check bool) "has extra2" true has_extra2
  | Error e ->
      Alcotest.failf "decode failed: %s" (Toml.Error.to_string e)

let test_keep_unknown_roundtrip () =
  let c = {
    name = "test";
    extras = [("custom", Toml.Int 42L); ("flag", Toml.Bool true)]
  } in
  check_roundtrip "keep_unknown roundtrip" extensible_config_codec c

(* ---- Skip unknown (default) ---- *)

type lenient_config = {
  lname : string;
}

let lenient_codec =
  Table.(
    obj (fun lname -> { lname })
    |> mem "name" string ~enc:(fun c -> c.lname)
    |> skip_unknown
    |> finish
  )

let test_skip_unknown () =
  let input = {|
    [value]
    name = "test"
    ignored = 42
    also_ignored = "hello"
  |} in
  check_decode_table_ok "skip unknown" lenient_codec input { lname = "test" }

(* ============================================================================
   Array of Tables Tests
   ============================================================================ *)

type product = {
  name : string;
  price : float;
}

let product_codec =
  Table.(
    obj (fun name price -> { name; price })
    |> mem "name" string ~enc:(fun p -> p.name)
    |> mem "price" float ~enc:(fun p -> p.price)
    |> finish
  )

let test_array_of_tables () =
  let products_codec = array_of_tables product_codec in
  let input = {|
    [[value]]
    name = "Apple"
    price = 1.50

    [[value]]
    name = "Banana"
    price = 0.75
  |} in
  let expected = [
    { name = "Apple"; price = 1.50 };
    { name = "Banana"; price = 0.75 };
  ] in
  check_decode_ok "products" products_codec input expected

let test_array_of_tables_roundtrip () =
  let products_codec = array_of_tables product_codec in
  let products = [
    { name = "Apple"; price = 1.50 };
    { name = "Banana"; price = 0.75 };
  ] in
  check_roundtrip "products roundtrip" products_codec products

let test_array_of_tables_empty () =
  let products_codec = array_of_tables product_codec in
  check_decode_ok "empty" products_codec "value = []" []

(* ============================================================================
   Any/Value Codec Tests
   ============================================================================ *)

let test_value_codec () =
  check_decode_ok "int" value "value = 42" (Toml.Int 42L);
  check_decode_ok "string" value {|value = "hello"|} (Toml.String "hello");
  check_decode_ok "bool" value "value = true" (Toml.Bool true);
  check_decode_ok "float" value "value = 3.14" (Toml.Float 3.14);
  check_decode_ok "array" value "value = [1, 2, 3]"
    (Toml.Array [Toml.Int 1L; Toml.Int 2L; Toml.Int 3L])

let test_value_roundtrip () =
  check_roundtrip "int" value (Toml.Int 42L);
  check_roundtrip "string" value (Toml.String "hello");
  check_roundtrip "bool" value (Toml.Bool true)

let test_value_mems_codec () =
  let input = {|
    [value]
    a = 1
    b = "hello"
    c = true
  |} in
  let toml = Tomlt_bytesrw.parse input in
  let v = Toml.find "value" toml in
  match decode value_mems v with
  | Ok pairs ->
      Alcotest.(check int) "count" 3 (List.length pairs);
      let has_a = List.exists (fun (k, _) -> k = "a") pairs in
      let has_b = List.exists (fun (k, _) -> k = "b") pairs in
      let has_c = List.exists (fun (k, _) -> k = "c") pairs in
      Alcotest.(check bool) "has a" true has_a;
      Alcotest.(check bool) "has b" true has_b;
      Alcotest.(check bool) "has c" true has_c
  | Error e ->
      Alcotest.failf "decode failed: %s" (Toml.Error.to_string e)

type string_or_int_any = String of string | Int of int

let string_or_int_any_codec =
  any ()
    ~dec_string:(map string ~dec:(fun s -> String s))
    ~dec_int:(map int ~dec:(fun i -> Int i))
    ~enc:(function
      | String _ -> map string ~enc:(function String s -> s | _ -> "")
      | Int _ -> map int ~enc:(function Int i -> i | _ -> 0))

let test_any_codec () =
  check_decode_ok "string" string_or_int_any_codec {|value = "hello"|} (String "hello");
  check_decode_ok "int" string_or_int_any_codec "value = 42" (Int 42)

let test_any_type_error () =
  check_decode_error "bool not handled" string_or_int_any_codec "value = true"

(* ============================================================================
   Encoding/Decoding Function Tests
   ============================================================================ *)

let test_decode_string () =
  let toml_str = {|name = "test"|} in
  let codec = Table.(
    obj (fun name -> name)
    |> mem "name" string ~enc:Fun.id
    |> finish
  ) in
  match Tomlt_bytesrw.decode_string codec toml_str with
  | Ok name -> Alcotest.(check string) "name" "test" name
  | Error e -> Alcotest.failf "decode failed: %s" (Toml.Error.to_string e)

let test_decode_string_exn () =
  let toml_str = {|value = 42|} in
  let toml = Tomlt_bytesrw.parse toml_str in
  let v = Toml.find "value" toml in
  let result = decode_exn int v in
  Alcotest.(check int) "value" 42 result

let test_encode_string () =
  let codec = Table.(
    obj (fun name -> name)
    |> mem "name" string ~enc:Fun.id
    |> finish
  ) in
  let s = Tomlt_bytesrw.encode_string codec "test" in
  (* Just verify it produces valid TOML *)
  let _ = Tomlt_bytesrw.parse s in
  ()

(* ============================================================================
   Edge Cases and Error Handling
   ============================================================================ *)

let test_empty_table () =
  let empty_codec = Table.(
    obj ()
    |> finish
  ) in
  let input = "[value]" in
  check_decode_table_ok "empty table" empty_codec input ()

let test_unicode_keys () =
  let codec = Table.(
    obj (fun v -> v)
    |> mem "\xE4\xB8\xAD\xE6\x96\x87" string ~enc:Fun.id  (* "中文" in UTF-8 *)
    |> finish
  ) in
  let input = {|
    [value]
    "中文" = "hello"
  |} in
  check_decode_table_ok "unicode key" codec input "hello"

let test_special_string_values () =
  check_decode_ok "empty" string {|value = ""|} "";
  check_decode_ok "spaces" string {|value = "   "|} "   ";
  check_decode_ok "newlines" string {|value = "a\nb\nc"|} "a\nb\nc"

let test_large_integers () =
  check_decode_ok "large" int64 "value = 9007199254740992" 9007199254740992L;
  check_decode_ok "max i64" int64 "value = 9223372036854775807" 9223372036854775807L

let test_codec_kind_doc () =
  Alcotest.(check string) "bool kind" "boolean" (kind bool);
  Alcotest.(check string) "int kind" "integer" (kind int);
  Alcotest.(check string) "string kind" "string" (kind string);
  Alcotest.(check string) "float kind" "float" (kind float);

  let documented = with_doc ~kind:"custom" ~doc:"A custom codec" int in
  Alcotest.(check string) "custom kind" "custom" (kind documented);
  Alcotest.(check string) "custom doc" "A custom codec" (doc documented)

let test_duplicate_member_error () =
  try
    let _ = Table.(
      obj (fun a b -> (a, b))
      |> mem "same" int ~enc:fst
      |> mem "same" int ~enc:snd
      |> finish
    ) in
    Alcotest.fail "should raise on duplicate member"
  with Invalid_argument _ -> ()

(* ============================================================================
   Test Registration
   ============================================================================ *)

let tz_tests = [
  "utc", `Quick, test_tz_utc;
  "offset", `Quick, test_tz_offset;
  "parse", `Quick, test_tz_parse;
  "compare", `Quick, test_tz_compare;
]

let date_tests = [
  "basic", `Quick, test_date_basic;
  "equal", `Quick, test_date_equal;
  "compare", `Quick, test_date_compare;
  "parse", `Quick, test_date_parse;
  "edge cases", `Quick, test_date_edge_cases;
]

let time_tests = [
  "basic", `Quick, test_time_basic;
  "fractional", `Quick, test_time_fractional;
  "equal", `Quick, test_time_equal;
  "compare", `Quick, test_time_compare;
  "parse", `Quick, test_time_parse;
  "edge cases", `Quick, test_time_edge_cases;
]

let datetime_tests = [
  "basic", `Quick, test_datetime_basic;
  "with offset", `Quick, test_datetime_with_offset;
  "with frac", `Quick, test_datetime_with_frac;
  "parse", `Quick, test_datetime_parse;
  "equal compare", `Quick, test_datetime_equal_compare;
]

let datetime_local_tests = [
  "basic", `Quick, test_datetime_local_basic;
  "parse", `Quick, test_datetime_local_parse;
  "equal compare", `Quick, test_datetime_local_equal_compare;
]

let bool_tests = [
  "codec", `Quick, test_bool_codec;
  "roundtrip", `Quick, test_bool_roundtrip;
  "type error", `Quick, test_bool_type_error;
]

let int_tests = [
  "codec", `Quick, test_int_codec;
  "formats", `Quick, test_int_formats;
  "roundtrip", `Quick, test_int_roundtrip;
  "type error", `Quick, test_int_type_error;
]

let int32_tests = [
  "codec", `Quick, test_int32_codec;
  "roundtrip", `Quick, test_int32_roundtrip;
]

let int64_tests = [
  "codec", `Quick, test_int64_codec;
  "roundtrip", `Quick, test_int64_roundtrip;
]

let float_tests = [
  "codec", `Quick, test_float_codec;
  "special", `Quick, test_float_special;
  "roundtrip", `Quick, test_float_roundtrip;
  "type error", `Quick, test_float_type_error;
]

let number_tests = [
  "codec", `Quick, test_number_codec;
  "type error", `Quick, test_number_type_error;
]

let string_tests = [
  "codec", `Quick, test_string_codec;
  "escapes", `Quick, test_string_escapes;
  "multiline", `Quick, test_string_multiline;
  "roundtrip", `Quick, test_string_roundtrip;
  "type error", `Quick, test_string_type_error;
]

let ptime_codec_tests = [
  "ptime offset datetime", `Quick, test_ptime_codec;
  "ptime with timezone offset", `Quick, test_ptime_codec_offset;
  "ptime roundtrip", `Quick, test_ptime_codec_roundtrip;
  "ptime optional seconds", `Quick, test_ptime_codec_optional_seconds;
  "ptime_opt", `Quick, test_ptime_opt_codec;
  "ptime_opt rejects local", `Quick, test_ptime_opt_rejects_local;
  "ptime_span", `Quick, test_ptime_span_codec;
  "ptime_span roundtrip", `Quick, test_ptime_span_roundtrip;
  "ptime_date", `Quick, test_ptime_date_codec;
  "ptime_date roundtrip", `Quick, test_ptime_date_roundtrip;
  "ptime local datetime", `Quick, test_ptime_local_datetime;
  "ptime date as ptime", `Quick, test_ptime_date_as_ptime;
]

let ptime_full_codec_tests = [
  "offset datetime", `Quick, test_ptime_full_offset;
  "local datetime", `Quick, test_ptime_full_local_datetime;
  "local date", `Quick, test_ptime_full_date;
  "local time", `Quick, test_ptime_full_time;
  "roundtrip", `Quick, test_ptime_full_roundtrip;
]

let combinator_tests = [
  "map", `Quick, test_map_combinator;
  "map roundtrip", `Quick, test_map_roundtrip;
  "map int", `Quick, test_map_int;
  "const", `Quick, test_const;
  "enum", `Quick, test_enum;
  "enum roundtrip", `Quick, test_enum_roundtrip;
  "enum unknown", `Quick, test_enum_unknown;
  "enum type error", `Quick, test_enum_type_error;
  "option", `Quick, test_option_codec;
  "option roundtrip", `Quick, test_option_roundtrip;
  "result", `Quick, test_result_codec;
  "result roundtrip", `Quick, test_result_roundtrip;
  "recursive", `Quick, test_recursive_codec;
]

let array_tests = [
  "list", `Quick, test_list_codec;
  "list roundtrip", `Quick, test_list_roundtrip;
  "array", `Quick, test_array_codec;
  "array roundtrip", `Quick, test_array_roundtrip;
  "nested list", `Quick, test_nested_list;
  "list of tables", `Quick, test_list_of_tables;
  "list type error", `Quick, test_list_type_error;
]

let table_tests = [
  "basic", `Quick, test_table_codec;
  "roundtrip", `Quick, test_table_roundtrip;
  "missing member", `Quick, test_table_missing_member;
  "type error", `Quick, test_table_type_error;
  "optional members", `Quick, test_optional_members;
  "optional roundtrip", `Quick, test_optional_roundtrip;
  "opt_mem omits none", `Quick, test_opt_mem_omits_none;
  "enc_omit", `Quick, test_enc_omit;
  "nested tables", `Quick, test_nested_tables;
  "nested roundtrip", `Quick, test_nested_roundtrip;
  "deeply nested", `Quick, test_deeply_nested;
  "error unknown", `Quick, test_error_unknown;
  "keep unknown", `Quick, test_keep_unknown;
  "keep unknown roundtrip", `Quick, test_keep_unknown_roundtrip;
  "skip unknown", `Quick, test_skip_unknown;
]

let array_of_tables_tests = [
  "basic", `Quick, test_array_of_tables;
  "roundtrip", `Quick, test_array_of_tables_roundtrip;
  "empty", `Quick, test_array_of_tables_empty;
]

let any_value_tests = [
  "value codec", `Quick, test_value_codec;
  "value roundtrip", `Quick, test_value_roundtrip;
  "value_mems", `Quick, test_value_mems_codec;
  "any codec", `Quick, test_any_codec;
  "any type error", `Quick, test_any_type_error;
]

let function_tests = [
  "decode_string", `Quick, test_decode_string;
  "decode_exn", `Quick, test_decode_string_exn;
  "encode_string", `Quick, test_encode_string;
]

let edge_case_tests = [
  "empty table", `Quick, test_empty_table;
  "unicode keys", `Quick, test_unicode_keys;
  "special strings", `Quick, test_special_string_values;
  "large integers", `Quick, test_large_integers;
  "codec kind doc", `Quick, test_codec_kind_doc;
  "duplicate member error", `Quick, test_duplicate_member_error;
]

let () =
  Alcotest.run "tomlt_codec" [
    "tz", tz_tests;
    "date", date_tests;
    "time", time_tests;
    "datetime", datetime_tests;
    "datetime_local", datetime_local_tests;
    "bool", bool_tests;
    "int", int_tests;
    "int32", int32_tests;
    "int64", int64_tests;
    "float", float_tests;
    "number", number_tests;
    "string", string_tests;
    "ptime_codecs", ptime_codec_tests;
    "ptime_full", ptime_full_codec_tests;
    "combinators", combinator_tests;
    "arrays", array_tests;
    "tables", table_tests;
    "array_of_tables", array_of_tables_tests;
    "any_value", any_value_tests;
    "functions", function_tests;
    "edge_cases", edge_case_tests;
  ]
