(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Comprehensive tests for HTTP-date parsing per RFC 9110 Section 5.6.7 *)

let parse_http_date = Requests.Response.parse_http_date

(** Alcotest testable for Ptime.t *)
module Alcotest_ptime = struct
  let pp = Ptime.pp_rfc3339 ()
  let equal = Ptime.equal
  let testable = Alcotest.testable pp equal
end

(** Helper to create expected Ptime.t values *)
let make_time year month day hour min sec =
  match Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0)) with
  | Some t -> t
  | None -> failwith (Printf.sprintf "Invalid test time: %d-%02d-%02d %02d:%02d:%02d"
                        year month day hour min sec)

(** {1 RFC 1123 Format Tests} *)

let test_rfc1123_basic () =
  (* RFC 9110 Section 5.6.7: preferred format "Sun, 06 Nov 1994 08:49:37 GMT" *)
  let result = parse_http_date "Sun, 06 Nov 1994 08:49:37 GMT" in
  let expected = Some (make_time 1994 11 6 8 49 37) in
  Alcotest.(check (option Alcotest_ptime.testable))
    "RFC 1123 basic parsing" expected result

let test_rfc1123_all_months () =
  (* Test all month names *)
  let months = [
    ("Jan", 1); ("Feb", 2); ("Mar", 3); ("Apr", 4);
    ("May", 5); ("Jun", 6); ("Jul", 7); ("Aug", 8);
    ("Sep", 9); ("Oct", 10); ("Nov", 11); ("Dec", 12);
  ] in
  List.iter (fun (month_str, month_num) ->
    let date_str = Printf.sprintf "Mon, 01 %s 2020 00:00:00 GMT" month_str in
    let result = parse_http_date date_str in
    let expected = Some (make_time 2020 month_num 1 0 0 0) in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "RFC 1123 month %s" month_str) expected result
  ) months

let test_rfc1123_all_weekdays () =
  (* Test all weekday names - the weekday is not validated, just skipped *)
  let weekdays = ["Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"] in
  List.iter (fun wday ->
    let date_str = Printf.sprintf "%s, 06 Nov 1994 08:49:37 GMT" wday in
    let result = parse_http_date date_str in
    let expected = Some (make_time 1994 11 6 8 49 37) in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "RFC 1123 weekday %s" wday) expected result
  ) weekdays

let test_rfc1123_edge_dates () =
  (* Test edge cases for dates *)
  let test_cases = [
    ("Thu, 01 Jan 1970 00:00:00 GMT", 1970, 1, 1, 0, 0, 0, "Unix epoch");
    ("Fri, 31 Dec 1999 23:59:59 GMT", 1999, 12, 31, 23, 59, 59, "Y2K eve");
    ("Sat, 01 Jan 2000 00:00:00 GMT", 2000, 1, 1, 0, 0, 0, "Y2K");
    ("Tue, 29 Feb 2000 12:00:00 GMT", 2000, 2, 29, 12, 0, 0, "Leap year");
    ("Fri, 13 Dec 2024 23:59:59 GMT", 2024, 12, 13, 23, 59, 59, "Near current");
  ] in
  List.iter (fun (date_str, y, m, d, h, min, s, desc) ->
    let result = parse_http_date date_str in
    let expected = Some (make_time y m d h min s) in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "RFC 1123 edge: %s" desc) expected result
  ) test_cases

(** {1 RFC 850 Format Tests (Obsolete)} *)

let test_rfc850_basic () =
  (* RFC 850 format: "Sunday, 06-Nov-94 08:49:37 GMT" *)
  let result = parse_http_date "Sunday, 06-Nov-94 08:49:37 GMT" in
  let expected = Some (make_time 1994 11 6 8 49 37) in
  Alcotest.(check (option Alcotest_ptime.testable))
    "RFC 850 basic parsing (2-digit year)" expected result

let test_rfc850_year_interpretation () =
  (* Test Y2K two-digit year interpretation: 70-99 -> 1970-1999, 00-69 -> 2000-2069 *)
  let test_cases = [
    ("Monday, 01-Jan-70 00:00:00 GMT", 1970, "Year 70 -> 1970");
    ("Tuesday, 01-Jan-99 00:00:00 GMT", 1999, "Year 99 -> 1999");
    ("Saturday, 01-Jan-00 00:00:00 GMT", 2000, "Year 00 -> 2000");
    ("Sunday, 01-Jan-25 00:00:00 GMT", 2025, "Year 25 -> 2025");
    ("Thursday, 01-Jan-69 00:00:00 GMT", 2069, "Year 69 -> 2069");
  ] in
  List.iter (fun (date_str, expected_year, desc) ->
    let result = parse_http_date date_str in
    let expected = Some (make_time expected_year 1 1 0 0 0) in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "RFC 850 %s" desc) expected result
  ) test_cases

(** {1 ANSI C asctime() Format Tests (Obsolete)} *)

let test_asctime_basic () =
  (* asctime() format: "Sun Nov  6 08:49:37 1994" *)
  let result = parse_http_date "Sun Nov  6 08:49:37 1994" in
  let expected = Some (make_time 1994 11 6 8 49 37) in
  Alcotest.(check (option Alcotest_ptime.testable))
    "asctime basic parsing" expected result

let test_asctime_single_digit_day () =
  (* asctime has space-padded day for single digits *)
  let test_cases = [
    ("Sun Nov  1 08:49:37 1994", 1, "Day 1");
    ("Sun Nov  9 08:49:37 1994", 9, "Day 9");
  ] in
  List.iter (fun (date_str, day, desc) ->
    let result = parse_http_date date_str in
    let expected = Some (make_time 1994 11 day 8 49 37) in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "asctime %s" desc) expected result
  ) test_cases

(** {1 Invalid Input Tests} *)

let test_invalid_completely_wrong () =
  (* Completely invalid strings *)
  let invalid_inputs = [
    "";
    "not a date";
    "2024-12-13";  (* ISO 8601 not supported *)
    "12/13/2024";  (* US format not supported *)
    "13-Dec-2024"; (* No day name *)
  ] in
  List.iter (fun input ->
    let result = parse_http_date input in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "Invalid input: %S" input) None result
  ) invalid_inputs

let test_invalid_month_names () =
  (* Invalid month names *)
  let invalid_months = [
    "Sun, 06 Foo 1994 08:49:37 GMT";
    "Sun, 06 13 1994 08:49:37 GMT";  (* Numeric month *)
    "Sun, 06 November 1994 08:49:37 GMT";  (* Full month name *)
  ] in
  List.iter (fun input ->
    let result = parse_http_date input in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "Invalid month: %S" input) None result
  ) invalid_months

let test_invalid_dates () =
  (* Dates that are syntactically correct but semantically invalid *)
  let invalid_dates = [
    "Sun, 32 Jan 2020 00:00:00 GMT";  (* Day 32 *)
    "Sun, 00 Jan 2020 00:00:00 GMT";  (* Day 0 *)
    "Sun, 29 Feb 2021 00:00:00 GMT";  (* Feb 29 in non-leap year *)
    "Sun, 31 Apr 2020 00:00:00 GMT";  (* April has 30 days *)
  ] in
  List.iter (fun input ->
    let result = parse_http_date input in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "Invalid date: %S" input) None result
  ) invalid_dates

let test_invalid_times () =
  (* Invalid time components *)
  let invalid_times = [
    "Sun, 06 Nov 1994 25:00:00 GMT";  (* Hour 25 *)
    "Sun, 06 Nov 1994 00:60:00 GMT";  (* Minute 60 *)
    "Sun, 06 Nov 1994 00:00:60 GMT";  (* Second 60 (no leap second support) *)
  ] in
  List.iter (fun input ->
    let result = parse_http_date input in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "Invalid time: %S" input) None result
  ) invalid_times

(** {1 Whitespace and Case Tests} *)

let test_trimming_whitespace () =
  (* Should handle leading/trailing whitespace *)
  let test_cases = [
    "  Sun, 06 Nov 1994 08:49:37 GMT  ";
    "\tSun, 06 Nov 1994 08:49:37 GMT\t";
    "\n Sun, 06 Nov 1994 08:49:37 GMT \n";
  ] in
  let expected = Some (make_time 1994 11 6 8 49 37) in
  List.iter (fun input ->
    let result = parse_http_date input in
    Alcotest.(check (option Alcotest_ptime.testable))
      "Whitespace trimming" expected result
  ) test_cases

let test_case_insensitive_months () =
  (* Month names should be case-insensitive *)
  let test_cases = [
    ("Sun, 06 nov 1994 08:49:37 GMT", "lowercase");
    ("Sun, 06 NOV 1994 08:49:37 GMT", "uppercase");
    ("Sun, 06 NoV 1994 08:49:37 GMT", "mixed case");
  ] in
  let expected = Some (make_time 1994 11 6 8 49 37) in
  List.iter (fun (input, desc) ->
    let result = parse_http_date input in
    Alcotest.(check (option Alcotest_ptime.testable))
      (Printf.sprintf "Case insensitive: %s" desc) expected result
  ) test_cases

(** {1 Test Suite} *)

let () =
  Alcotest.run "HTTP Date Parsing (RFC 9110 Section 5.6.7)" [
    ("RFC 1123 format", [
      Alcotest.test_case "Basic parsing" `Quick test_rfc1123_basic;
      Alcotest.test_case "All months" `Quick test_rfc1123_all_months;
      Alcotest.test_case "All weekdays" `Quick test_rfc1123_all_weekdays;
      Alcotest.test_case "Edge dates" `Quick test_rfc1123_edge_dates;
    ]);
    ("RFC 850 format (obsolete)", [
      Alcotest.test_case "Basic parsing" `Quick test_rfc850_basic;
      Alcotest.test_case "Y2K year interpretation" `Quick test_rfc850_year_interpretation;
    ]);
    ("asctime format (obsolete)", [
      Alcotest.test_case "Basic parsing" `Quick test_asctime_basic;
      Alcotest.test_case "Single digit day" `Quick test_asctime_single_digit_day;
    ]);
    ("Invalid inputs", [
      Alcotest.test_case "Completely wrong format" `Quick test_invalid_completely_wrong;
      Alcotest.test_case "Invalid month names" `Quick test_invalid_month_names;
      Alcotest.test_case "Invalid dates" `Quick test_invalid_dates;
      Alcotest.test_case "Invalid times" `Quick test_invalid_times;
    ]);
    ("Whitespace and case", [
      Alcotest.test_case "Trimming whitespace" `Quick test_trimming_whitespace;
      Alcotest.test_case "Case insensitive months" `Quick test_case_insensitive_months;
    ]);
  ]
