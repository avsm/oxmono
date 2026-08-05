(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP-date parsing per RFC 9110 Section 5.6.7 *)

let src = Logs.Src.create "requests.http_date" ~doc:"HTTP Date Parsing"
module Log = (val Logs.src_log src : Logs.LOG)

(** Parse HTTP-date (RFC 9110 Section 5.6.7) to Ptime.t *)
let parse s =
  (* HTTP-date format: "Sun, 06 Nov 1994 08:49:37 GMT" (RFC 1123) *)
  (* Also supports obsolete formats per RFC 9110 *)
  let s = String.trim s in

  (* Helper to parse month name *)
  let parse_month month_str =
    match String.lowercase_ascii month_str with
    | "jan" -> 1 | "feb" -> 2 | "mar" -> 3 | "apr" -> 4
    | "may" -> 5 | "jun" -> 6 | "jul" -> 7 | "aug" -> 8
    | "sep" -> 9 | "oct" -> 10 | "nov" -> 11 | "dec" -> 12
    | _ -> failwith "invalid month"
  in

  (* Try different date formats in order of preference *)
  let parsers = [
    (* RFC 1123 format: "Sun, 06 Nov 1994 08:49:37 GMT" *)
    (fun () ->
       Scanf.sscanf s "%_s %d %s %d %d:%d:%d GMT"
         (fun day month_str year hour min sec ->
            let month = parse_month month_str in
            Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0))));

    (* RFC 850 format: "Sunday, 06-Nov-94 08:49:37 GMT" *)
    (fun () ->
       Scanf.sscanf s "%_s %d-%s@-%d %d:%d:%d GMT"
         (fun day month_str year2 hour min sec ->
            let year = if year2 >= 70 then 1900 + year2 else 2000 + year2 in
            let month = parse_month month_str in
            Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0))));

    (* ANSI C asctime() format: "Sun Nov  6 08:49:37 1994" *)
    (fun () ->
       Scanf.sscanf s "%_s %s %d %d:%d:%d %d"
         (fun month_str day hour min sec year ->
            let month = parse_month month_str in
            Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0))));
  ] in

  (* Try each parser until one succeeds *)
  List.find_map (fun parser ->
    try parser () with _ -> None
  ) parsers
