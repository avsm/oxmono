(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Every contact in the live store must migrate, and the result must
    re-encode and decode unchanged. This is the gate for switching the store
    to V2.

    The store is read in place rather than copied into the repository,
    because it holds real personal data. That means this test cannot run in
    CI or in a fresh clone, so an absent store is a skip and not a
    failure.

    Beyond the pass/fail gate, this prints a migration summary: accounts
    produced per platform, how many [urls] entries were promoted to
    accounts versus kept as links, the most common link hosts, and the
    YAML line count before and after. The promoted/link split uses
    {!Sortal_schema.Migrate.classify_url}, the same decision [v1_to_v2]
    makes, so the summary cannot drift from what the migration actually
    does. The "top link hosts" grouping is display only: it groups the
    URLs [classify_url] already decided to keep as links, by a locally
    parsed host, purely so the summary reads well. Getting that grouping
    slightly wrong cannot make the promoted/link counts wrong, because it
    runs after the classification, not instead of it. *)

module Migrate = Sortal_schema.Migrate
module V1 = Sortal_schema.V1.Contact
module V2 = Sortal_schema.V2.Contact
module Account = Sortal_schema.V2.Account
module Platform = Sortal_schema.V2.Platform

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let store_dir () =
  match Sys.getenv_opt "SORTAL_DATA_DIR" with
  | Some d -> d
  | None -> Filename.concat (Sys.getenv "HOME") ".local/share/sortal"

(* The number of lines in [s], counting newline characters rather than the
   length of [String.split_on_char '\n' s]: every file in the store ends
   with a trailing newline, so splitting on it yields one extra, empty
   element after the last line, which would overcount every file by one. *)
let line_count s = String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 s

let incr_tbl tbl key =
  let n = match Hashtbl.find_opt tbl key with Some n -> n | None -> 0 in
  Hashtbl.replace tbl key (n + 1)

let sorted_by_key tbl =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let top_n tbl n =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (_, a) (_, b) -> compare (b : int) a)
  |> List.filteri (fun i _ -> i < n)

(* A host for the "top link hosts" table, for display only. Not the
   classification: a link's host has already been decided not to name a
   platform by [Migrate.classify_url] before this is ever called. *)
let display_host url =
  let cut_at c s =
    match String.index_opt s c with Some i -> String.sub s 0 i | None -> s
  in
  let url = cut_at '#' (cut_at '?' url) in
  match String.split_on_char '/' url with
  | _scheme :: "" :: host :: _ ->
      let host =
        if String.starts_with ~prefix:"www." host then
          String.sub host 4 (String.length host - 4)
        else host
      in
      Some host
  | _ -> None

let () =
  let dir = store_dir () in
  if not (Sys.file_exists dir && Sys.is_directory dir) then begin
    Printf.printf "- no store at %s, skipping migration check\n" dir;
    exit 0
  end;
  let files =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".yaml")
    |> List.sort String.compare
  in
  if files = [] then begin
    Printf.printf "- store at %s holds no contacts, skipping\n" dir;
    exit 0
  end;
  let failures = ref [] in
  let migrate_errors = ref 0 in
  let round_trip_mismatches = ref 0 in
  let clean = ref 0 in
  let account_counts = Hashtbl.create 16 in
  let promoted_counts = Hashtbl.create 16 in
  let link_hosts = Hashtbl.create 64 in
  let link_count = ref 0 in
  let lines_before = ref 0 in
  let lines_after = ref 0 in
  let yaml_encode_failures = ref 0 in
  List.iter
    (fun f ->
      let path = Filename.concat dir f in
      let yaml = read_file path in
      lines_before := !lines_before + line_count yaml;
      let reader = Bytesrw.Bytes.Reader.of_string yaml in
      match Yamlt.decode V1.json_t reader with
      | Error e -> failures := (f, "V1 decode: " ^ e) :: !failures
      | Ok v1 -> (
          (* Tally the promoted/link split against the same [urls] field
             the migration reads. This runs regardless of whether the
             migration itself succeeds, so a contact that later fails to
             migrate still contributes to the "shape of the input" part
             of the summary. *)
          List.iter
            (fun (u : V1.url_entry) ->
              match Migrate.classify_url u.url with
              | `Account a ->
                  incr_tbl promoted_counts (Platform.key (Account.platform a))
              | `Link -> (
                  incr link_count;
                  match display_host u.url with
                  | Some h -> incr_tbl link_hosts h
                  | None -> incr_tbl link_hosts "(no host)"))
            (V1.urls v1);
          match Migrate.v1_to_v2 v1 with
          | Error e ->
              incr migrate_errors;
              failures := (f, "migrate: " ^ e) :: !failures
          | Ok v2 -> (
              List.iter
                (fun a ->
                  incr_tbl account_counts (Platform.key (Account.platform a)))
                (V2.accounts v2);
              let buf = Buffer.create 4096 in
              let writer = Bytesrw.Bytes.Writer.of_buffer buf in
              (match Yamlt.encode V2.json_t v2 ~eod:true writer with
              | Ok () -> lines_after := !lines_after + line_count (Buffer.contents buf)
              | Error _ -> incr yaml_encode_failures);
              match Jsont_bytesrw.encode_string V2.json_t v2 with
              | Error e -> failures := (f, "V2 encode: " ^ e) :: !failures
              | Ok json -> (
                  match Jsont_bytesrw.decode_string V2.json_t json with
                  | Error e -> failures := (f, "V2 decode: " ^ e) :: !failures
                  | Ok again ->
                      if again <> v2 then begin
                        incr round_trip_mismatches;
                        failures := (f, "round trip differs") :: !failures
                      end
                      else incr clean))))
    files;
  Printf.printf "\n--- migration summary ---\n";
  Printf.printf
    "contacts: %d, clean: %d, migrate errors: %d, round-trip mismatches: %d\n"
    (List.length files) !clean !migrate_errors !round_trip_mismatches;
  Printf.printf "accounts by platform:\n";
  sorted_by_key account_counts
  |> List.iter (fun (k, v) -> Printf.printf "  %-12s %d\n" k v);
  Printf.printf "urls promoted to accounts by platform:\n";
  sorted_by_key promoted_counts
  |> List.iter (fun (k, v) -> Printf.printf "  %-12s %d\n" k v);
  Printf.printf "urls kept as links: %d\n" !link_count;
  Printf.printf "top 10 link hosts:\n";
  top_n link_hosts 10
  |> List.iter (fun (h, v) -> Printf.printf "  %-30s %d\n" h v);
  Printf.printf "yaml lines before: %d, projected after: %d\n" !lines_before
    !lines_after;
  if !yaml_encode_failures > 0 then
    Printf.printf "yaml encode failures (excluded from lines after): %d\n"
      !yaml_encode_failures;
  match !failures with
  | [] -> Printf.printf "\n\xe2\x9c\x93 %d contacts migrate cleanly\n" (List.length files)
  | fs ->
      List.iter (fun (f, why) -> Printf.eprintf "%s: %s\n" f why) fs;
      Printf.eprintf "%d of %d contacts failed\n" (List.length fs)
        (List.length files);
      exit 1
