(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* psl_test.ml - Command-line tool for testing the Public Suffix List library

   Usage:
     psl_test registrable <domain>
     psl_test suffix <domain>
     psl_test is_suffix <domain>
     psl_test is_registrable <domain>

   This tool is used by the cram tests to verify correct behavior.
*)

let psl = Publicsuffix.create ()
let print_error e = Printf.printf "ERROR: %s\n" (Publicsuffix.error_to_string e)
let print_result = function Ok s -> print_endline s | Error e -> print_error e

let print_bool_result = function
  | Ok b -> print_endline (string_of_bool b)
  | Error e -> print_error e

let print_result_with_section = function
  | Ok (s, sec) ->
      let sec_str =
        match sec with
        | Publicsuffix.ICANN -> "ICANN"
        | Publicsuffix.Private -> "PRIVATE"
      in
      Printf.printf "%s (%s)\n" s sec_str
  | Error e -> print_error e

let () =
  if Array.length Sys.argv < 2 then begin
    print_endline "Usage: psl_test <command> [args...]";
    print_endline "Commands:";
    print_endline "  registrable <domain>    - Get registrable domain";
    print_endline "  suffix <domain>         - Get public suffix";
    print_endline
      "  is_suffix <domain>      - Check if domain is a public suffix";
    print_endline
      "  is_registrable <domain> - Check if domain is a registrable domain";
    print_endline
      "  registrable_section <domain> - Get registrable domain with section";
    print_endline "  suffix_section <domain> - Get public suffix with section";
    print_endline "  stats                   - Print rule statistics";
    exit 1
  end;
  match Sys.argv.(1) with
  | "registrable" when Array.length Sys.argv >= 3 ->
      print_result (Publicsuffix.registrable_domain psl Sys.argv.(2))
  | "suffix" when Array.length Sys.argv >= 3 ->
      print_result (Publicsuffix.public_suffix psl Sys.argv.(2))
  | "is_suffix" when Array.length Sys.argv >= 3 ->
      print_bool_result (Publicsuffix.is_public_suffix psl Sys.argv.(2))
  | "is_registrable" when Array.length Sys.argv >= 3 ->
      print_bool_result (Publicsuffix.is_registrable_domain psl Sys.argv.(2))
  | "registrable_section" when Array.length Sys.argv >= 3 ->
      print_result_with_section
        (Publicsuffix.registrable_domain_with_section psl Sys.argv.(2))
  | "suffix_section" when Array.length Sys.argv >= 3 ->
      print_result_with_section
        (Publicsuffix.public_suffix_with_section psl Sys.argv.(2))
  | "stats" ->
      Printf.printf "Total rules: %d\n" (Publicsuffix.rule_count psl);
      Printf.printf "ICANN rules: %d\n" (Publicsuffix.icann_rule_count psl);
      Printf.printf "Private rules: %d\n" (Publicsuffix.private_rule_count psl)
  | cmd ->
      Printf.eprintf "Unknown command or missing arguments: %s\n" cmd;
      exit 1
