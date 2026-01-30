(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
---------------------------------------------------------------------------*)

open Cmdliner

let psl = lazy (Publicsuffix.create ())
let psl () = Lazy.force psl

(* Helper functions for printing results *)

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

let registrable_cmd =
  let doc = "Get the registrable domain for a given domain" in
  let info = Cmd.info "registrable" ~doc in
  let term =
    Term.(const print_result $ Publicsuffix_cmd.registrable_term (psl ()))
  in
  Cmd.v info term

let suffix_cmd =
  let doc = "Get the public suffix for a given domain" in
  let info = Cmd.info "suffix" ~doc in
  let term =
    Term.(const print_result $ Publicsuffix_cmd.suffix_term (psl ()))
  in
  Cmd.v info term

let is_suffix_cmd =
  let doc = "Check if a domain is a public suffix" in
  let info = Cmd.info "is_suffix" ~doc in
  let term =
    Term.(const print_bool_result $ Publicsuffix_cmd.is_suffix_term (psl ()))
  in
  Cmd.v info term

let is_registrable_cmd =
  let doc = "Check if a domain is a registrable domain" in
  let info = Cmd.info "is_registrable" ~doc in
  let term =
    Term.(
      const print_bool_result $ Publicsuffix_cmd.is_registrable_term (psl ()))
  in
  Cmd.v info term

let registrable_section_cmd =
  let doc = "Get the registrable domain with section information" in
  let info = Cmd.info "registrable_section" ~doc in
  let term =
    Term.(
      const print_result_with_section
      $ Publicsuffix_cmd.registrable_section_term (psl ()))
  in
  Cmd.v info term

let suffix_section_cmd =
  let doc = "Get the public suffix with section information" in
  let info = Cmd.info "suffix_section" ~doc in
  let term =
    Term.(
      const print_result_with_section
      $ Publicsuffix_cmd.suffix_section_term (psl ()))
  in
  Cmd.v info term

let stats_cmd =
  let doc = "Print statistics about the Public Suffix List" in
  let info = Cmd.info "stats" ~doc in
  let term =
    Term.(
      const (fun (total, icann, private_rules) ->
          Printf.printf "Total rules: %d\n" total;
          Printf.printf "ICANN rules: %d\n" icann;
          Printf.printf "Private rules: %d\n" private_rules)
      $ Publicsuffix_cmd.stats_term (psl ()))
  in
  Cmd.v info term

let version_cmd =
  let doc = "Print version information about the Public Suffix List data" in
  let info = Cmd.info "version" ~doc in
  let term =
    Term.(
      const (fun (version, commit) ->
          Printf.printf "Version: %s\n" version;
          Printf.printf "Commit: %s\n" commit)
      $ Publicsuffix_cmd.version_term (psl ()))
  in
  Cmd.v info term

let default_cmd =
  let doc = "Query the Public Suffix List" in
  let sdocs = Manpage.s_common_options in
  let info = Cmd.info "publicsuffix" ~version:"%%VERSION%%" ~doc ~sdocs in
  Cmd.group info
    [
      registrable_cmd;
      suffix_cmd;
      is_suffix_cmd;
      is_registrable_cmd;
      registrable_section_cmd;
      suffix_section_cmd;
      stats_cmd;
      version_cmd;
    ]

let () = exit (Cmd.eval default_cmd)
