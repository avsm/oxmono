open Cmdliner

let query_failed = ref false

let print_result = function
  | Ok s -> print_endline s
  | Error e ->
      query_failed := true;
      Printf.eprintf "ERROR: %s\n" (Pubsuffix.error_to_string e)
;;

let print_bool_result = function
  | Ok b -> print_endline (string_of_bool b)
  | Error e ->
      query_failed := true;
      Printf.eprintf "ERROR: %s\n" (Pubsuffix.error_to_string e)
;;

let print_result_with_section = function
  | Ok (s, sec) ->
    let sec_str =
      match sec with
      | Pubsuffix.ICANN -> "ICANN"
      | Pubsuffix.Private -> "PRIVATE"
    in
    Printf.printf "%s (%s)\n" s sec_str
  | Error e ->
      query_failed := true;
      Printf.eprintf "ERROR: %s\n" (Pubsuffix.error_to_string e)
;;

let domain_arg =
  let doc = "The domain name to query." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"DOMAIN" ~doc)
;;

let query name doc f printer =
  let term = Term.(const (fun d -> printer (f d)) $ domain_arg) in
  Cmd.v (Cmd.info name ~doc) term
;;

let report name doc f = Cmd.v (Cmd.info name ~doc) Term.(const f $ const ())

let stats_cmd =
  report "stats" "Print statistics about the Public Suffix List" (fun () ->
    Printf.printf "Total rules: %d\n" Pubsuffix.rule_count;
    Printf.printf "ICANN rules: %d\n" Pubsuffix.icann_rule_count;
    Printf.printf "Private rules: %d\n" Pubsuffix.private_rule_count)
;;

let version_cmd =
  report
    "version"
    "Print version information about the Public Suffix List data"
    (fun () ->
       Printf.printf "Version: %s\n" Pubsuffix.version;
       Printf.printf "Commit: %s\n" Pubsuffix.commit)
;;

let default_cmd =
  let doc = "Query the Public Suffix List" in
  let sdocs = Manpage.s_common_options in
  let info = Cmd.info "httpz-pubsuffix" ~version:"%%VERSION%%" ~doc ~sdocs in
  Cmd.group
    info
    [ query
        "registrable"
        "Get the registrable domain for a given domain"
        Pubsuffix.registrable_domain
        print_result
    ; query
        "suffix"
        "Get the public suffix for a given domain"
        Pubsuffix.public_suffix
        print_result
    ; query
        "is_suffix"
        "Check if a domain is a public suffix"
        Pubsuffix.is_public_suffix
        print_bool_result
    ; query
        "is_registrable"
        "Check if a domain is a registrable domain"
        Pubsuffix.is_registrable_domain
        print_bool_result
    ; query
        "registrable_section"
        "Get the registrable domain with section information"
        Pubsuffix.registrable_domain_with_section
        print_result_with_section
    ; query
        "suffix_section"
        "Get the public suffix with section information"
        Pubsuffix.public_suffix_with_section
        print_result_with_section
    ; stats_cmd
    ; version_cmd
    ]
;;

let () =
  let status = Cmd.eval default_cmd in
  exit (if !query_failed then 2 else status)
