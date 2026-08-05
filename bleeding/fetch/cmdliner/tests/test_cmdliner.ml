(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The command-line surface must stay exactly as [Requests.Cmd] had it, since
    the immich, typesense and peertube commands expose it verbatim. *)

module C = Fetch_cmdliner

let app = "myapp"

let eval : type a. a Cmdliner.Term.t -> string list -> a =
 fun term args ->
  let cmd = Cmdliner.Cmd.v (Cmdliner.Cmd.info "test") term in
  let argv = Array.of_list ("test" :: args) in
  match Cmdliner.Cmd.eval_value ~argv cmd with
  | Ok (`Ok v) -> v
  | Ok _ -> Alcotest.fail "unexpected cmdliner outcome"
  | Error _ -> Alcotest.fail ("cmdliner rejected: " ^ String.concat " " args)

let source_str s = Format.asprintf "%a" C.pp_source s

let contains needle s =
  let n = String.length needle and l = String.length s in
  let rec go i = i + n <= l && (String.sub s i n = needle || go (i + 1)) in
  go 0

(* ------------------------------------------------------------------ *)
(* Individual flags                                                    *)
(* ------------------------------------------------------------------ *)

let test_persist_cookies () =
  let t = C.persist_cookies_term app in
  Alcotest.(check bool) "default off" false (eval t []).value;
  Alcotest.(check string) "default source" "default" (source_str (eval t []).source);
  Alcotest.(check bool) "flag on" true (eval t [ "--persist-cookies" ]).value;
  Alcotest.(check string)
    "flag source" "cmdline"
    (source_str (eval t [ "--persist-cookies" ]).source)

let test_verify_tls () =
  let t = C.verify_tls_term app in
  Alcotest.(check bool) "verifies by default" true (eval t []).value;
  Alcotest.(check bool)
    "--no-verify-tls disables" false
    (eval t [ "--no-verify-tls" ]).value

let test_timeout () =
  let t = C.timeout_term app in
  Alcotest.(check (option (float 0.001))) "no default" None (eval t []).value;
  Alcotest.(check (option (float 0.001)))
    "--timeout" (Some 2.5)
    (eval t [ "--timeout"; "2.5" ]).value

let test_retries () =
  let t = C.retries_term app in
  Alcotest.(check int) "default 3" 3 (eval t []).value;
  Alcotest.(check int) "--max-retries" 7 (eval t [ "--max-retries"; "7" ]).value

let test_retry_backoff () =
  let t = C.retry_backoff_term app in
  Alcotest.(check (float 0.0001)) "default 0.3" 0.3 (eval t []).value;
  Alcotest.(check (float 0.0001))
    "--retry-backoff" 1.5
    (eval t [ "--retry-backoff"; "1.5" ]).value

let test_follow_redirects () =
  let t = C.follow_redirects_term app in
  Alcotest.(check bool) "follows by default" true (eval t []).value;
  Alcotest.(check bool)
    "--no-follow-redirects" false
    (eval t [ "--no-follow-redirects" ]).value

let test_max_redirects () =
  let t = C.max_redirects_term app in
  Alcotest.(check int) "default 10" 10 (eval t []).value;
  Alcotest.(check int)
    "--max-redirects" 2
    (eval t [ "--max-redirects"; "2" ]).value

let test_user_agent () =
  let t = C.user_agent_term app in
  Alcotest.(check (option string)) "no default" None (eval t []).value;
  Alcotest.(check (option string))
    "--user-agent" (Some "me/1.0")
    (eval t [ "--user-agent"; "me/1.0" ]).value

let test_verbose_http () =
  let t = C.verbose_http_term app in
  Alcotest.(check bool) "off by default" false (eval t []).value;
  Alcotest.(check bool) "--verbose-http" true (eval t [ "--verbose-http" ]).value

let test_proxy () =
  let t = C.proxy_term app in
  let c = eval t [ "--proxy"; "http://p:8080"; "--no-proxy"; "localhost" ] in
  let value o = Option.map (fun (w : string C.with_source) -> w.value) o in
  Alcotest.(check (option string))
    "--proxy" (Some "http://p:8080") (value c.proxy_url);
  Alcotest.(check (option string))
    "--no-proxy" (Some "localhost") (value c.no_proxy)

(* ------------------------------------------------------------------ *)
(* The whole term, and what it maps onto                               *)
(* ------------------------------------------------------------------ *)

let with_config args f () =
  Eio_main.run @@ fun env -> f (eval (C.config_term app env#fs) args)

let test_config_term c =
  Alcotest.(check bool) "verify_tls" false c.C.verify_tls.value;
  Alcotest.(check (option (float 0.001))) "timeout" (Some 12.) c.C.timeout.value;
  Alcotest.(check int) "max_retries" 0 c.C.max_retries.value;
  Alcotest.(check bool) "persist_cookies" true c.C.persist_cookies.value;
  Alcotest.(check (option string))
    "user_agent" (Some "ua/1") c.C.user_agent.value

let test_redirects_on c = Alcotest.(check int) "follows" 5 (C.redirects c)
let test_redirects_off c = Alcotest.(check int) "does not follow" 0 (C.redirects c)

let test_retry_config_none c =
  Alcotest.(check bool) "no retries" true (C.retry_config c = None)

let test_retry_config_some c =
  match C.retry_config c with
  | None -> Alcotest.fail "expected a retry config"
  | Some r ->
      Alcotest.(check int) "max_retries" 4 r.Fetch.Retry.max_retries;
      Alcotest.(check (float 0.0001))
        "backoff_factor" 1.25 r.Fetch.Retry.backoff_factor

let test_pp_config c =
  let with_sources = Format.asprintf "%a" (C.pp_config ~show_sources:true) c in
  let without = Format.asprintf "%a" (C.pp_config ~show_sources:false) c in
  Alcotest.(check bool) "sources shown" true (contains "[cmdline]" with_sources);
  Alcotest.(check bool) "sources hidden" false (contains "[cmdline]" without);
  Alcotest.(check bool) "has max_retries" true (contains "max_retries" without)

let test_env_docs () =
  let docs = C.env_docs app in
  List.iter
    (fun v ->
      Alcotest.(check bool) ("documents " ^ v) true (contains v docs))
    [ "MYAPP_TIMEOUT"; "MYAPP_MAX_RETRIES"; "MYAPP_VERBOSE_HTTP"; "NO_PROXY" ]

(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "fetch-cmdliner"
    [
      ( "flags",
        [
          ("--persist-cookies", `Quick, test_persist_cookies);
          ("--no-verify-tls", `Quick, test_verify_tls);
          ("--timeout", `Quick, test_timeout);
          ("--max-retries", `Quick, test_retries);
          ("--retry-backoff", `Quick, test_retry_backoff);
          ("--no-follow-redirects", `Quick, test_follow_redirects);
          ("--max-redirects", `Quick, test_max_redirects);
          ("--user-agent", `Quick, test_user_agent);
          ("--verbose-http", `Quick, test_verbose_http);
          ("--proxy/--no-proxy", `Quick, test_proxy);
          ("env_docs", `Quick, test_env_docs);
        ] );
      ( "mapping",
        [
          ( "config_term",
            `Quick,
            with_config
              [ "--no-verify-tls"; "--timeout"; "12"; "--max-retries"; "0";
                "--persist-cookies"; "--user-agent"; "ua/1" ]
              test_config_term );
          ( "redirects",
            `Quick,
            with_config [ "--max-redirects"; "5" ] test_redirects_on );
          ( "redirects disabled",
            `Quick,
            with_config
              [ "--no-follow-redirects"; "--max-redirects"; "5" ]
              test_redirects_off );
          ( "retry_config none",
            `Quick,
            with_config [ "--max-retries"; "0" ] test_retry_config_none );
          ( "retry_config",
            `Quick,
            with_config
              [ "--max-retries"; "4"; "--retry-backoff"; "1.25" ]
              test_retry_config_some );
          ( "pp_config",
            `Quick,
            with_config [ "--max-retries"; "4" ] test_pp_config );
        ] );
    ]
