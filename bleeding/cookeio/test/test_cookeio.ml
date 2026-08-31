(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Tests for the cookeio cookie model and the client jar. *)

let time f = Option.get (Ptime.of_float_s f)
let date d t = Option.get (Ptime.of_date_time (d, (t, 0)))

let ptime = Alcotest.testable Ptime.pp Ptime.equal

let expiry =
  let pp ppf = function
    | `Session -> Format.pp_print_string ppf "session"
    | `At t -> Format.fprintf ppf "at %a" Ptime.pp t
  in
  let equal a b =
    match a, b with
    | `Session, `Session -> true
    | `At a, `At b -> Ptime.equal a b
    | _ -> false
  in
  Alcotest.testable pp equal

let now = time 1000.0

let parse ?(now = now) ?(host = "www.example.com") ?(path = "/") line =
  Cookeio.parse_set_cookie ~now ~host ~path line

let parse_ok ?now ?host ?path line =
  match parse ?now ?host ?path line with
  | Ok c -> c
  | Error e -> Alcotest.failf "expected %S to parse, got: %s" line e

let parse_err ?now ?host ?path line =
  match parse ?now ?host ?path line with
  | Ok _ -> Alcotest.failf "expected %S to be rejected" line
  | Error e -> e

let contains ~sub s =
  let n = String.length sub in
  let rec go i = i + n <= String.length s
                 && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0

let check_reason line reason sub =
  if not (contains ~sub reason) then
    Alcotest.failf "rejection of %S said %S, expected it to mention %S"
      line reason sub

(* {1 Set-Cookie basics} *)

let test_parse_basic () =
  let c = parse_ok "sid=abc123" in
  Alcotest.(check string) "name" "sid" (Cookeio.name c);
  Alcotest.(check string) "value" "abc123" (Cookeio.value c);
  Alcotest.(check string) "domain is the host" "www.example.com"
    (Cookeio.domain c);
  Alcotest.(check bool) "host-only without Domain" true (Cookeio.host_only c);
  Alcotest.(check string) "default path" "/" (Cookeio.path c);
  Alcotest.(check expiry) "session" `Session (Cookeio.expiry c);
  Alcotest.(check bool) "not secure" false (Cookeio.secure c);
  Alcotest.(check bool) "not http-only" false (Cookeio.http_only c);
  Alcotest.(check bool) "not partitioned" false (Cookeio.partitioned c);
  Alcotest.(check ptime) "creation stamped" now (Cookeio.creation_time c);
  Alcotest.(check ptime) "access stamped" now (Cookeio.last_access c)

let test_parse_flags () =
  let c = parse_ok "a=1; Secure; HttpOnly" in
  Alcotest.(check bool) "secure" true (Cookeio.secure c);
  Alcotest.(check bool) "http-only" true (Cookeio.http_only c);
  (* attribute names are case-insensitive (s5.2) *)
  let c = parse_ok "a=1; SECURE; httponly" in
  Alcotest.(check bool) "SECURE" true (Cookeio.secure c);
  Alcotest.(check bool) "httponly" true (Cookeio.http_only c)

let test_parse_unknown_attribute () =
  let c = parse_ok "a=1; Version=1; Comment=hi" in
  Alcotest.(check string) "cookie survives unknown attributes" "1"
    (Cookeio.value c)

let test_parse_syntax_errors () =
  check_reason "no-equals" (parse_err "no-equals") "missing '='";
  check_reason "=v" (parse_err "=v") "not a token";
  check_reason "bad name" (parse_err "a b=1") "not a token";
  check_reason "ctl in value" (parse_err "a=b\x01c") "invalid character";
  check_reason "semicolonless dquote" (parse_err "a=\"b") "invalid character"

let test_parse_value_forms () =
  Alcotest.(check string) "spaces allowed as browsers do" "delete me"
    (Cookeio.value (parse_ok "a=delete me"));
  let quoted = parse_ok {|a="quoted"|} in
  Alcotest.(check string) "quoted value kept verbatim" {|"quoted"|}
    (Cookeio.value quoted);
  Alcotest.(check string) "trimmed strips the wrapper" "quoted"
    (Cookeio.value_trimmed quoted);
  (* an unmatched quote is not a wrapper (and, arriving on the wire,
     not a valid value at all) *)
  let half =
    Cookeio.v ~domain:"e.com" ~path:"/" ~name:"a" ~value:{|"half|}
      ~expiry:`Session ~now ()
  in
  Alcotest.(check string) "no wrapper, no trim" {|"half|}
    (Cookeio.value_trimmed half);
  Alcotest.(check string) "empty value" ""
    (Cookeio.value (parse_ok "a="))

(* {1 The Domain attribute (s5.2.3/s5.3)} *)

let test_domain_widening () =
  let c = parse_ok "a=1; Domain=example.com" in
  Alcotest.(check bool) "not host-only" false (Cookeio.host_only c);
  Alcotest.(check string) "widened to the parent" "example.com"
    (Cookeio.domain c)

let test_domain_normalization () =
  Alcotest.(check string) "leading dot stripped" "example.com"
    (Cookeio.domain (parse_ok "a=1; Domain=.example.com"));
  Alcotest.(check string) "lowercased" "example.com"
    (Cookeio.domain (parse_ok "a=1; Domain=EXAMPLE.Com"))

let test_domain_empty_ignored () =
  let c = parse_ok "a=1; Domain=" in
  Alcotest.(check bool) "empty Domain leaves host-only" true
    (Cookeio.host_only c);
  Alcotest.(check string) "domain is the host" "www.example.com"
    (Cookeio.domain c)

let test_domain_must_cover_host () =
  let line = "a=1; Domain=other.com" in
  check_reason line (parse_err line) "does not cover";
  (* narrowing to a subdomain is not covering either *)
  let line = "a=1; Domain=www.example.com" in
  check_reason line (parse_err ~host:"example.com" line) "does not cover";
  (* dot-alignment: notexample.com is not covered by example.com's suffix *)
  let line = "a=1; Domain=example.com" in
  check_reason line (parse_err ~host:"notexample.com" line) "does not cover"

let test_domain_public_suffix () =
  let line = "a=1; Domain=com" in
  check_reason line (parse_err line) "public suffix";
  let line = "a=1; Domain=co.uk" in
  check_reason line (parse_err ~host:"foo.co.uk" line) "public suffix";
  (* a private-section suffix is refused the same way *)
  let line = "a=1; Domain=github.io" in
  check_reason line (parse_err ~host:"foo.github.io" line) "public suffix";
  (* unless the host is exactly that suffix *)
  let c = parse_ok ~host:"github.io" "a=1; Domain=github.io" in
  Alcotest.(check string) "exact-match public suffix allowed" "github.io"
    (Cookeio.domain c)

let test_domain_ip_literal () =
  (* an IP may set a host-only cookie for itself, and even name itself *)
  let c = parse_ok ~host:"127.0.0.1" "a=1; Domain=127.0.0.1" in
  Alcotest.(check string) "IP self-domain" "127.0.0.1" (Cookeio.domain c);
  (* but a suffix of an IP is not a domain match *)
  let line = "a=1; Domain=0.1" in
  check_reason line (parse_err ~host:"127.0.0.1" line) "does not cover"

(* {1 The Path attribute (s5.2.4/s5.1.4)} *)

let test_path_attribute () =
  Alcotest.(check string) "explicit path" "/app"
    (Cookeio.path (parse_ok "a=1; Path=/app"));
  Alcotest.(check string) "default from request path" "/a/b"
    (Cookeio.path (parse_ok ~path:"/a/b/c.html" "a=1"));
  Alcotest.(check string) "default at one level" "/"
    (Cookeio.path (parse_ok ~path:"/a" "a=1"));
  Alcotest.(check string) "relative Path takes the default" "/a"
    (Cookeio.path (parse_ok ~path:"/a/b" "a=1; Path=relative"));
  Alcotest.(check string) "empty Path takes the default" "/a"
    (Cookeio.path (parse_ok ~path:"/a/b" "a=1; Path="))

(* {1 Expiry (s5.2.1/s5.2.2/s5.3 step 3)} *)

let test_max_age () =
  Alcotest.(check expiry) "Max-Age adds to now" (`At (time 1060.0))
    (Cookeio.expiry (parse_ok "a=1; Max-Age=60"));
  let zero = parse_ok "a=1; Max-Age=0" in
  Alcotest.(check bool) "Max-Age=0 is already expired" true
    (Cookeio.is_expired ~now zero);
  let neg = parse_ok "a=1; Max-Age=-1" in
  Alcotest.(check bool) "negative Max-Age is already expired" true
    (Cookeio.is_expired ~now neg)

let test_max_age_wins_over_expires () =
  let exp = "Expires=Wed, 21 Oct 2015 07:28:00 GMT" in
  Alcotest.(check expiry) "Max-Age first" (`At (time 1060.0))
    (Cookeio.expiry (parse_ok (Fmt.str "a=1; Max-Age=60; %s" exp)));
  Alcotest.(check expiry) "Expires first" (`At (time 1060.0))
    (Cookeio.expiry (parse_ok (Fmt.str "a=1; %s; Max-Age=60" exp)))

let test_max_age_lexing () =
  Alcotest.(check expiry) "hex is not digits" `Session
    (Cookeio.expiry (parse_ok "a=1; Max-Age=0x10"));
  Alcotest.(check expiry) "underscores are not digits" `Session
    (Cookeio.expiry (parse_ok "a=1; Max-Age=1_000"));
  Alcotest.(check expiry) "an unparseable repeat keeps the earlier value"
    (`At (time 1005.0))
    (Cookeio.expiry (parse_ok "a=1; Max-Age=5; Max-Age=x"))

let test_expires_formats () =
  let expect = `At (date (2015, 10, 21) (7, 28, 0)) in
  let check fmt line =
    Alcotest.(check expiry) fmt expect (Cookeio.expiry (parse_ok line))
  in
  check "rfc1123" "a=1; Expires=Wed, 21 Oct 2015 07:28:00 GMT";
  check "rfc850" "a=1; Expires=Wednesday, 21-Oct-15 07:28:00 GMT";
  check "asctime" "a=1; Expires=Wed Oct 21 07:28:00 2015";
  check "dashed variant" "a=1; Expires=Wed, 21-Oct-2015 07:28:00 GMT";
  check "case-insensitive month and tz"
    "a=1; Expires=Wed, 21 OCT 2015 07:28:00 gmt"

let test_expires_two_digit_years () =
  Alcotest.(check expiry) "95 is 1995" (`At (date (1995, 10, 21) (7, 28, 0)))
    (Cookeio.expiry (parse_ok "a=1; Expires=Sat, 21-Oct-95 07:28:00 GMT"));
  Alcotest.(check expiry) "25 is 2025" (`At (date (2025, 10, 21) (7, 28, 0)))
    (Cookeio.expiry (parse_ok "a=1; Expires=Tue, 21-Oct-25 07:28:00 GMT"));
  (* the s5.1.1 boundary: 69 is on the 2000 side, 70 on the 1900 side *)
  Alcotest.(check expiry) "69 is 2069" (`At (date (2069, 10, 21) (7, 28, 0)))
    (Cookeio.expiry (parse_ok "a=1; Expires=Mon, 21-Oct-69 07:28:00 GMT"));
  Alcotest.(check expiry) "70 is 1970" (`At (date (1970, 10, 21) (7, 28, 0)))
    (Cookeio.expiry (parse_ok "a=1; Expires=Wed, 21-Oct-70 07:28:00 GMT"))

let test_expires_invalid_ignored () =
  Alcotest.(check expiry) "unparseable Expires leaves a session cookie"
    `Session
    (Cookeio.expiry (parse_ok "a=1; Expires=not-a-date"))

(* {1 Prefixes, SameSite, Partitioned (RFC 6265bis, CHIPS)} *)

let test_secure_prefix () =
  check_reason "__Secure- without Secure"
    (parse_err "__Secure-a=1") "__Secure-";
  ignore (parse_ok "__Secure-a=1; Secure");
  (* matched case-insensitively *)
  check_reason "__secure- without Secure"
    (parse_err "__secure-a=1") "__Secure-";
  Alcotest.(check bool) "has_secure_prefix" true
    (Cookeio.has_secure_prefix "__HOST-x");
  Alcotest.(check bool) "plain name" false (Cookeio.has_secure_prefix "sid")

let test_host_prefix () =
  check_reason "__Host- without Secure" (parse_err "__Host-a=1; Path=/")
    "Secure";
  check_reason "__Host- with Domain"
    (parse_err "__Host-a=1; Secure; Path=/; Domain=example.com") "Domain";
  check_reason "__Host- without Path=/"
    (parse_err "__Host-a=1; Secure; Path=/app") "Path=/";
  let c = parse_ok "__Host-a=1; Secure; Path=/" in
  Alcotest.(check bool) "well-formed __Host- accepted" true
    (Cookeio.host_only c)

let test_same_site () =
  let ss line = Cookeio.same_site (parse_ok line) in
  Alcotest.(check bool) "strict" true (ss "a=1; SameSite=Strict" = Some `Strict);
  Alcotest.(check bool) "lax" true (ss "a=1; SameSite=Lax" = Some `Lax);
  Alcotest.(check bool) "none+secure" true
    (ss "a=1; SameSite=None; Secure" = Some `None);
  Alcotest.(check bool) "invalid value ignored" true
    (ss "a=1; SameSite=Odd" = None);
  check_reason "SameSite=None without Secure"
    (parse_err "a=1; SameSite=None") "SameSite=None"

let test_partitioned () =
  let c = parse_ok "a=1; Secure; Partitioned" in
  Alcotest.(check bool) "partitioned" true (Cookeio.partitioned c);
  check_reason "Partitioned without Secure"
    (parse_err "a=1; Partitioned") "Partitioned"

(* {1 Matching and ordering} *)

let test_domain_suffix_matches () =
  let m sub d = Cookeio.domain_suffix_matches ~sub d in
  Alcotest.(check bool) "equal" true (m "example.com" "example.com");
  Alcotest.(check bool) "subdomain" true (m "a.b.example.com" "example.com");
  Alcotest.(check bool) "not dot-aligned" false
    (m "notexample.com" "example.com");
  Alcotest.(check bool) "other direction" false
    (m "example.com" "www.example.com");
  Alcotest.(check bool) "IPv4 never suffix-matches" false
    (m "127.0.0.1" "0.1");
  Alcotest.(check bool) "IPv6 never suffix-matches" false
    (m "::1" "1")

let test_domain_matches_host_only () =
  let host_only = parse_ok ~host:"example.com" "a=1" in
  Alcotest.(check bool) "exact host" true
    (Cookeio.domain_matches ~host:"example.com" host_only);
  Alcotest.(check bool) "subdomain refused" false
    (Cookeio.domain_matches ~host:"www.example.com" host_only);
  let widened = parse_ok ~host:"example.com" "a=1; Domain=example.com" in
  Alcotest.(check bool) "widened cookie matches subdomain" true
    (Cookeio.domain_matches ~host:"www.example.com" widened)

let test_path_matches () =
  let at path = parse_ok ~host:"e.com" (Fmt.str "a=1; Path=%s" path) in
  let m request_path c = Cookeio.path_matches ~request_path c in
  Alcotest.(check bool) "identical" true (m "/app" (at "/app"));
  Alcotest.(check bool) "trailing slash prefix" true (m "/app/x" (at "/app/"));
  Alcotest.(check bool) "slash boundary" true (m "/app/x" (at "/app"));
  Alcotest.(check bool) "no false prefix" false (m "/apple" (at "/app"));
  Alcotest.(check bool) "root matches all" true (m "/anything" (at "/"));
  Alcotest.(check bool) "sibling no match" false (m "/other" (at "/app"))

let test_compare_order () =
  let c ~path ~now ~name =
    Cookeio.v ~domain:"e.com" ~path ~name ~value:"v" ~expiry:`Session
      ~now ()
  in
  let long = c ~path:"/a/b" ~now:(time 2000.0) ~name:"long" in
  let short_old = c ~path:"/a" ~now:(time 1000.0) ~name:"old" in
  let short_new = c ~path:"/a" ~now:(time 2000.0) ~name:"new" in
  let tie_a = c ~path:"/a" ~now:(time 1000.0) ~name:"aaa" in
  Alcotest.(check bool) "longer path first" true
    (Cookeio.compare_order long short_old < 0);
  Alcotest.(check bool) "earlier creation first" true
    (Cookeio.compare_order short_old short_new < 0);
  Alcotest.(check bool) "same tick falls back to the name" true
    (Cookeio.compare_order tie_a short_old < 0)

let test_is_expired () =
  let session = parse_ok "a=1" in
  Alcotest.(check bool) "session never expires" false
    (Cookeio.is_expired ~now:(time 4e9) session);
  let dated = parse_ok "a=1; Max-Age=60" in
  Alcotest.(check bool) "before" false
    (Cookeio.is_expired ~now:(time 1060.0) dated);
  Alcotest.(check bool) "after" true
    (Cookeio.is_expired ~now:(time 1061.0) dated)

(* {1 The server side} *)

let test_parse_cookie_header () =
  Alcotest.(check (list (pair string string))) "basic"
    [ ("a", "1"); ("b", "2") ]
    (Cookeio.parse_cookie_header "a=1; b=2");
  Alcotest.(check (list (pair string string)))
    "repeated names kept in order"
    [ ("sid", "specific"); ("sid", "general") ]
    (Cookeio.parse_cookie_header "sid=specific; sid=general");
  Alcotest.(check (list (pair string string))) "malformed segments dropped"
    [ ("ok", "1") ]
    (Cookeio.parse_cookie_header "stray; bad name=1; ok=1; b=\x01");
  Alcotest.(check (list (pair string string))) "empty header" []
    (Cookeio.parse_cookie_header "")

let test_set_cookie_header () =
  let base ?(host_only = true) ?(secure = false) ?(http_only = false)
      ?(partitioned = false) ?same_site ~expiry () =
    Cookeio.v ~domain:"example.com" ~path:"/app" ~name:"sid" ~value:"abc"
      ~host_only ~secure ~http_only ~partitioned ?same_site ~expiry ~now ()
  in
  Alcotest.(check string) "host-only omits Domain" "sid=abc; Path=/app"
    (Cookeio.set_cookie_header (base ~expiry:`Session ()));
  Alcotest.(check string) "widened names its Domain"
    "sid=abc; Domain=example.com; Path=/app"
    (Cookeio.set_cookie_header (base ~host_only:false ~expiry:`Session ()));
  Alcotest.(check string) "expiry as IMF-fixdate"
    "sid=abc; Expires=Wed, 21 Oct 2015 07:28:00 GMT; Path=/app"
    (Cookeio.set_cookie_header
       (base ~expiry:(`At (date (2015, 10, 21) (7, 28, 0))) ()));
  Alcotest.(check string) "all the flags"
    "sid=abc; Path=/app; Secure; HttpOnly; Partitioned; SameSite=Lax"
    (Cookeio.set_cookie_header
       (base ~secure:true ~http_only:true ~partitioned:true ~same_site:`Lax
          ~expiry:`Session ()))

let test_set_cookie_round_trip () =
  (* what a server emits, a client stores back unchanged *)
  let emitted =
    Cookeio.v ~domain:"example.com" ~path:"/" ~name:"sid" ~value:"abc"
      ~host_only:false ~secure:true ~http_only:true
      ~expiry:(`At (date (2036, 10, 21) (7, 28, 0))) ~now ()
  in
  let line = Cookeio.set_cookie_header emitted in
  let stored = parse_ok ~host:"www.example.com" ~path:"/" line in
  Alcotest.(check string) "name" "sid" (Cookeio.name stored);
  Alcotest.(check string) "value" "abc" (Cookeio.value stored);
  Alcotest.(check string) "domain" "example.com" (Cookeio.domain stored);
  Alcotest.(check bool) "host_only" false (Cookeio.host_only stored);
  Alcotest.(check string) "path" "/" (Cookeio.path stored);
  Alcotest.(check bool) "secure" true (Cookeio.secure stored);
  Alcotest.(check bool) "http_only" true (Cookeio.http_only stored);
  Alcotest.(check expiry) "expiry"
    (`At (date (2036, 10, 21) (7, 28, 0))) (Cookeio.expiry stored)

let test_valid_name_value () =
  Alcotest.(check bool) "token name" true (Cookeio.valid_name "SID_1");
  Alcotest.(check bool) "empty name" false (Cookeio.valid_name "");
  Alcotest.(check bool) "separator in name" false (Cookeio.valid_name "a=b");
  Alcotest.(check bool) "plain value" true (Cookeio.valid_value "abc");
  Alcotest.(check bool) "space allowed" true (Cookeio.valid_value "a b");
  Alcotest.(check bool) "semicolon refused" false (Cookeio.valid_value "a;b");
  Alcotest.(check bool) "ctl refused" false (Cookeio.valid_value "a\nb")

(* {1 The jar: storage and retrieval} *)

let mock_jar ?(at = 1000.0) () =
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock at;
  (Cookeio_jar.in_memory ~clock (), clock)

let set_ok jar ?(host = "example.com") ?(path = "/") ?(https = true) line =
  match Cookeio_jar.set jar ~host ~path ~https line with
  | Ok () -> ()
  | Error e -> Alcotest.failf "jar refused %S: %s" line e

let set_err jar ?(host = "example.com") ?(path = "/") ?(https = true) line =
  match Cookeio_jar.set jar ~host ~path ~https line with
  | Ok () -> Alcotest.failf "expected the jar to refuse %S" line
  | Error e -> e

let header jar ?(host = "example.com") ?(path = "/") ?(https = true) () =
  Cookeio_jar.header_for jar ~host ~path ~https

let test_jar_basic () =
  Eio_mock.Backend.run @@ fun () ->
  let jar, _clock = mock_jar () in
  set_ok jar "sid=abc";
  Alcotest.(check (option string)) "returned to the host" (Some "sid=abc")
    (header jar ());
  Alcotest.(check (option string)) "not to another host" None
    (header jar ~host:"other.com" ());
  Alcotest.(check (option string)) "host-only: not to a subdomain" None
    (header jar ~host:"www.example.com" ());
  set_ok jar "wide=1; Domain=example.com";
  Alcotest.(check (option string)) "widened: to the subdomain too"
    (Some "wide=1")
    (header jar ~host:"www.example.com" ());
  Cookeio_jar.clear jar;
  Alcotest.(check (option string)) "cleared" None (header jar ())

let test_jar_secure_and_paths () =
  Eio_mock.Backend.run @@ fun () ->
  let jar, clock = mock_jar () in
  set_ok jar "s=1; Secure";
  Alcotest.(check (option string)) "secure withheld from http" None
    (header jar ~https:false ());
  Alcotest.(check (option string)) "secure sent over https" (Some "s=1")
    (header jar ~https:true ());
  (* path filtering and s5.4 order: longer path first, then creation *)
  Eio_mock.Clock.set_time clock 1001.0;
  set_ok jar ~path:"/app/x" "deep=1";
  Eio_mock.Clock.set_time clock 1002.0;
  set_ok jar "root=1";
  Alcotest.(check (option string)) "deeper path first"
    (Some "deep=1; s=1; root=1")
    (header jar ~path:"/app/x" ());
  Alcotest.(check (option string)) "path filtered" (Some "s=1; root=1")
    (header jar ~path:"/" ())

let test_jar_replacement () =
  Eio_mock.Backend.run @@ fun () ->
  let jar, clock = mock_jar () in
  set_ok jar "a=old";
  Eio_mock.Clock.set_time clock 2000.0;
  set_ok jar "b=other";
  set_ok jar "a=new";
  Alcotest.(check (option string))
    "replacement keeps the old creation order" (Some "a=new; b=other")
    (header jar ());
  Alcotest.(check int) "still two cookies" 2
    (List.length (Cookeio_jar.cookies jar))

let test_jar_max_age_zero_deletes () =
  Eio_mock.Backend.run @@ fun () ->
  let jar, _clock = mock_jar () in
  set_ok jar "a=1";
  set_ok jar "a=; Max-Age=0";
  Alcotest.(check (option string)) "deleted at once" None (header jar ());
  Alcotest.(check int) "and not stored" 0
    (List.length (Cookeio_jar.cookies jar))

let test_jar_expiry_eviction () =
  Eio_mock.Backend.run @@ fun () ->
  let jar, clock = mock_jar () in
  set_ok jar "a=1; Max-Age=5";
  Alcotest.(check (option string)) "live" (Some "a=1") (header jar ());
  Eio_mock.Clock.set_time clock 1006.0;
  Alcotest.(check (option string)) "expired" None (header jar ());
  Alcotest.(check int) "and evicted from the store" 0
    (List.length (Cookeio_jar.cookies jar))

let test_jar_refusals () =
  Eio_mock.Backend.run @@ fun () ->
  let jar, _clock = mock_jar () in
  (* a plaintext cookie may not shadow a stored Secure one *)
  set_ok jar ~https:true "sid=real; Secure";
  let reason = set_err jar ~https:false "sid=fake" in
  check_reason "shadowing" reason "shadow";
  Alcotest.(check (option string)) "the Secure cookie survives"
    (Some "sid=real") (header jar ~https:true ());
  (* prefixes assert a secure channel *)
  let reason = set_err jar ~https:false "__Secure-a=1; Secure" in
  check_reason "prefix over plaintext" reason "plaintext";
  set_ok jar ~https:true "__Secure-a=1; Secure";
  (* the s6.1 size bound *)
  let reason = set_err jar (Fmt.str "big=%s" (String.make 5000 'x')) in
  check_reason "oversized" reason "4096";
  (* and a parse rejection surfaces its reason *)
  let reason = set_err jar "a=1; Domain=other.com" in
  check_reason "parse reason" reason "does not cover"

let test_jar_per_domain_lru () =
  Eio_mock.Backend.run @@ fun () ->
  let jar, clock = mock_jar () in
  for i = 0 to 50 do
    Eio_mock.Clock.set_time clock (1000.0 +. float_of_int i);
    set_ok jar (Fmt.str "c%d=%d" i i)
  done;
  let stored = Cookeio_jar.cookies jar in
  Alcotest.(check int) "capped at 50 per domain" 50 (List.length stored);
  let names = List.map Cookeio.name stored in
  Alcotest.(check bool) "the least recently used was evicted" false
    (List.mem "c0" names);
  Alcotest.(check bool) "the newest survived" true (List.mem "c50" names)

(* {1 The jar: Netscape persistence} *)

let test_netscape_fixture env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "cookies.txt") in
  let jar = Cookeio_jar.of_file ~clock ~save:`Manual path in
  let cookies = Cookeio_jar.cookies jar in
  Alcotest.(check int) "all fixture lines load" 7 (List.length cookies);
  let find name =
    match List.find_opt (fun c -> Cookeio.name c = name) cookies with
    | Some c -> c
    | None -> Alcotest.failf "fixture cookie %s not loaded" name
  in
  Alcotest.(check expiry) "0 expiry is a session cookie" `Session
    (Cookeio.expiry (find "cookie-1"));
  Alcotest.(check expiry) "epoch expiry preserved"
    (`At (time 1257894000.0))
    (Cookeio.expiry (find "cookie-3"));
  Alcotest.(check bool) "plain line is host-only" true
    (Cookeio.host_only (find "cookie-1"));
  Alcotest.(check bool) "TRUE flag includes subdomains" false
    (Cookeio.host_only (find "cookie-2"));
  Alcotest.(check bool) "secure flag" true (Cookeio.secure (find "cookie-5"));
  Alcotest.(check bool) "#HttpOnly_ marking" true
    (Cookeio.http_only (find "cookie-6"));
  Alcotest.(check bool) "#HttpOnly_ with subdomains" false
    (Cookeio.host_only (find "cookie-7"))

let test_netscape_round_trip env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "round-trip.txt") in
  let jar = Cookeio_jar.of_file ~clock path in (* auto-saves on change *)
  set_ok jar "plain=1";
  set_ok jar "marked=2; HttpOnly; Domain=example.com";
  set_ok jar
    "dated=3; Secure; Expires=Wed, 21 Oct 2036 07:28:00 GMT";
  let jar' = Cookeio_jar.of_file ~clock ~save:`Manual path in
  let cookies = Cookeio_jar.cookies jar' in
  Alcotest.(check int) "all three round-trip" 3 (List.length cookies);
  let find name = List.find (fun c -> Cookeio.name c = name) cookies in
  Alcotest.(check bool) "HttpOnly survives the file" true
    (Cookeio.http_only (find "marked"));
  Alcotest.(check bool) "so does the subdomain flag" false
    (Cookeio.host_only (find "marked"));
  Alcotest.(check bool) "and Secure" true (Cookeio.secure (find "dated"));
  Alcotest.(check expiry) "and a whole-second expiry"
    (`At (date (2036, 10, 21) (7, 28, 0)))
    (Cookeio.expiry (find "dated"));
  Alcotest.(check expiry) "sessions stay sessions" `Session
    (Cookeio.expiry (find "plain"))

let test_netscape_manual_save env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "manual-save.txt") in
  let jar = Cookeio_jar.of_file ~clock ~save:`Manual path in
  set_ok jar "a=1";
  let reload () =
    Cookeio_jar.cookies (Cookeio_jar.of_file ~clock ~save:`Manual path)
  in
  Alcotest.(check int) "nothing written before flush" 0
    (List.length (reload ()));
  Cookeio_jar.flush jar;
  Alcotest.(check int) "flush writes" 1 (List.length (reload ()))

(* {1 Suite} *)

let () =
  Eio_main.run @@ fun env ->
  let open Alcotest in
  run "cookeio"
    [
      ( "set-cookie",
        [
          test_case "basics" `Quick test_parse_basic;
          test_case "flags" `Quick test_parse_flags;
          test_case "unknown attributes" `Quick test_parse_unknown_attribute;
          test_case "syntax errors" `Quick test_parse_syntax_errors;
          test_case "value forms" `Quick test_parse_value_forms;
        ] );
      ( "domain",
        [
          test_case "widening" `Quick test_domain_widening;
          test_case "normalization" `Quick test_domain_normalization;
          test_case "empty ignored" `Quick test_domain_empty_ignored;
          test_case "must cover the host" `Quick test_domain_must_cover_host;
          test_case "public suffixes" `Quick test_domain_public_suffix;
          test_case "IP literals" `Quick test_domain_ip_literal;
        ] );
      ( "path",
        [ test_case "attribute and defaults" `Quick test_path_attribute ] );
      ( "expiry",
        [
          test_case "max-age" `Quick test_max_age;
          test_case "max-age wins over expires" `Quick
            test_max_age_wins_over_expires;
          test_case "max-age lexing" `Quick test_max_age_lexing;
          test_case "expires formats" `Quick test_expires_formats;
          test_case "two-digit years" `Quick test_expires_two_digit_years;
          test_case "invalid expires ignored" `Quick
            test_expires_invalid_ignored;
        ] );
      ( "prefixes",
        [
          test_case "__Secure-" `Quick test_secure_prefix;
          test_case "__Host-" `Quick test_host_prefix;
          test_case "SameSite" `Quick test_same_site;
          test_case "Partitioned" `Quick test_partitioned;
        ] );
      ( "matching",
        [
          test_case "domain suffixes" `Quick test_domain_suffix_matches;
          test_case "host-only" `Quick test_domain_matches_host_only;
          test_case "paths" `Quick test_path_matches;
          test_case "ordering" `Quick test_compare_order;
          test_case "expiry" `Quick test_is_expired;
        ] );
      ( "server",
        [
          test_case "parse Cookie" `Quick test_parse_cookie_header;
          test_case "emit Set-Cookie" `Quick test_set_cookie_header;
          test_case "round trip" `Quick test_set_cookie_round_trip;
          test_case "syntax predicates" `Quick test_valid_name_value;
        ] );
      ( "jar",
        [
          test_case "store and retrieve" `Quick test_jar_basic;
          test_case "secure and paths" `Quick test_jar_secure_and_paths;
          test_case "replacement" `Quick test_jar_replacement;
          test_case "max-age=0 deletes" `Quick test_jar_max_age_zero_deletes;
          test_case "expiry eviction" `Quick test_jar_expiry_eviction;
          test_case "refusals" `Quick test_jar_refusals;
          test_case "per-domain LRU cap" `Quick test_jar_per_domain_lru;
        ] );
      ( "netscape",
        [
          test_case "fixture" `Quick (fun () -> test_netscape_fixture env);
          test_case "round trip" `Quick (fun () ->
              test_netscape_round_trip env);
          test_case "manual save" `Quick (fun () ->
              test_netscape_manual_save env);
        ] );
    ]
