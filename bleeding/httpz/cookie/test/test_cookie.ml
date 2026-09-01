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
;;

let now = time 1000.0

let parse ?(now = now) ?(host = "www.example.com") ?(path = "/") line =
  Cookie.parse_set_cookie ~now ~host ~path line
;;

let parse_ok ?now ?host ?path line =
  match parse ?now ?host ?path line with
  | Ok c -> c
  | Error e -> Alcotest.failf "expected %S to parse, got: %s" line e
;;

let parse_err ?now ?host ?path line =
  match parse ?now ?host ?path line with
  | Ok _ -> Alcotest.failf "expected %S to be rejected" line
  | Error e -> e
;;

let contains ~sub s =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0
;;

let check_reason line reason sub =
  if not (contains ~sub reason)
  then Alcotest.failf "rejection of %S said %S, expected it to mention %S" line reason sub
;;

(* {1 Set-Cookie basics} *)

let test_parse_basic () =
  let c = parse_ok "sid=abc123" in
  Alcotest.(check string) "name" "sid" (Cookie.name c);
  Alcotest.(check string) "value" "abc123" (Cookie.value c);
  Alcotest.(check string) "domain is the host" "www.example.com" (Cookie.domain c);
  Alcotest.(check bool) "host-only without Domain" true (Cookie.host_only c);
  Alcotest.(check string) "default path" "/" (Cookie.path c);
  Alcotest.(check expiry) "session" `Session (Cookie.expiry c);
  Alcotest.(check bool) "not secure" false (Cookie.secure c);
  Alcotest.(check bool) "not http-only" false (Cookie.http_only c);
  Alcotest.(check bool) "not partitioned" false (Cookie.partitioned c);
  Alcotest.(check ptime) "creation stamped" now (Cookie.creation_time c);
  Alcotest.(check ptime) "access stamped" now (Cookie.last_access c)
;;

let test_parse_flags () =
  let c = parse_ok "a=1; Secure; HttpOnly" in
  Alcotest.(check bool) "secure" true (Cookie.secure c);
  Alcotest.(check bool) "http-only" true (Cookie.http_only c);
  let c = parse_ok "a=1; SECURE; httponly" in
  Alcotest.(check bool) "SECURE" true (Cookie.secure c);
  Alcotest.(check bool) "httponly" true (Cookie.http_only c)
;;

let test_parse_unknown_attribute () =
  let c = parse_ok "a=1; Version=1; Comment=hi" in
  Alcotest.(check string) "cookie survives unknown attributes" "1" (Cookie.value c)
;;

let test_parse_syntax_errors () =
  check_reason "no-equals" (parse_err "no-equals") "missing '='";
  check_reason "=v" (parse_err "=v") "not a token";
  check_reason "bad name" (parse_err "a b=1") "not a token";
  check_reason "ctl in value" (parse_err "a=b\x01c") "invalid character";
  check_reason "semicolonless dquote" (parse_err "a=\"b") "invalid character"
;;

let test_parse_value_forms () =
  Alcotest.(check string)
    "spaces allowed as browsers do"
    "delete me"
    (Cookie.value (parse_ok "a=delete me"));
  let quoted = parse_ok {|a="quoted"|} in
  Alcotest.(check string) "quoted value kept verbatim" {|"quoted"|} (Cookie.value quoted);
  Alcotest.(check string)
    "trimmed strips the wrapper"
    "quoted"
    (Cookie.value_trimmed quoted);
  let bare =
    Cookie.v ~domain:"e.com" ~path:"/" ~name:"a" ~value:"half" ~expiry:`Session ~now ()
  in
  Alcotest.(check string) "no wrapper, no trim" "half" (Cookie.value_trimmed bare);
  (* An unbalanced quote is not a wrapper, so the octet rule rejects it. *)
  check_reason {|a="half|} (parse_err {|a="half|}) "invalid character";
  Alcotest.(check string) "empty value" "" (Cookie.value (parse_ok "a="))
;;

(* A name or value outside the grammar would emit a second pair into a Cookie
   header, so the constructor refuses one rather than the serializer. *)
let test_v_validates () =
  let raises what f =
    match f () with
    | (_ : Cookie.t) -> Alcotest.failf "expected %s to raise" what
    | exception Invalid_argument _ -> ()
  in
  let make ~name ~value () =
    Cookie.v ~domain:"e.com" ~path:"/" ~name ~value ~expiry:`Session ~now ()
  in
  raises "an empty name" (make ~name:"" ~value:"v");
  raises "a name with a separator" (make ~name:"a=b" ~value:"v");
  raises "a name with a space" (make ~name:"a b" ~value:"v");
  raises "a value with a semicolon" (make ~name:"a" ~value:"x; admin=1");
  raises "a value with a control byte" (make ~name:"a" ~value:"x\ny");
  raises "an unbalanced quote" (make ~name:"a" ~value:{|"half|});
  raises "an invalid domain" (fun () ->
    Cookie.v
      ~domain:"bad domain"
      ~path:"/"
      ~name:"a"
      ~value:"v"
      ~expiry:`Session
      ~now
      ());
  raises "a relative path" (fun () ->
    Cookie.v
      ~domain:"e.com"
      ~path:"relative"
      ~name:"a"
      ~value:"v"
      ~expiry:`Session
      ~now
      ());
  raises "a path with a control byte" (fun () ->
    Cookie.v
      ~domain:"e.com"
      ~path:"/x\ty"
      ~name:"a"
      ~value:"v"
      ~expiry:`Session
      ~now
      ());
  let ok = make ~name:"a" ~value:{|"quoted"|} () in
  Alcotest.(check string) "a quoted value is accepted" {|"quoted"|} (Cookie.value ok)
;;

(* {1 The Domain attribute (s5.2.3/s5.3)} *)

let test_domain_widening () =
  let c = parse_ok "a=1; Domain=example.com" in
  Alcotest.(check bool) "not host-only" false (Cookie.host_only c);
  Alcotest.(check string) "widened to the parent" "example.com" (Cookie.domain c)
;;

let test_domain_normalization () =
  Alcotest.(check string)
    "leading dot stripped"
    "example.com"
    (Cookie.domain (parse_ok "a=1; Domain=.example.com"));
  Alcotest.(check string)
    "lowercased"
    "example.com"
    (Cookie.domain (parse_ok "a=1; Domain=EXAMPLE.Com"))
;;

let test_domain_empty_ignored () =
  let c = parse_ok "a=1; Domain=" in
  Alcotest.(check bool) "empty Domain leaves host-only" true (Cookie.host_only c);
  Alcotest.(check string) "domain is the host" "www.example.com" (Cookie.domain c)
;;

let test_domain_must_cover_host () =
  let line = "a=1; Domain=other.com" in
  check_reason line (parse_err line) "does not cover";
  let line = "a=1; Domain=www.example.com" in
  check_reason line (parse_err ~host:"example.com" line) "does not cover";
  let line = "a=1; Domain=example.com" in
  check_reason line (parse_err ~host:"notexample.com" line) "does not cover"
;;

let test_domain_public_suffix () =
  let line = "a=1; Domain=com" in
  check_reason line (parse_err line) "public suffix";
  let line = "a=1; Domain=co.uk" in
  check_reason line (parse_err ~host:"foo.co.uk" line) "public suffix";
  let line = "a=1; Domain=github.io" in
  check_reason line (parse_err ~host:"foo.github.io" line) "public suffix";
  (* RFC 6265 s5.3 step 5: the attribute is ignored, not honoured, so the
     cookie stays on the suffix host and never reaches a site beneath it. *)
  let c = parse_ok ~host:"github.io" "a=1; Domain=github.io" in
  Alcotest.(check string)
    "exact-match public suffix stays put"
    "github.io"
    (Cookie.domain c);
  Alcotest.(check bool) "and is stored host-only" true (Cookie.host_only c);
  Alcotest.(check bool)
    "so a sibling never matches"
    false
    (Cookie.domain_matches ~host:"foo.github.io" c);
  let c = parse_ok ~host:"s3.amazonaws.com" "sid=x; Domain=s3.amazonaws.com" in
  Alcotest.(check bool) "the private section counts too" true (Cookie.host_only c);
  Alcotest.(check bool)
    "no bucket sees it"
    false
    (Cookie.domain_matches ~host:"evil-bucket.s3.amazonaws.com" c)
;;

let test_domain_ip_literal () =
  let c = parse_ok ~host:"127.0.0.1" "a=1; Domain=127.0.0.1" in
  Alcotest.(check string) "IP self-domain" "127.0.0.1" (Cookie.domain c);
  Alcotest.(check bool) "stored host-only" true (Cookie.host_only c);
  let line = "a=1; Domain=0.1" in
  check_reason line (parse_err ~host:"127.0.0.1" line) "does not cover";
  (* An address reached through a non-canonical spelling is still an address:
     [Domain=1] would otherwise suffix-match every other [.1] spelling. *)
  let line = "a=1; Domain=1" in
  check_reason line (parse_err ~host:"0x7f.1" line) "does not cover";
  let line = "a=1; Domain=1" in
  check_reason line (parse_err ~host:"127.0.0.1" line) "does not cover";
  (* A name whose last label is an address spelling cannot lend it its scope. *)
  let line = "a=1; Domain=0x1" in
  check_reason line (parse_err ~host:"foo.0x1" line) "IP literal"
;;

(* {1 The Path attribute (s5.2.4/s5.1.4)} *)

let test_path_attribute () =
  Alcotest.(check string) "explicit path" "/app" (Cookie.path (parse_ok "a=1; Path=/app"));
  Alcotest.(check string)
    "default from request path"
    "/a/b"
    (Cookie.path (parse_ok ~path:"/a/b/c.html" "a=1"));
  Alcotest.(check string)
    "default at one level"
    "/"
    (Cookie.path (parse_ok ~path:"/a" "a=1"));
  Alcotest.(check string)
    "relative Path takes the default"
    "/a"
    (Cookie.path (parse_ok ~path:"/a/b" "a=1; Path=relative"));
  Alcotest.(check string)
    "empty Path takes the default"
    "/a"
    (Cookie.path (parse_ok ~path:"/a/b" "a=1; Path="))
;;

(* {1 Expiry (s5.2.1/s5.2.2/s5.3 step 3)} *)

let test_max_age () =
  Alcotest.(check expiry)
    "Max-Age adds to now"
    (`At (time 1060.0))
    (Cookie.expiry (parse_ok "a=1; Max-Age=60"));
  let zero = parse_ok "a=1; Max-Age=0" in
  Alcotest.(check bool) "Max-Age=0 is already expired" true (Cookie.is_expired ~now zero);
  let neg = parse_ok "a=1; Max-Age=-1" in
  Alcotest.(check bool)
    "negative Max-Age is already expired"
    true
    (Cookie.is_expired ~now neg)
;;

let test_max_age_wins_over_expires () =
  let exp = "Expires=Wed, 21 Oct 2015 07:28:00 GMT" in
  Alcotest.(check expiry)
    "Max-Age first"
    (`At (time 1060.0))
    (Cookie.expiry (parse_ok (Fmt.str "a=1; Max-Age=60; %s" exp)));
  Alcotest.(check expiry)
    "Expires first"
    (`At (time 1060.0))
    (Cookie.expiry (parse_ok (Fmt.str "a=1; %s; Max-Age=60" exp)))
;;

let test_max_age_lexing () =
  Alcotest.(check expiry)
    "hex is not digits"
    `Session
    (Cookie.expiry (parse_ok "a=1; Max-Age=0x10"));
  Alcotest.(check expiry)
    "underscores are not digits"
    `Session
    (Cookie.expiry (parse_ok "a=1; Max-Age=1_000"));
  Alcotest.(check expiry)
    "an unparseable repeat keeps the earlier value"
    (`At (time 1005.0))
    (Cookie.expiry (parse_ok "a=1; Max-Age=5; Max-Age=x"))
;;

let test_expires_formats () =
  let expect = `At (date (2015, 10, 21) (7, 28, 0)) in
  let check fmt line =
    Alcotest.(check expiry) fmt expect (Cookie.expiry (parse_ok line))
  in
  check "rfc1123" "a=1; Expires=Wed, 21 Oct 2015 07:28:00 GMT";
  check "rfc850" "a=1; Expires=Wednesday, 21-Oct-15 07:28:00 GMT";
  check "asctime" "a=1; Expires=Wed Oct 21 07:28:00 2015";
  check "dashed variant" "a=1; Expires=Wed, 21-Oct-2015 07:28:00 GMT";
  check "case-insensitive month and tz" "a=1; Expires=Wed, 21 OCT 2015 07:28:00 gmt"
  ;
  check "reordered cookie-date tokens"
    "a=1; Expires=07:28:00 garbage 2015 Oct 21";
  check "delimiter variants"
    "a=1; Expires=2015/Oct/21 07:28:00";
  check "one-digit time and component suffixes"
    "a=1; Expires=21st Oct-extra 2015year 7:28:0GMT:ignored";
  Alcotest.(check expiry) "digit immediately after month rejects that token"
    `Session (Cookie.expiry (parse_ok "a=1; Expires=21 Jan2 2015 7:28:0"))
;;

let test_expires_two_digit_years () =
  Alcotest.(check expiry)
    "95 is 1995"
    (`At (date (1995, 10, 21) (7, 28, 0)))
    (Cookie.expiry (parse_ok "a=1; Expires=Sat, 21-Oct-95 07:28:00 GMT"));
  Alcotest.(check expiry)
    "25 is 2025"
    (`At (date (2025, 10, 21) (7, 28, 0)))
    (Cookie.expiry (parse_ok "a=1; Expires=Tue, 21-Oct-25 07:28:00 GMT"));
  Alcotest.(check expiry)
    "69 is 2069"
    (`At (date (2069, 10, 21) (7, 28, 0)))
    (Cookie.expiry (parse_ok "a=1; Expires=Mon, 21-Oct-69 07:28:00 GMT"));
  Alcotest.(check expiry)
    "70 is 1970"
    (`At (date (1970, 10, 21) (7, 28, 0)))
    (Cookie.expiry (parse_ok "a=1; Expires=Wed, 21-Oct-70 07:28:00 GMT"))
;;

let test_expires_invalid_ignored () =
  Alcotest.(check expiry)
    "unparseable Expires leaves a session cookie"
    `Session
    (Cookie.expiry (parse_ok "a=1; Expires=not-a-date"));
  Alcotest.(check expiry)
    "a year before 1601 leaves a session cookie"
    `Session
    (Cookie.expiry
       (parse_ok "a=1; Expires=Sun, 06 Nov 1500 08:49:37 GMT"));
  Alcotest.(check expiry)
    "a leap second leaves a session cookie"
    `Session
    (Cookie.expiry
       (parse_ok "a=1; Expires=Sun, 06 Nov 1994 08:49:60 GMT"))
;;

(* {1 Prefixes, SameSite, Partitioned (RFC 6265bis, CHIPS)} *)

let test_secure_prefix () =
  check_reason "__Secure- without Secure" (parse_err "__Secure-a=1") "__Secure-";
  ignore (parse_ok "__Secure-a=1; Secure");
  check_reason "__secure- without Secure" (parse_err "__secure-a=1") "__Secure-";
  Alcotest.(check bool) "has_secure_prefix" true (Cookie.has_secure_prefix "__HOST-x");
  Alcotest.(check bool) "plain name" false (Cookie.has_secure_prefix "sid")
;;

let test_host_prefix () =
  check_reason "__Host- without Secure" (parse_err "__Host-a=1; Path=/") "Secure";
  check_reason
    "__Host- with Domain"
    (parse_err "__Host-a=1; Secure; Path=/; Domain=example.com")
    "Domain";
  check_reason
    "__Host- without Path=/"
    (parse_err "__Host-a=1; Secure; Path=/app")
    "Path=/";
  let c = parse_ok "__Host-a=1; Secure; Path=/" in
  Alcotest.(check bool) "well-formed __Host- accepted" true (Cookie.host_only c)
;;

let test_same_site () =
  let ss line = Cookie.same_site (parse_ok line) in
  Alcotest.(check bool) "strict" true (ss "a=1; SameSite=Strict" = Some `Strict);
  Alcotest.(check bool) "lax" true (ss "a=1; SameSite=Lax" = Some `Lax);
  Alcotest.(check bool) "none+secure" true (ss "a=1; SameSite=None; Secure" = Some `None);
  Alcotest.(check bool) "invalid value ignored" true (ss "a=1; SameSite=Odd" = None);
  check_reason
    "SameSite=None without Secure"
    (parse_err "a=1; SameSite=None")
    "SameSite=None"
;;

let test_partitioned () =
  let c = parse_ok "a=1; Secure; Partitioned" in
  Alcotest.(check bool) "partitioned" true (Cookie.partitioned c);
  check_reason "Partitioned without Secure" (parse_err "a=1; Partitioned") "Partitioned"
;;

(* {1 Matching and ordering} *)

let test_domain_suffix_matches () =
  let m sub d = Cookie.domain_suffix_matches ~sub d in
  Alcotest.(check bool) "equal" true (m "example.com" "example.com");
  Alcotest.(check bool) "subdomain" true (m "a.b.example.com" "example.com");
  Alcotest.(check bool) "not dot-aligned" false (m "notexample.com" "example.com");
  Alcotest.(check bool) "other direction" false (m "example.com" "www.example.com");
  Alcotest.(check bool) "IPv4 never suffix-matches" false (m "127.0.0.1" "0.1");
  Alcotest.(check bool) "IPv6 never suffix-matches" false (m "::1" "1")
;;

let test_domain_matches_host_only () =
  let host_only = parse_ok ~host:"example.com" "a=1" in
  Alcotest.(check bool)
    "exact host"
    true
    (Cookie.domain_matches ~host:"example.com" host_only);
  Alcotest.(check bool)
    "subdomain refused"
    false
    (Cookie.domain_matches ~host:"www.example.com" host_only);
  let widened = parse_ok ~host:"example.com" "a=1; Domain=example.com" in
  Alcotest.(check bool)
    "widened cookie matches subdomain"
    true
    (Cookie.domain_matches ~host:"www.example.com" widened)
;;

let test_path_matches () =
  let at path = parse_ok ~host:"e.com" (Fmt.str "a=1; Path=%s" path) in
  let m request_path c = Cookie.path_matches ~request_path c in
  Alcotest.(check bool) "identical" true (m "/app" (at "/app"));
  Alcotest.(check bool) "trailing slash prefix" true (m "/app/x" (at "/app/"));
  Alcotest.(check bool) "slash boundary" true (m "/app/x" (at "/app"));
  Alcotest.(check bool) "no false prefix" false (m "/apple" (at "/app"));
  Alcotest.(check bool) "root matches all" true (m "/anything" (at "/"));
  Alcotest.(check bool) "empty request path is root" true (m "" (at "/"));
  Alcotest.(check bool) "sibling no match" false (m "/other" (at "/app"))
;;

let test_compare_order () =
  let c ~path ~now ~name =
    Cookie.v ~domain:"e.com" ~path ~name ~value:"v" ~expiry:`Session ~now ()
  in
  let long = c ~path:"/a/b" ~now:(time 2000.0) ~name:"long" in
  let short_old = c ~path:"/a" ~now:(time 1000.0) ~name:"old" in
  let short_new = c ~path:"/a" ~now:(time 2000.0) ~name:"new" in
  let tie_a = c ~path:"/a" ~now:(time 1000.0) ~name:"aaa" in
  Alcotest.(check bool) "longer path first" true (Cookie.compare_order long short_old < 0);
  Alcotest.(check bool)
    "earlier creation first"
    true
    (Cookie.compare_order short_old short_new < 0);
  Alcotest.(check bool)
    "same tick falls back to the name"
    true
    (Cookie.compare_order tie_a short_old < 0)
;;

let test_is_expired () =
  let session = parse_ok "a=1" in
  Alcotest.(check bool)
    "session never expires"
    false
    (Cookie.is_expired ~now:(time 4e9) session);
  let dated = parse_ok "a=1; Max-Age=60" in
  Alcotest.(check bool) "at expiry" true (Cookie.is_expired ~now:(time 1060.0) dated);
  Alcotest.(check bool) "after" true (Cookie.is_expired ~now:(time 1061.0) dated)
;;

(* {1 The server side} *)

let test_parse_cookie_header () =
  Alcotest.(check (list (pair string string)))
    "basic"
    [ "a", "1"; "b", "2" ]
    (Cookie.parse_cookie_header "a=1; b=2");
  Alcotest.(check (list (pair string string)))
    "repeated names kept in order"
    [ "sid", "specific"; "sid", "general" ]
    (Cookie.parse_cookie_header "sid=specific; sid=general");
  Alcotest.(check (list (pair string string)))
    "malformed segments dropped"
    [ "ok", "1" ]
    (Cookie.parse_cookie_header "stray; bad name=1; ok=1; b=\x01");
  Alcotest.(check (list (pair string string)))
    "empty header"
    []
    (Cookie.parse_cookie_header "")
;;

let test_set_cookie_header () =
  let base
        ?(host_only = true)
        ?(secure = false)
        ?(http_only = false)
        ?(partitioned = false)
        ?same_site
        ~expiry
        ()
    =
    Cookie.v
      ~domain:"example.com"
      ~path:"/app"
      ~name:"sid"
      ~value:"abc"
      ~host_only
      ~secure
      ~http_only
      ~partitioned
      ?same_site
      ~expiry
      ~now
      ()
  in
  Alcotest.(check string)
    "host-only omits Domain"
    "sid=abc; Path=/app"
    (Cookie.set_cookie_header (base ~expiry:`Session ()));
  Alcotest.(check string)
    "widened names its Domain"
    "sid=abc; Domain=example.com; Path=/app"
    (Cookie.set_cookie_header (base ~host_only:false ~expiry:`Session ()));
  Alcotest.(check string)
    "expiry as IMF-fixdate"
    "sid=abc; Expires=Wed, 21 Oct 2015 07:28:00 GMT; Path=/app"
    (Cookie.set_cookie_header (base ~expiry:(`At (date (2015, 10, 21) (7, 28, 0))) ()));
  Alcotest.(check string)
    "all the flags"
    "sid=abc; Path=/app; Secure; HttpOnly; Partitioned; SameSite=Lax"
    (Cookie.set_cookie_header
       (base
          ~secure:true
          ~http_only:true
          ~partitioned:true
          ~same_site:`Lax
          ~expiry:`Session
          ()))
;;

let test_set_cookie_round_trip () =
  let emitted =
    Cookie.v
      ~domain:"example.com"
      ~path:"/"
      ~name:"sid"
      ~value:"abc"
      ~host_only:false
      ~secure:true
      ~http_only:true
      ~expiry:(`At (date (2036, 10, 21) (7, 28, 0)))
      ~now
      ()
  in
  let line = Cookie.set_cookie_header emitted in
  let stored = parse_ok ~host:"www.example.com" ~path:"/" line in
  Alcotest.(check string) "name" "sid" (Cookie.name stored);
  Alcotest.(check string) "value" "abc" (Cookie.value stored);
  Alcotest.(check string) "domain" "example.com" (Cookie.domain stored);
  Alcotest.(check bool) "host_only" false (Cookie.host_only stored);
  Alcotest.(check string) "path" "/" (Cookie.path stored);
  Alcotest.(check bool) "secure" true (Cookie.secure stored);
  Alcotest.(check bool) "http_only" true (Cookie.http_only stored);
  Alcotest.(check expiry)
    "expiry"
    (`At (date (2036, 10, 21) (7, 28, 0)))
    (Cookie.expiry stored)
;;

let test_valid_name_value () =
  Alcotest.(check bool) "token name" true (Cookie.valid_name "SID_1");
  Alcotest.(check bool) "empty name" false (Cookie.valid_name "");
  Alcotest.(check bool) "separator in name" false (Cookie.valid_name "a=b");
  Alcotest.(check bool) "non-ASCII name" false (Cookie.valid_name "a\255");
  Alcotest.(check bool) "plain value" true (Cookie.valid_value "abc");
  Alcotest.(check bool) "space allowed" true (Cookie.valid_value "a b");
  Alcotest.(check bool) "semicolon refused" false (Cookie.valid_value "a;b");
  Alcotest.(check bool) "ctl refused" false (Cookie.valid_value "a\nb")
;;

(* {1 The jar: storage and retrieval} *)

let mock_jar ?(at = 1000.0) () =
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock at;
  Cookie_jar.in_memory ~clock (), clock
;;

let set_ok jar ?(host = "example.com") ?(path = "/") ?(https = true) line =
  match Cookie_jar.set jar ~host ~path ~https line with
  | Ok () -> ()
  | Error e -> Alcotest.failf "jar refused %S: %s" line e
;;

let set_err jar ?(host = "example.com") ?(path = "/") ?(https = true) line =
  match Cookie_jar.set jar ~host ~path ~https line with
  | Ok () -> Alcotest.failf "expected the jar to refuse %S" line
  | Error e -> e
;;

let header jar ?(host = "example.com") ?(path = "/") ?(https = true) () =
  Cookie_jar.header_for jar ~host ~path ~https
;;

let test_jar_basic () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, _clock = mock_jar () in
  set_ok jar "sid=abc";
  Alcotest.(check (option string)) "returned to the host" (Some "sid=abc") (header jar ());
  Alcotest.(check (option string))
    "not to another host"
    None
    (header jar ~host:"other.com" ());
  Alcotest.(check (option string))
    "host-only: not to a subdomain"
    None
    (header jar ~host:"www.example.com" ());
  set_ok jar "wide=1; Domain=example.com";
  Alcotest.(check (option string))
    "widened: to the subdomain too"
    (Some "wide=1")
    (header jar ~host:"www.example.com" ());
  Cookie_jar.clear jar;
  Alcotest.(check (option string)) "cleared" None (header jar ())
;;

let test_jar_secure_and_paths () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, clock = mock_jar () in
  set_ok jar "s=1; Secure";
  Alcotest.(check (option string))
    "secure withheld from http"
    None
    (header jar ~https:false ());
  Alcotest.(check (option string))
    "secure sent over https"
    (Some "s=1")
    (header jar ~https:true ());
  Eio_mock.Clock.set_time clock 1001.0;
  set_ok jar ~path:"/app/x" "deep=1";
  Eio_mock.Clock.set_time clock 1002.0;
  set_ok jar "root=1";
  Alcotest.(check (option string))
    "deeper path first"
    (Some "deep=1; s=1; root=1")
    (header jar ~path:"/app/x" ());
  Alcotest.(check (option string))
    "path filtered"
    (Some "s=1; root=1")
    (header jar ~path:"/" ())
;;

let test_jar_replacement () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, clock = mock_jar () in
  set_ok jar "a=old";
  Eio_mock.Clock.set_time clock 2000.0;
  set_ok jar "b=other";
  set_ok jar "a=new";
  Alcotest.(check (option string))
    "replacement keeps the old creation order"
    (Some "a=new; b=other")
    (header jar ());
  Alcotest.(check int) "still two cookies" 2 (List.length (Cookie_jar.cookies jar))
;;

let test_jar_max_age_zero_deletes () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, _clock = mock_jar () in
  set_ok jar "a=1";
  set_ok jar "a=; Max-Age=0";
  Alcotest.(check (option string)) "deleted at once" None (header jar ());
  Alcotest.(check int) "and not stored" 0 (List.length (Cookie_jar.cookies jar))
;;

let test_jar_expiry_eviction () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, clock = mock_jar () in
  set_ok jar "a=1; Max-Age=5";
  Alcotest.(check (option string)) "live" (Some "a=1") (header jar ());
  Eio_mock.Clock.set_time clock 1006.0;
  Alcotest.(check (option string)) "expired" None (header jar ());
  Alcotest.(check int)
    "and evicted from the store"
    0
    (List.length (Cookie_jar.cookies jar))
;;

let test_jar_refusals () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, _clock = mock_jar () in
  set_ok jar ~https:true "sid=real; Secure";
  let reason = set_err jar ~https:false "sid=fake" in
  check_reason "shadowing" reason "shadow";
  Alcotest.(check (option string))
    "the Secure cookie survives"
    (Some "sid=real")
    (header jar ~https:true ());
  let reason = set_err jar ~https:false "__Secure-a=1; Secure" in
  check_reason "prefix over plaintext" reason "plaintext";
  let reason = set_err jar ~https:false "ordinary=1; Secure" in
  check_reason "Secure over plaintext" reason "plaintext";
  set_ok jar ~https:true "__Secure-a=1; Secure";
  let reason = set_err jar (Fmt.str "big=%s" (String.make 5000 'x')) in
  check_reason "oversized" reason "4096";
  let reason = set_err jar "a=1; Domain=other.com" in
  check_reason "parse reason" reason "does not cover"
;;

let test_expired_secure_does_not_shadow () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, clock = mock_jar () in
  set_ok jar ~https:true "sid=real; Secure; Max-Age=1";
  Eio_mock.Clock.set_time clock 1001.0;
  set_ok jar ~https:false "sid=fresh";
  Alcotest.(check (option string)) "expired Secure entry was pruned"
    (Some "sid=fresh") (header jar ~https:false ())
;;

(* Path and Domain are stored and written to the backing file with the pair,
   so they share its byte budget. *)
let test_jar_size_cap () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, _clock = mock_jar () in
  let long_path = "/" ^ String.make 8490 'p' in
  let reason = set_err jar (Fmt.str "big=1; Path=%s" long_path) in
  check_reason "a long path" reason "8192";
  Alcotest.(check int) "nothing stored" 0 (List.length (Cookie_jar.cookies jar));
  set_ok jar (Fmt.str "fits=%s" (String.make 4000 'x'));
  (* draft-ietf-httpbis-rfc6265bis s5.7 asks a user agent to support at least
     4096 octets of name and value alone; the cap must leave room for an
     ordinary path and domain on top of that, not carve them out of it. *)
  set_ok jar (Fmt.str "a=%s" (String.make 4095 'x'));
  (* One octet past that minimum is not required, and this jar draws the
     line there rather than admitting an unbounded name and value. *)
  let reason = set_err jar (Fmt.str "a=%s" (String.make 4096 'x')) in
  check_reason "one past the name-value minimum" reason "4096"
;;

(* The end-to-end shape of the public-suffix and IP-literal rules: a cookie
   set by a suffix host, or by an address, reaches nothing else. *)
let test_jar_host_only_scopes () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, _clock = mock_jar () in
  set_ok jar ~host:"s3.amazonaws.com" "sid=x; Domain=s3.amazonaws.com";
  Alcotest.(check (option string))
    "the suffix host keeps it"
    (Some "sid=x")
    (header jar ~host:"s3.amazonaws.com" ());
  Alcotest.(check (option string))
    "no bucket receives it"
    None
    (header jar ~host:"evil-bucket.s3.amazonaws.com" ());
  set_ok jar ~host:"127.0.0.1" "a=1; Domain=127.0.0.1";
  Alcotest.(check (option string))
    "the address itself still gets its cookie"
    (Some "a=1")
    (header jar ~host:"127.0.0.1" ());
  let reason = set_err jar ~host:"0x7f.1" "a=1; Domain=1" in
  check_reason "a hex-spelled host" reason "does not cover"
;;

let test_jar_per_domain_lru () =
  Eio_mock.Backend.run
  @@ fun () ->
  let jar, clock = mock_jar () in
  for i = 0 to 50 do
    Eio_mock.Clock.set_time clock (1000.0 +. float_of_int i);
    set_ok jar (Fmt.str "c%d=%d" i i)
  done;
  let stored = Cookie_jar.cookies jar in
  Alcotest.(check int) "capped at 50 per domain" 50 (List.length stored);
  let names = List.map Cookie.name stored in
  Alcotest.(check bool) "the least recently used was evicted" false (List.mem "c0" names);
  Alcotest.(check bool) "the newest survived" true (List.mem "c50" names)
;;

(* {1 The jar: Netscape persistence} *)

let test_netscape_fixture env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "cookies.txt") in
  let jar = Cookie_jar.of_file ~clock ~save:`Manual path in
  let cookies = Cookie_jar.cookies jar in
  Alcotest.(check int) "only live fixture lines load" 2 (List.length cookies);
  let find name =
    match List.find_opt (fun c -> Cookie.name c = name) cookies with
    | Some c -> c
    | None -> Alcotest.failf "fixture cookie %s not loaded" name
  in
  Alcotest.(check expiry)
    "0 expiry is a session cookie"
    `Session
    (Cookie.expiry (find "cookie-1"));
  Alcotest.(check bool)
    "plain line is host-only"
    true
    (Cookie.host_only (find "cookie-1"));
  Alcotest.(check bool)
    "TRUE flag includes subdomains"
    false
    (Cookie.host_only (find "cookie-2"));
  ()
;;

let test_netscape_round_trip env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "round-trip.txt") in
  Eio.Path.unlink ~missing_ok:true path;
  let jar = Cookie_jar.of_file ~clock path in
  set_ok jar "plain=1";
  set_ok jar "marked=2; HttpOnly; Domain=example.com";
  set_ok jar "dated=3; Secure; Expires=Wed, 21 Oct 2036 07:28:00 GMT";
  let jar' = Cookie_jar.of_file ~clock ~save:`Manual path in
  let cookies = Cookie_jar.cookies jar' in
  Alcotest.(check int) "all three round-trip" 3 (List.length cookies);
  let find name = List.find (fun c -> Cookie.name c = name) cookies in
  Alcotest.(check bool)
    "HttpOnly survives the file"
    true
    (Cookie.http_only (find "marked"));
  Alcotest.(check bool)
    "so does the subdomain flag"
    false
    (Cookie.host_only (find "marked"));
  Alcotest.(check bool) "and Secure" true (Cookie.secure (find "dated"));
  Alcotest.(check expiry)
    "and a whole-second expiry"
    (`At (date (2036, 10, 21) (7, 28, 0)))
    (Cookie.expiry (find "dated"));
  Alcotest.(check expiry) "sessions stay sessions" `Session (Cookie.expiry (find "plain"))
;;

(* The file is its own trust boundary: a value carrying ';' would become a
   second pair in every emitted Cookie header. *)
let test_netscape_skips_invalid env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "invalid-lines.txt") in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    path
    "# Netscape HTTP Cookie File\n\
     example.com\tFALSE\t/\tFALSE\t0\tgood\tvalue\n\
     example.com\tFALSE\t/\tFALSE\t0\tbad name\tvalue\n\
     example.com\tFALSE\t/\tFALSE\t0\tsplit\tx; admin=1\n\
     .com\tTRUE\t/\tFALSE\t0\twide\tvalue\n\
     example.com\tMAYBE\t/\tFALSE\t0\tbadflag\tvalue\n\
     bad domain\tFALSE\t/\tFALSE\t0\tbaddomain\tvalue\n\
     example.com\tFALSE\trelative\tFALSE\t0\tbadpath\tvalue\n\
     example.com\tFALSE\t/\tFALSE\t0\t\tvalue\n";
  let cookies = Cookie_jar.cookies (Cookie_jar.of_file ~clock ~save:`Manual path) in
  Alcotest.(check int) "only the well-formed line loads" 1 (List.length cookies);
  Alcotest.(check string) "and it is that line" "good" (Cookie.name (List.hd cookies))
;;

let test_netscape_expired_before_live env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "expired-before-live.txt") in
  let content = Buffer.create 4096 in
  for i = 0 to 49 do
    Buffer.add_string content
      (Fmt.str "example.com\tFALSE\t/\tFALSE\t1\texpired%d\tx\n" i)
  done;
  Buffer.add_string content "example.com\tFALSE\t/\tFALSE\t0\tlive\tx\n";
  Eio.Path.save ~create:(`Or_truncate 0o600) path (Buffer.contents content);
  let cookies = Cookie_jar.of_file ~clock ~save:`Manual path |> Cookie_jar.cookies in
  Alcotest.(check int) "expired records consume no domain quota" 1
    (List.length cookies);
  Alcotest.(check string) "later live record loads" "live"
    (Cookie.name (List.hd cookies))
;;

let test_netscape_identity_and_total_indexes env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "indexed-load.txt") in
  let content = Buffer.create 262_144 in
  Buffer.add_string content "first.example.com\tFALSE\t/\tFALSE\t0\tkey\tfirst\n";
  Buffer.add_string content "first.example.com\tFALSE\t/\tFALSE\t0\tkey\tduplicate\n";
  for i = 1 to 2999 do
    Buffer.add_string content
      (Fmt.str "host%d.example.com\tFALSE\t/\tFALSE\t0\tkey\t%d\n" i i)
  done;
  Buffer.add_string content
    "overflow.example.com\tFALSE\t/\tFALSE\t0\toverflow\tignored\n";
  Eio.Path.save ~create:(`Or_truncate 0o600) path (Buffer.contents content);
  let cookies = Cookie_jar.of_file ~clock ~save:`Manual path |> Cookie_jar.cookies in
  Alcotest.(check int) "total cap" 3000 (List.length cookies);
  let first =
    List.find
      (fun cookie -> Cookie.domain cookie = "first.example.com")
      cookies
  in
  Alcotest.(check string) "duplicate identity keeps first entry" "first"
    (Cookie.value first);
  Alcotest.(check bool) "scan stops when total cap is full" false
    (List.exists (fun cookie -> Cookie.name cookie = "overflow") cookies)
;;

let test_netscape_manual_save env =
  let clock = Eio.Stdenv.clock env in
  let path = Eio.Path.(Eio.Stdenv.cwd env / "manual-save.txt") in
  Eio.Path.unlink ~missing_ok:true path;
  let jar = Cookie_jar.of_file ~clock ~save:`Manual path in
  set_ok jar "a=1";
  let reload () = Cookie_jar.cookies (Cookie_jar.of_file ~clock ~save:`Manual path) in
  Alcotest.(check int) "nothing written before flush" 0 (List.length (reload ()));
  Cookie_jar.flush jar;
  Alcotest.(check int) "flush writes" 1 (List.length (reload ()))
;;

let test_netscape_requires_filename env =
  let clock = Eio.Stdenv.clock env in
  match Cookie_jar.of_file ~clock ~save:`Manual (Eio.Stdenv.cwd env) with
  | _ -> Alcotest.fail "a directory capability without a filename was accepted"
  | exception Invalid_argument _ -> ()
;;

type write_fault =
  | Fail_after_byte of unit Eio.Promise.u
  | Cancel_after_byte of unit Eio.Promise.u

let write_fault = ref None

(* Wrap the real directory provider only far enough to fault the next file
   write. Keeping the real open means the exclusive-create and cleanup paths
   exercise the production filesystem implementation. *)
let with_faulting_writes ((Eio.Resource.T (dir_state, dir_ops), path) : _ Eio.Path.t) =
  let module Dir = (val Eio.Resource.get dir_ops Eio.Fs.Pi.Dir) in
  let module Faulting_dir = struct
    include Dir

    let open_out state ~sw ~append ~create path =
      let file = Dir.open_out state ~sw ~append ~create path in
      match !write_fault with
      | None -> file
      | Some _ ->
          let Eio.Resource.T (file_state, file_ops) = file in
          let module File =
            (val Eio.Resource.get file_ops Eio.File.Pi.Write)
          in
          let module Faulting_file = struct
            include File

            let rec first_nonempty (local_ buffers) = exclave_
              match buffers with
              | [] -> failwith "fault injection received no writable byte"
              | buf :: rest ->
                  if Cstruct.length buf > 0 then buf else first_nonempty rest

            let single_write state (local_ buffers) =
              match !write_fault with
              | None -> File.single_write state buffers
              | Some fault ->
                  write_fault := None;
                  let local_ first = first_nonempty buffers in
                  let wrote =
                    File.single_write state
                      (stack_ [ Cstruct.sub_local first 0 1 ])
                  in
                  if wrote <> 1 then failwith "fault injection could not write one byte";
                  (match fault with
                   | Fail_after_byte started ->
                       Eio.Promise.resolve started ();
                       failwith "injected mid-write failure"
                   | Cancel_after_byte started ->
                       Eio.Promise.resolve started ();
                       raise
                         (Eio.Cancel.Cancelled
                            (Failure "injected mid-write cancellation")))

            let copy state ~src =
              Eio.Flow.Pi.simple_copy ~single_write state ~src
          end
          in
          Eio.Resource.T (file_state, Eio.File.Pi.rw (module Faulting_file))
  end
  in
  let wrapped_ops =
    Eio.Resource.handler
      [ Eio.Resource.H (Eio.Fs.Pi.Dir, (module Faulting_dir)) ]
  in
  Eio.Resource.T (dir_state, wrapped_ops), path
;;

let owned_temps dir name =
  List.filter
    (fun entry -> String.starts_with ~prefix:("." ^ name ^ ".httpz-tmp-") entry)
    (Eio.Path.read_dir dir)
;;

let test_netscape_interrupted_save env =
  let clock = Eio.Stdenv.clock env in
  let native_dir = Filename.temp_dir ~perms:0o700 "httpz-cookie-interrupt-" "" in
  let dir = Eio.Path.(Eio.Stdenv.fs env / native_dir) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
    (fun () ->
      let sentinel = Eio.Path.(dir / "sentinel") in
      Eio.Path.save ~create:(`Exclusive 0o600) sentinel "neighbor";
      let run name fault =
        let real_path = Eio.Path.(dir / name) in
        Eio.Path.save ~create:(`Exclusive 0o600) real_path "original";
        let started, notify_started = Eio.Promise.create () in
        write_fault := Some (fault notify_started);
        let path = with_faulting_writes real_path in
        let jar = Cookie_jar.of_file ~clock ~save:`Manual path in
        set_ok jar "changed=1";
        started, jar, real_path
      in
      let started, jar, real_path = run "failed-write.txt" (fun p -> Fail_after_byte p) in
      (match Cookie_jar.flush jar with
       | () -> Alcotest.fail "injected write unexpectedly succeeded"
       | exception Failure message when message = "injected mid-write failure" -> ()
       | exception exn ->
           Alcotest.failf "unexpected write failure: %s" (Printexc.to_string exn));
      ignore (Eio.Promise.await started);
      Alcotest.(check string) "failed write preserves old jar" "original"
        (Eio.Path.load real_path);
      Alcotest.(check (list string)) "failed write removes owned temp" []
        (owned_temps dir "failed-write.txt");

      let started, jar, real_path =
        run "cancelled-write.txt" (fun p -> Cancel_after_byte p)
      in
      (match Cookie_jar.flush jar with
       | () -> Alcotest.fail "injected cancellation unexpectedly succeeded"
       | exception Eio.Cancel.Cancelled _ -> ()
       | exception exn ->
           Alcotest.failf "unexpected cancellation: %s" (Printexc.to_string exn));
      ignore (Eio.Promise.await started);
      Alcotest.(check string) "cancelled write preserves old jar" "original"
        (Eio.Path.load real_path);
      Alcotest.(check (list string)) "cancelled write removes owned temp" []
        (owned_temps dir "cancelled-write.txt");
      Alcotest.(check string) "neighbor survives interrupted writes" "neighbor"
        (Eio.Path.load sentinel))
;;

let test_netscape_atomic_save env =
  let clock = Eio.Stdenv.clock env in
  let native_dir = Filename.temp_dir ~perms:0o700 "httpz-cookie-atomic-" "" in
  let dir = Eio.Path.(Eio.Stdenv.fs env / native_dir) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
    (fun () ->
      let path = Eio.Path.(dir / "jar.txt") in
      let old_tmp = Eio.Path.(dir / "jar.txt.tmp") in
      Eio.Path.save ~create:(`Exclusive 0o600) old_tmp "sentinel";
      let jar = Cookie_jar.of_file ~clock ~save:`Manual path in
      set_ok jar "a=1";
      let old_umask = Unix.umask 0o077 in
      Fun.protect
        ~finally:(fun () -> ignore (Unix.umask old_umask))
        (fun () -> Cookie_jar.flush jar);
      Alcotest.(check string) "fixed-name sentinel untouched" "sentinel"
        (Eio.Path.load old_tmp);
      let stat = Eio.Path.stat ~follow:true path in
      Alcotest.(check int) "saved jar has no group/other permissions" 0
        (stat.perm land 0o077);

      let victim = Eio.Path.(dir / "victim") in
      let path2 = Eio.Path.(dir / "other.txt") in
      Eio.Path.save ~create:(`Exclusive 0o600) victim "do not touch";
      Eio.Path.symlink ~link_to:"victim" Eio.Path.(dir / "other.txt.tmp");
      let jar2 = Cookie_jar.of_file ~clock ~save:`Manual path2 in
      set_ok jar2 "b=2";
      Cookie_jar.flush jar2;
      Alcotest.(check string) "legacy temp symlink target untouched" "do not touch"
        (Eio.Path.load victim);

      let shared = Eio.Path.(dir / "shared.txt") in
      let left = Cookie_jar.of_file ~clock ~save:`Manual shared in
      let right = Cookie_jar.of_file ~clock ~save:`Manual shared in
      set_ok left "left=1";
      set_ok right "right=1";
      Eio.Fiber.both (fun () -> Cookie_jar.flush left) (fun () -> Cookie_jar.flush right);
      let loaded = Cookie_jar.of_file ~clock ~save:`Manual shared |> Cookie_jar.cookies in
      Alcotest.(check int) "concurrent save leaves one complete snapshot" 1
        (List.length loaded);
      let leaked =
        List.filter
          (fun name -> String.starts_with ~prefix:".shared.txt.httpz-tmp-" name)
          (Eio.Path.read_dir dir)
      in
      Alcotest.(check (list string)) "concurrent temps removed" [] leaked;

      let failed_path = Eio.Path.(dir / "failed.txt") in
      let failed = Cookie_jar.of_file ~clock ~save:`Manual failed_path in
      set_ok failed "x=1";
      Eio.Path.mkdir ~perm:0o700 failed_path;
      (match Cookie_jar.flush failed with
       | () -> Alcotest.fail "rename over a directory unexpectedly succeeded"
       | exception _ -> ());
      let leaked =
        List.filter
          (fun name -> String.starts_with ~prefix:".failed.txt.httpz-tmp-" name)
          (Eio.Path.read_dir dir)
      in
      Alcotest.(check (list string)) "owned temp removed after failed rename" [] leaked)
;;

let test_netscape_file_cap env =
  let clock = Eio.Stdenv.clock env in
  let name = Printf.sprintf "httpz-cookie-cap-%d.txt" (Random.bits ()) in
  let path = Eio.Path.(Eio.Stdenv.fs env / "/tmp" / name) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.unlink ~missing_ok:true path)
    (fun () ->
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) path (fun flow ->
        let mib = String.make (1024 * 1024) 'x' in
        for _ = 1 to 33 do
          Eio.Flow.copy_string mib flow
        done);
      let jar = Cookie_jar.of_file ~clock ~save:`Manual path in
      Alcotest.(check int)
        "a file over 32 MiB is treated as empty" 0
        (List.length (Cookie_jar.cookies jar)))
;;

(* {1 Suite} *)

let () =
  Eio_main.run
  @@ fun env ->
  let open Alcotest in
  run
    "httpz.cookie"
    [ ( "set-cookie"
      , [ test_case "basics" `Quick test_parse_basic
        ; test_case "flags" `Quick test_parse_flags
        ; test_case "unknown attributes" `Quick test_parse_unknown_attribute
        ; test_case "syntax errors" `Quick test_parse_syntax_errors
        ; test_case "value forms" `Quick test_parse_value_forms
        ; test_case "constructor validation" `Quick test_v_validates
        ] )
    ; ( "domain"
      , [ test_case "widening" `Quick test_domain_widening
        ; test_case "normalization" `Quick test_domain_normalization
        ; test_case "empty ignored" `Quick test_domain_empty_ignored
        ; test_case "must cover the host" `Quick test_domain_must_cover_host
        ; test_case "public suffixes" `Quick test_domain_public_suffix
        ; test_case "IP literals" `Quick test_domain_ip_literal
        ] )
    ; "path", [ test_case "attribute and defaults" `Quick test_path_attribute ]
    ; ( "expiry"
      , [ test_case "max-age" `Quick test_max_age
        ; test_case "max-age wins over expires" `Quick test_max_age_wins_over_expires
        ; test_case "max-age lexing" `Quick test_max_age_lexing
        ; test_case "expires formats" `Quick test_expires_formats
        ; test_case "two-digit years" `Quick test_expires_two_digit_years
        ; test_case "invalid expires ignored" `Quick test_expires_invalid_ignored
        ] )
    ; ( "prefixes"
      , [ test_case "__Secure-" `Quick test_secure_prefix
        ; test_case "__Host-" `Quick test_host_prefix
        ; test_case "SameSite" `Quick test_same_site
        ; test_case "Partitioned" `Quick test_partitioned
        ] )
    ; ( "matching"
      , [ test_case "domain suffixes" `Quick test_domain_suffix_matches
        ; test_case "host-only" `Quick test_domain_matches_host_only
        ; test_case "paths" `Quick test_path_matches
        ; test_case "ordering" `Quick test_compare_order
        ; test_case "expiry" `Quick test_is_expired
        ] )
    ; ( "server"
      , [ test_case "parse Cookie" `Quick test_parse_cookie_header
        ; test_case "emit Set-Cookie" `Quick test_set_cookie_header
        ; test_case "round trip" `Quick test_set_cookie_round_trip
        ; test_case "syntax predicates" `Quick test_valid_name_value
        ] )
    ; ( "jar"
      , [ test_case "store and retrieve" `Quick test_jar_basic
        ; test_case "secure and paths" `Quick test_jar_secure_and_paths
        ; test_case "replacement" `Quick test_jar_replacement
        ; test_case "max-age=0 deletes" `Quick test_jar_max_age_zero_deletes
        ; test_case "expiry eviction" `Quick test_jar_expiry_eviction
        ; test_case "refusals" `Quick test_jar_refusals
        ; test_case "expired Secure does not shadow" `Quick test_expired_secure_does_not_shadow
        ; test_case "per-domain LRU cap" `Quick test_jar_per_domain_lru
        ; test_case "byte cap" `Quick test_jar_size_cap
        ; test_case "host-only scopes" `Quick test_jar_host_only_scopes
        ] )
    ; ( "netscape"
      , [ test_case "fixture" `Quick (fun () -> test_netscape_fixture env)
        ; test_case "round trip" `Quick (fun () -> test_netscape_round_trip env)
        ; test_case "manual save" `Quick (fun () -> test_netscape_manual_save env)
        ; test_case "requires filename" `Quick (fun () ->
            test_netscape_requires_filename env)
        ; test_case "atomic save" `Quick (fun () -> test_netscape_atomic_save env)
        ; test_case "interrupted save" `Quick (fun () ->
            test_netscape_interrupted_save env)
        ; test_case "invalid lines skipped" `Quick (fun () ->
            test_netscape_skips_invalid env)
        ; test_case "expired records before live" `Quick (fun () ->
            test_netscape_expired_before_live env)
        ; test_case "identity and total indexes" `Quick (fun () ->
            test_netscape_identity_and_total_indexes env)
        ; test_case "file size cap" `Slow (fun () ->
            test_netscape_file_cap env)
        ] )
    ]
;;
