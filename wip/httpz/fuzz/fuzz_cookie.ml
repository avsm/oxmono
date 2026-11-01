(* Crowbar fuzz testing for Cookie.parse_set_cookie and
   Cookie.parse_cookie_header: arbitrary input must never raise, and a
   successfully parsed cookie must always domain-match the host it was set
   for (RFC 6265 Section 5.3 step 5 / 5.4 step 3; see B2 and B3 in the
   security audit).
   To run: dune exec httpz/fuzz/fuzz_cookie.exe
   With AFL: afl-fuzz -i fuzz/corpus -o fuzz/findings -- ./_build/default/httpz/fuzz/fuzz_cookie.exe @@ *)
open Crowbar

let now_gen =
  map [ range 4_000_000_000 ] (fun seconds ->
    match Ptime.of_float_s (float_of_int seconds) with
    | Some t -> t
    | None -> Ptime.epoch)
;;

(* Realistic hosts (names, IP literals in several inet_aton spellings, and a
   bare public suffix) alongside arbitrary lower-cased bytes, so the fuzzer
   both exercises the public-suffix and IP-literal paths deliberately and
   still covers unstructured input. *)
let host_gen =
  choose
    [ const "example.com"
    ; const "a.example.com"
    ; const "www.github.io"
    ; const "com"
    ; const "co.uk"
    ; const "127.0.0.1"
    ; const "0x7f.1"
    ; const "2130706433"
    ; const "::1"
    ; map [ bytes ] String.lowercase_ascii
    ]
;;

let path_gen = choose [ const "/"; const "/a/b"; map [ bytes ] (fun s -> "/" ^ s) ]

(* Arbitrary Set-Cookie values, plus values deliberately shaped to attach a
   Domain attribute at, above, or unrelated to the host so the fail-closed
   public-suffix check and the host-only-storage-on-exact-match rule both get
   exercised. *)
let structured_value_gen =
  map
    [ choose
        [ const "example.com"
        ; const "a.example.com"
        ; const "com"
        ; const "co.uk"
        ; const "GITHUB.io"
        ; const "127.0.0.1"
        ; const "0x7f.1"
        ; bytes
        ]
    ; bool
    ]
    (fun domain_part quoted ->
       let value = if quoted then Printf.sprintf "sess=%S" domain_part else "sess=1" in
       Printf.sprintf "%s; Domain=%s; Path=/" value domain_part)
;;

let value_gen = choose [ bytes; structured_value_gen ]

let test_parse_set_cookie now host path value =
  try
    match Cookie.parse_set_cookie ~now ~host ~path value with
    | Ok cookie -> check (Cookie.domain_matches ~host cookie)
    | Error _ -> check true
  with
  | e -> failf "Cookie.parse_set_cookie raised: %s" (Printexc.to_string e)
;;

let test_parse_cookie_header value =
  try
    ignore (Cookie.parse_cookie_header value);
    check true
  with
  | e -> failf "Cookie.parse_cookie_header raised: %s" (Printexc.to_string e)
;;

let () =
  add_test
    ~name:"Cookie.parse_set_cookie: never raises, domain matches host"
    [ now_gen; host_gen; path_gen; value_gen ]
    test_parse_set_cookie;
  add_test
    ~name:"Cookie.parse_cookie_header: never raises"
    [ bytes ]
    test_parse_cookie_header
;;
