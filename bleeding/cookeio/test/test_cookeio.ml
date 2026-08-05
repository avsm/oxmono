(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cookeio
open Cookeio_jar

(* Testable helpers for Priority 2 types *)
let expiration_testable : Cookeio.Expiration.t Alcotest.testable =
  Alcotest.testable Cookeio.Expiration.pp Cookeio.Expiration.equal

let span_testable : Ptime.Span.t Alcotest.testable =
  Alcotest.testable Ptime.Span.pp Ptime.Span.equal

let same_site_testable : Cookeio.SameSite.t Alcotest.testable =
  Alcotest.testable Cookeio.SameSite.pp Cookeio.SameSite.equal

let cookie_testable : Cookeio.t Alcotest.testable =
  Alcotest.testable
    (fun ppf c ->
      Format.fprintf ppf
        "{ name=%S; value=%S; domain=%S; path=%S; secure=%b; http_only=%b; \
         partitioned=%b; expires=%a; max_age=%a; same_site=%a }"
        (Cookeio.name c) (Cookeio.value c) (Cookeio.domain c) (Cookeio.path c)
        (Cookeio.secure c) (Cookeio.http_only c) (Cookeio.partitioned c)
        (Format.pp_print_option (fun ppf e ->
             match e with
             | `Session -> Format.pp_print_string ppf "Session"
             | `DateTime t -> Format.fprintf ppf "DateTime(%a)" Ptime.pp t))
        (Cookeio.expires c)
        (Format.pp_print_option Ptime.Span.pp)
        (Cookeio.max_age c)
        (Format.pp_print_option (fun ppf -> function
          | `Strict -> Format.pp_print_string ppf "Strict"
          | `Lax -> Format.pp_print_string ppf "Lax"
          | `None -> Format.pp_print_string ppf "None"))
        (Cookeio.same_site c))
    (fun c1 c2 ->
      let expires_equal e1 e2 =
        match (e1, e2) with
        | None, None -> true
        | Some `Session, Some `Session -> true
        | Some (`DateTime t1), Some (`DateTime t2) -> Ptime.equal t1 t2
        | _ -> false
      in
      Cookeio.name c1 = Cookeio.name c2
      && Cookeio.value c1 = Cookeio.value c2
      && Cookeio.domain c1 = Cookeio.domain c2
      && Cookeio.path c1 = Cookeio.path c2
      && Cookeio.secure c1 = Cookeio.secure c2
      && Cookeio.http_only c1 = Cookeio.http_only c2
      && Cookeio.partitioned c1 = Cookeio.partitioned c2
      && expires_equal (Cookeio.expires c1) (Cookeio.expires c2)
      && Option.equal Ptime.Span.equal (Cookeio.max_age c1) (Cookeio.max_age c2)
      && Option.equal ( = ) (Cookeio.same_site c1) (Cookeio.same_site c2))

let test_load_mozilla_cookies env =
  let clock = Eio.Stdenv.clock env in
  let content =
    {|# Netscape HTTP Cookie File
# http://curl.haxx.se/rfc/cookie_spec.html
# This is a generated file!  Do not edit.

example.com	FALSE	/foo/	FALSE	0	cookie-1	v$1
.example.com	TRUE	/foo/	FALSE	0	cookie-2	v$2
example.com	FALSE	/foo/	FALSE	1257894000	cookie-3	v$3
example.com	FALSE	/foo/	FALSE	1257894000	cookie-4	v$4
example.com	FALSE	/foo/	TRUE	1257894000	cookie-5	v$5
#HttpOnly_example.com	FALSE	/foo/	FALSE	1257894000	cookie-6	v$6
#HttpOnly_.example.com	TRUE	/foo/	FALSE	1257894000	cookie-7	v$7
|}
  in
  let jar = from_mozilla_format ~clock content in
  let cookies = get_all_cookies jar in

  (* Check total number of cookies (should skip commented lines) *)
  Alcotest.(check int) "cookie count" 5 (List.length cookies);
  Alcotest.(check int) "count function" 5 (count jar);
  Alcotest.(check bool) "not empty" false (is_empty jar);

  let find_cookie name = List.find (fun c -> Cookeio.name c = name) cookies in

  (* Test cookie-1: session cookie on exact domain *)
  let cookie1 = find_cookie "cookie-1" in
  Alcotest.(check string)
    "cookie-1 domain" "example.com" (Cookeio.domain cookie1);
  Alcotest.(check string) "cookie-1 path" "/foo/" (Cookeio.path cookie1);
  Alcotest.(check string) "cookie-1 name" "cookie-1" (Cookeio.name cookie1);
  Alcotest.(check string) "cookie-1 value" "v$1" (Cookeio.value cookie1);
  Alcotest.(check bool) "cookie-1 secure" false (Cookeio.secure cookie1);
  Alcotest.(check bool) "cookie-1 http_only" false (Cookeio.http_only cookie1);
  Alcotest.(check (option expiration_testable))
    "cookie-1 expires" None (Cookeio.expires cookie1);
  Alcotest.(
    check
      (option
         (Alcotest.testable
            (fun ppf -> function
              | `Strict -> Format.pp_print_string ppf "Strict"
              | `Lax -> Format.pp_print_string ppf "Lax"
              | `None -> Format.pp_print_string ppf "None")
            ( = ))))
    "cookie-1 same_site" None
    (Cookeio.same_site cookie1);

  (* Test cookie-2: session cookie on subdomain pattern *)
  let cookie2 = find_cookie "cookie-2" in
  Alcotest.(check string)
    "cookie-2 domain" "example.com" (Cookeio.domain cookie2);
  Alcotest.(check string) "cookie-2 path" "/foo/" (Cookeio.path cookie2);
  Alcotest.(check string) "cookie-2 name" "cookie-2" (Cookeio.name cookie2);
  Alcotest.(check string) "cookie-2 value" "v$2" (Cookeio.value cookie2);
  Alcotest.(check bool) "cookie-2 secure" false (Cookeio.secure cookie2);
  Alcotest.(check bool) "cookie-2 http_only" false (Cookeio.http_only cookie2);
  Alcotest.(check (option expiration_testable))
    "cookie-2 expires" None (Cookeio.expires cookie2);

  (* Test cookie-3: non-session cookie with expiry *)
  let cookie3 = find_cookie "cookie-3" in
  let expected_expiry = Ptime.of_float_s 1257894000.0 in
  Alcotest.(check string)
    "cookie-3 domain" "example.com" (Cookeio.domain cookie3);
  Alcotest.(check string) "cookie-3 path" "/foo/" (Cookeio.path cookie3);
  Alcotest.(check string) "cookie-3 name" "cookie-3" (Cookeio.name cookie3);
  Alcotest.(check string) "cookie-3 value" "v$3" (Cookeio.value cookie3);
  Alcotest.(check bool) "cookie-3 secure" false (Cookeio.secure cookie3);
  Alcotest.(check bool) "cookie-3 http_only" false (Cookeio.http_only cookie3);
  begin match expected_expiry with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "cookie-3 expires"
        (Some (`DateTime t))
        (Cookeio.expires cookie3)
  | None -> Alcotest.fail "Expected expiry time for cookie-3"
  end;

  (* Test cookie-4: another non-session cookie *)
  let cookie4 = find_cookie "cookie-4" in
  Alcotest.(check string)
    "cookie-4 domain" "example.com" (Cookeio.domain cookie4);
  Alcotest.(check string) "cookie-4 path" "/foo/" (Cookeio.path cookie4);
  Alcotest.(check string) "cookie-4 name" "cookie-4" (Cookeio.name cookie4);
  Alcotest.(check string) "cookie-4 value" "v$4" (Cookeio.value cookie4);
  Alcotest.(check bool) "cookie-4 secure" false (Cookeio.secure cookie4);
  Alcotest.(check bool) "cookie-4 http_only" false (Cookeio.http_only cookie4);
  begin match expected_expiry with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "cookie-4 expires"
        (Some (`DateTime t))
        (Cookeio.expires cookie4)
  | None -> Alcotest.fail "Expected expiry time for cookie-4"
  end;

  (* Test cookie-5: secure cookie *)
  let cookie5 = find_cookie "cookie-5" in
  Alcotest.(check string)
    "cookie-5 domain" "example.com" (Cookeio.domain cookie5);
  Alcotest.(check string) "cookie-5 path" "/foo/" (Cookeio.path cookie5);
  Alcotest.(check string) "cookie-5 name" "cookie-5" (Cookeio.name cookie5);
  Alcotest.(check string) "cookie-5 value" "v$5" (Cookeio.value cookie5);
  Alcotest.(check bool) "cookie-5 secure" true (Cookeio.secure cookie5);
  Alcotest.(check bool) "cookie-5 http_only" false (Cookeio.http_only cookie5);
  begin match expected_expiry with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "cookie-5 expires"
        (Some (`DateTime t))
        (Cookeio.expires cookie5)
  | None -> Alcotest.fail "Expected expiry time for cookie-5"
  end

let test_load_from_file env =
  (* This test loads from the actual test/cookies.txt file using the load function *)
  let clock = Eio.Stdenv.clock env in
  let cwd = Eio.Stdenv.cwd env in
  let cookie_path = Eio.Path.(cwd / "cookies.txt") in
  let jar = load ~clock cookie_path in
  let cookies = get_all_cookies jar in

  (* Should have the same 5 cookies as the string test *)
  Alcotest.(check int) "file load cookie count" 5 (List.length cookies);

  let find_cookie name = List.find (fun c -> Cookeio.name c = name) cookies in

  (* Verify a few key cookies are loaded correctly *)
  let cookie1 = find_cookie "cookie-1" in
  Alcotest.(check string) "file cookie-1 value" "v$1" (Cookeio.value cookie1);
  Alcotest.(check string)
    "file cookie-1 domain" "example.com" (Cookeio.domain cookie1);
  Alcotest.(check bool) "file cookie-1 secure" false (Cookeio.secure cookie1);
  Alcotest.(check (option expiration_testable))
    "file cookie-1 expires" None (Cookeio.expires cookie1);

  let cookie5 = find_cookie "cookie-5" in
  Alcotest.(check string) "file cookie-5 value" "v$5" (Cookeio.value cookie5);
  Alcotest.(check bool) "file cookie-5 secure" true (Cookeio.secure cookie5);
  let expected_expiry = Ptime.of_float_s 1257894000.0 in
  begin match expected_expiry with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "file cookie-5 expires"
        (Some (`DateTime t))
        (Cookeio.expires cookie5)
  | None -> Alcotest.fail "Expected expiry time for cookie-5"
  end;

  (* Verify subdomain cookie *)
  let cookie2 = find_cookie "cookie-2" in
  Alcotest.(check string)
    "file cookie-2 domain" "example.com" (Cookeio.domain cookie2);
  Alcotest.(check (option expiration_testable))
    "file cookie-2 expires" None (Cookeio.expires cookie2)

let test_cookie_matching env =
  let clock = Eio.Stdenv.clock env in
  let jar = create () in

  (* Add test cookies with different domain patterns *)
  let exact_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"exact" ~value:"test1"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:Ptime.epoch ~last_access:Ptime.epoch ()
  in
  let subdomain_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"subdomain"
      ~value:"test2" ~secure:false ~http_only:false ?expires:None
      ?same_site:None ?max_age:None ~creation_time:Ptime.epoch
      ~last_access:Ptime.epoch ()
  in
  let secure_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"secure" ~value:"test3"
      ~secure:true ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:Ptime.epoch ~last_access:Ptime.epoch ()
  in

  add_cookie jar exact_cookie;
  add_cookie jar subdomain_cookie;
  add_cookie jar secure_cookie;

  (* Test exact domain matching - all three cookies should match example.com *)
  let cookies_http =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "http cookies count" 2 (List.length cookies_http);

  let cookies_https =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:true
  in
  Alcotest.(check int) "https cookies count" 3 (List.length cookies_https);

  (* Test subdomain matching - all cookies should match subdomains now *)
  let cookies_sub =
    get_cookies jar ~clock ~domain:"sub.example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "subdomain cookies count" 2 (List.length cookies_sub)

let test_empty_jar env =
  let clock = Eio.Stdenv.clock env in
  let jar = create () in
  Alcotest.(check bool) "empty jar" true (is_empty jar);
  Alcotest.(check int) "empty count" 0 (count jar);
  Alcotest.(check (list cookie_testable))
    "empty cookies" [] (get_all_cookies jar);

  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "no matching cookies" 0 (List.length cookies)

let test_round_trip_mozilla_format env =
  let clock = Eio.Stdenv.clock env in
  let jar = create () in

  let test_cookie =
    let expires =
      match Ptime.of_float_s 1257894000.0 with
      | Some t -> Some (`DateTime t)
      | None -> None
    in
    Cookeio.make ~domain:"example.com" ~path:"/test/" ~name:"test"
      ~value:"value" ~secure:true ~http_only:false ?expires ~same_site:`Strict
      ?max_age:None ~creation_time:Ptime.epoch ~last_access:Ptime.epoch ()
  in

  add_cookie jar test_cookie;

  (* Convert to Mozilla format and back *)
  let mozilla_format = to_mozilla_format jar in
  let jar2 = from_mozilla_format ~clock mozilla_format in
  let cookies2 = get_all_cookies jar2 in

  Alcotest.(check int) "round trip count" 1 (List.length cookies2);
  let cookie2 = List.hd cookies2 in
  Alcotest.(check string) "round trip name" "test" (Cookeio.name cookie2);
  Alcotest.(check string) "round trip value" "value" (Cookeio.value cookie2);
  Alcotest.(check string)
    "round trip domain" "example.com" (Cookeio.domain cookie2);
  Alcotest.(check string) "round trip path" "/test/" (Cookeio.path cookie2);
  Alcotest.(check bool) "round trip secure" true (Cookeio.secure cookie2);
  (* Note: http_only and same_site are lost in Mozilla format *)
  begin match Ptime.of_float_s 1257894000.0 with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "round trip expires"
        (Some (`DateTime t))
        (Cookeio.expires cookie2)
  | None -> Alcotest.fail "Expected expiry time"
  end

let test_cookie_expiry_with_mock_clock () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in

  (* Start at time 1000.0 for convenience *)
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add a cookie that expires at time 1500.0 (expires in 500 seconds) *)
  let expires_soon = Ptime.of_float_s 1500.0 |> Option.get in
  let cookie1 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"expires_soon"
      ~value:"value1" ~secure:false ~http_only:false
      ~expires:(`DateTime expires_soon) ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in

  (* Add a cookie that expires at time 2000.0 (expires in 1000 seconds) *)
  let expires_later = Ptime.of_float_s 2000.0 |> Option.get in
  let cookie2 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"expires_later"
      ~value:"value2" ~secure:false ~http_only:false
      ~expires:(`DateTime expires_later) ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in

  (* Add a session cookie (no expiry) *)
  let cookie3 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"session" ~value:"value3"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in

  add_cookie jar cookie1;
  add_cookie jar cookie2;
  add_cookie jar cookie3;

  Alcotest.(check int) "initial count" 3 (count jar);

  (* Advance time to 1600.0 - first cookie should expire *)
  Eio_mock.Clock.set_time clock 1600.0;
  clear_expired jar ~clock;

  Alcotest.(check int) "after first expiry" 2 (count jar);

  let cookies = get_all_cookies jar in
  let names = List.map Cookeio.name cookies |> List.sort String.compare in
  Alcotest.(check (list string))
    "remaining cookies after 1600s"
    [ "expires_later"; "session" ]
    names;

  (* Advance time to 2100.0 - second cookie should expire *)
  Eio_mock.Clock.set_time clock 2100.0;
  clear_expired jar ~clock;

  Alcotest.(check int) "after second expiry" 1 (count jar);

  let remaining = get_all_cookies jar in
  Alcotest.(check string)
    "only session cookie remains" "session"
    (Cookeio.name (List.hd remaining))

let test_get_cookies_filters_expired () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add an expired cookie (expired at time 500) *)
  let expired = Ptime.of_float_s 500.0 |> Option.get in
  let cookie_expired =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"expired"
      ~value:"old" ~secure:false ~http_only:false
      ~expires:(`DateTime expired)
      ~creation_time:(Ptime.of_float_s 100.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 100.0 |> Option.get)
      ()
  in

  (* Add a valid cookie (expires at time 2000) *)
  let valid_time = Ptime.of_float_s 2000.0 |> Option.get in
  let cookie_valid =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"valid"
      ~value:"current" ~secure:false ~http_only:false
      ~expires:(`DateTime valid_time)
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in

  (* Add a session cookie (no expiry) *)
  let cookie_session =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"session"
      ~value:"sess" ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in

  add_cookie jar cookie_expired;
  add_cookie jar cookie_valid;
  add_cookie jar cookie_session;

  (* get_all_cookies returns all including expired (for inspection) *)
  Alcotest.(check int) "get_all_cookies includes expired" 3
    (List.length (get_all_cookies jar));

  (* get_cookies should automatically filter out expired cookies *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "get_cookies filters expired" 2 (List.length cookies);

  let names = List.map Cookeio.name cookies |> List.sort String.compare in
  Alcotest.(check (list string))
    "only non-expired cookies returned"
    [ "session"; "valid" ]
    names

let test_max_age_parsing_with_mock_clock () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in

  (* Start at a known time *)
  Eio_mock.Clock.set_time clock 5000.0;

  (* Parse a Set-Cookie header with Max-Age *)
  let header = "session=abc123; Max-Age=3600; Secure; HttpOnly" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in

  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check string) "cookie name" "session" (Cookeio.name cookie);
  Alcotest.(check string) "cookie value" "abc123" (Cookeio.value cookie);
  Alcotest.(check bool) "cookie secure" true (Cookeio.secure cookie);
  Alcotest.(check bool) "cookie http_only" true (Cookeio.http_only cookie);

  (* Verify the expiry time is set correctly (5000.0 + 3600 = 8600.0) *)
  let expected_expiry = Ptime.of_float_s 8600.0 in
  begin match expected_expiry with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "expires set from max-age"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time"
  end;

  (* Verify creation time matches clock time *)
  let expected_creation = Ptime.of_float_s 5000.0 in
  Alcotest.(check (option (Alcotest.testable Ptime.pp Ptime.equal)))
    "creation time" expected_creation
    (Some (Cookeio.creation_time cookie))

let test_last_access_time_with_mock_clock () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in

  (* Start at time 3000.0 *)
  Eio_mock.Clock.set_time clock 3000.0;

  let jar = create () in

  (* Add a cookie *)
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"value"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 3000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 3000.0 |> Option.get)
      ()
  in
  add_cookie jar cookie;

  (* Verify initial last access time *)
  let cookies1 = get_all_cookies jar in
  let cookie1 = List.hd cookies1 in
  Alcotest.(check (option (Alcotest.testable Ptime.pp Ptime.equal)))
    "initial last access" (Ptime.of_float_s 3000.0)
    (Some (Cookeio.last_access cookie1));

  (* Advance time to 4000.0 *)
  Eio_mock.Clock.set_time clock 4000.0;

  (* Get cookies, which should update last access time to current clock time *)
  let _retrieved =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in

  (* Verify last access time was updated to the new clock time *)
  let cookies2 = get_all_cookies jar in
  let cookie2 = List.hd cookies2 in
  Alcotest.(check (option (Alcotest.testable Ptime.pp Ptime.equal)))
    "updated last access" (Ptime.of_float_s 4000.0)
    (Some (Cookeio.last_access cookie2))

let test_of_set_cookie_header_with_expires () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in

  (* Start at a known time *)
  Eio_mock.Clock.set_time clock 6000.0;

  (* Use RFC3339 format which is what Ptime.of_rfc3339 expects *)
  let header =
    "id=xyz789; Expires=2025-10-21T07:28:00Z; Path=/; Domain=.example.com"
  in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in

  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check string) "cookie name" "id" (Cookeio.name cookie);
  Alcotest.(check string) "cookie value" "xyz789" (Cookeio.value cookie);
  Alcotest.(check string) "cookie domain" "example.com" (Cookeio.domain cookie);
  Alcotest.(check string) "cookie path" "/" (Cookeio.path cookie);

  (* Verify expires is parsed correctly *)
  Alcotest.(check bool)
    "has expiry" true
    (Option.is_some (Cookeio.expires cookie));

  (* Verify the specific expiry time parsed from the RFC3339 date *)
  let expected_expiry = Ptime.of_rfc3339 "2025-10-21T07:28:00Z" in
  match expected_expiry with
  | Ok (time, _, _) ->
      Alcotest.(check (option expiration_testable))
        "expires matches parsed value"
        (Some (`DateTime time))
        (Cookeio.expires cookie)
  | Error _ -> Alcotest.fail "Failed to parse expected expiry time"

let test_samesite_none_validation () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in

  (* Start at a known time *)
  Eio_mock.Clock.set_time clock 7000.0;

  (* This should be rejected: SameSite=None without Secure *)
  let invalid_header = "token=abc; SameSite=None" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" invalid_header
  in

  Alcotest.(check bool)
    "invalid cookie rejected" true
    (Result.is_error cookie_opt);

  (* This should be accepted: SameSite=None with Secure *)
  let valid_header = "token=abc; SameSite=None; Secure" in
  let cookie_opt2 =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" valid_header
  in

  Alcotest.(check bool)
    "valid cookie accepted" true
    (Result.is_ok cookie_opt2);

  let cookie = Result.get_ok cookie_opt2 in
  Alcotest.(check bool) "cookie is secure" true (Cookeio.secure cookie);
  Alcotest.(
    check
      (option
         (Alcotest.testable
            (fun ppf -> function
              | `Strict -> Format.pp_print_string ppf "Strict"
              | `Lax -> Format.pp_print_string ppf "Lax"
              | `None -> Format.pp_print_string ppf "None")
            ( = ))))
    "samesite is None" (Some `None) (Cookeio.same_site cookie)

let test_domain_normalization () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Test parsing ".example.com" stores as "example.com" *)
  let header = "test=value; Domain=.example.com" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);
  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check string)
    "domain normalized" "example.com" (Cookeio.domain cookie);

  (* Test round-trip through Mozilla format normalizes domains *)
  let jar = create () in
  let test_cookie =
    Cookeio.make ~domain:".example.com" ~path:"/" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_cookie jar test_cookie;

  let mozilla_format = to_mozilla_format jar in
  let jar2 = from_mozilla_format ~clock mozilla_format in
  let cookies2 = get_all_cookies jar2 in
  Alcotest.(check int) "one cookie" 1 (List.length cookies2);
  Alcotest.(check string)
    "domain normalized after round-trip" "example.com"
    (Cookeio.domain (List.hd cookies2))

let test_max_age_stored_separately () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 5000.0;

  (* Parse a Set-Cookie header with Max-Age *)
  let header = "session=abc123; Max-Age=3600" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in

  (* Verify max_age is stored as a Ptime.Span *)
  Alcotest.(check bool)
    "max_age is set" true
    (Option.is_some (Cookeio.max_age cookie));
  let max_age_span = Option.get (Cookeio.max_age cookie) in
  Alcotest.(check (option int))
    "max_age is 3600 seconds" (Some 3600)
    (Ptime.Span.to_int_s max_age_span);

  (* Verify expires is also computed correctly *)
  let expected_expiry = Ptime.of_float_s 8600.0 in
  begin match expected_expiry with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "expires computed from max-age"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time"
  end

let test_max_age_negative_becomes_zero () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 5000.0;

  (* Parse a Set-Cookie header with negative Max-Age *)
  let header = "session=abc123; Max-Age=-100" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in

  (* Verify max_age is stored as 0 per RFC 6265 *)
  Alcotest.(check bool)
    "max_age is set" true
    (Option.is_some (Cookeio.max_age cookie));
  let max_age_span = Option.get (Cookeio.max_age cookie) in
  Alcotest.(check (option int))
    "negative max_age becomes 0" (Some 0)
    (Ptime.Span.to_int_s max_age_span);

  (* Verify expires is computed with 0 seconds *)
  let expected_expiry = Ptime.of_float_s 5000.0 in
  begin match expected_expiry with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "expires computed with 0 seconds"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time"
  end

let string_contains_substring s sub =
  try
    let len = String.length sub in
    let rec search i =
      if i + len > String.length s then false
      else if String.sub s i len = sub then true
      else search (i + 1)
    in
    search 0
  with _ -> false

let test_make_set_cookie_header_includes_max_age () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 5000.0;

  (* Create a cookie with max_age *)
  let max_age_span = Ptime.Span.of_int_s 3600 in
  let expires_time = Ptime.of_float_s 8600.0 |> Option.get in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"session" ~value:"abc123"
      ~secure:true ~http_only:true
      ?expires:(Some (`DateTime expires_time))
      ?max_age:(Some max_age_span) ?same_site:(Some `Strict)
      ~creation_time:(Ptime.of_float_s 5000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 5000.0 |> Option.get)
      ()
  in

  let header = make_set_cookie_header cookie in

  (* Verify the header includes Max-Age *)
  Alcotest.(check bool)
    "header includes Max-Age" true
    (string_contains_substring header "Max-Age=3600");

  (* Verify the header includes Expires *)
  Alcotest.(check bool)
    "header includes Expires" true
    (string_contains_substring header "Expires=");

  (* Verify the header includes other attributes *)
  Alcotest.(check bool)
    "header includes Secure" true
    (string_contains_substring header "Secure");
  Alcotest.(check bool)
    "header includes HttpOnly" true
    (string_contains_substring header "HttpOnly");
  Alcotest.(check bool)
    "header includes SameSite" true
    (string_contains_substring header "SameSite=Strict")

let test_max_age_round_trip () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 5000.0;

  (* Parse a cookie with Max-Age *)
  let header = "session=xyz; Max-Age=7200; Secure; HttpOnly" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);
  let cookie = Result.get_ok cookie_opt in

  (* Generate Set-Cookie header from the cookie *)
  let set_cookie_header = make_set_cookie_header cookie in

  (* Parse it back *)
  Eio_mock.Clock.set_time clock 5000.0;
  (* Reset clock to same time *)
  let cookie2_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" set_cookie_header
  in
  Alcotest.(check bool) "cookie re-parsed" true (Result.is_ok cookie2_opt);
  let cookie2 = Result.get_ok cookie2_opt in

  (* Verify max_age is preserved *)
  Alcotest.(check (option int))
    "max_age preserved"
    (Ptime.Span.to_int_s (Option.get (Cookeio.max_age cookie)))
    (Ptime.Span.to_int_s (Option.get (Cookeio.max_age cookie2)))

let test_domain_matching () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 2000.0;

  let jar = create () in

  (* Create a cookie with domain "example.com" *)
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"value"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 2000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 2000.0 |> Option.get)
      ()
  in
  add_cookie jar cookie;

  (* Test "example.com" cookie matches "example.com" request *)
  let cookies1 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "matches exact domain" 1 (List.length cookies1);

  (* Test "example.com" cookie matches "sub.example.com" request *)
  let cookies2 =
    get_cookies jar ~clock ~domain:"sub.example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "matches subdomain" 1 (List.length cookies2);

  (* Test "example.com" cookie matches "deep.sub.example.com" request *)
  let cookies3 =
    get_cookies jar ~clock ~domain:"deep.sub.example.com" ~path:"/"
      ~is_secure:false
  in
  Alcotest.(check int) "matches deep subdomain" 1 (List.length cookies3);

  (* Test "example.com" cookie doesn't match "notexample.com" *)
  let cookies4 =
    get_cookies jar ~clock ~domain:"notexample.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "doesn't match different domain" 0 (List.length cookies4);

  (* Test "example.com" cookie doesn't match "fakeexample.com" *)
  let cookies5 =
    get_cookies jar ~clock ~domain:"fakeexample.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "doesn't match prefix domain" 0 (List.length cookies5)

(** {1 HTTP Date Parsing Tests} *)

let test_http_date_fmt1 () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Test FMT1: "Wed, 21 Oct 2015 07:28:00 GMT" (RFC 1123) *)
  let header = "session=abc; Expires=Wed, 21 Oct 2015 07:28:00 GMT" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "FMT1 cookie parsed" true (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check bool)
    "FMT1 has expiry" true
    (Option.is_some (Cookeio.expires cookie));

  (* Verify the parsed time matches expected value *)
  let expected = Ptime.of_date_time ((2015, 10, 21), ((07, 28, 00), 0)) in
  begin match expected with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "FMT1 expiry correct"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time for FMT1"
  end

let test_http_date_fmt2 () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Test FMT2: "Wednesday, 21-Oct-15 07:28:00 GMT" (RFC 850 with abbreviated year) *)
  let header = "session=abc; Expires=Wednesday, 21-Oct-15 07:28:00 GMT" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "FMT2 cookie parsed" true (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check bool)
    "FMT2 has expiry" true
    (Option.is_some (Cookeio.expires cookie));

  (* Year 15 should be normalized to 2015 *)
  let expected = Ptime.of_date_time ((2015, 10, 21), ((07, 28, 00), 0)) in
  begin match expected with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "FMT2 expiry correct with year normalization"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time for FMT2"
  end

let test_http_date_fmt3 () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Test FMT3: "Wed Oct 21 07:28:00 2015" (asctime) *)
  let header = "session=abc; Expires=Wed Oct 21 07:28:00 2015" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "FMT3 cookie parsed" true (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check bool)
    "FMT3 has expiry" true
    (Option.is_some (Cookeio.expires cookie));

  let expected = Ptime.of_date_time ((2015, 10, 21), ((07, 28, 00), 0)) in
  begin match expected with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "FMT3 expiry correct"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time for FMT3"
  end

let test_http_date_fmt4 () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Test FMT4: "Wed, 21-Oct-2015 07:28:00 GMT" (variant) *)
  let header = "session=abc; Expires=Wed, 21-Oct-2015 07:28:00 GMT" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "FMT4 cookie parsed" true (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check bool)
    "FMT4 has expiry" true
    (Option.is_some (Cookeio.expires cookie));

  let expected = Ptime.of_date_time ((2015, 10, 21), ((07, 28, 00), 0)) in
  begin match expected with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "FMT4 expiry correct"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time for FMT4"
  end

let test_abbreviated_year_69_to_99 () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Year 95 should become 1995 *)
  let header = "session=abc; Expires=Wed, 21-Oct-95 07:28:00 GMT" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  let cookie = Result.get_ok cookie_opt in
  let expected = Ptime.of_date_time ((1995, 10, 21), ((07, 28, 00), 0)) in
  begin match expected with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "year 95 becomes 1995"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time for year 95"
  end;

  (* Year 69 should become 1969 *)
  let header2 = "session=abc; Expires=Wed, 10-Sep-69 20:00:00 GMT" in
  let cookie_opt2 =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header2
  in
  let cookie2 = Result.get_ok cookie_opt2 in
  let expected2 = Ptime.of_date_time ((1969, 9, 10), ((20, 0, 0), 0)) in
  begin match expected2 with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "year 69 becomes 1969"
        (Some (`DateTime t))
        (Cookeio.expires cookie2)
  | None -> Alcotest.fail "Expected expiry time for year 69"
  end;

  (* Year 99 should become 1999 *)
  let header3 = "session=abc; Expires=Thu, 10-Sep-99 20:00:00 GMT" in
  let cookie_opt3 =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header3
  in
  let cookie3 = Result.get_ok cookie_opt3 in
  let expected3 = Ptime.of_date_time ((1999, 9, 10), ((20, 0, 0), 0)) in
  begin match expected3 with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "year 99 becomes 1999"
        (Some (`DateTime t))
        (Cookeio.expires cookie3)
  | None -> Alcotest.fail "Expected expiry time for year 99"
  end

let test_abbreviated_year_0_to_68 () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Year 25 should become 2025 *)
  let header = "session=abc; Expires=Wed, 21-Oct-25 07:28:00 GMT" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  let cookie = Result.get_ok cookie_opt in
  let expected = Ptime.of_date_time ((2025, 10, 21), ((07, 28, 00), 0)) in
  begin match expected with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "year 25 becomes 2025"
        (Some (`DateTime t))
        (Cookeio.expires cookie)
  | None -> Alcotest.fail "Expected expiry time for year 25"
  end;

  (* Year 0 should become 2000 *)
  let header2 = "session=abc; Expires=Fri, 01-Jan-00 00:00:00 GMT" in
  let cookie_opt2 =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header2
  in
  let cookie2 = Result.get_ok cookie_opt2 in
  let expected2 = Ptime.of_date_time ((2000, 1, 1), ((0, 0, 0), 0)) in
  begin match expected2 with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "year 0 becomes 2000"
        (Some (`DateTime t))
        (Cookeio.expires cookie2)
  | None -> Alcotest.fail "Expected expiry time for year 0"
  end;

  (* Year 68 should become 2068 *)
  let header3 = "session=abc; Expires=Thu, 10-Sep-68 20:00:00 GMT" in
  let cookie_opt3 =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header3
  in
  let cookie3 = Result.get_ok cookie_opt3 in
  let expected3 = Ptime.of_date_time ((2068, 9, 10), ((20, 0, 0), 0)) in
  begin match expected3 with
  | Some t ->
      Alcotest.(check (option expiration_testable))
        "year 68 becomes 2068"
        (Some (`DateTime t))
        (Cookeio.expires cookie3)
  | None -> Alcotest.fail "Expected expiry time for year 68"
  end

let test_rfc3339_still_works () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Ensure RFC 3339 format still works for backward compatibility *)
  let header = "session=abc; Expires=2025-10-21T07:28:00Z" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool)
    "RFC 3339 cookie parsed" true
    (Result.is_ok cookie_opt);

  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check bool)
    "RFC 3339 has expiry" true
    (Option.is_some (Cookeio.expires cookie));

  (* Verify the time was parsed correctly *)
  let expected = Ptime.of_rfc3339 "2025-10-21T07:28:00Z" in
  match expected with
  | Ok (time, _, _) ->
      Alcotest.(check (option expiration_testable))
        "RFC 3339 expiry correct"
        (Some (`DateTime time))
        (Cookeio.expires cookie)
  | Error _ -> Alcotest.fail "Failed to parse expected RFC 3339 time"

let test_invalid_date_format_logs_warning () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Invalid date format should log a warning but still parse the cookie *)
  let header = "session=abc; Expires=InvalidDate" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in

  (* Cookie should still be parsed, just without expires *)
  Alcotest.(check bool)
    "cookie parsed despite invalid date" true
    (Result.is_ok cookie_opt);
  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check string) "cookie name correct" "session" (Cookeio.name cookie);
  Alcotest.(check string) "cookie value correct" "abc" (Cookeio.value cookie);
  (* expires should be None since date was invalid *)
  Alcotest.(check (option expiration_testable))
    "expires is None for invalid date" None (Cookeio.expires cookie)

let test_case_insensitive_month_parsing () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Test various case combinations for month names *)
  let test_cases =
    [
      ("session=abc; Expires=Wed, 21 oct 2015 07:28:00 GMT", "lowercase month");
      ("session=abc; Expires=Wed, 21 OCT 2015 07:28:00 GMT", "uppercase month");
      ("session=abc; Expires=Wed, 21 OcT 2015 07:28:00 GMT", "mixed case month");
      ("session=abc; Expires=Wed, 21 oCt 2015 07:28:00 GMT", "weird case month");
    ]
  in

  List.iter
    (fun (header, description) ->
      let cookie_opt =
        of_set_cookie_header
          ~now:(fun () ->
            Ptime.of_float_s (Eio.Time.now clock)
            |> Option.value ~default:Ptime.epoch)
          ~domain:"example.com" ~path:"/" header
      in
      Alcotest.(check bool)
        (description ^ " parsed") true
        (Result.is_ok cookie_opt);

      let cookie = Result.get_ok cookie_opt in
      Alcotest.(check bool)
        (description ^ " has expiry")
        true
        (Option.is_some (Cookeio.expires cookie));

      (* Verify the date was parsed correctly regardless of case *)
      let expires = Option.get (Cookeio.expires cookie) in
      match expires with
      | `DateTime ptime ->
          let year, month, _ = Ptime.to_date ptime in
          Alcotest.(check int) (description ^ " year correct") 2015 year;
          Alcotest.(check int)
            (description ^ " month correct (October=10)")
            10 month
      | `Session -> Alcotest.fail (description ^ " should not be session cookie"))
    test_cases

let test_case_insensitive_gmt_parsing () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Test various case combinations for GMT timezone *)
  let test_cases =
    [
      ("session=abc; Expires=Wed, 21 Oct 2015 07:28:00 GMT", "uppercase GMT");
      ("session=abc; Expires=Wed, 21 Oct 2015 07:28:00 gmt", "lowercase gmt");
      ("session=abc; Expires=Wed, 21 Oct 2015 07:28:00 Gmt", "mixed case Gmt");
      ("session=abc; Expires=Wed, 21 Oct 2015 07:28:00 GmT", "weird case GmT");
    ]
  in

  List.iter
    (fun (header, description) ->
      let cookie_opt =
        of_set_cookie_header
          ~now:(fun () ->
            Ptime.of_float_s (Eio.Time.now clock)
            |> Option.value ~default:Ptime.epoch)
          ~domain:"example.com" ~path:"/" header
      in
      Alcotest.(check bool)
        (description ^ " parsed") true
        (Result.is_ok cookie_opt);

      let cookie = Result.get_ok cookie_opt in
      Alcotest.(check bool)
        (description ^ " has expiry")
        true
        (Option.is_some (Cookeio.expires cookie));

      (* Verify the date was parsed correctly regardless of GMT case *)
      let expires = Option.get (Cookeio.expires cookie) in
      match expires with
      | `DateTime ptime ->
          let year, month, day = Ptime.to_date ptime in
          Alcotest.(check int) (description ^ " year correct") 2015 year;
          Alcotest.(check int)
            (description ^ " month correct (October=10)")
            10 month;
          Alcotest.(check int) (description ^ " day correct") 21 day
      | `Session -> Alcotest.fail (description ^ " should not be session cookie"))
    test_cases

(** {1 Delta Tracking Tests} *)

let test_add_original_not_in_delta () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"value"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_original jar cookie;

  (* Delta should be empty *)
  let delta = delta jar in
  Alcotest.(check int) "delta is empty" 0 (List.length delta);

  (* But the cookie should be in the jar *)
  Alcotest.(check int) "jar count is 1" 1 (count jar)

let test_add_cookie_appears_in_delta () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"value"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_cookie jar cookie;

  (* Delta should contain the cookie *)
  let delta = delta jar in
  Alcotest.(check int) "delta has 1 cookie" 1 (List.length delta);
  let delta_cookie = List.hd delta in
  Alcotest.(check string) "delta cookie name" "test" (Cookeio.name delta_cookie);
  Alcotest.(check string)
    "delta cookie value" "value"
    (Cookeio.value delta_cookie)

let test_remove_original_creates_removal_cookie () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"value"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_original jar cookie;

  (* Remove the cookie *)
  remove jar ~clock cookie;

  (* Delta should contain a removal cookie *)
  let delta = delta jar in
  Alcotest.(check int) "delta has 1 removal cookie" 1 (List.length delta);
  let removal_cookie = List.hd delta in
  Alcotest.(check string)
    "removal cookie name" "test"
    (Cookeio.name removal_cookie);
  Alcotest.(check string)
    "removal cookie has empty value" ""
    (Cookeio.value removal_cookie);

  (* Check Max-Age is 0 *)
  match Cookeio.max_age removal_cookie with
  | Some span ->
      Alcotest.(check (option int))
        "removal cookie Max-Age is 0" (Some 0) (Ptime.Span.to_int_s span)
  | None -> Alcotest.fail "removal cookie should have Max-Age"

let test_remove_delta_cookie_removes_it () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"value"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_cookie jar cookie;

  (* Remove the cookie *)
  remove jar ~clock cookie;

  (* Delta should be empty *)
  let delta = delta jar in
  Alcotest.(check int)
    "delta is empty after removing delta cookie" 0 (List.length delta)

let test_get_cookies_combines_original_and_delta () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add an original cookie *)
  let original =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"original"
      ~value:"orig_val" ~secure:false ~http_only:false ?expires:None
      ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_original jar original;

  (* Add a delta cookie *)
  let delta_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"delta"
      ~value:"delta_val" ~secure:false ~http_only:false ?expires:None
      ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_cookie jar delta_cookie;

  (* Get cookies should return both *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "both cookies returned" 2 (List.length cookies);

  let names = List.map Cookeio.name cookies |> List.sort String.compare in
  Alcotest.(check (list string)) "cookie names" [ "delta"; "original" ] names

let test_get_cookies_delta_takes_precedence () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add an original cookie *)
  let original =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"orig_val"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_original jar original;

  (* Add a delta cookie with the same name/domain/path *)
  let delta_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"delta_val"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_cookie jar delta_cookie;

  (* Get cookies should return only the delta cookie *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "only one cookie returned" 1 (List.length cookies);
  let cookie = List.hd cookies in
  Alcotest.(check string)
    "delta cookie value" "delta_val" (Cookeio.value cookie)

let test_get_cookies_excludes_removal_cookies () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add an original cookie *)
  let original =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"value"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_original jar original;

  (* Remove it *)
  remove jar ~clock original;

  (* Get cookies should return nothing *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "no cookies returned" 0 (List.length cookies);

  (* But delta should have the removal cookie *)
  let delta = delta jar in
  Alcotest.(check int) "delta has removal cookie" 1 (List.length delta)

let test_delta_returns_only_changed_cookies () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add original cookies *)
  let original1 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"orig1" ~value:"val1"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_original jar original1;

  let original2 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"orig2" ~value:"val2"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_original jar original2;

  (* Add a new delta cookie *)
  let new_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"new" ~value:"new_val"
      ~secure:false ~http_only:false ?expires:None ?same_site:None ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_cookie jar new_cookie;

  (* Delta should only contain the new cookie *)
  let delta = delta jar in
  Alcotest.(check int) "delta has 1 cookie" 1 (List.length delta);
  let delta_cookie = List.hd delta in
  Alcotest.(check string) "delta cookie name" "new" (Cookeio.name delta_cookie)

let test_removal_cookie_format () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"value"
      ~secure:true ~http_only:true ?expires:None ~same_site:`Strict
      ?max_age:None
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get)
      ()
  in
  add_original jar cookie;

  (* Remove the cookie *)
  remove jar ~clock cookie;

  (* Get the removal cookie *)
  let delta = delta jar in
  let removal = List.hd delta in

  (* Check all properties *)
  Alcotest.(check string)
    "removal cookie has empty value" "" (Cookeio.value removal);
  Alcotest.(check (option int))
    "removal cookie Max-Age is 0" (Some 0)
    (Option.bind (Cookeio.max_age removal) Ptime.Span.to_int_s);

  (* Check expires is in the past *)
  let now = Ptime.of_float_s 1000.0 |> Option.get in
  match Cookeio.expires removal with
  | Some (`DateTime exp) ->
      Alcotest.(check bool)
        "expires is in the past" true
        (Ptime.compare exp now < 0)
  | _ -> Alcotest.fail "removal cookie should have DateTime expires"

(* ============================================================================ *)
(* Priority 2 Tests *)
(* ============================================================================ *)

(* Priority 2.1: Partitioned Cookies *)

let test_partitioned_parsing env =
  let clock = Eio.Stdenv.clock env in

  match
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"widget.com" ~path:"/" "id=123; Partitioned; Secure"
  with
  | Ok c ->
      Alcotest.(check bool) "partitioned flag" true (partitioned c);
      Alcotest.(check bool) "secure flag" true (secure c)
  | Error msg -> Alcotest.fail ("Should parse valid Partitioned cookie: " ^ msg)

let test_partitioned_serialization env =
  let clock = Eio.Stdenv.clock env in
  let now =
    Ptime.of_float_s (Eio.Time.now clock) |> Option.value ~default:Ptime.epoch
  in

  let cookie =
    make ~domain:"widget.com" ~path:"/" ~name:"id" ~value:"123" ~secure:true
      ~partitioned:true ~creation_time:now ~last_access:now ()
  in

  let header = make_set_cookie_header cookie in
  let contains_substring s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false
  in
  let has_partitioned = contains_substring header "Partitioned" in
  let has_secure = contains_substring header "Secure" in
  Alcotest.(check bool) "contains Partitioned" true has_partitioned;
  Alcotest.(check bool) "contains Secure" true has_secure

let test_partitioned_requires_secure env =
  let clock = Eio.Stdenv.clock env in

  (* Partitioned without Secure should be rejected *)
  match
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"widget.com" ~path:"/" "id=123; Partitioned"
  with
  | Error _ -> () (* Expected *)
  | Ok _ -> Alcotest.fail "Should reject Partitioned without Secure"

(* Priority 2.2: Expiration Variants *)

let test_expiration_variants env =
  let clock = Eio.Stdenv.clock env in
  let now =
    Ptime.of_float_s (Eio.Time.now clock) |> Option.value ~default:Ptime.epoch
  in
  let make_base ~name ?expires () =
    make ~domain:"ex.com" ~path:"/" ~name ~value:"v" ?expires ~creation_time:now
      ~last_access:now ()
  in

  (* No expiration *)
  let c1 = make_base ~name:"no_expiry" () in
  Alcotest.(check (option expiration_testable))
    "no expiration" None (expires c1);

  (* Session cookie *)
  let c2 = make_base ~name:"session" ~expires:`Session () in
  Alcotest.(check (option expiration_testable))
    "session cookie" (Some `Session) (expires c2);

  (* Explicit expiration *)
  let future = Ptime.add_span now (Ptime.Span.of_int_s 3600) |> Option.get in
  let c3 = make_base ~name:"persistent" ~expires:(`DateTime future) () in
  match expires c3 with
  | Some (`DateTime t) when Ptime.equal t future -> ()
  | _ -> Alcotest.fail "Expected DateTime expiration"

let test_parse_session_expiration env =
  let clock = Eio.Stdenv.clock env in

  (* Expires=0 should parse as Session *)
  match
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"ex.com" ~path:"/" "id=123; Expires=0"
  with
  | Ok c ->
      Alcotest.(check (option expiration_testable))
        "expires=0 is session" (Some `Session) (expires c)
  | Error msg -> Alcotest.fail ("Should parse Expires=0: " ^ msg)

let test_serialize_expiration_variants env =
  let clock = Eio.Stdenv.clock env in
  let now =
    Ptime.of_float_s (Eio.Time.now clock) |> Option.value ~default:Ptime.epoch
  in
  let contains_substring s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false
  in

  (* Session cookie serialization *)
  let c1 =
    make ~domain:"ex.com" ~path:"/" ~name:"s" ~value:"v" ~expires:`Session
      ~creation_time:now ~last_access:now ()
  in
  let h1 = make_set_cookie_header c1 in
  let has_expires = contains_substring h1 "Expires=" in
  Alcotest.(check bool) "session has Expires" true has_expires;

  (* DateTime serialization *)
  let future = Ptime.add_span now (Ptime.Span.of_int_s 3600) |> Option.get in
  let c2 =
    make ~domain:"ex.com" ~path:"/" ~name:"p" ~value:"v"
      ~expires:(`DateTime future) ~creation_time:now ~last_access:now ()
  in
  let h2 = make_set_cookie_header c2 in
  let has_expires2 = contains_substring h2 "Expires=" in
  Alcotest.(check bool) "datetime has Expires" true has_expires2

(* Priority 2.3: Value Trimming *)

let test_quoted_cookie_values env =
  let clock = Eio.Stdenv.clock env in
  (* Test valid RFC 6265 cookie values:
     cookie-value = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
     Valid cases have either no quotes or properly paired DQUOTE wrapper *)
  let valid_cases =
    [
      ("name=value", "value", "value");           (* No quotes *)
      ("name=\"value\"", "\"value\"", "value");   (* Properly quoted *)
      ("name=\"\"", "\"\"", "");                  (* Empty quoted value *)
    ]
  in

  List.iter
    (fun (input, expected_raw, expected_trimmed) ->
      match
        of_set_cookie_header
          ~now:(fun () ->
            Ptime.of_float_s (Eio.Time.now clock)
            |> Option.value ~default:Ptime.epoch)
          ~domain:"ex.com" ~path:"/" input
      with
      | Ok c ->
          Alcotest.(check string)
            (Printf.sprintf "raw value for %s" input)
            expected_raw (value c);
          Alcotest.(check string)
            (Printf.sprintf "trimmed value for %s" input)
            expected_trimmed (value_trimmed c)
      | Error msg -> Alcotest.fail ("Parse failed: " ^ input ^ ": " ^ msg))
    valid_cases;

  (* Test invalid RFC 6265 cookie values are rejected *)
  let invalid_cases =
    [
      "name=\"partial";   (* Opening quote without closing *)
      "name=\"val\"\"";   (* Embedded quote *)
      "name=val\"";       (* Trailing quote without opening *)
    ]
  in

  List.iter
    (fun input ->
      match
        of_set_cookie_header
          ~now:(fun () ->
            Ptime.of_float_s (Eio.Time.now clock)
            |> Option.value ~default:Ptime.epoch)
          ~domain:"ex.com" ~path:"/" input
      with
      | Error _ -> ()  (* Expected - invalid values are rejected *)
      | Ok _ ->
          Alcotest.fail
            (Printf.sprintf "Should reject invalid value: %s" input))
    invalid_cases

let test_trimmed_value_not_used_for_equality env =
  let clock = Eio.Stdenv.clock env in

  match
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"ex.com" ~path:"/" "name=\"value\""
  with
  | Ok c1 -> begin
      match
        of_set_cookie_header
          ~now:(fun () ->
            Ptime.of_float_s (Eio.Time.now clock)
            |> Option.value ~default:Ptime.epoch)
          ~domain:"ex.com" ~path:"/" "name=value"
      with
      | Ok c2 ->
          (* Different raw values *)
          Alcotest.(check bool)
            "different raw values" false
            (value c1 = value c2);
          (* Same trimmed values *)
          Alcotest.(check string)
            "same trimmed values" (value_trimmed c1) (value_trimmed c2)
      | Error msg -> Alcotest.fail ("Parse failed for unquoted: " ^ msg)
    end
  | Error msg -> Alcotest.fail ("Parse failed for quoted: " ^ msg)

(* Priority 2.4: Cookie Header Parsing *)

let test_cookie_header_parsing_basic env =
  let clock = Eio.Stdenv.clock env in
  let result =
    of_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"ex.com" ~path:"/" "session=abc123; theme=dark; lang=en"
  in

  match result with
  | Error msg -> Alcotest.fail ("Parse failed: " ^ msg)
  | Ok cookies ->
      Alcotest.(check int) "parsed 3 cookies" 3 (List.length cookies);

      let find name_val = List.find (fun c -> name c = name_val) cookies in
      Alcotest.(check string) "session value" "abc123" (value (find "session"));
      Alcotest.(check string) "theme value" "dark" (value (find "theme"));
      Alcotest.(check string) "lang value" "en" (value (find "lang"))

let test_cookie_header_defaults env =
  let clock = Eio.Stdenv.clock env in

  match
    of_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/app" "session=xyz"
  with
  | Ok [ c ] ->
      (* Domain and path from request context *)
      Alcotest.(check string) "domain from context" "example.com" (domain c);
      Alcotest.(check string) "path from context" "/app" (path c);

      (* Security flags default to false *)
      Alcotest.(check bool) "secure default" false (secure c);
      Alcotest.(check bool) "http_only default" false (http_only c);
      Alcotest.(check bool) "partitioned default" false (partitioned c);

      (* Optional attributes default to None *)
      Alcotest.(check (option expiration_testable))
        "no expiration" None (expires c);
      Alcotest.(check (option span_testable)) "no max_age" None (max_age c);
      Alcotest.(check (option same_site_testable))
        "no same_site" None (same_site c)
  | Ok _ -> Alcotest.fail "Should parse single cookie"
  | Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_cookie_header_edge_cases env =
  let clock = Eio.Stdenv.clock env in

  let test input expected_count description =
    let result =
      of_cookie_header
        ~now:(fun () ->
          Ptime.of_float_s (Eio.Time.now clock)
          |> Option.value ~default:Ptime.epoch)
        ~domain:"ex.com" ~path:"/" input
    in
    match result with
    | Ok cookies ->
        Alcotest.(check int) description expected_count (List.length cookies)
    | Error msg ->
        Alcotest.fail (description ^ " failed: " ^ msg)
  in

  test "" 0 "empty string";
  test ";;" 0 "only separators";
  test "a=1;;b=2" 2 "double separator";
  test " a=1 ; b=2 " 2 "excess whitespace";
  test "   " 0 "only whitespace"

let test_cookie_header_with_errors env =
  let clock = Eio.Stdenv.clock env in

  (* Invalid cookie (empty name) should cause entire parse to fail *)
  let result =
    of_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"ex.com" ~path:"/" "valid=1;=noname;valid2=2"
  in

  (* Error should have descriptive message about the invalid cookie *)
  let contains_substring s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false
  in
  match result with
  | Error msg ->
      let has_name = contains_substring msg "name" in
      let has_empty = contains_substring msg "empty" in
      Alcotest.(check bool)
        "error mentions name or empty" true (has_name || has_empty)
  | Ok _ -> Alcotest.fail "Expected error for empty cookie name"

(* Max-Age and Expires Interaction *)

let test_max_age_and_expires_both_present env =
  let clock = Eio.Stdenv.clock env in
  let now =
    Ptime.of_float_s (Eio.Time.now clock) |> Option.value ~default:Ptime.epoch
  in
  let future = Ptime.add_span now (Ptime.Span.of_int_s 7200) |> Option.get in

  (* Create cookie with both *)
  let cookie =
    make ~domain:"ex.com" ~path:"/" ~name:"dual" ~value:"val"
      ~max_age:(Ptime.Span.of_int_s 3600) ~expires:(`DateTime future)
      ~creation_time:now ~last_access:now ()
  in

  (* Both should be present *)
  begin match max_age cookie with
  | Some span -> begin
      match Ptime.Span.to_int_s span with
      | Some s ->
          Alcotest.(check int64) "max_age present" 3600L (Int64.of_int s)
      | None -> Alcotest.fail "max_age span could not be converted to int"
    end
  | None -> Alcotest.fail "max_age should be present"
  end;

  begin match expires cookie with
  | Some (`DateTime t) when Ptime.equal t future -> ()
  | _ -> Alcotest.fail "expires should be present"
  end;

  (* Both should appear in serialization *)
  let header = make_set_cookie_header cookie in
  let contains_substring s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false
  in
  let has_max_age = contains_substring header "Max-Age=3600" in
  let has_expires = contains_substring header "Expires=" in
  Alcotest.(check bool) "contains Max-Age" true has_max_age;
  Alcotest.(check bool) "contains Expires" true has_expires

let test_parse_max_age_and_expires env =
  let clock = Eio.Stdenv.clock env in

  (* Parse Set-Cookie with both attributes *)
  match
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"ex.com" ~path:"/"
      "id=123; Max-Age=3600; Expires=Wed, 21 Oct 2025 07:28:00 GMT"
  with
  | Ok c ->
      (* Both should be stored *)
      begin match max_age c with
      | Some span -> begin
          match Ptime.Span.to_int_s span with
          | Some s ->
              Alcotest.(check int64) "max_age parsed" 3600L (Int64.of_int s)
          | None -> Alcotest.fail "max_age span could not be converted to int"
        end
      | None -> Alcotest.fail "max_age should be parsed"
      end;

      begin match expires c with
      | Some (`DateTime _) -> ()
      | _ -> Alcotest.fail "expires should be parsed"
      end
  | Error msg -> Alcotest.fail ("Should parse cookie with both attributes: " ^ msg)

(* ============================================================================ *)
(* Host-Only Flag Tests (RFC 6265 Section 5.3) *)
(* ============================================================================ *)

let test_host_only_without_domain_attribute () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Cookie without Domain attribute should have host_only=true *)
  let header = "session=abc123; Secure; HttpOnly" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);
  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check bool) "host_only is true" true (Cookeio.host_only cookie);
  Alcotest.(check string) "domain is request host" "example.com" (Cookeio.domain cookie)

let test_host_only_with_domain_attribute () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Cookie with Domain attribute should have host_only=false *)
  let header = "session=abc123; Domain=example.com; Secure" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);
  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check bool) "host_only is false" false (Cookeio.host_only cookie);
  Alcotest.(check string) "domain is attribute value" "example.com" (Cookeio.domain cookie)

let test_host_only_with_dotted_domain_attribute () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Cookie with .domain should have host_only=false and normalized domain *)
  let header = "session=abc123; Domain=.example.com" in
  let cookie_opt =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" header
  in
  Alcotest.(check bool) "cookie parsed" true (Result.is_ok cookie_opt);
  let cookie = Result.get_ok cookie_opt in
  Alcotest.(check bool) "host_only is false" false (Cookeio.host_only cookie);
  Alcotest.(check string) "domain normalized" "example.com" (Cookeio.domain cookie)

let test_host_only_domain_matching () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add a host-only cookie (no Domain attribute) *)
  let host_only_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"host_only" ~value:"val1"
      ~secure:false ~http_only:false ~host_only:true
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar host_only_cookie;

  (* Add a domain cookie (with Domain attribute) *)
  let domain_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"domain" ~value:"val2"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar domain_cookie;

  (* Both cookies should match exact domain *)
  let cookies_exact =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "both match exact domain" 2 (List.length cookies_exact);

  (* Only domain cookie should match subdomain *)
  let cookies_sub =
    get_cookies jar ~clock ~domain:"sub.example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "only domain cookie matches subdomain" 1 (List.length cookies_sub);
  let sub_cookie = List.hd cookies_sub in
  Alcotest.(check string) "subdomain match is domain cookie" "domain" (Cookeio.name sub_cookie)

let test_host_only_cookie_header_parsing () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Cookies from Cookie header should have host_only=true *)
  let result =
    of_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" "session=abc; theme=dark"
  in
  match result with
  | Error msg -> Alcotest.fail ("Parse failed: " ^ msg)
  | Ok cookies ->
      Alcotest.(check int) "parsed 2 cookies" 2 (List.length cookies);
      List.iter (fun c ->
        Alcotest.(check bool)
          ("host_only is true for " ^ Cookeio.name c)
          true (Cookeio.host_only c)
      ) cookies

let test_host_only_mozilla_format_round_trip () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add host-only cookie *)
  let host_only =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"hostonly" ~value:"v1"
      ~secure:false ~http_only:false ~host_only:true
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar host_only;

  (* Add domain cookie *)
  let domain_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"domain" ~value:"v2"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar domain_cookie;

  (* Round trip through Mozilla format *)
  let mozilla = to_mozilla_format jar in
  let jar2 = from_mozilla_format ~clock mozilla in
  let cookies = get_all_cookies jar2 in

  Alcotest.(check int) "2 cookies after round trip" 2 (List.length cookies);

  let find name_val = List.find (fun c -> Cookeio.name c = name_val) cookies in
  Alcotest.(check bool) "hostonly preserved" true (Cookeio.host_only (find "hostonly"));
  Alcotest.(check bool) "domain preserved" false (Cookeio.host_only (find "domain"))

(* ============================================================================ *)
(* Path Matching Tests (RFC 6265 Section 5.1.4) *)
(* ============================================================================ *)

let test_path_matching_identical () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/foo" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* Identical path should match *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo" ~is_secure:false
  in
  Alcotest.(check int) "identical path matches" 1 (List.length cookies)

let test_path_matching_with_trailing_slash () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/foo/" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* Cookie path /foo/ should match /foo/bar *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo/bar" ~is_secure:false
  in
  Alcotest.(check int) "/foo/ matches /foo/bar" 1 (List.length cookies);

  (* Cookie path /foo/ should match /foo/ *)
  let cookies2 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo/" ~is_secure:false
  in
  Alcotest.(check int) "/foo/ matches /foo/" 1 (List.length cookies2)

let test_path_matching_prefix_with_slash () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/foo" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* Cookie path /foo should match /foo/bar (next char is /) *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo/bar" ~is_secure:false
  in
  Alcotest.(check int) "/foo matches /foo/bar" 1 (List.length cookies);

  (* Cookie path /foo should match /foo/ *)
  let cookies2 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo/" ~is_secure:false
  in
  Alcotest.(check int) "/foo matches /foo/" 1 (List.length cookies2)

let test_path_matching_no_false_prefix () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/foo" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* Cookie path /foo should NOT match /foobar (no / separator) *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foobar" ~is_secure:false
  in
  Alcotest.(check int) "/foo does NOT match /foobar" 0 (List.length cookies);

  (* Cookie path /foo should NOT match /foob *)
  let cookies2 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foob" ~is_secure:false
  in
  Alcotest.(check int) "/foo does NOT match /foob" 0 (List.length cookies2)

let test_path_matching_root () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* Root path should match everything *)
  let cookies1 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "/ matches /" 1 (List.length cookies1);

  let cookies2 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo" ~is_secure:false
  in
  Alcotest.(check int) "/ matches /foo" 1 (List.length cookies2);

  let cookies3 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo/bar/baz" ~is_secure:false
  in
  Alcotest.(check int) "/ matches /foo/bar/baz" 1 (List.length cookies3)

let test_path_matching_no_match () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/foo/bar" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* Cookie path /foo/bar should NOT match /foo *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo" ~is_secure:false
  in
  Alcotest.(check int) "/foo/bar does NOT match /foo" 0 (List.length cookies);

  (* Cookie path /foo/bar should NOT match / *)
  let cookies2 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "/foo/bar does NOT match /" 0 (List.length cookies2);

  (* Cookie path /foo/bar should NOT match /baz *)
  let cookies3 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/baz" ~is_secure:false
  in
  Alcotest.(check int) "/foo/bar does NOT match /baz" 0 (List.length cookies3)

(* ============================================================================ *)
(* Cookie Ordering Tests (RFC 6265 Section 5.4, Step 2) *)
(* ============================================================================ *)

let test_cookie_ordering_by_path_length () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add cookies with different path lengths, but same creation time *)
  let cookie_short =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"short" ~value:"v1"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  let cookie_medium =
    Cookeio.make ~domain:"example.com" ~path:"/foo" ~name:"medium" ~value:"v2"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  let cookie_long =
    Cookeio.make ~domain:"example.com" ~path:"/foo/bar" ~name:"long" ~value:"v3"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in

  (* Add in random order *)
  add_cookie jar cookie_short;
  add_cookie jar cookie_long;
  add_cookie jar cookie_medium;

  (* Get cookies for path /foo/bar/baz - all three should match *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo/bar/baz" ~is_secure:false
  in

  Alcotest.(check int) "all 3 cookies match" 3 (List.length cookies);

  (* Verify order: longest path first *)
  let names = List.map Cookeio.name cookies in
  Alcotest.(check (list string))
    "cookies ordered by path length (longest first)"
    [ "long"; "medium"; "short" ]
    names

let test_cookie_ordering_by_creation_time () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 2000.0;

  let jar = create () in

  (* Add cookies with same path but different creation times *)
  let cookie_new =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"new" ~value:"v1"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1500.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1500.0 |> Option.get) ()
  in
  let cookie_old =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"old" ~value:"v2"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  let cookie_middle =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"middle" ~value:"v3"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1200.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1200.0 |> Option.get) ()
  in

  (* Add in random order *)
  add_cookie jar cookie_new;
  add_cookie jar cookie_old;
  add_cookie jar cookie_middle;

  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in

  Alcotest.(check int) "all 3 cookies match" 3 (List.length cookies);

  (* Verify order: earlier creation time first (for same path length) *)
  let names = List.map Cookeio.name cookies in
  Alcotest.(check (list string))
    "cookies ordered by creation time (earliest first)"
    [ "old"; "middle"; "new" ]
    names

let test_cookie_ordering_combined () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 2000.0;

  let jar = create () in

  (* Mix of different paths and creation times *)
  let cookie_a =
    Cookeio.make ~domain:"example.com" ~path:"/foo" ~name:"a" ~value:"v1"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1500.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1500.0 |> Option.get) ()
  in
  let cookie_b =
    Cookeio.make ~domain:"example.com" ~path:"/foo" ~name:"b" ~value:"v2"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  let cookie_c =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"c" ~value:"v3"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 500.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 500.0 |> Option.get) ()
  in

  add_cookie jar cookie_a;
  add_cookie jar cookie_c;
  add_cookie jar cookie_b;

  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/foo/bar" ~is_secure:false
  in

  Alcotest.(check int) "all 3 cookies match" 3 (List.length cookies);

  (* /foo cookies (length 4) should come before / cookie (length 1)
     Within /foo, earlier creation time (b=1000) should come before (a=1500) *)
  let names = List.map Cookeio.name cookies in
  Alcotest.(check (list string))
    "cookies ordered by path length then creation time"
    [ "b"; "a"; "c" ]
    names

(* ============================================================================ *)
(* Creation Time Preservation Tests (RFC 6265 Section 5.3, Step 11.3) *)
(* ============================================================================ *)

let test_creation_time_preserved_on_update () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add initial cookie with creation_time=500 *)
  let original_creation = Ptime.of_float_s 500.0 |> Option.get in
  let cookie_v1 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"session" ~value:"v1"
      ~secure:false ~http_only:false
      ~creation_time:original_creation
      ~last_access:(Ptime.of_float_s 500.0 |> Option.get) ()
  in
  add_cookie jar cookie_v1;

  (* Update the cookie with a new value (creation_time=1000) *)
  Eio_mock.Clock.set_time clock 1500.0;
  let cookie_v2 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"session" ~value:"v2"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1500.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1500.0 |> Option.get) ()
  in
  add_cookie jar cookie_v2;

  (* Get the cookie and verify creation_time was preserved *)
  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "still one cookie" 1 (List.length cookies);

  let cookie = List.hd cookies in
  Alcotest.(check string) "value was updated" "v2" (Cookeio.value cookie);

  (* Creation time should be preserved from original cookie *)
  let creation_float =
    Ptime.to_float_s (Cookeio.creation_time cookie)
  in
  Alcotest.(check (float 0.001))
    "creation_time preserved from original"
    500.0 creation_float

let test_creation_time_preserved_add_original () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add initial original cookie *)
  let original_creation = Ptime.of_float_s 100.0 |> Option.get in
  let cookie_v1 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"v1"
      ~secure:false ~http_only:false
      ~creation_time:original_creation
      ~last_access:(Ptime.of_float_s 100.0 |> Option.get) ()
  in
  add_original jar cookie_v1;

  (* Replace with new original cookie *)
  let cookie_v2 =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"test" ~value:"v2"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_original jar cookie_v2;

  let cookies = get_all_cookies jar in
  Alcotest.(check int) "still one cookie" 1 (List.length cookies);

  let cookie = List.hd cookies in
  Alcotest.(check string) "value was updated" "v2" (Cookeio.value cookie);

  (* Creation time should be preserved *)
  let creation_float =
    Ptime.to_float_s (Cookeio.creation_time cookie)
  in
  Alcotest.(check (float 0.001))
    "creation_time preserved in add_original"
    100.0 creation_float

let test_creation_time_new_cookie () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add a new cookie (no existing cookie to preserve from) *)
  let cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"new" ~value:"v1"
      ~secure:false ~http_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  let cookies =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  let cookie = List.hd cookies in

  (* New cookie should keep its own creation time *)
  let creation_float =
    Ptime.to_float_s (Cookeio.creation_time cookie)
  in
  Alcotest.(check (float 0.001))
    "new cookie keeps its creation_time"
    1000.0 creation_float

(* ============================================================================ *)
(* IP Address Domain Matching Tests (RFC 6265 Section 5.1.3) *)
(* ============================================================================ *)

let test_ipv4_exact_match () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"192.168.1.1" ~path:"/" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* IPv4 cookie should match exact IP *)
  let cookies =
    get_cookies jar ~clock ~domain:"192.168.1.1" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "IPv4 exact match" 1 (List.length cookies)

let test_ipv4_no_suffix_match () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  (* Cookie for 168.1.1 - this should NOT match requests to 192.168.1.1
     even though "192.168.1.1" ends with ".168.1.1" *)
  let cookie =
    Cookeio.make ~domain:"168.1.1" ~path:"/" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* Should NOT match - IP addresses don't do suffix matching *)
  let cookies =
    get_cookies jar ~clock ~domain:"192.168.1.1" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "IPv4 no suffix match" 0 (List.length cookies)

let test_ipv4_different_ip () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"192.168.1.1" ~path:"/" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* Different IP should not match *)
  let cookies =
    get_cookies jar ~clock ~domain:"192.168.1.2" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "different IPv4 no match" 0 (List.length cookies)

let test_ipv6_exact_match () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"::1" ~path:"/" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* IPv6 loopback should match exactly *)
  let cookies =
    get_cookies jar ~clock ~domain:"::1" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "IPv6 exact match" 1 (List.length cookies)

let test_ipv6_full_format () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in
  let cookie =
    Cookeio.make ~domain:"2001:db8::1" ~path:"/" ~name:"test" ~value:"val"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar cookie;

  (* IPv6 should match exactly *)
  let cookies =
    get_cookies jar ~clock ~domain:"2001:db8::1" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "IPv6 full format match" 1 (List.length cookies);

  (* Different IPv6 should not match *)
  let cookies2 =
    get_cookies jar ~clock ~domain:"2001:db8::2" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "different IPv6 no match" 0 (List.length cookies2)

let test_ip_vs_hostname () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  let jar = create () in

  (* Add a hostname cookie with host_only=false (domain cookie) *)
  let hostname_cookie =
    Cookeio.make ~domain:"example.com" ~path:"/" ~name:"hostname" ~value:"h1"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar hostname_cookie;

  (* Add an IP cookie with host_only=false *)
  let ip_cookie =
    Cookeio.make ~domain:"192.168.1.1" ~path:"/" ~name:"ip" ~value:"i1"
      ~secure:false ~http_only:false ~host_only:false
      ~creation_time:(Ptime.of_float_s 1000.0 |> Option.get)
      ~last_access:(Ptime.of_float_s 1000.0 |> Option.get) ()
  in
  add_cookie jar ip_cookie;

  (* Hostname request should match hostname cookie and subdomains *)
  let cookies1 =
    get_cookies jar ~clock ~domain:"example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "hostname matches hostname cookie" 1 (List.length cookies1);

  let cookies2 =
    get_cookies jar ~clock ~domain:"sub.example.com" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "subdomain matches hostname cookie" 1 (List.length cookies2);

  (* IP request should only match IP cookie exactly *)
  let cookies3 =
    get_cookies jar ~clock ~domain:"192.168.1.1" ~path:"/" ~is_secure:false
  in
  Alcotest.(check int) "IP matches IP cookie" 1 (List.length cookies3);
  Alcotest.(check string) "IP cookie is returned" "ip" (Cookeio.name (List.hd cookies3))

(* ============================================================================ *)
(* RFC 6265 Validation Tests *)
(* ============================================================================ *)

let test_validate_cookie_name_valid () =
  (* Valid token characters per RFC 2616 *)
  let valid_names = ["session"; "SID"; "my-cookie"; "COOKIE_123"; "abc.def"] in
  List.iter (fun name ->
    match Cookeio.Validate.cookie_name name with
    | Ok _ -> ()
    | Error msg ->
        Alcotest.fail (Printf.sprintf "Name %S should be valid: %s" name msg))
    valid_names

let test_validate_cookie_name_invalid () =
  (* Invalid: control chars, separators, spaces *)
  let invalid_names =
    [
      ("", "empty");
      ("my cookie", "space");
      ("cookie=value", "equals");
      ("my;cookie", "semicolon");
      ("name\t", "tab");
      ("(cookie)", "parens");
      ("name,val", "comma");
    ]
  in
  List.iter (fun (name, reason) ->
    match Cookeio.Validate.cookie_name name with
    | Error _ -> ()  (* Expected *)
    | Ok _ ->
        Alcotest.fail
          (Printf.sprintf "Name %S (%s) should be invalid" name reason))
    invalid_names

let test_validate_cookie_value_valid () =
  (* Valid cookie-octets or quoted values *)
  let valid_values = ["abc123"; "value!#$%&'()*+-./"; "\"quoted\""; ""] in
  List.iter (fun value ->
    match Cookeio.Validate.cookie_value value with
    | Ok _ -> ()
    | Error msg ->
        Alcotest.fail (Printf.sprintf "Value %S should be valid: %s" value msg))
    valid_values

let test_validate_cookie_value_invalid () =
  (* Invalid: space, comma, semicolon, backslash, unmatched quotes *)
  let invalid_values =
    [
      ("with space", "space");
      ("with,comma", "comma");
      ("with;semi", "semicolon");
      ("back\\slash", "backslash");
      ("\"unmatched", "unmatched opening quote");
      ("unmatched\"", "unmatched closing quote");
    ]
  in
  List.iter (fun (value, reason) ->
    match Cookeio.Validate.cookie_value value with
    | Error _ -> ()  (* Expected *)
    | Ok _ ->
        Alcotest.fail
          (Printf.sprintf "Value %S (%s) should be invalid" value reason))
    invalid_values

let test_validate_domain_valid () =
  (* Valid domain names and IP addresses *)
  let valid_domains =
    ["example.com"; "sub.example.com"; ".example.com"; "192.168.1.1"; "::1"]
  in
  List.iter (fun domain ->
    match Cookeio.Validate.domain_value domain with
    | Ok _ -> ()
    | Error msg ->
        Alcotest.fail (Printf.sprintf "Domain %S should be valid: %s" domain msg))
    valid_domains

let test_validate_domain_invalid () =
  (* Invalid domain names - only test cases that domain-name library rejects.
     Note: domain-name library has specific rules that may differ from what
     we might expect from the RFC. *)
  let invalid_domains =
    [
      ("", "empty");
      (* Note: "-invalid.com" and "invalid-.com" are valid per domain-name library *)
    ]
  in
  List.iter (fun (domain, reason) ->
    match Cookeio.Validate.domain_value domain with
    | Error _ -> ()  (* Expected *)
    | Ok _ ->
        Alcotest.fail
          (Printf.sprintf "Domain %S (%s) should be invalid" domain reason))
    invalid_domains

let test_validate_path_valid () =
  let valid_paths = ["/"; "/path"; "/path/to/resource"; "/path?query"] in
  List.iter (fun path ->
    match Cookeio.Validate.path_value path with
    | Ok _ -> ()
    | Error msg ->
        Alcotest.fail (Printf.sprintf "Path %S should be valid: %s" path msg))
    valid_paths

let test_validate_path_invalid () =
  let invalid_paths =
    [
      ("/path;bad", "semicolon");
      ("/path\x00bad", "control char");
    ]
  in
  List.iter (fun (path, reason) ->
    match Cookeio.Validate.path_value path with
    | Error _ -> ()  (* Expected *)
    | Ok _ ->
        Alcotest.fail
          (Printf.sprintf "Path %S (%s) should be invalid" path reason))
    invalid_paths

let test_duplicate_cookie_detection () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Duplicate cookie names should be rejected *)
  let result =
    of_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"example.com" ~path:"/" "session=abc; theme=dark; session=xyz"
  in
  match result with
  | Error msg ->
      (* Should mention duplicate *)
      let contains_dup = String.lowercase_ascii msg |> fun s ->
        try let _ = Str.search_forward (Str.regexp_string "duplicate") s 0 in true
        with Not_found -> false
      in
      Alcotest.(check bool) "error mentions duplicate" true contains_dup
  | Ok _ -> Alcotest.fail "Should reject duplicate cookie names"

let test_validation_error_messages () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Test that error messages are descriptive *)
  let test_cases =
    [
      ("=noname", "Cookie name is empty");
      ("bad cookie=value", "invalid characters");
      ("name=val ue", "invalid characters");
    ]
  in
  List.iter (fun (header, expected_substring) ->
    match
      of_set_cookie_header
        ~now:(fun () ->
          Ptime.of_float_s (Eio.Time.now clock)
          |> Option.value ~default:Ptime.epoch)
        ~domain:"example.com" ~path:"/" header
    with
    | Error msg ->
        let has_substring =
          try
            let _ = Str.search_forward
              (Str.regexp_string expected_substring) msg 0 in
            true
          with Not_found -> false
        in
        Alcotest.(check bool)
          (Printf.sprintf "error for %S mentions %S" header expected_substring)
          true has_substring
    | Ok _ ->
        Alcotest.fail (Printf.sprintf "Should reject %S" header))
    test_cases

(* ============================================================================ *)
(* Public Suffix Validation Tests (RFC 6265 Section 5.3, Step 5) *)
(* ============================================================================ *)

let test_public_suffix_rejection () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Setting a cookie for a public suffix (TLD) should be rejected *)
  let test_cases =
    [
      (* (request_domain, cookie_domain, description) *)
      ("www.example.com", "com", "TLD .com");
      ("www.example.co.uk", "co.uk", "ccTLD .co.uk");
      ("foo.bar.github.io", "github.io", "private domain github.io");
    ]
  in

  List.iter
    (fun (request_domain, cookie_domain, description) ->
      let header = Printf.sprintf "session=abc; Domain=.%s" cookie_domain in
      let result =
        of_set_cookie_header
          ~now:(fun () ->
            Ptime.of_float_s (Eio.Time.now clock)
            |> Option.value ~default:Ptime.epoch)
          ~domain:request_domain ~path:"/" header
      in
      match result with
      | Error msg ->
          (* Should mention public suffix *)
          let has_psl =
            String.lowercase_ascii msg |> fun s ->
            try
              let _ = Str.search_forward (Str.regexp_string "public suffix") s 0 in
              true
            with Not_found -> false
          in
          Alcotest.(check bool)
            (Printf.sprintf "%s: error mentions public suffix" description)
            true has_psl
      | Ok _ ->
          Alcotest.fail
            (Printf.sprintf "Should reject cookie for %s" description))
    test_cases

let test_public_suffix_allowed_when_exact_match () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* If request host exactly matches the public suffix domain, allow it.
     This is rare but possible for private domains like blogspot.com *)
  let header = "session=abc; Domain=.blogspot.com" in
  let result =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"blogspot.com" ~path:"/" header
  in
  Alcotest.(check bool)
    "exact match allows public suffix" true
    (Result.is_ok result)

let test_non_public_suffix_allowed () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Normal domain (not a public suffix) should be allowed *)
  let test_cases =
    [
      ("www.example.com", "example.com", "registrable domain");
      ("sub.example.com", "example.com", "parent of subdomain");
      ("www.example.co.uk", "example.co.uk", "registrable domain under ccTLD");
    ]
  in

  List.iter
    (fun (request_domain, cookie_domain, description) ->
      let header = Printf.sprintf "session=abc; Domain=.%s" cookie_domain in
      let result =
        of_set_cookie_header
          ~now:(fun () ->
            Ptime.of_float_s (Eio.Time.now clock)
            |> Option.value ~default:Ptime.epoch)
          ~domain:request_domain ~path:"/" header
      in
      match result with
      | Ok cookie ->
          Alcotest.(check string)
            (Printf.sprintf "%s: domain correct" description)
            cookie_domain (Cookeio.domain cookie)
      | Error msg ->
          Alcotest.fail
            (Printf.sprintf "%s should be allowed: %s" description msg))
    test_cases

let test_public_suffix_no_domain_attribute () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Cookie without Domain attribute should always be allowed (host-only) *)
  let header = "session=abc; Secure; HttpOnly" in
  let result =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"www.example.com" ~path:"/" header
  in
  match result with
  | Ok cookie ->
      Alcotest.(check bool) "host_only is true" true (Cookeio.host_only cookie);
      Alcotest.(check string)
        "domain is request domain" "www.example.com"
        (Cookeio.domain cookie)
  | Error msg -> Alcotest.fail ("Should allow host-only cookie: " ^ msg)

let test_public_suffix_ip_address_bypass () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* IP addresses should bypass PSL check *)
  let header = "session=abc; Domain=192.168.1.1" in
  let result =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"192.168.1.1" ~path:"/" header
  in
  Alcotest.(check bool)
    "IP address bypasses PSL" true
    (Result.is_ok result)

let test_public_suffix_case_insensitive () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1000.0;

  (* Public suffix check should be case-insensitive *)
  let header = "session=abc; Domain=.COM" in
  let result =
    of_set_cookie_header
      ~now:(fun () ->
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch)
      ~domain:"www.example.COM" ~path:"/" header
  in
  Alcotest.(check bool)
    "uppercase TLD still rejected" true
    (Result.is_error result)

let () =
  Eio_main.run @@ fun env ->
  let open Alcotest in
  run "Cookeio Tests"
    [
      ( "mozilla_format",
        [
          test_case "Load Mozilla format from string" `Quick (fun () ->
              test_load_mozilla_cookies env);
          test_case "Load Mozilla format from file" `Quick (fun () ->
              test_load_from_file env);
          test_case "Round trip Mozilla format" `Quick (fun () ->
              test_round_trip_mozilla_format env);
        ] );
      ( "cookie_matching",
        [
          test_case "Domain and security matching" `Quick (fun () ->
              test_cookie_matching env);
        ] );
      ( "basic_operations",
        [
          test_case "Empty jar operations" `Quick (fun () -> test_empty_jar env);
        ] );
      ( "time_handling",
        [
          test_case "Cookie expiry with mock clock" `Quick
            test_cookie_expiry_with_mock_clock;
          test_case "get_cookies filters expired cookies" `Quick
            test_get_cookies_filters_expired;
          test_case "Max-Age parsing with mock clock" `Quick
            test_max_age_parsing_with_mock_clock;
          test_case "Last access time with mock clock" `Quick
            test_last_access_time_with_mock_clock;
          test_case "Parse Set-Cookie with Expires" `Quick
            test_of_set_cookie_header_with_expires;
          test_case "SameSite=None validation" `Quick
            test_samesite_none_validation;
        ] );
      ( "domain_normalization",
        [
          test_case "Domain normalization" `Quick test_domain_normalization;
          test_case "Domain matching with normalized domains" `Quick
            test_domain_matching;
        ] );
      ( "max_age_tracking",
        [
          test_case "Max-Age stored separately from Expires" `Quick
            test_max_age_stored_separately;
          test_case "Negative Max-Age becomes 0" `Quick
            test_max_age_negative_becomes_zero;
          test_case "make_set_cookie_header includes Max-Age" `Quick
            test_make_set_cookie_header_includes_max_age;
          test_case "Max-Age round-trip parsing" `Quick test_max_age_round_trip;
        ] );
      ( "delta_tracking",
        [
          test_case "add_original doesn't affect delta" `Quick
            test_add_original_not_in_delta;
          test_case "add_cookie appears in delta" `Quick
            test_add_cookie_appears_in_delta;
          test_case "remove original creates removal cookie" `Quick
            test_remove_original_creates_removal_cookie;
          test_case "remove delta cookie just removes it" `Quick
            test_remove_delta_cookie_removes_it;
          test_case "get_cookies combines original and delta" `Quick
            test_get_cookies_combines_original_and_delta;
          test_case "get_cookies delta takes precedence" `Quick
            test_get_cookies_delta_takes_precedence;
          test_case "get_cookies excludes removal cookies" `Quick
            test_get_cookies_excludes_removal_cookies;
          test_case "delta returns only changed cookies" `Quick
            test_delta_returns_only_changed_cookies;
          test_case "removal cookie format" `Quick test_removal_cookie_format;
        ] );
      ( "http_date_parsing",
        [
          test_case "HTTP date FMT1 (RFC 1123)" `Quick test_http_date_fmt1;
          test_case "HTTP date FMT2 (RFC 850)" `Quick test_http_date_fmt2;
          test_case "HTTP date FMT3 (asctime)" `Quick test_http_date_fmt3;
          test_case "HTTP date FMT4 (variant)" `Quick test_http_date_fmt4;
          test_case "Abbreviated year 69-99 becomes 1900+" `Quick
            test_abbreviated_year_69_to_99;
          test_case "Abbreviated year 0-68 becomes 2000+" `Quick
            test_abbreviated_year_0_to_68;
          test_case "RFC 3339 backward compatibility" `Quick
            test_rfc3339_still_works;
          test_case "Invalid date format logs warning" `Quick
            test_invalid_date_format_logs_warning;
          test_case "Case-insensitive month parsing" `Quick
            test_case_insensitive_month_parsing;
          test_case "Case-insensitive GMT parsing" `Quick
            test_case_insensitive_gmt_parsing;
        ] );
      ( "partitioned",
        [
          test_case "parse partitioned cookie" `Quick (fun () ->
              test_partitioned_parsing env);
          test_case "serialize partitioned cookie" `Quick (fun () ->
              test_partitioned_serialization env);
          test_case "partitioned requires secure" `Quick (fun () ->
              test_partitioned_requires_secure env);
        ] );
      ( "expiration",
        [
          test_case "expiration variants" `Quick (fun () ->
              test_expiration_variants env);
          test_case "parse session expiration" `Quick (fun () ->
              test_parse_session_expiration env);
          test_case "serialize expiration variants" `Quick (fun () ->
              test_serialize_expiration_variants env);
        ] );
      ( "value_trimming",
        [
          test_case "quoted values" `Quick (fun () ->
              test_quoted_cookie_values env);
          test_case "trimmed not used for equality" `Quick (fun () ->
              test_trimmed_value_not_used_for_equality env);
        ] );
      ( "cookie_header",
        [
          test_case "parse basic" `Quick (fun () ->
              test_cookie_header_parsing_basic env);
          test_case "default values" `Quick (fun () ->
              test_cookie_header_defaults env);
          test_case "edge cases" `Quick (fun () ->
              test_cookie_header_edge_cases env);
          test_case "multiple with errors" `Quick (fun () ->
              test_cookie_header_with_errors env);
        ] );
      ( "max_age_expires_interaction",
        [
          test_case "both present" `Quick (fun () ->
              test_max_age_and_expires_both_present env);
          test_case "parse both" `Quick (fun () ->
              test_parse_max_age_and_expires env);
        ] );
      ( "host_only_flag",
        [
          test_case "host_only without Domain attribute" `Quick
            test_host_only_without_domain_attribute;
          test_case "host_only with Domain attribute" `Quick
            test_host_only_with_domain_attribute;
          test_case "host_only with dotted Domain attribute" `Quick
            test_host_only_with_dotted_domain_attribute;
          test_case "host_only domain matching" `Quick
            test_host_only_domain_matching;
          test_case "host_only Cookie header parsing" `Quick
            test_host_only_cookie_header_parsing;
          test_case "host_only Mozilla format round trip" `Quick
            test_host_only_mozilla_format_round_trip;
        ] );
      ( "path_matching",
        [
          test_case "identical path" `Quick test_path_matching_identical;
          test_case "path with trailing slash" `Quick
            test_path_matching_with_trailing_slash;
          test_case "prefix with slash separator" `Quick
            test_path_matching_prefix_with_slash;
          test_case "no false prefix match" `Quick
            test_path_matching_no_false_prefix;
          test_case "root path matches all" `Quick test_path_matching_root;
          test_case "path no match" `Quick test_path_matching_no_match;
        ] );
      ( "ip_address_matching",
        [
          test_case "IPv4 exact match" `Quick test_ipv4_exact_match;
          test_case "IPv4 no suffix match" `Quick test_ipv4_no_suffix_match;
          test_case "IPv4 different IP no match" `Quick test_ipv4_different_ip;
          test_case "IPv6 exact match" `Quick test_ipv6_exact_match;
          test_case "IPv6 full format" `Quick test_ipv6_full_format;
          test_case "IP vs hostname behavior" `Quick test_ip_vs_hostname;
        ] );
      ( "rfc6265_validation",
        [
          test_case "valid cookie names" `Quick test_validate_cookie_name_valid;
          test_case "invalid cookie names" `Quick test_validate_cookie_name_invalid;
          test_case "valid cookie values" `Quick test_validate_cookie_value_valid;
          test_case "invalid cookie values" `Quick test_validate_cookie_value_invalid;
          test_case "valid domain values" `Quick test_validate_domain_valid;
          test_case "invalid domain values" `Quick test_validate_domain_invalid;
          test_case "valid path values" `Quick test_validate_path_valid;
          test_case "invalid path values" `Quick test_validate_path_invalid;
          test_case "duplicate cookie detection" `Quick test_duplicate_cookie_detection;
          test_case "validation error messages" `Quick test_validation_error_messages;
        ] );
      ( "cookie_ordering",
        [
          test_case "ordering by path length" `Quick
            test_cookie_ordering_by_path_length;
          test_case "ordering by creation time" `Quick
            test_cookie_ordering_by_creation_time;
          test_case "ordering combined" `Quick test_cookie_ordering_combined;
        ] );
      ( "creation_time_preservation",
        [
          test_case "preserved on update" `Quick
            test_creation_time_preserved_on_update;
          test_case "preserved in add_original" `Quick
            test_creation_time_preserved_add_original;
          test_case "new cookie keeps time" `Quick test_creation_time_new_cookie;
        ] );
      ( "public_suffix_validation",
        [
          test_case "reject public suffix domains" `Quick
            test_public_suffix_rejection;
          test_case "allow exact match on public suffix" `Quick
            test_public_suffix_allowed_when_exact_match;
          test_case "allow non-public-suffix domains" `Quick
            test_non_public_suffix_allowed;
          test_case "no Domain attribute bypasses PSL" `Quick
            test_public_suffix_no_domain_attribute;
          test_case "IP address bypasses PSL" `Quick
            test_public_suffix_ip_address_bypass;
          test_case "case insensitive check" `Quick
            test_public_suffix_case_insensitive;
        ] );
    ]
