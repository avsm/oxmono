(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for the WebFinger library. *)

(** Helper module for substring check *)
module String = struct
  include String
  let is_substring ~sub s =
    let rec check i =
      if i + String.length sub > String.length s then false
      else if String.sub s i (String.length sub) = sub then true
      else check (i + 1)
    in
    check 0
end

let test_jrd_roundtrip () =
  let jrd = Webfinger.Jrd.make
    ~subject:"acct:user@example.com"
    ~aliases:["https://example.com/users/user"]
    ~properties:[
      ("http://example.com/ns/role", Some "admin");
      ("http://example.com/ns/verified", None);
    ]
    ~links:[
      Webfinger.Link.make
        ~rel:"self"
        ~type_:"application/activity+json"
        ~href:"https://example.com/users/user"
        ~titles:[("en", "User Profile"); ("und", "Profile")]
        ~properties:[("http://example.com/ns/verified", Some "true")]
        ();
      Webfinger.Link.make
        ~rel:"http://webfinger.net/rel/profile-page"
        ~type_:"text/html"
        ~href:"https://example.com/@user"
        ();
    ]
    ()
  in
  let json = Webfinger.Jrd.to_string jrd in
  match Webfinger.Jrd.of_string json with
  | Error e ->
      Alcotest.fail (Webfinger.error_to_string e)
  | Ok parsed ->
      Alcotest.(check (option string)) "subject" (Webfinger.Jrd.subject jrd) (Webfinger.Jrd.subject parsed);
      Alcotest.(check int) "aliases count" (List.length (Webfinger.Jrd.aliases jrd)) (List.length (Webfinger.Jrd.aliases parsed));
      Alcotest.(check int) "links count" (List.length (Webfinger.Jrd.links jrd)) (List.length (Webfinger.Jrd.links parsed))

let test_jrd_minimal () =
  let json = {|{}|} in
  match Webfinger.Jrd.of_string json with
  | Error e ->
      Alcotest.fail (Webfinger.error_to_string e)
  | Ok jrd ->
      Alcotest.(check (option string)) "subject" None (Webfinger.Jrd.subject jrd);
      Alcotest.(check int) "aliases" 0 (List.length (Webfinger.Jrd.aliases jrd));
      Alcotest.(check int) "properties" 0 (List.length (Webfinger.Jrd.properties jrd));
      Alcotest.(check int) "links" 0 (List.length (Webfinger.Jrd.links jrd))

let test_jrd_with_all_fields () =
  let json = {|{
    "subject": "acct:user@example.com",
    "aliases": ["https://example.com/~user", "https://example.com/users/user"],
    "properties": {
      "http://example.com/ns/role": "admin",
      "http://example.com/ns/nullable": null
    },
    "links": [
      {
        "rel": "self",
        "type": "application/activity+json",
        "href": "https://example.com/users/user",
        "titles": {
          "en": "User's ActivityPub Profile",
          "und": "Profile"
        },
        "properties": {
          "http://example.com/ns/verified": "true"
        }
      },
      {
        "rel": "http://webfinger.net/rel/profile-page",
        "type": "text/html",
        "href": "https://example.com/@user"
      }
    ]
  }|} in
  match Webfinger.Jrd.of_string json with
  | Error e ->
      Alcotest.fail (Webfinger.error_to_string e)
  | Ok jrd ->
      Alcotest.(check (option string)) "subject"
        (Some "acct:user@example.com") (Webfinger.Jrd.subject jrd);
      Alcotest.(check int) "aliases count" 2 (List.length (Webfinger.Jrd.aliases jrd));
      Alcotest.(check int) "properties count" 2 (List.length (Webfinger.Jrd.properties jrd));
      Alcotest.(check int) "links count" 2 (List.length (Webfinger.Jrd.links jrd));

      (* Check first link *)
      let link = List.hd (Webfinger.Jrd.links jrd) in
      Alcotest.(check string) "link rel" "self" (Webfinger.Link.rel link);
      Alcotest.(check (option string)) "link type" (Some "application/activity+json") (Webfinger.Link.type_ link);
      Alcotest.(check (option string)) "link href" (Some "https://example.com/users/user") (Webfinger.Link.href link);
      Alcotest.(check int) "link titles" 2 (List.length (Webfinger.Link.titles link));
      Alcotest.(check int) "link properties" 1 (List.length (Webfinger.Link.properties link))

let test_host_extraction () =
  (* acct: URIs *)
  Alcotest.(check (result string Alcotest.reject)) "acct URI"
    (Ok "example.com")
    (Webfinger.host_of_resource "acct:user@example.com");

  Alcotest.(check (result string Alcotest.reject)) "acct URI with port"
    (Ok "example.com:8080")
    (Webfinger.host_of_resource "acct:user@example.com:8080");

  (* https: URIs *)
  Alcotest.(check (result string Alcotest.reject)) "https URI"
    (Ok "example.com")
    (Webfinger.host_of_resource "https://example.com/users/user");

  (* Invalid URIs *)
  let invalid = Webfinger.host_of_resource "acct:noatsign" in
  Alcotest.(check bool) "invalid acct URI" true (Result.is_error invalid)

let test_webfinger_url () =
  let url = Webfinger.webfinger_url ~resource:"acct:user@example.com" "example.com" in
  (* Uri library doesn't percent-encode @ and : in query params - both forms are valid *)
  Alcotest.(check string) "basic URL"
    "https://example.com/.well-known/webfinger?resource=acct:user@example.com"
    url;

  let url_with_rels = Webfinger.webfinger_url
    ~resource:"acct:user@example.com"
    ~rels:["self"; "http://webfinger.net/rel/profile-page"]
    "example.com"
  in
  Alcotest.(check bool) "URL has rel params"
    true
    (String.length url_with_rels > String.length url)

let test_find_link () =
  let jrd = Webfinger.Jrd.make
    ~links:[
      Webfinger.Link.make ~rel:"self" ~href:"https://example.com/1" ();
      Webfinger.Link.make ~rel:"alternate" ~href:"https://example.com/2" ();
      Webfinger.Link.make ~rel:"self" ~href:"https://example.com/3" ();
    ]
    ()
  in
  (* find_link returns first match *)
  match Webfinger.Jrd.find_link ~rel:"self" jrd with
  | None -> Alcotest.fail "Expected to find link"
  | Some link ->
      Alcotest.(check (option string)) "href"
        (Some "https://example.com/1") (Webfinger.Link.href link);

      (* find_links returns all matches *)
      let links = Webfinger.Jrd.find_links ~rel:"self" jrd in
      Alcotest.(check int) "found all self links" 2 (List.length links);

      (* Non-existent rel *)
      Alcotest.(check (option Alcotest.reject)) "no match"
        None
        (Webfinger.Jrd.find_link ~rel:"nonexistent" jrd)

let test_link_title () =
  let link = Webfinger.Link.make
    ~rel:"self"
    ~titles:[("en", "English Title"); ("de", "German Title"); ("und", "Default")]
    ()
  in
  Alcotest.(check (option string)) "English"
    (Some "English Title")
    (Webfinger.Link.title ~lang:"en" link);

  Alcotest.(check (option string)) "German"
    (Some "German Title")
    (Webfinger.Link.title ~lang:"de" link);

  (* Falls back to "und" *)
  Alcotest.(check (option string)) "French falls back to und"
    (Some "Default")
    (Webfinger.Link.title ~lang:"fr" link);

  (* Default is "und" *)
  Alcotest.(check (option string)) "default is und"
    (Some "Default")
    (Webfinger.Link.title link)

(** {1 Acct URI Tests (RFC 7565)} *)

let test_acct_basic () =
  (* Basic parsing *)
  let acct = Webfinger.Acct.of_string_exn "acct:foobar@status.example.net" in
  Alcotest.(check string) "userpart" "foobar" (Webfinger.Acct.userpart acct);
  Alcotest.(check string) "host" "status.example.net" (Webfinger.Acct.host acct);

  (* Roundtrip *)
  let s = Webfinger.Acct.to_string acct in
  Alcotest.(check string) "roundtrip" "acct:foobar@status.example.net" s

let test_acct_make () =
  let acct = Webfinger.Acct.make ~userpart:"user" ~host:"example.com" in
  Alcotest.(check string) "userpart" "user" (Webfinger.Acct.userpart acct);
  Alcotest.(check string) "host" "example.com" (Webfinger.Acct.host acct);
  Alcotest.(check string) "to_string" "acct:user@example.com" (Webfinger.Acct.to_string acct)

let test_acct_percent_encoding () =
  (* Email address as userpart - @ must be percent-encoded per RFC 7565 Section 4 *)
  let acct = Webfinger.Acct.of_string_exn "acct:juliet%40capulet.example@shoppingsite.example" in
  Alcotest.(check string) "userpart with @" "juliet@capulet.example" (Webfinger.Acct.userpart acct);
  Alcotest.(check string) "host" "shoppingsite.example" (Webfinger.Acct.host acct);

  (* Roundtrip: the @ in the userpart should be percent-encoded *)
  let s = Webfinger.Acct.to_string acct in
  Alcotest.(check bool) "roundtrip contains %40"
    true (String.is_substring ~sub:"%40" s)

let test_acct_make_with_at () =
  (* Creating an acct with @ in userpart should auto-encode *)
  let acct = Webfinger.Acct.make ~userpart:"juliet@capulet.example" ~host:"shoppingsite.example" in
  let s = Webfinger.Acct.to_string acct in
  (* The @ in the userpart should be percent-encoded *)
  Alcotest.(check bool) "@ is encoded"
    true (String.is_substring ~sub:"%40" s);
  (* But we can still get the decoded userpart back *)
  Alcotest.(check string) "userpart preserved" "juliet@capulet.example" (Webfinger.Acct.userpart acct)

let test_acct_host_normalization () =
  (* Host should be normalized to lowercase *)
  let acct1 = Webfinger.Acct.of_string_exn "acct:user@Example.COM" in
  let acct2 = Webfinger.Acct.of_string_exn "acct:user@example.com" in
  Alcotest.(check bool) "equal after host normalization" true (Webfinger.Acct.equal acct1 acct2);
  Alcotest.(check string) "host is lowercase" "example.com" (Webfinger.Acct.host acct1)

let test_acct_equality () =
  let acct1 = Webfinger.Acct.of_string_exn "acct:User@Example.COM" in
  let acct2 = Webfinger.Acct.of_string_exn "acct:User@example.com" in
  let acct3 = Webfinger.Acct.of_string_exn "acct:user@example.com" in

  (* Same userpart, different host case - equal *)
  Alcotest.(check bool) "same user, diff host case" true (Webfinger.Acct.equal acct1 acct2);

  (* Different userpart - not equal (userpart is case-sensitive) *)
  Alcotest.(check bool) "different userpart" false (Webfinger.Acct.equal acct2 acct3)

let test_acct_invalid () =
  (* Missing scheme *)
  Alcotest.(check bool) "missing scheme"
    true (Result.is_error (Webfinger.Acct.of_string "user@example.com"));

  (* Missing @ *)
  Alcotest.(check bool) "missing @"
    true (Result.is_error (Webfinger.Acct.of_string "acct:userexample.com"));

  (* Empty userpart *)
  Alcotest.(check bool) "empty userpart"
    true (Result.is_error (Webfinger.Acct.of_string "acct:@example.com"));

  (* Empty host *)
  Alcotest.(check bool) "empty host"
    true (Result.is_error (Webfinger.Acct.of_string "acct:user@"))

let test_acct_webfinger_url () =
  let acct = Webfinger.Acct.of_string_exn "acct:user@example.com" in
  let url = Webfinger.webfinger_url_acct acct () in
  Alcotest.(check string) "webfinger URL"
    "https://example.com/.well-known/webfinger?resource=acct:user@example.com"
    url

let () =
  Alcotest.run "webfinger" [
    "jrd", [
      Alcotest.test_case "roundtrip" `Quick test_jrd_roundtrip;
      Alcotest.test_case "minimal" `Quick test_jrd_minimal;
      Alcotest.test_case "all fields" `Quick test_jrd_with_all_fields;
    ];
    "url", [
      Alcotest.test_case "host extraction" `Quick test_host_extraction;
      Alcotest.test_case "webfinger URL" `Quick test_webfinger_url;
    ];
    "accessors", [
      Alcotest.test_case "find_link" `Quick test_find_link;
      Alcotest.test_case "link_title" `Quick test_link_title;
    ];
    "acct", [
      Alcotest.test_case "basic" `Quick test_acct_basic;
      Alcotest.test_case "make" `Quick test_acct_make;
      Alcotest.test_case "percent encoding" `Quick test_acct_percent_encoding;
      Alcotest.test_case "make with @" `Quick test_acct_make_with_at;
      Alcotest.test_case "host normalization" `Quick test_acct_host_normalization;
      Alcotest.test_case "equality" `Quick test_acct_equality;
      Alcotest.test_case "invalid" `Quick test_acct_invalid;
      Alcotest.test_case "webfinger URL" `Quick test_acct_webfinger_url;
    ];
  ]
