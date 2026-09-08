(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The CardDAV client against a scripted server, so that discovery, the
   headers each request carries and the mapping of failures are checked
   without a network. *)

module Client = Carddav_eio.Client
module Data = Carddav.Data

let xml_headers =
  Http.Header.of_list [ ("content-type", "text/xml; charset=utf-8") ]

let base = "https://dav.example.com"

let path (req : Fetch.Middleware.request) =
  Fetch.Middleware.Url.path_and_query req.url

let body (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> ""
  | Fetch.String b -> b
  | Fetch.Stream _ -> Alcotest.fail "unexpected streaming request"

let meth (req : Fetch.Middleware.request) = Http.Method.to_string req.meth

let header name (req : Fetch.Middleware.request) =
  Http.Header.get req.headers name

let respond ?(status = 207) xml req =
  Fetch_mock.respond ~status ~headers:xml_headers xml req

let principal_body =
  {|<?xml version="1.0"?><D:multistatus xmlns:D="DAV:"><D:response><D:href>/dav/</D:href><D:propstat><D:prop><D:current-user-principal><D:href>/principals/alice/</D:href></D:current-user-principal></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>|}

let home_body =
  {|<?xml version="1.0"?><D:multistatus xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:carddav"><D:response><D:href>/principals/alice/</D:href><D:propstat><D:prop><C:addressbook-home-set><D:href>/addressbooks/alice/</D:href></C:addressbook-home-set></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>|}

let books_body =
  {|<?xml version="1.0"?><D:multistatus xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:carddav"><D:response><D:href>/addressbooks/alice/</D:href><D:propstat><D:prop><D:resourcetype><D:collection/></D:resourcetype></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response><D:response><D:href>/addressbooks/alice/default/</D:href><D:propstat><D:prop><D:resourcetype><D:collection/><C:addressbook/></D:resourcetype><D:displayname>Default</D:displayname><D:sync-token>http://example.com/sync/1</D:sync-token></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>|}

let vcard_text =
  "BEGIN:VCARD\r\nVERSION:4.0\r\nUID:u1\r\nFN:Alice\r\nEND:VCARD\r\n"

let query_body =
  Printf.sprintf
    {|<?xml version="1.0"?><D:multistatus xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:carddav"><D:response><D:href>/addressbooks/alice/default/u1.vcf</D:href><D:propstat><D:prop><D:getetag>"e1"</D:getetag><C:address-data>%s</C:address-data></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response><D:response><D:href>/addressbooks/alice/default/</D:href><D:status>HTTP/1.1 507 Insufficient Storage</D:status><D:error><D:number-of-matches-within-limits/></D:error></D:response></D:multistatus>|}
    vcard_text

let uid_conflict =
  {|<?xml version="1.0"?><D:error xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:carddav"><C:no-uid-conflict><D:href>/addressbooks/alice/default/other.vcf</D:href></C:no-uid-conflict></D:error>|}

let server seen req =
  seen :=
    ( meth req,
      path req,
      header "depth" req,
      header "if-match" req,
      header "if-none-match" req,
      body req )
    :: !seen;
  match (meth req, path req) with
  | "GET", "/.well-known/carddav" ->
      Fetch_mock.respond ~status:301
        ~headers:(Http.Header.of_list [ ("location", "/dav/") ])
        "" req
  | "PROPFIND", "/dav/" -> respond principal_body req
  | "PROPFIND", "/principals/alice/" -> respond home_body req
  | "PROPFIND", "/addressbooks/alice/" -> respond books_body req
  | "REPORT", "/addressbooks/alice/default/" -> respond query_body req
  | "GET", "/addressbooks/alice/default/u1.vcf" ->
      Fetch_mock.respond ~status:200
        ~headers:
          (Http.Header.of_list
             [ ("etag", "\"e1\""); ("content-type", "text/vcard") ])
        vcard_text req
  | "PUT", "/addressbooks/alice/default/u1.vcf" -> (
      match header "if-match" req with
      | Some "\"stale\"" -> Fetch_mock.respond ~status:412 "" req
      | _ ->
          Fetch_mock.respond ~status:204
            ~headers:(Http.Header.of_list [ ("etag", "\"e2\"") ])
            "" req)
  | "PUT", "/addressbooks/alice/default/dup.vcf" ->
      Fetch_mock.respond ~status:403 ~headers:xml_headers uid_conflict req
  | "DELETE", "/addressbooks/alice/default/gone.vcf" ->
      Fetch_mock.respond ~status:404 "" req
  | _ -> Fetch_mock.respond ~status:500 "unexpected" req

let with_client f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let transport = Fetch_mock.client (server seen) in
  match
    Client.connect ~sw
      ~credentials:[ Fetch.Credential.basic ~user:"alice" ~password:"x" ]
      transport
      (base ^ "/.well-known/carddav")
  with
  | Error e -> Alcotest.failf "connect: %s" (Client.error_to_string e)
  | Ok client -> f client seen

let ok what = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: %s" what (Client.error_to_string e)

let test_discovery () =
  with_client @@ fun client _ ->
  Alcotest.(check string)
    "principal"
    (base ^ "/principals/alice/")
    (Client.principal client);
  Alcotest.(check (list string))
    "home"
    [ base ^ "/addressbooks/alice/" ]
    (Client.home_sets client)

let test_addressbooks () =
  with_client @@ fun client seen ->
  let books = ok "addressbooks" (Client.addressbooks client) in
  Alcotest.(check int) "one book" 1 (List.length books);
  let book = List.hd books in
  Alcotest.(check string)
    "href"
    (base ^ "/addressbooks/alice/default/")
    book.href;
  Alcotest.(check (option string)) "name" (Some "Default") book.display_name;
  let depth =
    List.find_map
      (fun (m, p, d, _, _, _) ->
        if m = "PROPFIND" && p = "/addressbooks/alice/" then d else None)
      !seen
  in
  Alcotest.(check (option string)) "depth 1" (Some "1") depth

let test_query () =
  with_client @@ fun client seen ->
  let page =
    ok "query"
      (Client.query Data.vcard client
         (base ^ "/addressbooks/alice/default/")
         Carddav.Filter.all)
  in
  Alcotest.(check bool) "truncated" true page.truncated;
  Alcotest.(check int) "one" 1 (List.length page.entries);
  let e = List.hd page.entries in
  Alcotest.(check (option string)) "etag" (Some "\"e1\"") e.etag;
  Alcotest.(check string)
    "href absolute"
    (base ^ "/addressbooks/alice/default/u1.vcf")
    e.href;
  let sent =
    List.find_map
      (fun (m, _, _, _, _, b) -> if m = "REPORT" then Some b else None)
      !seen
  in
  match sent with
  | None -> Alcotest.fail "no REPORT"
  | Some b ->
      Alcotest.(check bool)
        "asks for vCard 4.0" true
        (let x = Result.get_ok (Httpz_dav.parse_xml b) in
         match Httpz_dav.find (Httpz_dav.dav "prop") x with
         | Some p -> (
             match Httpz_dav.find Carddav.Address_data.name p with
             | Some ad -> Httpz_dav.attr ("", "version") ad = Some "4.0"
             | None -> false)
         | None -> false)

let test_objects () =
  with_client @@ fun client seen ->
  let url = base ^ "/addressbooks/alice/default/u1.vcf" in
  let got = ok "get" (Client.get Data.vcard client url) in
  Alcotest.(check (option string)) "etag" (Some "\"e1\"") got.etag;
  Alcotest.(check (option string))
    "FN" (Some "Alice")
    (Option.map Vcard.Property.text (Vcard.find got.value "FN"));
  let etag =
    ok "put" (Client.put Data.vcard client ?etag:got.etag url got.value)
  in
  Alcotest.(check (option string)) "new etag" (Some "\"e2\"") etag;
  let if_match =
    List.find_map
      (fun (m, _, _, im, _, _) -> if m = "PUT" then im else None)
      !seen
  in
  Alcotest.(check (option string)) "If-Match sent" (Some "\"e1\"") if_match;
  (match Client.put Data.vcard client ~etag:"\"stale\"" url got.value with
  | Error (Client.Precondition_failed _) -> ()
  | _ -> Alcotest.fail "412 is not Precondition_failed");
  (match
     Client.add Data.vcard client ~name:"dup.vcf"
       (base ^ "/addressbooks/alice/default/")
       got.value
   with
  | Error (Client.Dav (403, conditions)) ->
      Alcotest.(check (option string))
        "conflicting href" (Some "/addressbooks/alice/default/other.vcf")
        (Carddav.Error.conflicting_uid conditions);
      let inm =
        List.find_map
          (fun (m, p, _, _, inm, _) ->
            if m = "PUT" && p = "/addressbooks/alice/default/dup.vcf" then inm
            else None)
          !seen
      in
      Alcotest.(check (option string)) "If-None-Match sent" (Some "*") inm
  | Error e -> Alcotest.failf "unexpected: %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "the conflict was not reported");
  match
    Client.delete client (base ^ "/addressbooks/alice/default/gone.vcf")
  with
  | Error (Client.Not_found _) -> ()
  | _ -> Alcotest.fail "404 is not Not_found"

let () =
  Alcotest.run "carddav-eio"
    [
      ( "client",
        [
          Alcotest.test_case "discovery" `Quick test_discovery;
          Alcotest.test_case "address books" `Quick test_addressbooks;
          Alcotest.test_case "query" `Quick test_query;
          Alcotest.test_case "address objects" `Quick test_objects;
        ] );
    ]
