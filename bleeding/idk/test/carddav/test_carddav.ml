(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ok name = function
  | Ok v -> v
  | Error msg -> Alcotest.failf "%s: %s" name msg

let check_bool name b = Alcotest.(check bool) name true b

(* {1 RFC 6352 Section 8 request examples} *)

let request_8_6_3 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:addressbook-query xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:carddav">
  <D:prop>
    <D:getetag/>
    <C:address-data>
      <C:prop name="VERSION"/>
      <C:prop name="UID"/>
      <C:prop name="NICKNAME"/>
      <C:prop name="EMAIL"/>
      <C:prop name="FN"/>
    </C:address-data>
  </D:prop>
  <C:filter>
    <C:prop-filter name="NICKNAME">
      <C:text-match collation="i;unicode-casemap"
                    match-type="equals"
      >me</C:text-match>
    </C:prop-filter>
  </C:filter>
</C:addressbook-query>|}

let request_8_6_4 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:addressbook-query xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:carddav">
  <D:prop>
    <D:getetag/>
    <C:address-data>
      <C:prop name="VERSION"/>
      <C:prop name="UID"/>
      <C:prop name="NICKNAME"/>
      <C:prop name="EMAIL"/>
      <C:prop name="FN"/>
    </C:address-data>
  </D:prop>
  <C:filter test="anyof">
    <C:prop-filter name="FN">
      <C:text-match collation="i;unicode-casemap"
                    match-type="contains"
      >daboo</C:text-match>
    </C:prop-filter>
    <C:prop-filter name="EMAIL">
      <C:text-match collation="i;unicode-casemap"
                    match-type="contains"
      >daboo</C:text-match>
    </C:prop-filter>
  </C:filter>
</C:addressbook-query>|}

let request_8_6_5 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:addressbook-query xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:carddav">
  <D:prop>
    <D:getetag/>
  </D:prop>
  <C:filter test="anyof">
    <C:prop-filter name="FN">
      <C:text-match collation="i;unicode-casemap"
                    match-type="contains"
      >daboo</C:text-match>
    </C:prop-filter>
  </C:filter>
  <C:limit>
    <C:nresults>2</C:nresults>
  </C:limit>
</C:addressbook-query>|}

let request_8_7_1 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:addressbook-multiget xmlns:D="DAV:"
                     xmlns:C="urn:ietf:params:xml:ns:carddav">
  <D:prop>
    <D:getetag/>
    <C:address-data>
      <C:prop name="VERSION"/>
      <C:prop name="UID"/>
      <C:prop name="NICKNAME"/>
      <C:prop name="EMAIL"/>
      <C:prop name="FN"/>
    </C:address-data>
  </D:prop>
  <D:href>/home/bernard/addressbook/vcf102.vcf</D:href>
  <D:href>/home/bernard/addressbook/vcf1.vcf</D:href>
</C:addressbook-multiget>|}

let query_of_string name s =
  let x = ok name (Httpz_dav.parse_xml s) in
  ok name (Carddav.Report.query_of_xml x)

let multiget_of_string name s =
  let x = ok name (Httpz_dav.parse_xml s) in
  ok name (Carddav.Report.multiget_of_xml x)

(* to_xml omits an attribute at its default value, RFC 6352 gives that same
   default when the attribute is absent, but a hand-written example may
   still spell the default out. So a request that round-trips is not one
   equal to the very first parse, but one whose second encode-decode pass
   is a fixed point of its first. *)
let query_roundtrips name q =
  let q2 =
    ok name (Carddav.Report.query_of_xml (Carddav.Report.query_to_xml q))
  in
  let q3 =
    ok name (Carddav.Report.query_of_xml (Carddav.Report.query_to_xml q2))
  in
  check_bool (name ^ " roundtrips") (q2 = q3)

let multiget_roundtrips name m =
  let m2 =
    ok name (Carddav.Report.multiget_of_xml (Carddav.Report.multiget_to_xml m))
  in
  let m3 =
    ok name (Carddav.Report.multiget_of_xml (Carddav.Report.multiget_to_xml m2))
  in
  check_bool (name ^ " roundtrips") (m2 = m3)

let address_data_props =
  [
    ("VERSION", false);
    ("UID", false);
    ("NICKNAME", false);
    ("EMAIL", false);
    ("FN", false);
  ]

let test_request_8_6_3 () =
  let q = query_of_string "8.6.3" request_8_6_3 in
  query_roundtrips "8.6.3" q;
  (match q.props with
  | Carddav.Report.Prop (names, Some d) ->
      check_bool "8.6.3 getetag" (names = [ Httpz_dav.Prop.getetag ]);
      check_bool "8.6.3 address-data props" (d.props = `Props address_data_props)
  | _ -> Alcotest.fail "8.6.3 props");
  check_bool "8.6.3 limit" (q.limit = None);
  check_bool "8.6.3 filter test" (q.filter.test = `Anyof && q.filter.props <> []);
  match q.filter.props with
  | [ { Carddav.Filter.prop; prop_condition } ] ->
      check_bool "8.6.3 prop" (prop = "NICKNAME");
      check_bool "8.6.3 condition"
        (prop_condition
        = `Matches
            ( `Anyof,
              [
                Carddav.Filter.text_match ~match_type:`Equals
                  ~collation:"i;unicode-casemap" "me";
              ],
              [] ))
  | _ -> Alcotest.fail "8.6.3 prop-filter"

let test_request_8_6_4 () =
  let q = query_of_string "8.6.4" request_8_6_4 in
  query_roundtrips "8.6.4" q;
  check_bool "8.6.4 filter test" (q.filter.test = `Anyof);
  check_bool "8.6.4 prop-filter count" (List.length q.filter.props = 2);
  List.iter2
    (fun expected (pf : Carddav.Filter.prop_filter) ->
      check_bool "8.6.4 prop names" (pf.prop = expected))
    [ "FN"; "EMAIL" ] q.filter.props;
  List.iter
    (fun (pf : Carddav.Filter.prop_filter) ->
      check_bool "8.6.4 condition"
        (pf.prop_condition
        = `Matches
            ( `Anyof,
              [
                Carddav.Filter.text_match ~collation:"i;unicode-casemap" "daboo";
              ],
              [] )))
    q.filter.props

let test_request_8_6_5 () =
  let q = query_of_string "8.6.5" request_8_6_5 in
  query_roundtrips "8.6.5" q;
  (match q.props with
  | Carddav.Report.Prop (names, None) ->
      check_bool "8.6.5 getetag" (names = [ Httpz_dav.Prop.getetag ])
  | _ -> Alcotest.fail "8.6.5 props");
  check_bool "8.6.5 limit" (q.limit = Some 2);
  match q.filter.props with
  | [ { Carddav.Filter.prop; prop_condition } ] ->
      check_bool "8.6.5 prop" (prop = "FN");
      check_bool "8.6.5 condition"
        (prop_condition
        = `Matches
            ( `Anyof,
              [
                Carddav.Filter.text_match ~collation:"i;unicode-casemap" "daboo";
              ],
              [] ))
  | _ -> Alcotest.fail "8.6.5 prop-filter"

let test_request_8_7_1 () =
  let m = multiget_of_string "8.7.1" request_8_7_1 in
  multiget_roundtrips "8.7.1" m;
  (match m.Carddav.Report.props with
  | Carddav.Report.Prop (names, Some d) ->
      check_bool "8.7.1 getetag" (names = [ Httpz_dav.Prop.getetag ]);
      check_bool "8.7.1 address-data props" (d.props = `Props address_data_props)
  | _ -> Alcotest.fail "8.7.1 props");
  check_bool "8.7.1 hrefs"
    (m.hrefs
    = [
        "/home/bernard/addressbook/vcf102.vcf";
        "/home/bernard/addressbook/vcf1.vcf";
      ])

(* {1 RFC 6352 Section 8 response examples} *)

let response_8_6_3 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<D:multistatus xmlns:D="DAV:"
               xmlns:C="urn:ietf:params:xml:ns:carddav">
  <D:response>
    <D:href>/home/bernard/addressbook/v102.vcf</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"23ba4d-ff11fb"</D:getetag>
        <C:address-data>BEGIN:VCARD
VERSION:3.0
NICKNAME:me
UID:34222-232@example.com
FN:Cyrus Daboo
EMAIL:daboo@example.com
END:VCARD
</C:address-data>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
</D:multistatus>|}

let response_8_6_4 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<D:multistatus xmlns:D="DAV:"
               xmlns:C="urn:ietf:params:xml:ns:carddav">
  <D:response>
    <D:href>/home/bernard/addressbook/v102.vcf</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"23ba4d-ff11fb"</D:getetag>
        <C:address-data>BEGIN:VCARD
VERSION:3.0
NICKNAME:me
UID:34222-232@example.com
FN:David Boo
EMAIL:daboo@example.com
END:VCARD
</C:address-data>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
  <D:response>
    <D:href>/home/bernard/addressbook/v104.vcf</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"23ba4d-ff11fc"</D:getetag>
        <C:address-data>BEGIN:VCARD
VERSION:3.0
NICKNAME:oliver
UID:34222-23222@example.com
FN:Oliver Daboo
EMAIL:oliver@example.com
END:VCARD
</C:address-data>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
</D:multistatus>|}

let response_8_6_5 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<D:multistatus xmlns:D="DAV:"
               xmlns:C="urn:ietf:params:xml:ns:carddav">
  <D:response>
    <D:href>/home/bernard/addressbook/</D:href>
    <D:status>HTTP/1.1 507 Insufficient Storage</D:status>
    <D:error><D:number-of-matches-within-limits/></D:error>
    <D:responsedescription xml:lang="en">
      Only two matching records were returned
    </D:responsedescription>
  </D:response>
  <D:response>
    <D:href>/home/bernard/addressbook/v102.vcf</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"23ba4d-ff11fb"</D:getetag>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
  <D:response>
    <D:href>/home/bernard/addressbook/v104.vcf</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"23ba4d-ff11fc"</D:getetag>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
</D:multistatus>|}

let response_8_7_1 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<D:multistatus xmlns:D="DAV:"
               xmlns:C="urn:ietf:params:xml:ns:carddav">
  <D:response>
    <D:href>/home/bernard/addressbook/vcf102.vcf</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"23ba4d-ff11fb"</D:getetag>
        <C:address-data>BEGIN:VCARD
VERSION:3.0
NICKNAME:me
UID:34222-232@example.com
FN:Cyrus Daboo
EMAIL:daboo@example.com
END:VCARD
</C:address-data>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
  <D:response>
    <D:href>/home/bernard/addressbook/vcf1.vcf</D:href>
    <D:status>HTTP/1.1 404 Resource not found</D:status>
  </D:response>
</D:multistatus>|}

let entry_vcard e =
  match e.Carddav.Report.data with
  | None -> Alcotest.fail "no data"
  | Some s -> ok "vcard" (Vcard.one_of_string s)

let find_prop card name = Vcard.find card name

let test_response_8_6_3 () =
  let m =
    ok "8.6.3r"
      ((fun s -> Result.bind (Httpz_dav.parse_xml s) Httpz_dav.multistatus)
         response_8_6_3)
  in
  let outcome =
    Carddav.Report.outcome_of_multistatus ~base:"/home/bernard/addressbook/" m
  in
  check_bool "8.6.3r not truncated" (not outcome.truncated);
  match outcome.entries with
  | [ e ] ->
      check_bool "8.6.3r href" (e.href = "/home/bernard/addressbook/v102.vcf");
      check_bool "8.6.3r etag" (e.etag = Some {|"23ba4d-ff11fb"|});
      let card = entry_vcard e in
      check_bool "8.6.3r FN"
        (Option.map Vcard.Property.text (find_prop card "FN")
        = Some "Cyrus Daboo");
      check_bool "8.6.3r EMAIL"
        (Option.map Vcard.Property.text (find_prop card "EMAIL")
        = Some "daboo@example.com")
  | _ -> Alcotest.fail "8.6.3r entries"

let test_response_8_6_4 () =
  let m =
    ok "8.6.4r"
      ((fun s -> Result.bind (Httpz_dav.parse_xml s) Httpz_dav.multistatus)
         response_8_6_4)
  in
  let outcome =
    Carddav.Report.outcome_of_multistatus ~base:"/home/bernard/addressbook/" m
  in
  check_bool "8.6.4r not truncated" (not outcome.truncated);
  match outcome.entries with
  | [ e1; e2 ] ->
      check_bool "8.6.4r href1" (e1.href = "/home/bernard/addressbook/v102.vcf");
      check_bool "8.6.4r href2" (e2.href = "/home/bernard/addressbook/v104.vcf");
      let c1 = entry_vcard e1 and c2 = entry_vcard e2 in
      check_bool "8.6.4r FN1"
        (Option.map Vcard.Property.text (find_prop c1 "FN") = Some "David Boo");
      check_bool "8.6.4r FN2"
        (Option.map Vcard.Property.text (find_prop c2 "FN")
        = Some "Oliver Daboo")
  | _ -> Alcotest.fail "8.6.4r entries"

let test_response_8_6_5 () =
  let m =
    ok "8.6.5r"
      ((fun s -> Result.bind (Httpz_dav.parse_xml s) Httpz_dav.multistatus)
         response_8_6_5)
  in
  let outcome =
    Carddav.Report.outcome_of_multistatus ~base:"/home/bernard/addressbook/" m
  in
  check_bool "8.6.5r truncated" outcome.truncated;
  match outcome.entries with
  | [ e1; e2 ] ->
      check_bool "8.6.5r href1" (e1.href = "/home/bernard/addressbook/v102.vcf");
      check_bool "8.6.5r etag1" (e1.etag = Some {|"23ba4d-ff11fb"|});
      check_bool "8.6.5r data1" (e1.data = None);
      check_bool "8.6.5r href2" (e2.href = "/home/bernard/addressbook/v104.vcf");
      check_bool "8.6.5r etag2" (e2.etag = Some {|"23ba4d-ff11fc"|})
  | _ -> Alcotest.fail "8.6.5r entries"

let test_response_8_7_1 () =
  let m =
    ok "8.7.1r"
      ((fun s -> Result.bind (Httpz_dav.parse_xml s) Httpz_dav.multistatus)
         response_8_7_1)
  in
  let outcome =
    Carddav.Report.outcome_of_multistatus ~base:"/home/bernard/addressbook/" m
  in
  check_bool "8.7.1r not truncated" (not outcome.truncated);
  (match outcome.entries with
  | [ e ] ->
      check_bool "8.7.1r href" (e.href = "/home/bernard/addressbook/vcf102.vcf");
      let card = entry_vcard e in
      check_bool "8.7.1r FN"
        (Option.map Vcard.Property.text (find_prop card "FN")
        = Some "Cyrus Daboo")
  | _ -> Alcotest.fail "8.7.1r entries");
  check_bool "8.7.1r missing"
    (Carddav.Report.missing m = [ "/home/bernard/addressbook/vcf1.vcf" ])

(* {1 Carddav_filter.matches} *)

let card =
  Vcard.v ~version:"3.0"
    [
      Vcard.Property.of_text "FN" "Cyrus Daboo";
      Vcard.Property.of_text "NICKNAME" "me";
      Vcard.Property.v
        ~params:[ Vcard.Param.v "TYPE" [ "work"; "pref" ] ]
        "EMAIL" "cyrus@example.com";
      Vcard.Property.v ~group:"item1"
        ~params:[ Vcard.Param.v "TYPE" [ "work"; "voice" ] ]
        "TEL" "412 605 0499";
    ]

let card_matches f = Carddav.Filter.matches f card

let test_filter_match_types () =
  let open Carddav.Filter in
  check_bool "equals"
    (card_matches
       (v [ prop "NICKNAME" [ text_match ~match_type:`Equals "me" ] ]));
  check_bool "equals wrong"
    (not
       (card_matches
          (v [ prop "NICKNAME" [ text_match ~match_type:`Equals "you" ] ])));
  check_bool "contains"
    (card_matches
       (v [ prop "FN" [ text_match ~match_type:`Contains "Daboo" ] ]));
  check_bool "contains wrong"
    (not
       (card_matches
          (v [ prop "FN" [ text_match ~match_type:`Contains "Nope" ] ])));
  check_bool "starts-with"
    (card_matches
       (v [ prop "FN" [ text_match ~match_type:`Starts_with "Cyrus" ] ]));
  check_bool "starts-with wrong"
    (not
       (card_matches
          (v [ prop "FN" [ text_match ~match_type:`Starts_with "Daboo" ] ])));
  check_bool "ends-with"
    (card_matches
       (v [ prop "FN" [ text_match ~match_type:`Ends_with "Daboo" ] ]));
  check_bool "ends-with wrong"
    (not
       (card_matches
          (v [ prop "FN" [ text_match ~match_type:`Ends_with "Cyrus" ] ])))

let test_filter_negate () =
  let open Carddav.Filter in
  check_bool "negate flips a mismatch to a match"
    (card_matches
       (v
          [ prop "FN" [ text_match ~match_type:`Equals ~negate:true "Nobody" ] ]));
  check_bool "negate flips a match to a mismatch"
    (not
       (card_matches
          (v
             [
               prop "FN"
                 [ text_match ~match_type:`Equals ~negate:true "Cyrus Daboo" ];
             ])))

let test_filter_collation () =
  let open Carddav.Filter in
  check_bool "default collation folds ascii case"
    (card_matches
       (v [ prop "NICKNAME" [ text_match ~match_type:`Equals "ME" ] ]));
  check_bool "i;ascii-casemap folds ascii case"
    (card_matches
       (v
          [
            prop "NICKNAME"
              [
                text_match ~match_type:`Equals ~collation:"i;ascii-casemap" "ME";
              ];
          ]));
  check_bool "i;unicode-casemap folds ascii case"
    (card_matches
       (v
          [
            prop "NICKNAME"
              [
                text_match ~match_type:`Equals ~collation:"i;unicode-casemap"
                  "ME";
              ];
          ]));
  check_bool "i;octet compares bytes"
    (not
       (card_matches
          (v
             [
               prop "NICKNAME"
                 [ text_match ~match_type:`Equals ~collation:"i;octet" "ME" ];
             ])));
  check_bool "i;octet matches on identical bytes"
    (card_matches
       (v
          [
            prop "NICKNAME"
              [ text_match ~match_type:`Equals ~collation:"i;octet" "me" ];
          ]));
  check_bool "an unknown collation behaves as i;octet"
    (not
       (card_matches
          (v
             [
               prop "NICKNAME"
                 [ text_match ~match_type:`Equals ~collation:"i;bogus" "ME" ];
             ])))

let test_filter_param () =
  let open Carddav.Filter in
  check_bool "param-filter with a matching text-match"
    (card_matches
       (v
          [
            prop "EMAIL" []
              ~params:
                [
                  param "TYPE" (Some (text_match ~match_type:`Contains "work"));
                ];
          ]));
  check_bool "param-filter with a non-matching text-match"
    (not
       (card_matches
          (v
             [
               prop "EMAIL" []
                 ~params:
                   [
                     param "TYPE"
                       (Some (text_match ~match_type:`Contains "home"));
                   ];
             ])));
  check_bool "param-filter is-not-defined on an absent parameter"
    (card_matches
       (v [ prop "EMAIL" [] ~params:[ param_not_defined "LANGUAGE" ] ]));
  check_bool "param-filter is-not-defined on a present parameter"
    (not
       (card_matches
          (v [ prop "EMAIL" [] ~params:[ param_not_defined "TYPE" ] ])))

let test_filter_is_not_defined () =
  let open Carddav.Filter in
  check_bool "not_defined on an absent property"
    (card_matches (v [ prop_not_defined "ORG" ]));
  check_bool "not_defined on a present property"
    (not (card_matches (v [ prop_not_defined "FN" ])))

let test_filter_group () =
  let open Carddav.Filter in
  check_bool "a name without a group matches any group"
    (card_matches (v [ prop "TEL" [] ]));
  check_bool "a name with a group matches only that group"
    (card_matches (v [ prop "item1.TEL" [] ]));
  check_bool "a name with a different group does not match"
    (not (card_matches (v [ prop "item2.TEL" [] ])))

let test_filter_test () =
  let open Carddav.Filter in
  let hit = prop "FN" [ text_match ~match_type:`Contains "Daboo" ] in
  let miss = prop "NICKNAME" [ text_match ~match_type:`Equals "nobody" ] in
  check_bool "anyof matches when one prop-filter matches"
    (card_matches (v ~test:`Anyof [ hit; miss ]));
  check_bool "allof does not match when one prop-filter fails"
    (not (card_matches (v ~test:`Allof [ hit; miss ])));
  check_bool "allof matches when every prop-filter matches"
    (card_matches (v ~test:`Allof [ hit; hit ]));
  let m1 = text_match ~match_type:`Starts_with "Cyrus"
  and m2 = text_match ~match_type:`Ends_with "Daboo" in
  check_bool "a prop-filter's allof needs every text-match to match"
    (card_matches (v [ prop ~test:`Allof "FN" [ m1; m2 ] ]));
  let bad = text_match ~match_type:`Starts_with "Nope" in
  check_bool "a prop-filter's allof fails if one text-match fails"
    (not (card_matches (v [ prop ~test:`Allof "FN" [ m1; bad ] ])));
  check_bool "a prop-filter's anyof needs only one text-match to match"
    (card_matches (v [ prop ~test:`Anyof "FN" [ m1; bad ] ]));
  check_bool "an empty filter matches every address object" (card_matches all)

(* {1 Carddav_addressbook} *)

let radicale_response =
  {|<?xml version='1.0' encoding='utf-8'?>
<multistatus xmlns="DAV:" xmlns:CR="urn:ietf:params:xml:ns:carddav"><response><href>/alice/</href><propstat><prop><resourcetype><principal /><collection /></resourcetype></prop><status>HTTP/1.1 200 OK</status></propstat><propstat><prop><displayname /><getetag /><CR:supported-address-data /><CR:addressbook-description /></prop><status>HTTP/1.1 404 Not Found</status></propstat></response><response><href>/alice/book/</href><propstat><prop><resourcetype><CR:addressbook /><collection /></resourcetype><displayname>Test</displayname><getetag>"4ea1c69c"</getetag><sync-token>http://radicale.org/ns/sync/843a8f07</sync-token><CR:supported-address-data><CR:address-data-type content-type="text/vcard" version="3.0" /></CR:supported-address-data><CR:addressbook-description>desc</CR:addressbook-description><supported-report-set><supported-report><report><sync-collection /></report></supported-report><supported-report><report><CR:addressbook-multiget /></report></supported-report><supported-report><report><CR:addressbook-query /></report></supported-report></supported-report-set></prop><status>HTTP/1.1 200 OK</status></propstat></response></multistatus>|}

let test_addressbook () =
  let m =
    ok "radicale"
      ((fun s -> Result.bind (Httpz_dav.parse_xml s) Httpz_dav.multistatus)
         radicale_response)
  in
  let books = Carddav.Addressbook.of_multistatus m in
  match books with
  | [ b ] ->
      check_bool "href" (b.href = "/alice/book/");
      check_bool "display_name" (b.display_name = Some "Test");
      check_bool "description" (b.description = Some "desc");
      check_bool "etag" (b.etag = Some {|"4ea1c69c"|});
      check_bool "sync_token"
        (b.sync_token = Some "http://radicale.org/ns/sync/843a8f07");
      check_bool "data_types" (b.data_types = [ ("text/vcard", "3.0") ]);
      check_bool "reports"
        (b.reports
        = [
            Httpz_dav.dav "sync-collection";
            Carddav.Property.addressbook_multiget;
            Carddav.Property.addressbook_query;
          ])
  | _ -> Alcotest.failf "expected one address book, got %d" (List.length books)

(* {1 Carddav_data} *)

let test_data () =
  let vc =
    Vcard.v ~version:"4.0"
      [
        Vcard.Property.of_text "FN" "A B";
        Vcard.Property.of_text "UID" "urn:uuid:1";
      ]
  in
  let s = Vcard.to_string vc in
  let vc2 = ok "vcard decode" (Carddav.Data.vcard.decode s) in
  check_bool "vcard decode roundtrips" (Vcard.equal vc vc2);
  Alcotest.(check string)
    "vcard encode" s
    (ok "vcard encode" (Carddav.Data.vcard.encode vc));
  let r = ok "raw decode" (Carddav.Data.raw.decode s) in
  Alcotest.(check string) "raw decode is identity" s r;
  Alcotest.(check string)
    "raw encode is identity" s
    (ok "raw encode" (Carddav.Data.raw.encode s));
  Alcotest.(check (option string))
    "uid" (Some "urn:uuid:1")
    (Carddav.Data.uid Carddav.Data.vcard vc);
  let no_uid =
    Vcard.v ~version:"4.0" [ Vcard.Property.of_text "FN" "No UID" ]
  in
  Alcotest.(check (option string))
    "no uid" None
    (Carddav.Data.uid Carddav.Data.vcard no_uid);
  let d = Carddav.Data.address_data Carddav.Data.vcard in
  check_bool "address_data content-type" (d.content_type = Some "text/vcard");
  check_bool "address_data version" (d.version = Some "4.0");
  check_bool "address_data props" (d.props = `Props [])

let () =
  Alcotest.run "carddav"
    [
      ( "requests",
        [
          Alcotest.test_case "8.6.3" `Quick test_request_8_6_3;
          Alcotest.test_case "8.6.4" `Quick test_request_8_6_4;
          Alcotest.test_case "8.6.5" `Quick test_request_8_6_5;
          Alcotest.test_case "8.7.1" `Quick test_request_8_7_1;
        ] );
      ( "responses",
        [
          Alcotest.test_case "8.6.3" `Quick test_response_8_6_3;
          Alcotest.test_case "8.6.4" `Quick test_response_8_6_4;
          Alcotest.test_case "8.6.5" `Quick test_response_8_6_5;
          Alcotest.test_case "8.7.1" `Quick test_response_8_7_1;
        ] );
      ( "filter",
        [
          Alcotest.test_case "match types" `Quick test_filter_match_types;
          Alcotest.test_case "negate" `Quick test_filter_negate;
          Alcotest.test_case "collation" `Quick test_filter_collation;
          Alcotest.test_case "param" `Quick test_filter_param;
          Alcotest.test_case "is-not-defined" `Quick test_filter_is_not_defined;
          Alcotest.test_case "group" `Quick test_filter_group;
          Alcotest.test_case "test" `Quick test_filter_test;
        ] );
      ("addressbook", [ Alcotest.test_case "radicale" `Quick test_addressbook ]);
      ("data", [ Alcotest.test_case "vcard, raw, uid" `Quick test_data ]);
    ]
