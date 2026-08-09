(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Schema-only tests (no I/O dependencies) *)

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0

let test_temporal () =
  (* Parse dates from strings *)
  let from_date = Sortal_schema.Temporal.parse_date_string "2020-01" |> Option.get in
  let until_date = Sortal_schema.Temporal.parse_date_string "2023-12" |> Option.get in
  let test_date_1 = Sortal_schema.Temporal.parse_date_string "2021-06" |> Option.get in
  let test_date_2 = Sortal_schema.Temporal.parse_date_string "2024-01" |> Option.get in

  let r = Sortal_schema.Temporal.make ~from:from_date ~until:until_date () in
  assert (Sortal_schema.Temporal.valid_at (Some r) ~date:test_date_1);
  assert (not (Sortal_schema.Temporal.valid_at (Some r) ~date:test_date_2));
  print_endline "✓ Temporal ranges work"

let test_feed_types () =
  let feed = Sortal_schema.Feed.make ~feed_type:Atom ~url:"https://example.com/feed" () in
  assert (Sortal_schema.Feed.url feed = "https://example.com/feed");
  print_endline "✓ Feed types work"

let test_contact_construction () =
  let c = Sortal_schema.Contact.make
    ~handle:"test"
    ~names:["Test User"]
    ~emails:[Sortal_schema.Contact.email_of_string "test@example.com"]
    () in
  assert (Sortal_schema.Contact.handle c = "test");
  assert (Sortal_schema.Contact.name c = "Test User");
  print_endline "✓ Contact construction works"

let test_json_roundtrip () =
  let c = Sortal_schema.Contact.make ~handle:"json" ~names:["JSON Test"] () in
  match Jsont_bytesrw.encode_string Sortal_schema.Contact.json_t c with
  | Ok json ->
      (match Jsont_bytesrw.decode_string Sortal_schema.Contact.json_t json with
       | Ok decoded ->
           assert (Sortal_schema.Contact.handle decoded = "json");
           assert (Sortal_schema.Contact.name decoded = "JSON Test");
           print_endline "✓ JSON roundtrip works"
       | Error e -> failwith ("Decode failed: " ^ e))
  | Error e -> failwith ("Encode failed: " ^ e)

let test_date () =
  let p = Sortal_schema.Date.parse in
  assert (p "2001" = Some (2001, 1, 1));
  assert (p "2001-03" = Some (2001, 3, 1));
  assert (p "2001-03-15" = Some (2001, 3, 15));
  assert (p "" = None);
  assert (p "not-a-date" = None);
  assert (p "2001-13-01" = None);
  assert (p "2001-02-30" = None);
  assert (p "0x10" = None);
  assert (p "0o17" = None);
  assert (p "2_006" = None);
  assert (p "+2001" = None);
  assert (p " 2001-03-15 " = None);
  assert (p "2001 - 03 - 15" = None);
  assert (p "2001-01-01-01" = None);
  assert (p "-0001" = None);
  assert (p "201" = None);
  assert (p "2001-3-15" = None);
  assert (Sortal_schema.Date.to_string (2001, 3, 15) = "2001-03-15");
  List.iter
    (fun d -> assert (p (Sortal_schema.Date.to_string d) = Some d))
    [ (2001, 1, 1); (2001, 3, 15); (1999, 12, 31); (2024, 2, 29) ];
  print_endline "✓ Date parsing works"

let test_platform () =
  let module P = Sortal_schema.Platform in
  assert (P.of_key "github" = Some (P.Simple P.Github));
  assert (P.of_key "mastodon" = Some (P.Federated P.Mastodon));
  assert (P.of_key "atproto" = Some P.Atproto);
  assert (P.of_key "githb" = None);
  assert (P.key (P.Simple P.Github) = "github");
  assert (P.key P.Atproto = "atproto");
  (* every platform round-trips through its key *)
  List.iter (fun id -> assert (P.of_key (P.key id) = Some id)) P.all;
  (* keys are unique *)
  let keys = List.map P.key P.all in
  assert (List.length (List.sort_uniq String.compare keys) = List.length keys);
  assert (P.simple_url P.Github "avsm" = "https://github.com/avsm");
  assert (P.simple_url P.Orcid "0000-0001-8954-2428"
          = "https://orcid.org/0000-0001-8954-2428");
  assert (P.simple_url P.LinkedIn "avsm" = "https://www.linkedin.com/in/avsm");
  assert (P.simple_url P.Threads "avsm" = "https://www.threads.com/@avsm");
  assert (P.simple_url P.Instagram "avsm" = "https://www.instagram.com/avsm");
  assert (P.simple_url P.Flickr "avsm" = "https://www.flickr.com/photos/avsm");
  assert (P.federated_url P.Mastodon ~user:"avsm" ~host:"amok.recoil.org"
          = "https://amok.recoil.org/@avsm");
  assert (P.federated_url P.Pixelfed ~user:"avsm" ~host:"pixelfed.social"
          = "https://pixelfed.social/@avsm");
  assert (P.federated_url P.Matrix ~user:"avsm" ~host:"recoil.org"
          = "https://matrix.to/#/@avsm:recoil.org");
  assert (P.federated_url P.Discourse ~user:"avsm" ~host:"discuss.ocaml.org"
          = "https://discuss.ocaml.org/u/avsm");
  (* PeerTube's [/c/] is a channel URL, not the [/a/] account form. The
     store's only PeerTube entry is a channel, so this pins that form. *)
  assert (P.federated_url P.PeerTube ~user:"anil" ~host:"crank.recoil.org"
          = "https://crank.recoil.org/c/anil/videos");
  (* Zulip cannot derive a user URL, only a host one *)
  assert (P.federated_url P.Zulip ~user:"Anil Madhavapeddy" ~host:"eeg.zulipchat.com"
          = "https://eeg.zulipchat.com");
  (* ORCID checksum, ISO 7064 MOD 11-2 *)
  assert (P.check_simple P.Orcid "0000-0001-8954-2428" = Ok ());
  assert (Result.is_error (P.check_simple P.Orcid "0000-0001-8954-2427"));
  assert (Result.is_error (P.check_simple P.Orcid "nonsense"));
  assert (P.check_simple P.Github "avsm" = Ok ());
  assert (Result.is_error (P.check_simple P.Github "not a handle"));
  (* an ORCID whose check digit is X, verified by hand against live data *)
  assert (P.check_simple P.Orcid "0000-0001-7424-572X" = Ok ());
  (* the check digit is case-sensitive: a lowercase x does not match *)
  assert (Result.is_error (P.check_simple P.Orcid "0000-0001-7424-572x"));
  (* AT Protocol handle syntax *)
  assert (P.check_atproto_handle "avsm.bsky.social" = Ok ());
  assert (Result.is_error (P.check_atproto_handle ""));
  assert (Result.is_error (P.check_atproto_handle "nodots"));
  (* a leading dot leaves an empty first segment *)
  assert (Result.is_error (P.check_atproto_handle ".bsky.social"));
  (* a trailing dot leaves an empty final segment *)
  assert (Result.is_error (P.check_atproto_handle "bsky.social."));
  (* a segment of exactly 63 characters is the limit, 64 exceeds it *)
  let seg63 = String.make 63 'a' in
  let seg64 = String.make 64 'a' in
  assert (P.check_atproto_handle (seg63 ^ ".bsky.social") = Ok ());
  assert (Result.is_error (P.check_atproto_handle (seg64 ^ ".bsky.social")));
  (* non-ASCII is rejected even though the segment length in bytes fits *)
  assert (Result.is_error (P.check_atproto_handle "caf\xc3\xa9.social"));
  (* the final segment must not start with a digit *)
  assert (Result.is_error (P.check_atproto_handle "bsky.4social"));
  (* a non-final segment starting with a digit is fine *)
  assert (P.check_atproto_handle "4bsky.social" = Ok ());
  print_endline "✓ Platform vocabulary works"

let decode_accounts s =
  Jsont_bytesrw.decode_string Sortal_schema.Account.json_t s

let encode_accounts a =
  Jsont_bytesrw.encode_string Sortal_schema.Account.json_t a

let test_account_codec () =
  let module A = Sortal_schema.Account in
  let module P = Sortal_schema.Platform in
  (* scalar form *)
  (match decode_accounts {|{"github":"avsm"}|} with
   | Ok [ a ] ->
       assert (A.platform a = P.Simple P.Github);
       assert (A.handle a = "avsm");
       assert (A.url a = "https://github.com/avsm")
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* federated form splits on the last @ *)
  (match decode_accounts {|{"mastodon":"avsm@amok.recoil.org"}|} with
   | Ok [ A.Federated (P.Mastodon, user, host) ] ->
       assert (user = "avsm");
       assert (host = "amok.recoil.org")
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* sequence form *)
  (match decode_accounts {|{"github":["avsm","avsm-work"]}|} with
   | Ok [ a; b ] -> assert (A.handle a = "avsm"); assert (A.handle b = "avsm-work")
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* atproto object form *)
  (match decode_accounts
           {|{"atproto":{"handle":"anil.recoil.org","did":"did:plc:x","apps":["bluesky","tangled"]}}|}
   with
   | Ok [ A.Atproto a ] ->
       assert (a.A.handle = "anil.recoil.org");
       assert (a.A.did = Some "did:plc:x");
       assert (a.A.apps = [ A.Bluesky; A.Tangled ])
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* an unknown platform key is a decode error, not a silent accept *)
  assert (Result.is_error (decode_accounts {|{"githb":"avsm"}|}));
  (* a federated platform given a bare handle is a decode error *)
  assert (Result.is_error (decode_accounts {|{"mastodon":"avsm"}|}));
  (* an unknown app is a decode error *)
  assert (Result.is_error
            (decode_accounts {|{"atproto":{"handle":"a.b","apps":["nope"]}}|}));
  (* a leading or trailing @ leaves an empty user or host, still an error *)
  assert (Result.is_error (decode_accounts {|{"mastodon":"@amok.recoil.org"}|}));
  assert (Result.is_error (decode_accounts {|{"mastodon":"avsm@"}|}));
  (* an atproto object without a handle member is a decode error *)
  assert (Result.is_error (decode_accounts {|{"atproto":{"did":"did:plc:x"}}|}));
  (* a typo'd member inside the atproto object is a decode error, not a
     silently ignored unknown member: "dad" does not become "did" *)
  assert (Result.is_error
            (decode_accounts {|{"atproto":{"handle":"a.b","dad":"did:plc:x"}}|}));
  (* the last '@' is where a federated handle splits, even with two of them;
     Platform.check_federated, not the split, is what would reject this *)
  (match decode_accounts {|{"mastodon":"a@b@c"}|} with
   | Ok [ A.Federated (P.Mastodon, user, host) ] ->
       assert (user = "a@b");
       assert (host = "c")
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* an empty array under a key yields no accounts for that key *)
  (match decode_accounts {|{"github":[]}|} with
   | Ok [] -> ()
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* a duplicate handle within one key's array is kept, not deduplicated *)
  (match decode_accounts {|{"github":["avsm","avsm"]}|} with
   | Ok [ a; b ] -> assert (A.handle a = "avsm"); assert (A.handle b = "avsm")
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* structural decoding and syntax checking are separate: a handle that
     fails its platform's syntax check still decodes, and [check] catches it *)
  (match decode_accounts {|{"github":"not a handle"}|} with
   | Ok [ a ] -> assert (Result.is_error (A.check a))
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* round trip, sequence form *)
  (match decode_accounts {|{"github":["avsm","avsm-work"]}|} with
   | Ok accounts ->
       (match encode_accounts accounts with
        | Ok out ->
            (match decode_accounts out with
             | Ok again -> assert (again = accounts)
             | Error e -> failwith e)
        | Error e -> failwith e)
   | Error e -> failwith e);
  (* round trip, atproto object form *)
  (match decode_accounts
           {|{"atproto":{"handle":"anil.recoil.org","did":"did:plc:x","apps":["bluesky","tangled"]}}|}
   with
   | Ok accounts ->
       (match encode_accounts accounts with
        | Ok out ->
            (match decode_accounts out with
             | Ok again -> assert (again = accounts)
             | Error e -> failwith e)
        | Error e -> failwith e)
   | Error e -> failwith e);
  (* round trip, mixed mapping *)
  let src =
    {|{"github":"avsm","mastodon":"avsm@amok.recoil.org","atproto":{"handle":"anil.recoil.org","apps":["bluesky"]}}|}
  in
  (match decode_accounts src with
   | Ok accounts ->
       (match encode_accounts accounts with
        | Ok out ->
            (match decode_accounts out with
             | Ok again -> assert (again = accounts)
             | Error e -> failwith e)
        | Error e -> failwith e)
   | Error e -> failwith e);
  (* canonicalisation is pinned at the JSON-text level, since decoded values
     alone cannot distinguish a scalar from a single-element sequence: a
     one-element array is re-encoded as a bare scalar *)
  (match decode_accounts {|{"github":["avsm"]}|} with
   | Ok accounts ->
       (match encode_accounts accounts with
        | Ok out ->
            let contains sub =
              let sl = String.length sub and ol = String.length out in
              let rec go i = i + sl <= ol && (String.sub out i sl = sub || go (i + 1)) in
              go 0
            in
            assert (contains {|"github":"avsm"|});
            assert (not (contains "["))
        | Error e -> failwith e)
   | Error e -> failwith e);
  (* an atproto account with neither a did nor an app narrows to a bare
     scalar handle on encode, the same shape it would decode from *)
  (match decode_accounts {|{"atproto":"anil.recoil.org"}|} with
   | Ok [ A.Atproto { did = None; apps = []; _ } ] as accounts -> (
       match encode_accounts (Result.get_ok accounts) with
       | Ok out -> assert (out = {|{"atproto":"anil.recoil.org"}|})
       | Error e -> failwith e)
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* an atproto account with a did stays an object on encode, since a bare
     scalar cannot carry it *)
  (match decode_accounts {|{"atproto":{"handle":"anil.recoil.org","did":"did:plc:x"}}|} with
   | Ok accounts -> (
       match encode_accounts accounts with
       | Ok out ->
           assert (out <> {|{"atproto":"anil.recoil.org"}|});
           (match decode_accounts out with
            | Ok again -> assert (again = accounts)
            | Error e -> failwith e)
       | Error e -> failwith e)
   | Error e -> failwith e);
  print_endline "✓ Account codec works"

let test_contact_v2 () =
  let module C = Sortal_schema.V2.Contact in
  let module A = Sortal_schema.Account in
  let module P = Sortal_schema.Platform in
  let c =
    C.make ~handle:"avsm" ~names:[ "Anil Madhavapeddy" ]
      ~emails:[ "anil@recoil.org"; "avsm2@cam.ac.uk" ]
      (* accounts are listed in platform-key order (atproto, github,
         mastodon): decoding always returns that order (see
         [Sortal_schema_account.json_t]), so a value built in a different
         order would not equal what a round trip through [json_t] produces *)
      ~accounts:
        [ A.Atproto { A.handle = "anil.recoil.org"; did = None;
                      apps = [ A.Bluesky ] };
          A.Simple (P.Github, "avsm");
          A.Federated (P.Mastodon, "avsm", "amok.recoil.org") ]
      ~links:[ { C.url = "https://anil.recoil.org"; label = None } ]
      ()
  in
  assert (C.handle c = "avsm");
  assert (C.name c = "Anil Madhavapeddy");
  assert (C.handle_on c (P.Simple P.Github) = Some "avsm");
  assert (C.handle_on c (P.Simple P.Twitter) = None);
  assert (C.url_on c (P.Simple P.Github) = Some "https://github.com/avsm");
  assert (C.atproto_handle c = Some "anil.recoil.org");
  assert (C.best_url c = Some "https://anil.recoil.org");
  (* a contact with no links falls back to an account URL *)
  let bare = C.make ~handle:"x" ~names:[ "X" ]
      ~accounts:[ A.Simple (P.Github, "x") ] () in
  assert (C.best_url bare = Some "https://github.com/x");
  (* best_url sorts accounts by platform key: "github" < "twitter", so the
     Github account wins even though Twitter is written first *)
  let sorted = C.make ~handle:"s" ~names:[ "S" ]
      ~accounts:
        [ A.Simple (P.Twitter, "s-tw"); A.Simple (P.Github, "s-gh") ] ()
  in
  assert (C.best_url sorted = Some "https://github.com/s-gh");
  (* round trip *)
  (match Jsont_bytesrw.encode_string C.json_t c with
   | Ok json ->
       (match Jsont_bytesrw.decode_string C.json_t json with
        | Ok d -> assert (d = c)
        | Error e -> failwith e)
   | Error e -> failwith e);
  (* empty collections are omitted, and an unlabelled link is a bare string *)
  let minimal = C.make ~handle:"m" ~names:[ "M" ]
      ~links:[ { C.url = "https://m.example"; label = None } ] () in
  (match Jsont_bytesrw.encode_string C.json_t minimal with
   | Ok json ->
       assert (not (contains json "\"emails\""));
       assert (not (contains json "\"accounts\""));
       assert (not (contains json "\"affiliations\""));
       assert (not (contains json "\"vcard\""));
       assert (contains json "\"https://m.example\"");
       assert (not (contains json "\"label\""))
   | Error e -> failwith e);
  (* a labelled link survives a round trip as an object, not a bare string *)
  let labelled = C.make ~handle:"l" ~names:[ "L" ]
      ~links:[ { C.url = "https://l.example"; label = Some "home page" } ] ()
  in
  (match Jsont_bytesrw.encode_string C.json_t labelled with
   | Ok json ->
       assert (contains json "\"url\":\"https://l.example\"");
       assert (contains json "\"label\":\"home page\"");
       (* the link is an object, not the bare-string form an unlabelled
          link would take *)
       assert (not (contains json "\"links\":[\"https://l.example\"]"));
       (match Jsont_bytesrw.decode_string C.json_t json with
        | Ok d -> assert (d = labelled)
        | Error e -> failwith e)
   | Error e -> failwith e);
  (* the version member must be 2, and 1 must be rejected *)
  assert (Result.is_error
            (Jsont_bytesrw.decode_string C.json_t
               {|{"version":1,"kind":"person","handle":"a","names":["A"]}|}));
  (* a missing version member is rejected too *)
  assert (Result.is_error
            (Jsont_bytesrw.decode_string C.json_t
               {|{"kind":"person","handle":"a","names":["A"]}|}));
  (* an empty names list is accepted by [make], since it is [check] that
     enforces non-emptiness, and [name] falls back to the handle *)
  let noname = C.make ~handle:"h" ~names:[] () in
  assert (C.names noname = []);
  assert (C.name noname = "h");
  assert (Result.is_error (C.check noname));
  (* a bad handle on an otherwise well-formed account is caught by [check] *)
  let bad_account = C.make ~handle:"b" ~names:[ "B" ]
      ~accounts:[ A.Simple (P.Github, "not a handle") ] () in
  assert (Result.is_error (C.check bad_account));
  (* a well-formed contact passes [check] *)
  assert (Result.is_ok (C.check c));
  (* a vcard passthrough round trips and is not omitted when present *)
  let with_vcard = C.make ~handle:"v" ~names:[ "V" ]
      ~vcard:[ ("FN", "V Test"); ("TEL", "+44 1223 000000") ] () in
  (match Jsont_bytesrw.encode_string C.json_t with_vcard with
   | Ok json ->
       assert (contains json "\"vcard\"");
       (match Jsont_bytesrw.decode_string C.json_t json with
        | Ok d -> assert (d = with_vcard); assert (C.vcard d = C.vcard with_vcard)
        | Error e -> failwith e)
   | Error e -> failwith e);
  (* an affiliation with only [from] omits [until] on encoding *)
  let aff = { C.org = "Cambridge"; department = None; title = None;
              url = None; address = None; from = Some (2015, 10, 1);
              until = None } in
  let affiliated = C.make ~handle:"a" ~names:[ "A" ] ~affiliations:[ aff ] () in
  (match Jsont_bytesrw.encode_string C.json_t affiliated with
   | Ok json ->
       assert (contains json "\"from\":\"2015-10-01\"");
       assert (not (contains json "\"until\""));
       (match Jsont_bytesrw.decode_string C.json_t json with
        | Ok d -> assert (C.affiliations d = [ aff ])
        | Error e -> failwith e)
   | Error e -> failwith e);
  assert (C.current_affiliation affiliated = Some aff);
  (* [set_atproto_did] on a contact with no AT Protocol account is a no-op *)
  assert (C.set_atproto_did bare "did:plc:x" = bare);
  assert (C.atproto_did c = None);
  let with_did = C.set_atproto_did c "did:plc:x" in
  assert (C.atproto_did with_did = Some "did:plc:x");
  (* [remove_feed] for a URL that is not present leaves the contact alone *)
  let feed = Sortal_schema.Feed.make ~feed_type:Atom
      ~url:"https://anil.recoil.org/feed.xml" () in
  let with_feed = C.add_feed c feed in
  assert (List.length (C.feeds with_feed) = 1);
  assert (C.remove_feed with_feed "https://nope.example" = with_feed);
  assert (C.feeds (C.remove_feed with_feed (Sortal_schema.Feed.url feed)) = []);
  print_endline "✓ V2 contact works"

(* Task 10's [migrate] command tells a V1 file from a V2 one by trying the
   V2 decoder first and falling back to V1 on failure. That only works if
   the V2 decoder rejects every shape a V1 or malformed [version] member
   can take, so each is pinned here rather than assumed. *)
let test_v2_rejects_non_v2 () =
  let module V2 = Sortal_schema.V2.Contact in
  let decode s = Jsont_bytesrw.decode_string V2.json_t s in
  assert (Result.is_error
            (decode {|{"version":1,"kind":"person","handle":"a","names":["A"]}|}));
  assert (Result.is_error
            (decode {|{"version":3,"kind":"person","handle":"a","names":["A"]}|}));
  assert (Result.is_error
            (decode {|{"kind":"person","handle":"a","names":["A"]}|}));
  assert (Result.is_error
            (decode {|{"version":"2","kind":"person","handle":"a","names":["A"]}|}));
  (* a non-integer number is not a schema version either *)
  assert (Result.is_error
            (decode {|{"version":2.5,"kind":"person","handle":"a","names":["A"]}|}));
  print_endline "✓ V2 decoder rejects non-V2 input"

let () =
  print_endline "\n=== Schema Tests ===\n";
  test_temporal ();
  test_feed_types ();
  test_contact_construction ();
  test_json_roundtrip ();
  test_date ();
  test_platform ();
  test_account_codec ();
  test_contact_v2 ();
  test_v2_rejects_non_v2 ();
  print_endline "\n=== All Schema Tests Passed ===\n"
