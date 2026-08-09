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
    ~emails:["test@example.com"]
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

let test_migrate () =
  let module V1 = Sortal_schema.V1.Contact in
  let module V2 = Sortal_schema.V2.Contact in
  let module A = Sortal_schema.Account in
  let module P = Sortal_schema.Platform in
  let migrate c =
    match Sortal_schema.Migrate.v1_to_v2 c with
    | Ok v2 -> v2
    | Error e -> failwith e
  in
  (* a github service loses its url, kind and primary *)
  let c =
    V1.make ~handle:"a" ~names:[ "A" ]
      ~services:
        [ V1.make_service ~kind:V1.Github ~handle:"avsm" ~primary:false
            "https://github.com/avsm" ]
      ()
  in
  assert (V2.handle_on (migrate c) (P.Simple P.Github) = Some "avsm");

  (* a service with no handle derives one from the url tail *)
  let c =
    V1.make ~handle:"b" ~names:[ "B" ]
      ~services:[ V1.make_service ~kind:V1.Github "https://github.com/mdales" ]
      ()
  in
  assert (V2.handle_on (migrate c) (P.Simple P.Github) = Some "mdales");

  (* a service whose url is really a bare handle, as mdales.yaml has *)
  let c =
    V1.make ~handle:"c" ~names:[ "C" ]
      ~services:[ V1.make_service ~kind:V1.Github "mdales" ]
      ()
  in
  assert (V2.handle_on (migrate c) (P.Simple P.Github) = Some "mdales");

  (* both mastodon spellings converge *)
  let c =
    V1.make ~handle:"d" ~names:[ "D" ]
      ~services:
        [ V1.make_service ~kind:(V1.ActivityPub V1.Mastodon)
            ~handle:"avsm@amok.recoil.org" "https://amok.recoil.org/@avsm" ]
      ()
  in
  assert (
    V2.handle_on (migrate c) (P.Federated P.Mastodon)
    = Some "avsm@amok.recoil.org");

  (* photo splits by host *)
  let c =
    V1.make ~handle:"e" ~names:[ "E" ]
      ~services:
        [
          V1.make_service ~kind:V1.Photo ~handle:"avsm"
            "https://www.instagram.com/avsm";
          V1.make_service ~kind:V1.Photo ~handle:"avsm"
            "https://www.flickr.com/photos/avsm";
        ]
      ()
  in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.Instagram) = Some "avsm");
  assert (V2.handle_on m (P.Simple P.Flickr) = Some "avsm");

  (* the orcid field becomes an account *)
  let c =
    V1.make ~handle:"f" ~names:[ "F" ] ~orcid:"0000-0001-8954-2428" ()
  in
  assert (
    V2.handle_on (migrate c) (P.Simple P.Orcid) = Some "0000-0001-8954-2428");

  (* a linkedin url is promoted out of urls *)
  let c =
    V1.make ~handle:"g" ~names:[ "G" ]
      ~urls:
        [ V1.url_of_string "https://www.linkedin.com/in/anilmadhavapeddy" ]
      ()
  in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.LinkedIn) = Some "anilmadhavapeddy");
  assert (V2.links m = []);

  (* a github.io page is a personal site, not a github account *)
  let c =
    V1.make ~handle:"h" ~names:[ "H" ]
      ~urls:[ V1.url_of_string "https://ancazugo.github.io/" ]
      ()
  in
  let m = migrate c in
  assert (V2.account_on m (P.Simple P.Github) = None);
  assert (List.length (V2.links m) = 1);

  (* emails lose their type and keep their order *)
  let c =
    V1.make ~handle:"i" ~names:[ "I" ]
      ~emails:
        [
          V1.make_email ~type_:V1.Personal "anil@recoil.org";
          V1.make_email ~type_:V1.Work "avsm2@cam.ac.uk";
        ]
      ()
  in
  assert (V2.emails (migrate c) = [ "anil@recoil.org"; "avsm2@cam.ac.uk" ]);

  (* an unknown custom service kind is an error, not a silent drop *)
  let c =
    V1.make ~handle:"j" ~names:[ "J" ]
      ~services:
        [ V1.make_service ~kind:(V1.Custom "myspace") "https://myspace.com/j" ]
      ()
  in
  assert (Result.is_error (Sortal_schema.Migrate.v1_to_v2 c));

  (* a kinded service with no handle and no url has nothing to identify it
     by. Migration does not itself validate handles, the same as the orcid
     field below, so it produces an account with an empty handle rather than
     erroring: [Account.check] is what catches this. *)
  let c =
    V1.make ~handle:"k" ~names:[ "K" ]
      ~services:[ V1.make_service ~kind:V1.Github "" ]
      ()
  in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.Github) = Some "");
  assert (Result.is_error (A.check (Option.get (V2.account_on m (P.Simple P.Github)))));

  (* an orcid that fails its checksum still becomes an account: migration
     converts the field, it does not validate it. [Account.check] catches
     the bad checksum, matching [Platform.check_simple] in test_platform. *)
  let c =
    V1.make ~handle:"l" ~names:[ "L" ] ~orcid:"0000-0001-8954-2427" ()
  in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.Orcid) = Some "0000-0001-8954-2427");
  assert (Result.is_error (A.check (Option.get (V2.account_on m (P.Simple P.Orcid)))));

  (* an atproto block and a kind:bluesky service with different handles are
     two distinct identities, and migration keeps both rather than
     collapsing them: only an exact platform-and-handle duplicate merges *)
  let c =
    V1.make ~handle:"m" ~names:[ "M" ]
      ~atproto:
        {
          V1.atp_handle = "neil.example.com";
          atp_did = None;
          atp_services =
            [ { atp_type = V1.ATBluesky; atp_url = "https://bsky.app/profile/neil.example.com" } ];
        }
      ~services:
        [
          V1.make_service ~kind:(V1.Custom "bluesky")
            ~handle:"lawrennd.bsky.social"
            "https://bsky.app/profile/lawrennd.bsky.social";
        ]
      ()
  in
  let m = migrate c in
  let atproto_accounts =
    List.filter (fun a -> A.platform a = P.Atproto) (V2.accounts m)
  in
  assert (List.length atproto_accounts = 2);

  (* an organization's temporal range moves to the affiliation it becomes,
     since V2 keeps dates only on affiliations *)
  let c =
    V1.make ~handle:"n" ~names:[ "N" ]
      ~organizations:
        [ V1.make_org ~from:(2010, 1, 1) ~until:(2015, 6, 1) "Acme" ]
      ()
  in
  let m = migrate c in
  (match V2.affiliations m with
   | [ aff ] ->
       assert (aff.V2.org = "Acme");
       assert (aff.V2.from = Some (2010, 1, 1));
       assert (aff.V2.until = Some (2015, 6, 1))
   | _ -> assert false);

  (* icon is the fallback photo when there is no thumbnail *)
  let c =
    V1.make ~handle:"o" ~names:[ "O" ] ~icon:"https://example.com/o.png" ()
  in
  assert (V2.photo (migrate c) = Some "https://example.com/o.png");

  (* a urls entry that is not a URL at all is kept as a link, not an error *)
  let c =
    V1.make ~handle:"p" ~names:[ "P" ]
      ~urls:[ V1.url_of_string "just some text, not a url" ]
      ()
  in
  let m = migrate c in
  assert (V2.links m = [ { V2.url = "just some text, not a url"; label = None } ]);
  assert (V2.accounts m = []);

  (* Scholar's id is the [user] query parameter, not everything after the
     first [=]: most Scholar URLs in the store have a trailing [&hl=..]
     that must not be folded into the id *)
  let c =
    V1.make ~handle:"q" ~names:[ "Q" ]
      ~urls:
        [ V1.url_of_string
            "https://scholar.google.com/citations?user=lBhFXdIAAAAJ&hl=en" ]
      ()
  in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.Scholar) = Some "lBhFXdIAAAAJ");

  (* the [user] parameter need not come first *)
  let c =
    V1.make ~handle:"r" ~names:[ "R" ]
      ~urls:
        [ V1.url_of_string
            "https://scholar.google.com/citations?hl=en&user=lBhFXdIAAAAJ" ]
      ()
  in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.Scholar) = Some "lBhFXdIAAAAJ");

  (* a citations URL with no [user] parameter at all has no id to extract,
     so it stays a link rather than becoming a bogus account *)
  let c =
    V1.make ~handle:"s" ~names:[ "S" ]
      ~urls:[ V1.url_of_string "https://scholar.google.com/citations?hl=en" ]
      ()
  in
  let m = migrate c in
  assert (V2.account_on m (P.Simple P.Scholar) = None);
  assert (List.length (V2.links m) = 1);

  (* a regional Scholar domain is the same platform as scholar.google.com,
     since a profile id is global, not per-domain *)
  let c =
    V1.make ~handle:"t" ~names:[ "T" ]
      ~urls:
        [ V1.url_of_string
            "https://scholar.google.ca/citations?user=bc4TxdsAAAAJ" ]
      ()
  in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.Scholar) = Some "bc4TxdsAAAAJ");

  (* Scholar hosts are matched on label structure, not a string prefix: a
     prefix test would also accept a crafted host such as
     [scholar.google.com.evil.example], which has "scholar.google." as a
     literal prefix but is not a Google host at all *)
  let scholar = Some (P.Simple P.Scholar) in
  let platform_of_host = Sortal_schema.Migrate.platform_of_host in
  assert (platform_of_host "scholar.google.com" = scholar);
  assert (platform_of_host "scholar.google.ca" = scholar);
  assert (platform_of_host "scholar.google.co.uk" = scholar);
  assert (platform_of_host "scholar.google.com.evil.example" = None);
  assert (platform_of_host "notscholar.google.com" = None);
  assert (platform_of_host "google.com" = None);
  assert (platform_of_host "www.google.com" = None);

  (* a [#fragment] glued onto the id by the [&]-split alone is rejected by
     the character-set check, not folded into the handle *)
  let c =
    V1.make ~handle:"v" ~names:[ "V" ]
      ~urls:
        [ V1.url_of_string
            "https://scholar.google.com/citations?user=lBhFXdIAAAAJ#foo" ]
      ()
  in
  let m = migrate c in
  assert (V2.account_on m (P.Simple P.Scholar) = None);
  assert (List.length (V2.links m) = 1);

  (* an empty [user=] behaves like a missing parameter, not an empty
     handle *)
  let c =
    V1.make ~handle:"w" ~names:[ "W" ]
      ~urls:[ V1.url_of_string "https://scholar.google.com/citations?user=&hl=en" ]
      ()
  in
  let m = migrate c in
  assert (V2.account_on m (P.Simple P.Scholar) = None);

  (* a percent-encoded value is rejected undecoded: '%' is not in the id's
     character set *)
  let c =
    V1.make ~handle:"x" ~names:[ "X" ]
      ~urls:
        [ V1.url_of_string
            "https://scholar.google.com/citations?user=lB%20hFXdI" ]
      ()
  in
  let m = migrate c in
  assert (V2.account_on m (P.Simple P.Scholar) = None);

  (* any other character outside [A-Za-z0-9_-] is rejected the same way *)
  let c =
    V1.make ~handle:"y" ~names:[ "Y" ]
      ~urls:
        [ V1.url_of_string "https://scholar.google.com/citations?user=abc def" ]
      ()
  in
  let m = migrate c in
  assert (V2.account_on m (P.Simple P.Scholar) = None);

  (* an organization's email has no V2 affiliation field to carry it, so it
     is an error rather than a silent drop, naming the contact and the
     organization *)
  let c =
    V1.make ~handle:"u" ~names:[ "U" ]
      ~organizations:[ V1.make_org ~email:"acme@example.com" "Acme" ]
      ()
  in
  (match Sortal_schema.Migrate.v1_to_v2 c with
   | Error e ->
       assert (contains e "u");
       assert (contains e "Acme")
   | Ok _ -> assert false);

  print_endline "✓ V1 to V2 migration works"

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
  test_migrate ();
  test_v2_rejects_non_v2 ();
  print_endline "\n=== All Schema Tests Passed ===\n"
