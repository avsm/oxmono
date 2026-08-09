(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Schema-only tests (no I/O dependencies) *)

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

let () =
  print_endline "\n=== Schema Tests ===\n";
  test_temporal ();
  test_feed_types ();
  test_contact_construction ();
  test_json_roundtrip ();
  test_date ();
  test_platform ();
  test_account_codec ();
  print_endline "\n=== All Schema Tests Passed ===\n"
