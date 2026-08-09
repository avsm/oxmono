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

let () =
  print_endline "\n=== Schema Tests ===\n";
  test_temporal ();
  test_feed_types ();
  test_contact_construction ();
  test_json_roundtrip ();
  test_date ();
  test_platform ();
  print_endline "\n=== All Schema Tests Passed ===\n"
