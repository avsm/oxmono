(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Jscontact

let ok name = function
  | Ok v -> v
  | Error msg -> Alcotest.failf "%s: %s" name msg

let decode t s = Jsont_bytesrw.decode_string' t s
let encode t v = Jsont_bytesrw.encode_string' t v

let json_of s =
  match decode Jsont.json s with
  | Ok j -> j
  | Error e -> Alcotest.failf "json: %s" (Jsont.Error.to_string e)

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let vcard_of_fragment s =
  let lines = List.filter (fun l -> String.trim l <> "") (Vcard.unfold s) in
  let framed =
    if List.exists (fun l -> String.uppercase_ascii l = "BEGIN:VCARD") lines
    then lines
    else ("BEGIN:VCARD" :: "VERSION:4.0" :: lines) @ [ "END:VCARD" ]
  in
  ok "vcard" (Vcard.one_of_string (String.concat "\r\n" framed ^ "\r\n"))

let card_of_fragment frag =
  let frag =
    if String.trim frag <> "" && (String.trim frag).[0] = '{' then frag
    else "{" ^ frag ^ "}"
  in
  (* RFC 9553 Section 2.8.1 types an anniversary date as
     "PartialDate|Timestamp (defaultType: PartialDate)", so Section 1.3.4
     requires "@type" on a Timestamp. The fixtures omit it. *)
  let rec tag_timestamps = function
    | Jsont.Object (mems, meta) ->
        let has name = List.exists (fun ((n, _), _) -> n = name) mems in
        let mems =
          List.map (fun ((k, km), v) -> ((k, km), tag_timestamps v)) mems
        in
        let mems =
          if has "utc" && not (has "@type") then
            (("@type", Jsont.Meta.none), Jsont.Json.string "Timestamp") :: mems
          else mems
        in
        Jsont.Object (mems, meta)
    | Jsont.Array (vs, meta) -> Jsont.Array (List.map tag_timestamps vs, meta)
    | v -> v
  in
  match tag_timestamps (json_of frag) with
  | Jsont.Object (mems, meta) ->
      let has name = List.exists (fun ((n, _), _) -> n = name) mems in
      let add name v mems =
        if has name then mems else ((name, Jsont.Meta.none), v) :: mems
      in
      let mems =
        mems
        |> add "uid" (Jsont.Json.string "corpus")
        |> add "version" (Jsont.Json.string "1.0")
        |> add "@type" (Jsont.Json.string "Card")
      in
      Jsont.Json.decode Card.jsont (Jsont.Object (mems, meta))
  | _ -> Error "not an object"

let show_json j = match encode Jsont.json j with Ok s -> s | Error _ -> ""

let maps =
  [
    "nicknames";
    "organizations";
    "titles";
    "emails";
    "onlineServices";
    "phones";
    "preferredLanguages";
    "calendars";
    "schedulingAddresses";
    "addresses";
    "cryptoKeys";
    "directories";
    "links";
    "media";
    "anniversaries";
    "notes";
    "personalInfo";
  ]

let dropped = [ "vCard"; "vCardProps"; "vCardParams"; "vCardName"; "uid" ]

(* Relabels the Ids of every map to k1, k2 ... in order, and follows the
   references to them, so that two Cards compare regardless of the Ids their
   converters chose. Members with no counterpart in the other converter are
   dropped. *)
let canonical json =
  let tables = Hashtbl.create 16 in
  (* Entries are numbered in the order of their values, with the references
     among them dropped, so that neither the Ids nor the order of a map
     matter. *)
  let relabel map entries =
    let rec drop = function
      | Jsont.Object (mems, m) ->
          Jsont.Object
            ( List.filter_map
                (fun ((k, km), v) ->
                  if k = "organizationId" then None else Some ((k, km), drop v))
                mems,
              m )
      | Jsont.Array (vs, m) -> Jsont.Array (List.map drop vs, m)
      | v -> v
    in
    let text (_, v) = show_json (drop v) in
    let sorted =
      List.stable_sort (fun x y -> compare (text x) (text y)) entries
    in
    List.iteri
      (fun i ((k, _), _) ->
        Hashtbl.replace tables (map, k) (Printf.sprintf "k%d" (i + 1)))
      sorted
  in
  let lookup map k =
    Option.value ~default:k (Hashtbl.find_opt tables (map, k))
  in
  (* Language tags compare regardless of case, RFC 5646 Section 2.1.1. *)
  let rec strip = function
    | Jsont.Object (mems, m) ->
        Jsont.Object
          ( List.filter_map
              (fun ((k, km), v) ->
                match v with
                | _ when List.mem k dropped -> None
                | Jsont.String (l, lm) when k = "language" ->
                    Some ((k, km), Jsont.String (String.lowercase_ascii l, lm))
                | Jsont.Object (ls, lm) when k = "localizations" ->
                    Some
                      ( (k, km),
                        Jsont.Object
                          ( List.map
                              (fun ((l, lm), v) ->
                                ((String.lowercase_ascii l, lm), strip v))
                              ls,
                            lm ) )
                | v -> Some ((k, km), strip v))
              mems,
            m )
    | Jsont.Array (vs, m) -> Jsont.Array (List.map strip vs, m)
    | v -> v
  in
  let rec sort_unordered = function
    | Jsont.Object (mems, m) ->
        let ordered =
          List.exists
            (fun ((k, _), v) ->
              k = "isOrdered"
              && match v with Jsont.Bool (b, _) -> b | _ -> false)
            mems
        in
        Jsont.Object
          ( List.map
              (fun ((k, km), v) ->
                match v with
                | Jsont.Array (cs, am) when k = "components" && not ordered ->
                    ( (k, km),
                      Jsont.Array
                        ( List.sort Jsont.Json.compare
                            (List.map sort_unordered cs),
                          am ) )
                | v -> ((k, km), sort_unordered v))
              mems,
            m )
    | Jsont.Array (vs, m) -> Jsont.Array (List.map sort_unordered vs, m)
    | v -> v
  in
  let json = sort_unordered (strip json) in
  (match json with
  | Jsont.Object (mems, _) ->
      List.iter
        (fun ((k, _), v) ->
          match v with
          | Jsont.Object (entries, _) when List.mem k maps -> relabel k entries
          | Jsont.Object (smems, _) when k = "speakToAs" -> (
              match Jsont.Json.find_mem "pronouns" smems with
              | Some (_, Jsont.Object (entries, _)) ->
                  relabel "speakToAs/pronouns" entries
              | _ -> ())
          | _ -> ())
        mems
  | _ -> ());
  let rec rewrite ~map = function
    | Jsont.Object (mems, m) ->
        Jsont.Object
          ( List.map
              (fun ((k, km), v) ->
                match (map, k, v) with
                | Some map, _, _ -> ((lookup map k, km), rewrite ~map:None v)
                | None, "organizationId", Jsont.String (s, sm) ->
                    ((k, km), Jsont.String (lookup "organizations" s, sm))
                | None, "localizations", Jsont.Object (langs, lm) ->
                    ( (k, km),
                      Jsont.Object
                        ( List.map
                            (fun ((lang, lkm), patch) ->
                              match patch with
                              | Jsont.Object (entries, pm) ->
                                  ( (lang, lkm),
                                    Jsont.Object
                                      ( List.map
                                          (fun ((path, pkm), pv) ->
                                            let path =
                                              match
                                                String.split_on_char '/' path
                                              with
                                              | mp :: id :: rest
                                                when List.mem mp maps ->
                                                  String.concat "/"
                                                    (mp :: lookup mp id :: rest)
                                              | "speakToAs" :: "pronouns" :: id
                                                :: rest ->
                                                  String.concat "/"
                                                    ("speakToAs" :: "pronouns"
                                                    :: lookup
                                                         "speakToAs/pronouns" id
                                                    :: rest)
                                              | _ -> path
                                            in
                                            ((path, pkm), pv))
                                          (List.sort
                                             (fun ((a, _), _) ((b, _), _) ->
                                               compare a b)
                                             entries),
                                        pm ) )
                              | v -> ((lang, lkm), v))
                            langs,
                          lm ) )
                | None, _, Jsont.Object _ when List.mem k maps ->
                    ((k, km), rewrite ~map:(Some k) v)
                | None, "speakToAs", Jsont.Object (smems, sm) ->
                    ( (k, km),
                      Jsont.Object
                        ( List.map
                            (fun ((n, nm), v) ->
                              if n = "pronouns" then
                                ( (n, nm),
                                  rewrite ~map:(Some "speakToAs/pronouns") v )
                              else ((n, nm), v))
                            smems,
                          sm ) )
                | _ -> ((k, km), rewrite ~map:None v))
              mems,
            m )
    | Jsont.Array (vs, m) -> Jsont.Array (List.map (rewrite ~map:None) vs, m)
    | v -> v
  in
  rewrite ~map:None json

let card_json c = ok "encode" (Jsont.Json.encode Card.jsont c)

(* Two Cards are the same if they agree once localized in every language they
   hold, and with their localizations removed, so that a patch of an object
   and a patch of its properties compare equal. *)
let same a b =
  let languages (c : Card.t) =
    List.sort compare
      (List.map
         (fun (l, _) -> String.lowercase_ascii l)
         (Option.value ~default:[] c.localizations))
  in
  let base (c : Card.t) = { c with localizations = None } in
  let eq x y =
    Jsont.Json.equal (canonical (card_json x)) (canonical (card_json y))
  in
  eq (base a) (base b)
  && languages a = languages b
  && List.for_all
       (fun lang ->
         let tag (c : Card.t) =
           List.find
             (fun (l, _) -> String.lowercase_ascii l = lang)
             (Option.value ~default:[] c.localizations)
           |> fst
         in
         match
           (Card.localize a ~language:(tag a), Card.localize b ~language:(tag b))
         with
         | Ok (Some x), Ok (Some y) -> eq (base x) (base y)
         | _ -> false)
       (languages a)

let show c =
  match encode Card.jsont c with
  | Ok s -> s
  | Error e -> Jsont.Error.to_string e

type block = { title : string; kind : [ `Test | `Convert ]; body : string }

let blocks s =
  let lines = String.split_on_char '\n' s in
  let rec go acc cur = function
    | [] -> List.rev (match cur with Some b -> b :: acc | None -> acc)
    | l :: rest when String.starts_with ~prefix:"> " l ->
        let acc = match cur with Some b -> b :: acc | None -> acc in
        let kind, title =
          if String.starts_with ~prefix:"> test" l then
            (`Test, String.trim (String.sub l 6 (String.length l - 6)))
          else (`Convert, "")
        in
        go acc (Some { title; kind; body = "" }) rest
    | l :: rest -> (
        match cur with
        | Some b -> go acc (Some { b with body = b.body ^ l ^ "\n" }) rest
        | None -> go acc None rest)
  in
  go [] None lines

let is_json body =
  match
    List.find_opt
      (fun l -> String.trim l <> "")
      (String.split_on_char '\n' body)
  with
  | Some l ->
      let c = (String.trim l).[0] in
      c = '"' || c = '{'
  | None -> false

(* Groups a test block with the convert blocks that follow it. *)
let cases s =
  let rec go acc = function
    | [] -> List.rev acc
    | ({ kind = `Test; _ } as t) :: rest ->
        let rec converts cs = function
          | ({ kind = `Convert; _ } as c) :: rest -> converts (c :: cs) rest
          | rest -> (List.rev cs, rest)
        in
        let cs, rest = converts [] rest in
        go ((t, cs) :: acc) rest
    | _ :: rest -> go acc rest
  in
  go [] (blocks s)

let corpus_files () =
  let dir = "../corpus/data" in
  List.sort compare
    (List.filter
       (fun f -> Filename.check_suffix f ".txt")
       (Array.to_list (Sys.readdir dir)))
  |> List.map (fun f -> (f, read_file (Filename.concat dir f)))

let uid () = "corpus"

(* The fixtures whose calcard reading differs from this library's reading of
   RFC 9555. Each is a known divergence, not a defect.

   - Multiple FN: of several FN properties without LANGUAGE, the one with the
     fewest parameters converts, per Section 2.5.2; calcard takes the first.
   - GEO TZ, ADR with GEO and TZ parameters, ADR with GEO and TZ parameters
     and properties, TZ #2, Full card #3: a TZ property without
     VALUE=utc-offset is text, per RFC 6350 Section 6.5.1, and converts
     verbatim; a TZ parameter that is an offset converts to an Etc/GMT name
     as Section 2.8.2 says. calcard does the reverse.
   - Full card #3: an ANNIVERSARY of type DATE-TIME does not convert, per
     Section 2.2.2, and a GENDER is structured text in jCard.
   - PHONETIC, PHONETIC with PROP-ID, N ALTID: a phonetic N in another
     language localizes the phonetic properties of each component, as Figure
     5 of RFC 9555 shows; calcard replaces the components.
   - ROLE, GRAMGENDER: a lone property in the dominant language sets the
     Card's language, as Figure 3 of Section 2.3.11 shows; calcard keeps the
     parameter.
   - SOCIALPROFILE: a TEXT value is the user, per Section 2.7.5; calcard puts
     it in the uri.
   - N FN: an FN marked DERIVED is not converted, as Section 2.3.7 allows, so
     that a Card converted to a vCard and back is unchanged. *)
let import_divergences =
  [
    "003_names.txt: Multiple FN";
    "005_address.txt: RFC9555 GEO TZ";
    "005_address.txt: ADR with GEO and TZ parameters";
    "005_address.txt: ADR with GEO and TZ parameters and properties";
    "013_localizations.txt: RFC9555 PHONETIC";
    "013_localizations.txt: RFC9555 PHONETIC with PROP-ID";
    "015_rfc6350.txt: RFC6350 ROLE";
    "015_rfc6350.txt: RFC6350 TZ #2";
    "015_rfc6350.txt: RFC6350 Full card #3";
    "017_rfc9554.txt: RFC9554 GRAMGENDER";
    "017_rfc9554.txt: RFC9554 SOCIALPROFILE";
    "017_rfc9554.txt: RFC9554 N FN";
    "017_rfc9554.txt: RFC9554 N ALTID";
  ]

(* The Cards that do not survive a trip through vCard unchanged. Both hold
   the timeZone "+0100", which is not an IANA name, and its TZ parameter
   re-reads as "Etc/GMT-1" under Section 2.8.2. *)
let roundtrip_divergences =
  [
    "005_address.txt: ADR with GEO and TZ parameters";
    "005_address.txt: ADR with GEO and TZ parameters and properties";
  ]

let check_divergences ~expected failed =
  let names = List.sort compare failed in
  Alcotest.(check (list string))
    "known divergences"
    (List.sort compare expected)
    names

let test_import_corpus () =
  let total = ref 0 and failed = ref [] in
  List.iter
    (fun (file, s) ->
      List.iter
        (fun (t, converts) ->
          match converts with
          | expected :: _ when (not (is_json t.body)) && is_json expected.body
            -> (
              incr total;
              let name = file ^ ": " ^ t.title in
              match
                ( Jscontact_vcard.of_vcard ~uid (vcard_of_fragment t.body),
                  card_of_fragment expected.body )
              with
              | Ok mine, Ok theirs ->
                  if not (same mine theirs) then
                    failed := (name, show mine, show theirs) :: !failed
              | Error msg, _ -> failed := (name, "error: " ^ msg, "") :: !failed
              | _, Error msg ->
                  failed := (name, "fixture: " ^ msg, "") :: !failed)
          | _ -> ())
        (cases s))
    (corpus_files ());
  List.iter
    (fun (n, a, b) ->
      Printf.printf "MISMATCH %s\n  mine:   %s\n  theirs: %s\n" n a b)
    (List.rev !failed);
  Printf.printf "import corpus: %d cases, %d mismatches\n%!" !total
    (List.length !failed);
  Alcotest.(check bool) "cases" true (!total > 100);
  check_divergences ~expected:import_divergences
    (List.map (fun (n, _, _) -> n) !failed)

let test_roundtrip_corpus () =
  let total = ref 0 and failed = ref [] in
  let check name c =
    incr total;
    match Jscontact_vcard.to_vcard c with
    | Error msg -> failed := (name, "export: " ^ msg) :: !failed
    | Ok v -> (
        match
          Jscontact_vcard.of_vcard ~uid
            (ok "reparse" (Vcard.one_of_string (Vcard.to_string v)))
        with
        | Error msg ->
            failed :=
              (name, "import: " ^ msg ^ "\n" ^ Vcard.to_string v) :: !failed
        | Ok c' ->
            if not (same c c') then
              failed :=
                ( name,
                  Printf.sprintf "%s\n  via:  %s\n  back: %s" (show c)
                    (Vcard.to_string v) (show c') )
                :: !failed)
  in
  List.iter
    (fun (file, s) ->
      List.iter
        (fun (t, converts) ->
          let name = file ^ ": " ^ t.title in
          if is_json t.body then
            match card_of_fragment t.body with
            | Ok c -> check name c
            | Error _ -> ()
          else
            match converts with
            | expected :: _ when is_json expected.body -> (
                match card_of_fragment expected.body with
                | Ok c -> check name c
                | Error _ -> ())
            | _ -> ())
        (cases s))
    (corpus_files ());
  List.iter
    (fun (n, d) -> Printf.printf "ROUNDTRIP %s\n  %s\n" n d)
    (List.rev !failed);
  Printf.printf "roundtrip corpus: %d cases, %d failures\n%!" !total
    (List.length !failed);
  Alcotest.(check bool) "cases" true (!total > 100);
  check_divergences ~expected:roundtrip_divergences (List.map fst !failed)

let test_examples () =
  let v =
    vcard_of_fragment
      "N;SORT-AS=\"Stevenson,John \
       Philip\":Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.;;Jr."
  in
  let c = ok "n" (Jscontact_vcard.of_vcard ~uid v) in
  let n = Option.get c.name in
  Alcotest.(check int) "components" 8 (List.length (Option.get n.components));
  Alcotest.(check bool)
    "sortAs" true
    (n.sort_as = Some [ (`Surname, "Stevenson"); (`Given, "John Philip") ]);
  let v =
    vcard_of_fragment
      "TEL;VALUE=uri;PREF=1;TYPE=\"voice,home\":tel:+1-555-555-5555;ext=5555"
  in
  let c = ok "tel" (Jscontact_vcard.of_vcard ~uid v) in
  let _, p = List.hd (Option.get c.phones) in
  Alcotest.(check bool) "contexts" true (p.contexts = Some [ `Private ]);
  Alcotest.(check bool) "features" true (p.features = Some [ `Voice ]);
  Alcotest.(check (option int)) "pref" (Some 1) p.pref;
  let v = vcard_of_fragment "JSPROP;JSPTR=\"example.com:foo\":{\"bar\":1234}" in
  let c = ok "jsprop" (Jscontact_vcard.of_vcard ~uid v) in
  Alcotest.(check bool)
    "vendor property" true
    (Unknown.find c.unknown "example.com:foo" <> None);
  let back = ok "export" (Jscontact_vcard.to_vcard c) in
  Alcotest.(check bool) "jsprop back" true (Vcard.find back "JSPROP" <> None)

(* Regressions for the review against the RFCs. *)
let test_review () =
  let conv s = ok s (Jscontact_vcard.of_vcard ~uid (vcard_of_fragment s)) in
  let back c = ok "export" (Jscontact_vcard.to_vcard c) in
  let roundtrip name c =
    let c' =
      ok name
        (Jscontact_vcard.of_vcard ~uid
           (ok name (Vcard.one_of_string (Vcard.to_string (back c)))))
    in
    Alcotest.(check bool) name true (same c c')
  in
  (* An unconverted TYPE value stays a parameter, Section 2.3.22. *)
  let c = conv "EMAIL;TYPE=work,x-custom:test@example.com" in
  let _, e = List.hd (Option.get c.emails) in
  (match Unknown.find e.unknown "vCardParams" with
  | Some (Jsont.Object (mems, _)) ->
      Alcotest.(check bool)
        "x-custom kept" true
        (Jsont.Json.find_mem "type" mems <> None)
  | _ -> Alcotest.fail "vCardParams missing");
  let v = back c in
  Alcotest.(check (list string))
    "type back" [ "work"; "x-custom" ]
    (Vcard.Property.types (Option.get (Vcard.find v "EMAIL")));
  (* A multi-valued NICKNAME with a PROP-ID names the first value alone. *)
  let c = conv "NICKNAME;PROP-ID=n1:Jim,Jimmie" in
  Alcotest.(check int) "two nicknames" 2 (List.length (Option.get c.nicknames));
  (* VALUE=date is a PartialDate, Section 2.2.2. *)
  let c = conv "BDAY;VALUE=date:19960415" in
  Alcotest.(check int) "bday" 1 (List.length (Option.get c.anniversaries));
  (* A JSCOMPS whose positionals do not number the values is ignored. *)
  let c = conv "N;JSCOMPS=\";1\":Doe;Jane;;;;;" in
  Alcotest.(check bool) "not ordered" false (Option.get c.name).is_ordered;
  (* A phonetic ADR in the base language pronounces its components, and a
     phonetic N before the plain N still does. *)
  let c =
    conv "N;ALTID=1;PHONETIC=ipa:/smɪθ/;/dʒɑːn/;;;;;\nN;ALTID=1:Smith;John;;;;;"
  in
  let n = Option.get c.name in
  Alcotest.(check (option string))
    "n phonetic" (Some "/dʒɑːn/")
    (List.nth (Option.get n.components) 1).phonetic;
  let c =
    conv
      "ADR;ALTID=1:;;Main St;Town;;;;;;;;;;;;;;\n\
       ADR;ALTID=1;PHONETIC=ipa:;;/meɪn/;/taʊn/;;;;;;;;;;;;;;"
  in
  let _, a = List.hd (Option.get c.addresses) in
  Alcotest.(check bool)
    "adr phonetic" true
    (List.exists
       (fun (x : Address.Component.t) -> x.phonetic = Some "/taʊn/")
       (Option.get a.components));
  Alcotest.(check bool) "adr system" true (a.phonetic_system = Some `Ipa);
  roundtrip "phonetic adr" c;
  (* A TZ parameter that is an offset is an Etc/GMT zone, Section 2.3.23. *)
  let c = conv "ADR;TZ=-0500:;;1 St;Town;;;;;;;;;;;;;;" in
  let _, a = List.hd (Option.get c.addresses) in
  Alcotest.(check (option string)) "tz param" (Some "Etc/GMT+5") a.time_zone;
  roundtrip "tz param" c;
  (* Grouped GEO and TZ join one address, Section 2.8.3. *)
  let c = conv "g1.GEO:geo:1,2\ng1.TZ;VALUE=utc-offset:-0500" in
  Alcotest.(check int) "one address" 1 (List.length (Option.get c.addresses));
  (* UID as text is unescaped, RFC 6350 Section 6.7.6. *)
  let c = conv "UID;VALUE=text:foo\\,bar" in
  Alcotest.(check string) "uid text" "foo,bar" c.uid;
  (* A grouped X-ABLABEL that labels nothing survives in vCardProps. *)
  let c = conv "g1.X-ABLabel:Lonely" in
  Alcotest.(check bool)
    "label kept" true
    (Unknown.find c.unknown "vCardProps" <> None);
  (* FN: the one with the fewest parameters is the name, Section 2.5.2. *)
  let c = conv "FN;X-A=1:Long\nFN:Short" in
  Alcotest.(check (option string))
    "fewest params" (Some "Short") (Option.get c.name).full;
  (* A localized email round-trips as a LANGUAGE variant. *)
  let c =
    Card.make ~language:"en"
      ~emails:
        [
          (Id.v "e1", Contact.Email_address.make ~label:"Work" "a@example.com");
        ]
      ~localizations:
        [
          ( "fr",
            Patch.v
              [ ("emails/e1/label", Patch.Set (Jsont.Json.string "Travail")) ]
          );
        ]
      "u1"
  in
  let v = back c in
  Alcotest.(check int) "two emails" 2 (List.length (Vcard.find_all v "EMAIL"));
  roundtrip "localized email" c;
  (* A phonetic name without localizations still links its N pair by ALTID. *)
  let c =
    Card.make
      ~name:
        (Name.make ~phonetic_system:`Ipa
           ~components:
             [ Name.Component.make ~phonetic:"/smɪθ/" `Surname "Smith" ]
           ())
      "u2"
  in
  let v = back c in
  Alcotest.(check int) "two N" 2 (List.length (Vcard.find_all v "N"));
  Alcotest.(check bool)
    "altid" true
    (List.for_all
       (fun p -> Vcard.Property.altid p <> None)
       (Vcard.find_all v "N"));
  roundtrip "phonetic name" c;
  (* Unknown members of nested objects become JSPROPs. *)
  let c =
    Card.make
      ~notes:
        [
          ( Id.v "n1",
            Info.Note.make
              ~author:
                (Info.Note.Author.make ~name:"A"
                   ~unknown:
                     (Unknown.of_list
                        [ ("example.com:x", Jsont.Json.bool true) ])
                   ())
              "note" );
        ]
      "u3"
  in
  let v = back c in
  Alcotest.(check (option string))
    "author jsprop" (Some "notes/n1/author/example.com:x")
    (Option.bind (Vcard.find v "JSPROP") (fun p ->
         Vcard.Property.find_first p "JSPTR"));
  roundtrip "author unknown" c;
  (* jCard: unknown values stay raw, booleans and numbers are JSON values. *)
  let j = Jscontact_vcard.Jcard.of_property (Vcard.Property.v "X-FOO" "a,b") in
  (match j with
  | Jsont.Array
      ([ _; _; Jsont.String ("unknown", _); Jsont.String ("a,b", _) ], _) ->
      ()
  | _ -> Alcotest.fail "unknown raw");
  let p = ok "back" (Jscontact_vcard.Jcard.to_property j) in
  Alcotest.(check string) "raw back" "a,b" (Vcard.Property.value p);
  let j =
    Jscontact_vcard.Jcard.of_property
      (Vcard.Property.v
         ~params:[ Vcard.Param.v "VALUE" [ "boolean" ] ]
         "X-B" "true")
  in
  match j with
  | Jsont.Array ([ _; _; _; Jsont.Bool (true, _) ], _) -> ()
  | _ -> Alcotest.fail "boolean json"

let () =
  Alcotest.run "jscontact-vcard"
    [
      ( "conv",
        [
          Alcotest.test_case "RFC 9555 examples" `Quick test_examples;
          Alcotest.test_case "review regressions" `Quick test_review;
          Alcotest.test_case "calcard import corpus" `Quick test_import_corpus;
          Alcotest.test_case "calcard round trips" `Quick test_roundtrip_corpus;
        ] );
    ]
