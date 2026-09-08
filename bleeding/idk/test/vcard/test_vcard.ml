(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ok name = function
  | Ok v -> v
  | Error msg -> Alcotest.failf "%s: %s" name msg

let fails name = function
  | Ok _ -> Alcotest.failf "%s: should fail" name
  | Error _ -> ()

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let test_text () =
  let module T = Vcard.Text in
  Alcotest.(check string)
    "unescape" "a,b;c\\d\ne"
    (T.unescape "a\\,b\\;c\\\\d\\ne");
  Alcotest.(check string) "unknown escape kept" "a\\xb" (T.unescape "a\\xb");
  Alcotest.(check string) "escape" "a\\,b;c\\\\d\\ne" (T.escape "a,b;c\\d\ne");
  Alcotest.(check string) "escape component" "a\\;b" (T.escape_component "a;b");
  Alcotest.(check (list string))
    "list" [ "Jim"; "Jimmie,Jr" ]
    (T.list_of_string "Jim,Jimmie\\,Jr");
  Alcotest.(check (list (list string)))
    "structured"
    [
      [ "Stevenson" ];
      [ "John" ];
      [ "Philip"; "Paul" ];
      [ "Dr." ];
      [ "Jr."; "M.D." ];
    ]
    (T.structured_of_string "Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.");
  Alcotest.(check string)
    "roundtrip" "x\\;y\\,z\\\\w\\n"
    (T.structured_to_string (T.structured_of_string "x\\;y\\,z\\\\w\\n"))

let test_param () =
  let module P = Vcard.Param in
  Alcotest.(check string) "decode" "a\nb^c\"d" (P.decode_value "a^nb^^c^'d");
  Alcotest.(check string) "unknown caret kept" "a^xb" (P.decode_value "a^xb");
  Alcotest.(check string) "encode plain" "Bar" (P.encode_value "Bar");
  Alcotest.(check string)
    "encode quoted" "\"geo:1,2\"" (P.encode_value "geo:1,2");
  Alcotest.(check string)
    "encode escapes" "a^nb^^c^'d"
    (P.encode_value "a\nb^c\"d");
  let ps = [ P.v "type" [ "work"; "voice" ]; P.v "TYPE" [ "home" ] ] in
  Alcotest.(check (list string))
    "find_all"
    [ "work"; "voice"; "home" ]
    (P.values_named ps "Type")

let dates =
  [
    "19850412";
    "1985-04";
    "1985";
    "--0412";
    "--04";
    "---12";
    "T102200";
    "T1022";
    "T10";
    "T-2200";
    "T--00";
    "T102200Z";
    "T102200-0800";
    "19961022T140000";
    "--1022T1400";
    "---22T14";
  ]

let test_dates () =
  let module D = Vcard.Date in
  List.iter
    (fun s ->
      let d = ok s (D.of_string s) in
      Alcotest.(check string) s s (D.to_string d))
    dates;
  List.iter
    (fun s ->
      let t = ok s (D.Timestamp.of_string s) in
      Alcotest.(check string)
        s
        (if s = "19961022T140000-05" then "19961022T140000-0500" else s)
        (D.Timestamp.to_string t))
    [
      "19961022T140000";
      "19961022T140000Z";
      "19961022T140000-05";
      "19961022T140000-0500";
    ];
  fails "month 13" (D.Cal_date.of_string "19851312");
  fails "yyyymm" (D.Cal_date.of_string "198504");
  fails "hour 24" (D.Time.of_string "240000");
  fails "not a timestamp" (D.Timestamp.of_string "19961022");
  let t = ok "ts" (D.Timestamp.of_string "20090808T143000-0500") in
  let p = Option.get (D.Timestamp.to_ptime t) in
  Alcotest.(check string)
    "ptime" "2009-08-08T19:30:00Z"
    (Ptime.to_rfc3339 ~tz_offset_s:0 p);
  Alcotest.(check int)
    "offset" (-300)
    (ok "off" (D.Utc_offset.of_string "-0500"))

let test_property () =
  let module P = Vcard.Property in
  let p =
    ok "tel"
      (P.of_string
         "TEL;VALUE=uri;PREF=1;TYPE=\"voice,home\":tel:+1-555-555-5555;ext=5555")
  in
  Alcotest.(check string) "name" "TEL" (P.name p);
  Alcotest.(check (list string)) "types" [ "voice"; "home" ] (P.types p);
  Alcotest.(check (option int)) "pref" (Some 1) (P.pref p);
  Alcotest.(check bool)
    "uri" true
    (Vcard.Value_type.equal (P.value_type p) Vcard.Value_type.Uri);
  Alcotest.(check string) "value" "tel:+1-555-555-5555;ext=5555" (P.value p);
  let p2 = ok "reparse" (P.of_string (P.to_string p)) in
  Alcotest.(check bool) "fixed point" true (P.equal p p2);
  let g = ok "group" (P.of_string "item1.X-ABLabel:foo") in
  Alcotest.(check (option string)) "group" (Some "item1") (P.group g);
  Alcotest.(check string) "upper" "X-ABLABEL" (P.name g);
  let q =
    ok "6868" (P.of_string "NOTE;AUTHOR-NAME=\"_:l33tHckr:_\";X-A=a^'b:x")
  in
  Alcotest.(check (option string))
    "quoted" (Some "_:l33tHckr:_")
    (P.find_first q "author-name");
  Alcotest.(check (option string)) "caret" (Some "a\"b") (P.find_first q "X-A");
  let bare = ok "bare" (P.of_string "TEL;HOME:123") in
  Alcotest.(check (option (list string)))
    "bare param" (Some [])
    (P.find_values bare "HOME");
  Alcotest.(check bool)
    "default text" true
    (Vcard.Value_type.equal (P.value_type bare) Vcard.Value_type.Text);
  fails "no colon" (P.of_string "FN");
  fails "no name" (P.of_string ":x");
  fails "unclosed quote" (P.of_string "FN;GEO=\"urn:");
  let n = P.of_text "NOTE" "a,b\nc" in
  Alcotest.(check string) "of_text" "NOTE:a\\,b\\nc" (P.to_string n);
  Alcotest.(check string) "text" "a,b\nc" (P.text n);
  Alcotest.(check (list int64))
    "integers" [ 1L; -2L ]
    (ok "ints" (P.integers (P.v "X-I" "+1,-2")));
  Alcotest.(check (list int64))
    "64-bit" [ 9223372036854775807L ]
    (ok "big" (P.integers (P.v "X-I" "9223372036854775807")));
  Alcotest.(check bool)
    "boolean" true
    (ok "bool" (P.boolean (P.v "X-B" "True")))

let test_fold () =
  let long = String.concat "" (List.init 60 (fun _ -> "ö")) in
  let line = "NOTE:" ^ long in
  let folded = Vcard.fold line in
  List.iter
    (fun l ->
      Alcotest.(check bool) "valid utf-8" true (String.is_valid_utf_8 l);
      Alcotest.(check bool) "at most 75 octets" true (String.length l <= 75))
    (List.map
       (fun l -> if l <> "" && l.[0] = ' ' then l else l)
       (String.split_on_char '\n'
          (String.concat "\n" (String.split_on_char '\r' folded))));
  Alcotest.(check (list string)) "unfold" [ line ] (Vcard.unfold folded);
  Alcotest.(check (list string))
    "tab and lf"
    [ "FN:Mr. John Q. Public, Esq." ]
    (Vcard.unfold "FN:Mr. \n\tJohn Q. \n Public, \n Esq.")

let author =
  String.concat "\r\n"
    [
      "BEGIN:VCARD";
      "VERSION:4.0";
      "FN:Simon Perreault";
      "N:Perreault;Simon;;;ing. jr,M.Sc.";
      "BDAY:--0203";
      "ANNIVERSARY:20090808T1430-0500";
      "GENDER:M";
      "LANG;PREF=1:fr";
      "LANG;PREF=2:en";
      "ORG;TYPE=work:Viagenie";
      "ADR;TYPE=work:;Suite D2-630;2875 Laurier;";
      " Quebec;QC;G1V 2M2;Canada";
      "TEL;VALUE=uri;TYPE=\"work,voice\";PREF=1:tel:+1-418-656-9254;ext=102";
      "TEL;VALUE=uri;TYPE=\"work,cell,voice,video,text\":tel:+1-418-262-6501";
      "EMAIL;TYPE=work:simon.perreault@viagenie.ca";
      "GEO;TYPE=work:geo:46.772673,-71.282945";
      "KEY;TYPE=work;VALUE=uri:";
      " http://www.viagenie.ca/simon.perreault/simon.asc";
      "TZ:-0500";
      "URL;TYPE=home:http://nomis80.org";
      "END:VCARD";
      "";
    ]

let test_author () =
  let cards = ok "parse" (Vcard.of_string author) in
  Alcotest.(check int) "one card" 1 (List.length cards);
  let c = List.hd cards in
  let c = ok "validate" (Vcard.validate c) in
  Alcotest.(check int) "properties" 16 (List.length (Vcard.properties c));
  let n = ok "n" (Vcard.N.of_property (Option.get (Vcard.find c "N"))) in
  Alcotest.(check (list string))
    "suffixes" [ "ing. jr"; "M.Sc." ] n.Vcard.N.suffixes;
  let adr =
    ok "adr" (Vcard.Adr.of_property (Option.get (Vcard.find c "adr")))
  in
  Alcotest.(check (list string)) "locality" [ "Quebec" ] adr.Vcard.Adr.locality;
  let bday = Option.get (Vcard.find c "BDAY") in
  (match ok "bday" (Vcard.Property.date_and_or_time bday) with
  | Vcard.Date.Date { month = Some 2; day = Some 3; year = None } -> ()
  | _ -> Alcotest.fail "BDAY is --0203");
  let tz = Option.get (Vcard.find c "TZ") in
  Alcotest.(check int) "tz" (-300) (ok "tz" (Vcard.Property.utc_offset tz));
  let again = ok "reparse" (Vcard.of_string (Vcard.to_string c)) in
  Alcotest.(check bool) "fixed point" true (Vcard.equal c (List.hd again))

let test_n_adr () =
  let n =
    Vcard.N.of_value "Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.;;Jr."
  in
  Alcotest.(check (list string)) "given2" [ "Philip"; "Paul" ] n.additional;
  Alcotest.(check (list string)) "generation" [ "Jr." ] n.generation;
  Alcotest.(check string)
    "seven components" "Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.;;Jr."
    (Vcard.N.to_value n);
  Alcotest.(check string)
    "old N gets seven" "Public;John;Quinlan;Mr.;Esq.;;"
    (Vcard.N.to_value (Vcard.N.of_value "Public;John;Quinlan;Mr.;Esq."));
  let a =
    Vcard.Adr.of_value
      ";;123 Main Street;Any Town;CA;91921-1234;U.S.A;;;;123;Main Street;;;;;;"
  in
  Alcotest.(check (list string)) "street number" [ "123" ] a.street_number;
  Alcotest.(check bool) "extended" true (Vcard.Adr.has_extended a);
  Alcotest.(check bool)
    "old ADR" false
    (Vcard.Adr.has_extended
       (Vcard.Adr.of_value ";;123 Main Street;Any Town;CA;91921-1234;U.S.A."));
  Alcotest.(check int)
    "eighteen fields" 18
    (List.length (String.split_on_char ';' (Vcard.Adr.to_value a)))

let test_jscomps () =
  let module J = Vcard.Jscomps in
  let j = ok "simple" (J.of_string ";1;0") in
  Alcotest.(check bool) "no default" true (j.default_separator = None);
  Alcotest.(check int) "two" 2 (List.length j.entries);
  let j = ok "secondary" (J.of_string ";1;2;2,1;0;6;4,1") in
  Alcotest.(check bool) "2,1" true (List.nth j.entries 2 = J.Position (2, 1));
  Alcotest.(check string) "print" ";1;2;2,1;0;6;4,1" (J.to_string j);
  let j = ok "separators" (J.of_string "s,\\, ;11;s, ;10;3") in
  Alcotest.(check (option string)) "default" (Some ", ") j.default_separator;
  Alcotest.(check bool) "space" true (List.nth j.entries 1 = J.Separator " ");
  Alcotest.(check string) "print seps" "s,\\, ;11;s, ;10;3" (J.to_string j);
  fails "bad first" (J.of_string "1;2");
  fails "bad entry" (J.of_string ";x");
  fails "no entries" (J.of_string "");
  fails "separator alone" (J.of_string "s,foo");
  fails "huge index" (J.of_string ";99999999999999999999999");
  Alcotest.(check bool)
    "trailing backslash kept" true
    (List.nth (ok "trailing" (J.of_string ";s,a\\")).entries 0
    = J.Separator "a\\")

let card lines =
  String.concat "\r\n"
    (("BEGIN:VCARD" :: "VERSION:4.0" :: lines) @ [ "END:VCARD"; "" ])

let test_validate () =
  let one s = List.hd (ok "parse" (Vcard.of_string s)) in
  fails "no FN" (Vcard.validate (one (card [ "N:Doe;Jane;;;" ])));
  fails "two N" (Vcard.validate (one (card [ "FN:x"; "N:a;;;;"; "N:b;;;;" ])));
  ignore
    (ok "altid N"
       (Vcard.validate
          (one
             (card
                [
                  "FN:x";
                  "N;ALTID=1;LANGUAGE=en:a;;;;";
                  "N;ALTID=1;LANGUAGE=fr:b;;;;";
                ]))));
  fails "member" (Vcard.validate (one (card [ "FN:x"; "MEMBER:urn:uuid:1" ])));
  ignore
    (ok "group member"
       (Vcard.validate
          (one (card [ "FN:x"; "KIND:group"; "MEMBER:urn:uuid:1" ]))));
  fails "pref 0" (Vcard.validate (one (card [ "FN;PREF=0:x" ])));
  fails "prop-id" (Vcard.validate (one (card [ "FN;PROP-ID=a b:x" ])));
  fails "version"
    (Vcard.validate (one "BEGIN:VCARD\r\nVERSION:3.0\r\nFN:x\r\nEND:VCARD\r\n"));
  fails "no end" (Vcard.of_string "BEGIN:VCARD\r\nVERSION:4.0\r\nFN:x\r\n");
  fails "no version" (Vcard.of_string "BEGIN:VCARD\r\nFN:x\r\nEND:VCARD\r\n");
  fails "nothing" (Vcard.of_string "VERSION:4.0");
  fails "second VERSION" (Vcard.of_string (card [ "FN:x"; "VERSION:3.0" ]));
  fails "BEGIN with a parameter"
    (Vcard.of_string "BEGIN;X=1:VCARD\r\nVERSION:4.0\r\nFN:x\r\nEND:VCARD\r\n");
  fails "pref +1" (Vcard.validate (one (card [ "FN;PREF=+1:x" ])));
  fails "pref 007" (Vcard.validate (one (card [ "FN;PREF=007:x" ])));
  ignore (ok "pref 100" (Vcard.validate (one (card [ "FN;PREF=100:x" ]))));
  ignore (ok "pref 05" (Vcard.validate (one (card [ "FN;PREF=05:x" ]))));
  fails "pref 00" (Vcard.validate (one (card [ "FN;PREF=00:x" ])));
  fails "pid syntax" (Vcard.validate (one (card [ "FN;PID=a:x" ])));
  fails "pid on N" (Vcard.validate (one (card [ "FN:x"; "N;PID=1:a;;;;" ])));
  ignore
    (ok "pid on TEL"
       (Vcard.validate (one (card [ "FN:x"; "TEL;PID=3.1,4.2:1" ]))));
  ignore (ok "one_of_string" (Vcard.one_of_string (card [ "FN:x" ])));
  fails "one_of_string with two"
    (Vcard.one_of_string (card [ "FN:a" ] ^ card [ "FN:b" ]));
  (match Vcard.of_string "BEGIN:VCARD\r\nVERSION:4.0\r\nFN:x\r\n" with
  | Error msg ->
      Alcotest.(check bool)
        "line in error" true
        (String.starts_with ~prefix:"line 1" msg)
  | Ok _ -> Alcotest.fail "unterminated");
  Alcotest.(check int)
    "two cards" 2
    (List.length
       (ok "two" (Vcard.of_string (card [ "FN:a" ] ^ "\r\n" ^ card [ "FN:b" ]))))

let blocks s =
  let lines = String.split_on_char '\n' s in
  let flush acc buf =
    match List.rev buf with [] -> acc | b -> String.concat "\n" b :: acc
  in
  let rec go acc buf = function
    | [] -> List.rev (flush acc buf)
    | l :: rest when String.starts_with ~prefix:"> " l ->
        go (flush acc buf) [] rest
    | l :: rest -> go acc (l :: buf) rest
  in
  go [] [] lines

let test_vcard4 () =
  let n = ref 0 in
  List.iter
    (fun block ->
      let cards =
        ok
          (String.sub block 0 (min 40 (String.length block)))
          (Vcard.of_string block)
      in
      List.iter
        (fun c ->
          incr n;
          let again = ok "reparse" (Vcard.of_string (Vcard.to_string c)) in
          Alcotest.(check bool)
            "fixed point" true
            (Vcard.equal c (List.hd again)))
        cards)
    (blocks (read_file "data/vcard4.txt"));
  Alcotest.(check bool) "cards read" true (!n >= 90)

(* The calcard fixtures hold vCard fragments without BEGIN and END. Every
   content line must parse and print to a line that parses to the same
   property. *)
let is_vcard_line l =
  let l = String.trim l in
  l <> "" && ((l.[0] >= 'A' && l.[0] <= 'Z') || (l.[0] >= 'a' && l.[0] <= 'z'))

let test_calcard () =
  let dir = "../corpus/data" in
  let n = ref 0 in
  Array.iter
    (fun f ->
      if Filename.check_suffix f ".txt" then
        List.iter
          (fun block ->
            let first =
              List.find_opt
                (fun l -> String.trim l <> "")
                (String.split_on_char '\n' block)
            in
            match first with
            | Some l when is_vcard_line l ->
                List.iter
                  (fun line ->
                    if String.trim line <> "" then begin
                      incr n;
                      let p = ok line (Vcard.Property.of_string line) in
                      let q =
                        ok line
                          (Vcard.Property.of_string
                             (Vcard.Property.to_string p))
                      in
                      Alcotest.(check bool) line true (Vcard.Property.equal p q)
                    end)
                  (Vcard.unfold block)
            | _ -> ())
          (blocks (read_file (Filename.concat dir f))))
    (Sys.readdir dir);
  Alcotest.(check bool) "lines read" true (!n >= 300)

let () =
  Alcotest.run "vcard"
    [
      ( "vcard",
        [
          Alcotest.test_case "text escaping" `Quick test_text;
          Alcotest.test_case "parameters" `Quick test_param;
          Alcotest.test_case "dates" `Quick test_dates;
          Alcotest.test_case "properties" `Quick test_property;
          Alcotest.test_case "folding" `Quick test_fold;
          Alcotest.test_case "RFC 6350 Section 8" `Quick test_author;
          Alcotest.test_case "N and ADR" `Quick test_n_adr;
          Alcotest.test_case "JSCOMPS" `Quick test_jscomps;
          Alcotest.test_case "validate" `Quick test_validate;
          Alcotest.test_case "vcard4 corpus" `Quick test_vcard4;
          Alcotest.test_case "calcard corpus" `Quick test_calcard;
        ] );
    ]
