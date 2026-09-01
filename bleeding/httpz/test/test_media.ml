module Media = Httpz.Media

let csv =
  Media.of_strings
    "text/csv"
    ~params:[ "charset", "utf-8" ]
    ~encode:(fun fields -> String.concat "," fields)
    ~decode:(fun s -> if s = "" then Error "empty" else Ok (String.split_on_char ',' s))
;;

let check_bool = Alcotest.(check bool)
let check_str = Alcotest.(check string)

let test_round_trip () =
  check_str "encode" "a,b" (Media.encode csv [ "a"; "b" ]);
  match Media.decode csv "x,y" with
  | Ok v -> Alcotest.(check (list string)) "decode" [ "x"; "y" ] v
  | Error e -> Alcotest.fail (Media.error_to_string e)
;;

let test_types () =
  check_str "media_type" "text/csv" (Media.media_type csv);
  check_str "content_type" "text/csv; charset=utf-8" (Media.content_type csv);
  check_str "text" "text/plain; charset=utf-8" (Media.content_type Media.text);
  check_str
    "folded"
    "application/json"
    (Media.media_type
       (Media.of_strings "Application/JSON" ~encode:Fun.id ~decode:Result.ok));
  let quoted value =
    Media.encoder ~params:[ "note", value ] "text/plain" Fun.id
    |> Media.content_type
  in
  check_str "parameter space is quoted" "text/plain; note=\"a b\""
    (quoted "a b");
  check_str "parameter delimiter is quoted" "text/plain; note=\"a;b\""
    (quoted "a;b");
  check_str "parameter quote and slash are escaped"
    "text/plain; note=\"a\\\"b\\\\c\"" (quoted "a\"b\\c");
  check_str "empty parameter is quoted" "text/plain; note=\"\"" (quoted "")
;;

let test_accepts () =
  check_bool "exact" true (Media.accepts csv (Some "text/csv"));
  check_bool "params ignored" true (Media.accepts csv (Some "text/csv; header=present"));
  check_bool "case" true (Media.accepts csv (Some "Text/CSV"));
  check_bool "other" false (Media.accepts csv (Some "text/plain"));
  check_bool "absent" false (Media.accepts csv None);
  check_bool "octets absent" true (Media.accepts Media.octets None);
  check_bool "octets any" true (Media.accepts Media.octets (Some "image/png"));
  let j =
    Media.of_strings
      ~accept:[ "application/*+json"; "text/*" ]
      "application/json"
      ~encode:Fun.id
      ~decode:Result.ok
  in
  check_bool "suffix" true (Media.accepts j (Some "application/vnd.api+json"));
  check_bool "suffix miss" false (Media.accepts j (Some "application/xml"));
  check_bool "type wildcard" true (Media.accepts j (Some "text/anything"))
;;

let test_matches () =
  check_bool "star" true (Media.matches ~range:"*/*" "text/html");
  check_bool "subtype star" true (Media.matches ~range:"text/*" "text/html");
  check_bool "subtype star miss" false (Media.matches ~range:"text/*" "application/json");
  check_bool
    "exact with params"
    true
    (Media.matches ~range:"text/html; q=0.5" "text/html; charset=utf-8");
  check_bool
    "folded and padded"
    true
    (Media.matches
       ~range:" APPLICATION/*+JSON ; q=0.5 "
       "application/vnd.example+json; charset=utf-8");
  check_bool
    "suffix longer than subtype"
    false
    (Media.matches ~range:"application/*+json" "application/a");
  check_bool "garbage" false (Media.matches ~range:"nonsense" "text/html")
;;

let test_release_syntax () =
  List.iter (fun range -> check_bool ("invalid range " ^ range) false
    (Media.matches ~range "text/html"))
    ["\rtext/html"; "text/html\n"; "text/html\012"; "*/html";
     "te*xt/html"; "text/h*"; "text/html; ignored=\r"];
  let fields = List.init 1000 (fun i -> Printf.sprintf "application/x-%d" i) in
  let value = Media.accept_header fields in
  let entries = String.split_on_char ',' value in
  let qualities = List.mapi (fun i entry -> if i = 0 then 1.
    else match String.index_opt entry '=' with
    | Some pos -> float_of_string (String.sub entry (pos + 1) (String.length entry - pos - 1))
    | None -> failwith "missing generated quality") entries in
  let rec descending = function a :: (b :: _ as rest) -> a > b && descending rest
    | [last] -> last > 0. | [] -> true in
  check_bool "1000 strict preferences" true (descending qualities);
  Alcotest.check_raises "preference bound"
    (Invalid_argument "Media.accept_header: at most 1000 preferences")
    (fun () -> ignore (Media.accept_header ("text/plain" :: fields)))

let test_shared_syntax () =
  List.iter
    (fun (range, media, expected) ->
      let padded = "prefix" ^ range ^ "suffix" in
      let actual =
        Media.Syntax.specificity ~range:padded ~pos:6 ~len:(String.length range) media
      in
      Alcotest.(check int) (range ^ " -> " ^ media) expected actual;
      check_bool "whole-string matcher agrees" (expected >= 0)
        (Media.matches ~range media))
    [ "*/*", "text/plain", 0;
      "text/*", "text/plain", 1;
      "TEXT/PLAIN", "text/plain; charset=utf-8", 2;
      "application/*+json", "application/problem+json", 2;
      "application/*+json", "application/json", -1;
      "*/plain", "text/plain", -1;
      "text/p*", "text/plain", -1;
      "text/*", "text/*", -1;
      "text/plain\r", "text/plain", -1 ];
  List.iter
    (fun (pos, len) ->
      check_bool "slice bounds" false
        (Media.Syntax.valid_range "text/plain" ~pos ~len);
      Alcotest.(check int) "match slice bounds" (-1)
        (Media.Syntax.specificity ~range:"text/plain" ~pos ~len "text/plain"))
    [ -1, 1; 0, -1; max_int, 1; 0, max_int; 9, 2 ];
  check_bool "empty slice" false (Media.Syntax.valid_type "" ~pos:0 ~len:0);
  check_bool "type rejects wildcard" false
    (Media.Syntax.valid_type "text/*" ~pos:0 ~len:6)

let test_accept_header () =
  check_str "one" "text/csv" (Media.accept_header [ "text/csv" ]);
  check_str
    "three"
    "text/csv, text/plain;q=0.9, text/html;q=0.8"
    (Media.accept_header
       [ Media.media_type csv; Media.media_type Media.text; Media.media_type Media.html ])
;;

let test_one_way () =
  let enc = Media.encoder "text/html" (fun n -> string_of_int n) in
  let dec = Media.decoder "text/plain" (fun s -> Ok (String.length s)) in
  check_bool "encoder" true (Media.can_encode enc && not (Media.can_decode enc));
  check_bool "decoder" true (Media.can_decode dec && not (Media.can_encode dec));
  check_str "enc" "42" (Media.encode enc 42);
  Alcotest.check_raises
    "decode with encoder"
    (Invalid_argument "Media: text/html cannot decode")
    (fun () -> ignore (Media.decode enc "x"));
  Alcotest.check_raises
    "encode with decoder"
    (Invalid_argument "Media: text/plain cannot encode")
    (fun () -> ignore (Media.encode dec 1))
;;

let test_invalid () =
  let bad what f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.failf "%s accepted" what
  in
  bad "no slash" (fun () -> Media.encoder "json" Fun.id);
  bad "bad accept" (fun () -> Media.decoder ~accept:[ "nope" ] "text/plain" Result.ok);
  bad "bad param" (fun () ->
    Media.encoder ~params:[ "char set", "x" ] "text/plain" Fun.id)
;;

let test_map () =
  let ints =
    Media.map
      csv
      ~decode:(fun fields ->
        match List.map int_of_string_opt fields |> List.for_all Option.is_some with
        | true -> Ok (List.filter_map int_of_string_opt fields)
        | false -> Error "not all numbers")
      ~encode:(fun ints -> List.map string_of_int ints)
  in
  check_str "encode" "1,2" (Media.encode ints [ 1; 2 ]);
  (match Media.decode ints "3,4" with
   | Ok v -> Alcotest.(check (list int)) "decode" [ 3; 4 ] v
   | Error e -> Alcotest.fail (Media.error_to_string e));
  match Media.decode ints "3,x" with
  | Error (Media.Malformed { message = "not all numbers"; _ }) -> ()
  | _ -> Alcotest.fail "expected malformed"
;;

let test_lines () =
  let rows = Media.lines ~accept:[ "text/x-rows" ] "text/rows" csv in
  check_str "type" "text/rows" (Media.seq_media_type rows);
  check_bool "accepts" true (Media.seq_accepts rows (Some "text/x-rows"));
  check_str "item" "a,b\n" (Media.encode_item rows [ "a"; "b" ]);
  check_str
    "items"
    "a\nb,c\n"
    (Media.encode_items rows (List.to_seq [ [ "a" ]; [ "b"; "c" ] ]));
  (match Media.decode_items rows "a,b\r\n\nc\n" with
   | Ok v -> Alcotest.(check (list (list string))) "decode" [ [ "a"; "b" ]; [ "c" ] ] v
   | Error e -> Alcotest.fail (Media.error_to_string e));
  match Media.decode_items rows "a\n \nb" with
  | Ok v -> Alcotest.(check (list (list string))) "blank skipped" [ [ "a" ]; [ "b" ] ] v
  | Error e -> Alcotest.fail (Media.error_to_string e)
;;

let test_errors () =
  check_str
    "unsupported"
    "unsupported media type \"text/x\""
    (Media.error_to_string (Media.Unsupported (Some "text/x")));
  check_str "unsupported value is escaped"
    "unsupported media type \"text/x\\nforged\""
    (Media.error_to_string (Media.Unsupported (Some "text/x\nforged")));
  check_str "none" "no media type given" (Media.error_to_string (Media.Unsupported None));
  check_str
    "malformed"
    "malformed body: why"
    (Media.error_to_string (Media.Malformed (Media.malformed "why")));
  let loc =
    Media.Loc.v ~first_byte:9 ~last_byte:11 ~first_line:(2, 5) ~last_line:(2, 5)
  in
  check_str "location" "line 2, columns 5-7" (Fmt.str "%a" Media.Loc.pp loc);
  check_str
    "located malformed"
    "malformed body: at line 2, columns 5-7: why"
    (Media.error_to_string (Media.Malformed (Media.malformed ~loc "why")));
  check_str
    "too large"
    "body exceeds the 12-byte limit"
    (Media.error_to_string (Media.Too_large 12));
  check_str
    "terminal controls"
    "before\\x1B]52;c;secret\\x07after\\u{009B}red"
    (Media.sanitize_diagnostic
       "before\x1b]52;c;secret\x07after\xc2\x9bred");
  check_str
    "printable UTF-8 is preserved"
    "snowman \xe2\x98\x83"
    (Media.sanitize_diagnostic "snowman \xe2\x98\x83")
;;

let () =
  Alcotest.run
    "media"
    [ ( "codec"
      , [ Alcotest.test_case "round trip" `Quick test_round_trip
        ; Alcotest.test_case "types" `Quick test_types
        ; Alcotest.test_case "accepts" `Quick test_accepts
        ; Alcotest.test_case "matches" `Quick test_matches
        ; Alcotest.test_case "accept header" `Quick test_accept_header
        ; Alcotest.test_case "release syntax" `Quick test_release_syntax
        ; Alcotest.test_case "shared syntax" `Quick test_shared_syntax
        ; Alcotest.test_case "one way" `Quick test_one_way
        ; Alcotest.test_case "invalid" `Quick test_invalid
        ; Alcotest.test_case "map" `Quick test_map
        ; Alcotest.test_case "lines" `Quick test_lines
        ; Alcotest.test_case "errors" `Quick test_errors
        ] )
    ]
;;
