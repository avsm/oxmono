module Media = Httpz.Media
module U = Httpz.Urlencoded

let check_str = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let pairs = Alcotest.(list (pair string string))
let check_pairs = Alcotest.check pairs

(* The urlencoded byte serializer of the WHATWG URL Standard. *)
let test_encode () =
  check_str "empty" "" (U.encode []);
  check_str "simple" "a=b&c=d" (U.encode [ ("a", "b"); ("c", "d") ]);
  check_str "space" "k=a+b" (U.encode [ ("k", "a b") ]);
  check_str "tilde and star" "k=%7E*" (U.encode [ ("k", "~*") ]);
  check_str "delimiters" "k=%26%3D%25%2B" (U.encode [ ("k", "&=%+") ]);
  check_str "utf8" "k=%C3%A9" (U.encode [ ("k", "\xc3\xa9") ]);
  check_str "empty value" "k=" (U.encode [ ("k", "") ]);
  check_str "empty name" "=v" (U.encode [ ("", "v") ]);
  check_str "repeated" "k=1&k=2" (U.encode [ ("k", "1"); ("k", "2") ]);
  check_str "name escaped" "a+b=c" (U.encode [ ("a b", "c") ]);
  check_str "unreserved" "-._*=-._*" (U.encode [ ("-._*", "-._*") ]);
  check_str "control" "k=%00%0D%0A" (U.encode [ ("k", "\x00\r\n") ]);
  check_str "high byte" "k=%FF" (U.encode [ ("k", "\xff") ]);
  check_str "slash" "k=%2Fa" (U.encode [ ("k", "/a") ])

let test_decode () =
  check_pairs "empty" [] (U.decode "");
  check_pairs "simple" [ ("a", "b"); ("c", "d") ] (U.decode "a=b&c=d");
  check_pairs "plus" [ ("k", "a b") ] (U.decode "k=a+b");
  check_pairs "escapes" [ ("k", "&=%+") ] (U.decode "k=%26%3D%25%2B");
  check_pairs "utf8" [ ("k", "\xc3\xa9") ] (U.decode "k=%c3%a9");
  check_pairs "lowercase hex" [ ("k", "\xff") ] (U.decode "k=%ff");
  check_pairs "empty value" [ ("k", "") ] (U.decode "k=");
  check_pairs "repeated" [ ("k", "1"); ("k", "2") ] (U.decode "k=1&k=2");
  check_pairs "empty sequences and no equals"
    [ ("a", "1"); ("b", "2"); ("c", "") ]
    (U.decode "a=1&&b=2&c");
  check_pairs "leading and trailing amps" [ ("a", "1") ] (U.decode "&&a=1&&");
  check_pairs "second equals is data" [ ("a", "b=c") ] (U.decode "a=b=c");
  check_pairs "escaped equals in name" [ ("a=b", "c") ] (U.decode "a%3Db=c");
  check_pairs "name only escaped" [ ("a b", "") ] (U.decode "a+b")

(* An escape that is not two hexadecimal digits, or is cut short by the end of
   the sequence, is data. *)
let test_decode_bad_escapes () =
  check_pairs "invalid and truncated" [ ("%zz %4", "") ] (U.decode "%zz+%4");
  check_pairs "trailing percent" [ ("k", "a%") ] (U.decode "k=a%");
  check_pairs "one digit" [ ("k", "%4") ] (U.decode "k=%4");
  check_pairs "escape split by amp" [ ("k", "%4"); ("1", "") ] (U.decode "k=%4&1");
  check_pairs "percent then escape" [ ("k", "%\xff") ] (U.decode "k=%%ff")

let round_trip =
  [ [];
    [ ("a", "b") ];
    [ ("", "") ];
    [ ("k", "") ];
    [ ("", "v") ];
    [ ("k", "1"); ("k", "2"); ("j", "3") ];
    [ ("a b", "c d") ];
    [ ("&", "="); ("%", "+") ];
    [ ("\xc3\xa9", "\xe2\x82\xac") ];
    [ ("\x00\r\n\t", "\x7f\xff") ];
    [ ("~*-._", "~*-._") ];
    [ ("a=b", "c&d") ];
    [ ("q", String.init 256 Char.chr) ] ]

let test_round_trip () =
  List.iteri
    (fun i l ->
      Alcotest.check pairs (Printf.sprintf "round trip %d" i) l
        (U.decode (U.encode l)))
    round_trip

let test_media () =
  check_str "content type" "application/x-www-form-urlencoded"
    (Media.content_type Media.form);
  check_bool "accepts" true
    (Media.accepts Media.form
       (Some "application/x-www-form-urlencoded; charset=UTF-8"));
  check_bool "accepts folded" true
    (Media.accepts Media.form (Some "Application/X-WWW-Form-Urlencoded"));
  check_bool "rejects other" false (Media.accepts Media.form (Some "text/plain"));
  check_bool "rejects absent" false (Media.accepts Media.form None);
  check_str "encode" "a=1&b=x+y"
    (Media.encode Media.form [ ("a", "1"); ("b", "x y") ]);
  match Media.decode Media.form "a=1&b=x+y" with
  | Ok v -> check_pairs "decode" [ ("a", "1"); ("b", "x y") ] v
  | Error e -> Alcotest.fail (Media.error_to_string e)

let () =
  Alcotest.run "urlencoded"
    [ ( "codec",
        [ Alcotest.test_case "encode" `Quick test_encode;
          Alcotest.test_case "decode" `Quick test_decode;
          Alcotest.test_case "bad escapes" `Quick test_decode_bad_escapes;
          Alcotest.test_case "round trip" `Quick test_round_trip;
          Alcotest.test_case "media" `Quick test_media ] ) ]
