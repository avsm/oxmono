(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for the Flag_color module. *)

open Mail_flag

let color_testable = Alcotest.testable Flag_color.pp ( = )

let test_to_keywords () =
  (* Test color to keyword list conversion *)
  Alcotest.(check int)
    "red = 0 keywords" 0
    (List.length (Flag_color.to_keywords `Red));
  Alcotest.(check int)
    "orange = 1 keyword" 1
    (List.length (Flag_color.to_keywords `Orange));
  Alcotest.(check int)
    "yellow = 1 keyword" 1
    (List.length (Flag_color.to_keywords `Yellow));
  Alcotest.(check int)
    "green = 2 keywords" 2
    (List.length (Flag_color.to_keywords `Green));
  Alcotest.(check int)
    "blue = 1 keyword" 1
    (List.length (Flag_color.to_keywords `Blue));
  Alcotest.(check int)
    "purple = 2 keywords" 2
    (List.length (Flag_color.to_keywords `Purple));
  Alcotest.(check int)
    "gray = 2 keywords" 2
    (List.length (Flag_color.to_keywords `Gray));
  (* Check specific keywords *)
  Alcotest.(check bool)
    "orange has bit0" true
    (List.mem `MailFlagBit0 (Flag_color.to_keywords `Orange));
  Alcotest.(check bool)
    "yellow has bit1" true
    (List.mem `MailFlagBit1 (Flag_color.to_keywords `Yellow));
  Alcotest.(check bool)
    "blue has bit2" true
    (List.mem `MailFlagBit2 (Flag_color.to_keywords `Blue));
  Alcotest.(check bool)
    "green has bit0" true
    (List.mem `MailFlagBit0 (Flag_color.to_keywords `Green));
  Alcotest.(check bool)
    "green has bit1" true
    (List.mem `MailFlagBit1 (Flag_color.to_keywords `Green));
  Alcotest.(check bool)
    "purple has bit0" true
    (List.mem `MailFlagBit0 (Flag_color.to_keywords `Purple));
  Alcotest.(check bool)
    "purple has bit2" true
    (List.mem `MailFlagBit2 (Flag_color.to_keywords `Purple));
  Alcotest.(check bool)
    "gray has bit1" true
    (List.mem `MailFlagBit1 (Flag_color.to_keywords `Gray));
  Alcotest.(check bool)
    "gray has bit2" true
    (List.mem `MailFlagBit2 (Flag_color.to_keywords `Gray))

let test_of_keywords () =
  (* Test keyword list to color conversion *)
  (* Empty list returns None (ambiguous: no color vs Red) *)
  Alcotest.(check (option color_testable))
    "empty = None" None
    (Flag_color.of_keywords []);
  Alcotest.(check (option color_testable))
    "bit0 = orange" (Some `Orange)
    (Flag_color.of_keywords [ `MailFlagBit0 ]);
  Alcotest.(check (option color_testable))
    "bit1 = yellow" (Some `Yellow)
    (Flag_color.of_keywords [ `MailFlagBit1 ]);
  Alcotest.(check (option color_testable))
    "bit2 = blue" (Some `Blue)
    (Flag_color.of_keywords [ `MailFlagBit2 ]);
  Alcotest.(check (option color_testable))
    "bits 0,1 = green" (Some `Green)
    (Flag_color.of_keywords [ `MailFlagBit0; `MailFlagBit1 ]);
  Alcotest.(check (option color_testable))
    "bits 0,2 = purple" (Some `Purple)
    (Flag_color.of_keywords [ `MailFlagBit0; `MailFlagBit2 ]);
  Alcotest.(check (option color_testable))
    "bits 1,2 = gray" (Some `Gray)
    (Flag_color.of_keywords [ `MailFlagBit1; `MailFlagBit2 ]);
  (* All bits = undefined *)
  Alcotest.(check (option color_testable))
    "all bits = undefined" None
    (Flag_color.of_keywords [ `MailFlagBit0; `MailFlagBit1; `MailFlagBit2 ])

let test_of_keywords_default_red () =
  (* Test keyword list to color with Red default for empty list *)
  Alcotest.(check (option color_testable))
    "empty = red" (Some `Red)
    (Flag_color.of_keywords_default_red []);
  Alcotest.(check (option color_testable))
    "bit0 = orange" (Some `Orange)
    (Flag_color.of_keywords_default_red [ `MailFlagBit0 ]);
  Alcotest.(check (option color_testable))
    "bits 0,1 = green" (Some `Green)
    (Flag_color.of_keywords_default_red [ `MailFlagBit0; `MailFlagBit1 ]);
  (* All bits still undefined *)
  Alcotest.(check (option color_testable))
    "all bits = undefined" None
    (Flag_color.of_keywords_default_red
       [ `MailFlagBit0; `MailFlagBit1; `MailFlagBit2 ])

let test_keywords_roundtrip () =
  (* Test that to_keywords -> of_keywords_default_red preserves non-red colors *)
  let test_color c =
    let kws = Flag_color.to_keywords c in
    Alcotest.(check (option color_testable))
      (Flag_color.to_string c) (Some c)
      (Flag_color.of_keywords_default_red kws)
  in
  test_color `Red;
  test_color `Orange;
  test_color `Yellow;
  test_color `Green;
  test_color `Blue;
  test_color `Purple;
  test_color `Gray

let test_to_string () =
  (* Test color to string conversion *)
  Alcotest.(check string) "red" "red" (Flag_color.to_string `Red);
  Alcotest.(check string) "orange" "orange" (Flag_color.to_string `Orange);
  Alcotest.(check string) "yellow" "yellow" (Flag_color.to_string `Yellow);
  Alcotest.(check string) "green" "green" (Flag_color.to_string `Green);
  Alcotest.(check string) "blue" "blue" (Flag_color.to_string `Blue);
  Alcotest.(check string) "purple" "purple" (Flag_color.to_string `Purple);
  Alcotest.(check string) "gray" "gray" (Flag_color.to_string `Gray)

let test_of_string () =
  (* Test string to color conversion *)
  Alcotest.(check (option color_testable))
    "red" (Some `Red)
    (Flag_color.of_string "red");
  Alcotest.(check (option color_testable))
    "orange" (Some `Orange)
    (Flag_color.of_string "orange");
  Alcotest.(check (option color_testable))
    "yellow" (Some `Yellow)
    (Flag_color.of_string "yellow");
  Alcotest.(check (option color_testable))
    "green" (Some `Green)
    (Flag_color.of_string "green");
  Alcotest.(check (option color_testable))
    "blue" (Some `Blue)
    (Flag_color.of_string "blue");
  Alcotest.(check (option color_testable))
    "purple" (Some `Purple)
    (Flag_color.of_string "purple");
  Alcotest.(check (option color_testable))
    "gray" (Some `Gray)
    (Flag_color.of_string "gray");
  Alcotest.(check (option color_testable))
    "grey" (Some `Gray)
    (Flag_color.of_string "grey");
  (* Case insensitive *)
  Alcotest.(check (option color_testable))
    "RED" (Some `Red)
    (Flag_color.of_string "RED");
  Alcotest.(check (option color_testable))
    "Orange" (Some `Orange)
    (Flag_color.of_string "Orange");
  (* Unknown *)
  Alcotest.(check (option color_testable))
    "unknown" None
    (Flag_color.of_string "unknown")

let test_string_roundtrip () =
  (* Test that to_string -> of_string preserves the color *)
  let test_color c =
    let s = Flag_color.to_string c in
    Alcotest.(check (option color_testable)) s (Some c) (Flag_color.of_string s)
  in
  test_color `Red;
  test_color `Orange;
  test_color `Yellow;
  test_color `Green;
  test_color `Blue;
  test_color `Purple;
  test_color `Gray

let test_equal () =
  Alcotest.(check bool) "same color" true (Flag_color.equal `Red `Red);
  Alcotest.(check bool) "different colors" false (Flag_color.equal `Red `Green)

let test_compare () =
  Alcotest.(check int) "same color" 0 (Flag_color.compare `Red `Red);
  Alcotest.(check bool)
    "distinct colors compare nonzero" true
    (Flag_color.compare `Red `Green <> 0)

let () =
  Alcotest.run "Flag_color"
    [
      ( "keywords",
        [
          Alcotest.test_case "to_keywords" `Quick test_to_keywords;
          Alcotest.test_case "of_keywords" `Quick test_of_keywords;
          Alcotest.test_case "of_keywords_default_red" `Quick
            test_of_keywords_default_red;
          Alcotest.test_case "roundtrip" `Quick test_keywords_roundtrip;
        ] );
      ( "strings",
        [
          Alcotest.test_case "to_string" `Quick test_to_string;
          Alcotest.test_case "of_string" `Quick test_of_string;
          Alcotest.test_case "roundtrip" `Quick test_string_roundtrip;
        ] );
      ( "equality",
        [
          Alcotest.test_case "equal" `Quick test_equal;
          Alcotest.test_case "compare" `Quick test_compare;
        ] );
    ]
