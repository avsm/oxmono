(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for the Keyword module. *)

open Mail_flag

let keyword_testable = Alcotest.testable Keyword.pp Keyword.equal

let test_of_string_standard () =
  (* Test standard keywords with $ prefix *)
  Alcotest.(check keyword_testable)
    "seen with $" `Seen
    (Keyword.of_string "$seen");
  Alcotest.(check keyword_testable)
    "answered with $" `Answered
    (Keyword.of_string "$answered");
  Alcotest.(check keyword_testable)
    "flagged with $" `Flagged
    (Keyword.of_string "$flagged");
  Alcotest.(check keyword_testable)
    "draft with $" `Draft
    (Keyword.of_string "$draft");
  Alcotest.(check keyword_testable)
    "deleted with $" `Deleted
    (Keyword.of_string "$deleted");
  Alcotest.(check keyword_testable)
    "forwarded with $" `Forwarded
    (Keyword.of_string "$forwarded")

let test_of_string_no_prefix () =
  (* Test standard keywords without prefix *)
  Alcotest.(check keyword_testable)
    "seen no prefix" `Seen (Keyword.of_string "seen");
  Alcotest.(check keyword_testable)
    "answered no prefix" `Answered
    (Keyword.of_string "answered");
  Alcotest.(check keyword_testable)
    "flagged no prefix" `Flagged
    (Keyword.of_string "flagged")

let test_of_string_case_insensitive () =
  (* Test case insensitivity *)
  Alcotest.(check keyword_testable)
    "SEEN uppercase" `Seen
    (Keyword.of_string "$SEEN");
  Alcotest.(check keyword_testable)
    "Seen mixed" `Seen (Keyword.of_string "Seen");
  Alcotest.(check keyword_testable)
    "FORWARDED uppercase" `Forwarded
    (Keyword.of_string "$FORWARDED")

let test_of_string_imap_format () =
  (* Test IMAP system flag format with backslash *)
  Alcotest.(check keyword_testable) "\\Seen" `Seen (Keyword.of_string "\\Seen");
  Alcotest.(check keyword_testable)
    "\\Answered" `Answered
    (Keyword.of_string "\\Answered");
  Alcotest.(check keyword_testable)
    "\\Draft" `Draft
    (Keyword.of_string "\\Draft")

let test_of_string_spam () =
  (* Test spam-related keywords *)
  Alcotest.(check keyword_testable)
    "phishing" `Phishing
    (Keyword.of_string "$phishing");
  Alcotest.(check keyword_testable) "junk" `Junk (Keyword.of_string "$junk");
  Alcotest.(check keyword_testable)
    "notjunk" `NotJunk
    (Keyword.of_string "$notjunk")

let test_of_string_extended () =
  (* Test extended keywords *)
  Alcotest.(check keyword_testable)
    "hasattachment" `HasAttachment
    (Keyword.of_string "$hasattachment");
  Alcotest.(check keyword_testable)
    "hasnoattachment" `HasNoAttachment
    (Keyword.of_string "$hasnoattachment");
  Alcotest.(check keyword_testable) "muted" `Muted (Keyword.of_string "$muted");
  Alcotest.(check keyword_testable)
    "followed" `Followed
    (Keyword.of_string "$followed")

let test_of_string_flag_bits () =
  (* Test Apple Mail flag bits *)
  Alcotest.(check keyword_testable)
    "mailflagbit0" `MailFlagBit0
    (Keyword.of_string "$mailflagbit0");
  Alcotest.(check keyword_testable)
    "mailflagbit1" `MailFlagBit1
    (Keyword.of_string "$mailflagbit1");
  Alcotest.(check keyword_testable)
    "mailflagbit2" `MailFlagBit2
    (Keyword.of_string "$mailflagbit2")

let test_of_string_custom () =
  (* Test custom keywords. An unrecognised keyword keeps its exact spelling:
     "$important" and "important" are two distinct JMAP keywords, so the
     sigil must not be stripped. Only an IMAP backslash is dropped, since
     RFC 8621 Section 4.1.1 forbids it in a JMAP keyword. *)
  Alcotest.(check keyword_testable)
    "custom keyword" (`Custom "my-label")
    (Keyword.of_string "my-label");
  Alcotest.(check keyword_testable)
    "custom with $" (`Custom "$custom")
    (Keyword.of_string "$custom");
  Alcotest.(check keyword_testable)
    "IANA $important" (`Custom "$important")
    (Keyword.of_string "$important");
  Alcotest.(check keyword_testable)
    "IMAP \\Recent" (`Custom "Recent")
    (Keyword.of_string "\\Recent");
  (* [keyword_testable] compares [`Custom] payloads case-insensitively, so the
     case-preservation claim has to be checked on the string itself. *)
  Alcotest.(check string)
    "case preserved" "MyLabel"
    (Keyword.to_string (Keyword.of_string "MyLabel"));
  Alcotest.(check string)
    "IMAP \\Recent spelling" "Recent"
    (Keyword.to_string (Keyword.of_string "\\Recent"))

let test_of_string_to_string_roundtrip () =
  (* to_string is a left inverse of of_string on unrecognised keywords *)
  let roundtrip s =
    Alcotest.(check string)
      ("roundtrip " ^ s) s
      (Keyword.to_string (Keyword.of_string s))
  in
  roundtrip "$important";
  roundtrip "$my_custom_flag";
  roundtrip "my-label";
  Alcotest.(check string)
    "backslash dropped" "Recent"
    (Keyword.to_string (Keyword.of_string "\\Recent"));
  (* Known keywords normalise to their canonical JMAP spelling *)
  Alcotest.(check string)
    "seen canonical" "$seen"
    (Keyword.to_string (Keyword.of_string "\\Seen"));
  Alcotest.(check string)
    "bit0 canonical" "$MailFlagBit0"
    (Keyword.to_string (Keyword.of_string "$mailflagbit0"))

let test_to_string () =
  (* Test conversion to JMAP format *)
  Alcotest.(check string) "seen" "$seen" (Keyword.to_string `Seen);
  Alcotest.(check string) "answered" "$answered" (Keyword.to_string `Answered);
  Alcotest.(check string) "flagged" "$flagged" (Keyword.to_string `Flagged);
  Alcotest.(check string) "draft" "$draft" (Keyword.to_string `Draft);
  Alcotest.(check string) "deleted" "$deleted" (Keyword.to_string `Deleted);
  Alcotest.(check string)
    "forwarded" "$forwarded"
    (Keyword.to_string `Forwarded);
  Alcotest.(check string) "junk" "$junk" (Keyword.to_string `Junk);
  Alcotest.(check string)
    "mailflagbit0" "$MailFlagBit0"
    (Keyword.to_string `MailFlagBit0);
  Alcotest.(check string)
    "custom" "my-label"
    (Keyword.to_string (`Custom "my-label"));
  Alcotest.(check string)
    "custom with $" "$important"
    (Keyword.to_string (`Custom "$important"))

let test_to_imap_string () =
  (* Test conversion to IMAP format *)
  Alcotest.(check string) "seen" "\\Seen" (Keyword.to_imap_string `Seen);
  Alcotest.(check string)
    "answered" "\\Answered"
    (Keyword.to_imap_string `Answered);
  Alcotest.(check string)
    "flagged" "\\Flagged"
    (Keyword.to_imap_string `Flagged);
  Alcotest.(check string) "draft" "\\Draft" (Keyword.to_imap_string `Draft);
  Alcotest.(check string)
    "deleted" "\\Deleted"
    (Keyword.to_imap_string `Deleted);
  (* Non-system flags use $ prefix *)
  Alcotest.(check string)
    "forwarded" "$Forwarded"
    (Keyword.to_imap_string `Forwarded);
  Alcotest.(check string) "junk" "$Junk" (Keyword.to_imap_string `Junk);
  Alcotest.(check string)
    "mailflagbit0" "$MailFlagBit0"
    (Keyword.to_imap_string `MailFlagBit0);
  (* A custom keyword is an IMAP atom and is emitted verbatim *)
  Alcotest.(check string)
    "custom bare" "my-label"
    (Keyword.to_imap_string (`Custom "my-label"));
  Alcotest.(check string)
    "custom $" "$important"
    (Keyword.to_imap_string (`Custom "$important"));
  Alcotest.(check string)
    "unmodelled system flag" "Recent"
    (Keyword.to_imap_string (Keyword.of_string "\\Recent"))

let test_is_standard () =
  (* Test is_standard predicate *)
  Alcotest.(check bool) "seen is standard" true (Keyword.is_standard `Seen);
  Alcotest.(check bool)
    "answered is standard" true
    (Keyword.is_standard `Answered);
  Alcotest.(check bool)
    "flagged is standard" true
    (Keyword.is_standard `Flagged);
  Alcotest.(check bool) "draft is standard" true (Keyword.is_standard `Draft);
  Alcotest.(check bool)
    "deleted is standard" true
    (Keyword.is_standard `Deleted);
  (* Forwarded is NOT an IMAP system flag *)
  Alcotest.(check bool)
    "forwarded is not standard" false
    (Keyword.is_standard `Forwarded);
  Alcotest.(check bool) "junk is not standard" false (Keyword.is_standard `Junk);
  Alcotest.(check bool)
    "custom is not standard" false
    (Keyword.is_standard (`Custom "x"))

let test_mutual_exclusion () =
  (* Test mutually exclusive pairs *)
  Alcotest.(check bool)
    "has/hasno attachment" true
    (Keyword.is_mutually_exclusive `HasAttachment `HasNoAttachment);
  Alcotest.(check bool)
    "hasno/has attachment" true
    (Keyword.is_mutually_exclusive `HasNoAttachment `HasAttachment);
  Alcotest.(check bool)
    "junk/notjunk" true
    (Keyword.is_mutually_exclusive `Junk `NotJunk);
  Alcotest.(check bool)
    "notjunk/junk" true
    (Keyword.is_mutually_exclusive `NotJunk `Junk);
  Alcotest.(check bool)
    "muted/followed" true
    (Keyword.is_mutually_exclusive `Muted `Followed);
  Alcotest.(check bool)
    "followed/muted" true
    (Keyword.is_mutually_exclusive `Followed `Muted);
  (* Non-exclusive pairs *)
  Alcotest.(check bool)
    "seen/flagged" false
    (Keyword.is_mutually_exclusive `Seen `Flagged);
  Alcotest.(check bool)
    "seen/seen" false
    (Keyword.is_mutually_exclusive `Seen `Seen);
  Alcotest.(check bool)
    "junk/muted" false
    (Keyword.is_mutually_exclusive `Junk `Muted)

let test_equal () =
  (* Test equality *)
  Alcotest.(check bool) "same keyword" true (Keyword.equal `Seen `Seen);
  Alcotest.(check bool)
    "different keywords" false
    (Keyword.equal `Seen `Flagged);
  (* Custom keywords are compared case-insensitively *)
  Alcotest.(check bool)
    "custom same" true
    (Keyword.equal (`Custom "label") (`Custom "label"));
  Alcotest.(check bool)
    "custom case insensitive" true
    (Keyword.equal (`Custom "Label") (`Custom "label"));
  Alcotest.(check bool)
    "custom different" false
    (Keyword.equal (`Custom "a") (`Custom "b"))

let test_compare () =
  (* Test comparison *)
  Alcotest.(check int) "same keyword" 0 (Keyword.compare `Seen `Seen);
  Alcotest.(check bool)
    "custom vs non-custom" true
    (Keyword.compare (`Custom "a") `Seen > 0);
  Alcotest.(check bool)
    "non-custom vs custom" true
    (Keyword.compare `Seen (`Custom "a") < 0)

let pp_flag_color ppf c =
  let s =
    match c with
    | `Red -> "Red"
    | `Orange -> "Orange"
    | `Yellow -> "Yellow"
    | `Green -> "Green"
    | `Blue -> "Blue"
    | `Purple -> "Purple"
    | `Gray -> "Gray"
  in
  Format.pp_print_string ppf s

let eq_flag_color a b =
  match (a, b) with
  | `Red, `Red
  | `Orange, `Orange
  | `Yellow, `Yellow
  | `Green, `Green
  | `Blue, `Blue
  | `Purple, `Purple
  | `Gray, `Gray ->
      true
  | _ -> false

let flag_color_testable = Alcotest.testable pp_flag_color eq_flag_color

let test_flag_color_of_keywords () =
  (* Test Apple Mail flag color extraction *)
  Alcotest.(check (option flag_color_testable))
    "no bits = red" (Some `Red)
    (Keyword.flag_color_of_keywords []);
  Alcotest.(check (option flag_color_testable))
    "bit0 = orange" (Some `Orange)
    (Keyword.flag_color_of_keywords [ `MailFlagBit0 ]);
  Alcotest.(check (option flag_color_testable))
    "bit1 = yellow" (Some `Yellow)
    (Keyword.flag_color_of_keywords [ `MailFlagBit1 ]);
  Alcotest.(check (option flag_color_testable))
    "bit2 = blue" (Some `Blue)
    (Keyword.flag_color_of_keywords [ `MailFlagBit2 ]);
  Alcotest.(check (option flag_color_testable))
    "bits 0,2 = purple" (Some `Purple)
    (Keyword.flag_color_of_keywords [ `MailFlagBit0; `MailFlagBit2 ]);
  Alcotest.(check (option flag_color_testable))
    "bits 1,2 = gray" (Some `Gray)
    (Keyword.flag_color_of_keywords [ `MailFlagBit1; `MailFlagBit2 ]);
  Alcotest.(check (option flag_color_testable))
    "bits 0,1 = green" (Some `Green)
    (Keyword.flag_color_of_keywords [ `MailFlagBit0; `MailFlagBit1 ]);
  (* Undefined encoding: all three bits set *)
  Alcotest.(check (option flag_color_testable))
    "all bits = invalid" None
    (Keyword.flag_color_of_keywords
       [ `MailFlagBit0; `MailFlagBit1; `MailFlagBit2 ]);
  (* Non-flag-bit keywords in the list are ignored *)
  Alcotest.(check (option flag_color_testable))
    "ignores other keywords" (Some `Orange)
    (Keyword.flag_color_of_keywords [ `Seen; `MailFlagBit0 ])

let test_flag_color_to_keywords () =
  (* Test Apple Mail flag color encoding *)
  Alcotest.(check int)
    "red = no bits" 0
    (List.length (Keyword.flag_color_to_keywords `Red));
  Alcotest.(check int)
    "orange = 1 bit" 1
    (List.length (Keyword.flag_color_to_keywords `Orange));
  Alcotest.(check int)
    "yellow = 1 bit" 1
    (List.length (Keyword.flag_color_to_keywords `Yellow));
  Alcotest.(check int)
    "blue = 1 bit" 1
    (List.length (Keyword.flag_color_to_keywords `Blue));
  Alcotest.(check int)
    "purple = 2 bits" 2
    (List.length (Keyword.flag_color_to_keywords `Purple));
  Alcotest.(check int)
    "gray = 2 bits" 2
    (List.length (Keyword.flag_color_to_keywords `Gray));
  Alcotest.(check int)
    "green = 2 bits" 2
    (List.length (Keyword.flag_color_to_keywords `Green))

let () =
  Alcotest.run "Keyword"
    [
      ( "of_string",
        [
          Alcotest.test_case "standard keywords" `Quick test_of_string_standard;
          Alcotest.test_case "no prefix" `Quick test_of_string_no_prefix;
          Alcotest.test_case "case insensitive" `Quick
            test_of_string_case_insensitive;
          Alcotest.test_case "IMAP format" `Quick test_of_string_imap_format;
          Alcotest.test_case "spam keywords" `Quick test_of_string_spam;
          Alcotest.test_case "extended keywords" `Quick test_of_string_extended;
          Alcotest.test_case "flag bits" `Quick test_of_string_flag_bits;
          Alcotest.test_case "custom" `Quick test_of_string_custom;
          Alcotest.test_case "roundtrip" `Quick
            test_of_string_to_string_roundtrip;
        ] );
      ("to_string", [ Alcotest.test_case "JMAP format" `Quick test_to_string ]);
      ( "to_imap_string",
        [ Alcotest.test_case "IMAP format" `Quick test_to_imap_string ] );
      ( "predicates",
        [
          Alcotest.test_case "is_standard" `Quick test_is_standard;
          Alcotest.test_case "is_mutually_exclusive" `Quick
            test_mutual_exclusion;
        ] );
      ( "equality",
        [
          Alcotest.test_case "equal" `Quick test_equal;
          Alcotest.test_case "compare" `Quick test_compare;
        ] );
      ( "flag_color",
        [
          Alcotest.test_case "of_keywords" `Quick test_flag_color_of_keywords;
          Alcotest.test_case "to_keywords" `Quick test_flag_color_to_keywords;
        ] );
    ]
