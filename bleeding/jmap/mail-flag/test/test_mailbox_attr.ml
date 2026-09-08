(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for the Mailbox_attr module. *)

open Mail_flag

let attr_testable =
  Alcotest.testable Mailbox_attr.pp (fun a b ->
      Mailbox_attr.to_string a = Mailbox_attr.to_string b)

(* A testable for special_use which is a subtype of t *)
let pp_special_use ppf (x : Mailbox_attr.special_use) =
  Mailbox_attr.pp ppf (x :> Mailbox_attr.t)

let special_use_testable : Mailbox_attr.special_use Alcotest.testable =
  Alcotest.testable pp_special_use (fun a b ->
      Mailbox_attr.to_string (a :> Mailbox_attr.t)
      = Mailbox_attr.to_string (b :> Mailbox_attr.t))

let test_of_string_special_use () =
  (* Test special-use attributes with backslash prefix *)
  Alcotest.(check attr_testable)
    "\\Drafts" `Drafts
    (Mailbox_attr.of_string "\\Drafts");
  Alcotest.(check attr_testable)
    "\\Sent" `Sent
    (Mailbox_attr.of_string "\\Sent");
  Alcotest.(check attr_testable)
    "\\Trash" `Trash
    (Mailbox_attr.of_string "\\Trash");
  Alcotest.(check attr_testable)
    "\\Junk" `Junk
    (Mailbox_attr.of_string "\\Junk");
  Alcotest.(check attr_testable)
    "\\Archive" `Archive
    (Mailbox_attr.of_string "\\Archive");
  Alcotest.(check attr_testable) "\\All" `All (Mailbox_attr.of_string "\\All");
  Alcotest.(check attr_testable)
    "\\Flagged" `Flagged
    (Mailbox_attr.of_string "\\Flagged");
  Alcotest.(check attr_testable)
    "\\Important" `Important
    (Mailbox_attr.of_string "\\Important");
  Alcotest.(check attr_testable)
    "\\Inbox" `Inbox
    (Mailbox_attr.of_string "\\Inbox")

let test_of_string_no_backslash () =
  (* Test special-use attributes without backslash prefix *)
  Alcotest.(check attr_testable)
    "drafts" `Drafts
    (Mailbox_attr.of_string "drafts");
  Alcotest.(check attr_testable) "sent" `Sent (Mailbox_attr.of_string "sent");
  Alcotest.(check attr_testable) "trash" `Trash (Mailbox_attr.of_string "trash");
  Alcotest.(check attr_testable) "inbox" `Inbox (Mailbox_attr.of_string "inbox")

let test_of_string_case_insensitive () =
  (* Test case insensitivity *)
  Alcotest.(check attr_testable)
    "DRAFTS" `Drafts
    (Mailbox_attr.of_string "DRAFTS");
  Alcotest.(check attr_testable)
    "\\SENT" `Sent
    (Mailbox_attr.of_string "\\SENT");
  Alcotest.(check attr_testable) "Trash" `Trash (Mailbox_attr.of_string "Trash")

let test_of_string_list_attrs () =
  (* Test LIST attributes *)
  Alcotest.(check attr_testable)
    "\\Noinferiors" `Noinferiors
    (Mailbox_attr.of_string "\\Noinferiors");
  Alcotest.(check attr_testable)
    "\\Noselect" `Noselect
    (Mailbox_attr.of_string "\\Noselect");
  Alcotest.(check attr_testable)
    "\\Marked" `Marked
    (Mailbox_attr.of_string "\\Marked");
  Alcotest.(check attr_testable)
    "\\Unmarked" `Unmarked
    (Mailbox_attr.of_string "\\Unmarked");
  Alcotest.(check attr_testable)
    "\\Subscribed" `Subscribed
    (Mailbox_attr.of_string "\\Subscribed");
  Alcotest.(check attr_testable)
    "\\HasChildren" `HasChildren
    (Mailbox_attr.of_string "\\HasChildren");
  Alcotest.(check attr_testable)
    "\\HasNoChildren" `HasNoChildren
    (Mailbox_attr.of_string "\\HasNoChildren");
  Alcotest.(check attr_testable)
    "\\NonExistent" `NonExistent
    (Mailbox_attr.of_string "\\NonExistent");
  Alcotest.(check attr_testable)
    "\\Remote" `Remote
    (Mailbox_attr.of_string "\\Remote")

let test_of_string_junk_alias () =
  (* Test that "spam" is recognized as Junk *)
  Alcotest.(check attr_testable) "spam" `Junk (Mailbox_attr.of_string "spam");
  Alcotest.(check attr_testable)
    "\\Spam" `Junk
    (Mailbox_attr.of_string "\\Spam")

let test_of_string_extended () =
  (* Test extended special-use attributes *)
  Alcotest.(check attr_testable)
    "\\Snoozed" `Snoozed
    (Mailbox_attr.of_string "\\Snoozed");
  Alcotest.(check attr_testable)
    "\\Scheduled" `Scheduled
    (Mailbox_attr.of_string "\\Scheduled");
  Alcotest.(check attr_testable)
    "\\Memos" `Memos
    (Mailbox_attr.of_string "\\Memos")

let test_of_string_extension () =
  (* Test unknown extensions *)
  Alcotest.(check attr_testable)
    "X-Custom" (`Extension "x-custom")
    (Mailbox_attr.of_string "X-Custom");
  Alcotest.(check attr_testable)
    "\\X-MyAttr" (`Extension "x-myattr")
    (Mailbox_attr.of_string "\\X-MyAttr")

let test_to_string () =
  (* Test conversion to IMAP wire format *)
  Alcotest.(check string) "Drafts" "\\Drafts" (Mailbox_attr.to_string `Drafts);
  Alcotest.(check string) "Sent" "\\Sent" (Mailbox_attr.to_string `Sent);
  Alcotest.(check string) "Trash" "\\Trash" (Mailbox_attr.to_string `Trash);
  Alcotest.(check string) "Junk" "\\Junk" (Mailbox_attr.to_string `Junk);
  Alcotest.(check string)
    "Noselect" "\\Noselect"
    (Mailbox_attr.to_string `Noselect);
  Alcotest.(check string)
    "HasChildren" "\\HasChildren"
    (Mailbox_attr.to_string `HasChildren);
  Alcotest.(check string)
    "Extension" "\\x-custom"
    (Mailbox_attr.to_string (`Extension "x-custom"));
  (* draft-ietf-mailmaint-messageflag-mailboxattribute Section 4.2: these three
     attribute names have no implied backslash.  Neither does "Inbox", which
     RFC 8621 Section 10.5.1 registers as a "JMAP only" attribute name. *)
  Alcotest.(check string) "Snoozed" "Snoozed" (Mailbox_attr.to_string `Snoozed);
  Alcotest.(check string)
    "Scheduled" "Scheduled"
    (Mailbox_attr.to_string `Scheduled);
  Alcotest.(check string) "Memos" "Memos" (Mailbox_attr.to_string `Memos);
  Alcotest.(check string) "Inbox" "Inbox" (Mailbox_attr.to_string `Inbox)

let test_to_jmap_role () =
  (* Test special-use to JMAP role conversion *)
  Alcotest.(check (option string))
    "drafts role" (Some "drafts")
    (Mailbox_attr.to_jmap_role `Drafts);
  Alcotest.(check (option string))
    "sent role" (Some "sent")
    (Mailbox_attr.to_jmap_role `Sent);
  Alcotest.(check (option string))
    "trash role" (Some "trash")
    (Mailbox_attr.to_jmap_role `Trash);
  Alcotest.(check (option string))
    "junk role" (Some "junk")
    (Mailbox_attr.to_jmap_role `Junk);
  Alcotest.(check (option string))
    "inbox role" (Some "inbox")
    (Mailbox_attr.to_jmap_role `Inbox);
  Alcotest.(check (option string))
    "archive role" (Some "archive")
    (Mailbox_attr.to_jmap_role `Archive);
  Alcotest.(check (option string))
    "all role" (Some "all")
    (Mailbox_attr.to_jmap_role `All);
  Alcotest.(check (option string))
    "flagged role" (Some "flagged")
    (Mailbox_attr.to_jmap_role `Flagged);
  Alcotest.(check (option string))
    "important role" (Some "important")
    (Mailbox_attr.to_jmap_role `Important);
  Alcotest.(check (option string))
    "snoozed role" (Some "snoozed")
    (Mailbox_attr.to_jmap_role `Snoozed);
  Alcotest.(check (option string))
    "scheduled role" (Some "scheduled")
    (Mailbox_attr.to_jmap_role `Scheduled);
  Alcotest.(check (option string))
    "memos role" (Some "memos")
    (Mailbox_attr.to_jmap_role `Memos);
  (* \\Subscribed is a LIST name attribute, not a special use: RFC 8621
     Section 2 requires a role to be unique within an account, and models
     subscription with the separate isSubscribed Boolean. *)
  Alcotest.(check (option string))
    "subscribed has no role" None
    (Mailbox_attr.to_jmap_role `Subscribed);
  (* LIST attributes have no JMAP role *)
  Alcotest.(check (option string))
    "noselect no role" None
    (Mailbox_attr.to_jmap_role `Noselect);
  Alcotest.(check (option string))
    "haschildren no role" None
    (Mailbox_attr.to_jmap_role `HasChildren);
  Alcotest.(check (option string))
    "extension no role" None
    (Mailbox_attr.to_jmap_role (`Extension "x"))

let test_of_jmap_role () =
  (* Test JMAP role to special-use conversion *)
  Alcotest.(check (option special_use_testable))
    "drafts" (Some `Drafts)
    (Mailbox_attr.of_jmap_role "drafts");
  Alcotest.(check (option special_use_testable))
    "sent" (Some `Sent)
    (Mailbox_attr.of_jmap_role "sent");
  Alcotest.(check (option special_use_testable))
    "trash" (Some `Trash)
    (Mailbox_attr.of_jmap_role "trash");
  Alcotest.(check (option special_use_testable))
    "junk" (Some `Junk)
    (Mailbox_attr.of_jmap_role "junk");
  Alcotest.(check (option special_use_testable))
    "inbox" (Some `Inbox)
    (Mailbox_attr.of_jmap_role "inbox");
  (* of_jmap_role is deliberately more permissive than to_jmap_role here. *)
  Alcotest.(check (option special_use_testable))
    "subscribed" (Some `Subscribed)
    (Mailbox_attr.of_jmap_role "subscribed");
  Alcotest.(check (option special_use_testable))
    "unknown" None
    (Mailbox_attr.of_jmap_role "unknown")

let test_special_use_role_agreement () =
  (* is_special_use must agree with to_jmap_role on every constructor *)
  let all : Mailbox_attr.t list =
    [
      `Noinferiors;
      `Noselect;
      `Marked;
      `Unmarked;
      `Subscribed;
      `HasChildren;
      `HasNoChildren;
      `NonExistent;
      `Remote;
      `All;
      `Archive;
      `Drafts;
      `Flagged;
      `Important;
      `Inbox;
      `Junk;
      `Sent;
      `Trash;
      `Snoozed;
      `Scheduled;
      `Memos;
      `Extension "x-custom";
    ]
  in
  List.iter
    (fun attr ->
      let name = Mailbox_attr.to_string attr in
      Alcotest.(check bool)
        ("agree " ^ name)
        (Mailbox_attr.is_special_use attr)
        (Option.is_some (Mailbox_attr.to_jmap_role attr)))
    all

let test_is_special_use () =
  (* Test is_special_use predicate *)
  Alcotest.(check bool)
    "drafts is special-use" true
    (Mailbox_attr.is_special_use `Drafts);
  Alcotest.(check bool)
    "sent is special-use" true
    (Mailbox_attr.is_special_use `Sent);
  Alcotest.(check bool)
    "inbox is special-use" true
    (Mailbox_attr.is_special_use `Inbox);
  Alcotest.(check bool)
    "subscribed is not special-use" false
    (Mailbox_attr.is_special_use `Subscribed);
  (* LIST attributes are not special-use *)
  Alcotest.(check bool)
    "noselect not special-use" false
    (Mailbox_attr.is_special_use `Noselect);
  Alcotest.(check bool)
    "haschildren not special-use" false
    (Mailbox_attr.is_special_use `HasChildren);
  Alcotest.(check bool)
    "extension not special-use" false
    (Mailbox_attr.is_special_use (`Extension "x"))

let test_is_selectable () =
  (* Test is_selectable predicate *)
  Alcotest.(check bool)
    "drafts selectable" true
    (Mailbox_attr.is_selectable `Drafts);
  Alcotest.(check bool)
    "inbox selectable" true
    (Mailbox_attr.is_selectable `Inbox);
  Alcotest.(check bool)
    "haschildren selectable" true
    (Mailbox_attr.is_selectable `HasChildren);
  (* Noselect and NonExistent are not selectable *)
  Alcotest.(check bool)
    "noselect not selectable" false
    (Mailbox_attr.is_selectable `Noselect);
  Alcotest.(check bool)
    "nonexistent not selectable" false
    (Mailbox_attr.is_selectable `NonExistent)

let test_roundtrip () =
  (* Test that to_string -> of_string preserves the attribute *)
  let test_attr attr =
    let s = Mailbox_attr.to_string attr in
    Alcotest.(check attr_testable)
      ("roundtrip " ^ s) attr (Mailbox_attr.of_string s)
  in
  test_attr `Drafts;
  test_attr `Sent;
  test_attr `Trash;
  test_attr `Junk;
  test_attr `Inbox;
  test_attr `Noselect;
  test_attr `HasChildren;
  test_attr `Snoozed;
  test_attr `Scheduled;
  test_attr `Memos;
  test_attr `Subscribed

let test_equal () =
  Alcotest.(check bool)
    "same attribute" true
    (Mailbox_attr.equal `Drafts `Drafts);
  Alcotest.(check bool)
    "different attributes" false
    (Mailbox_attr.equal `Drafts `Trash)

let test_compare () =
  Alcotest.(check int) "same attribute" 0 (Mailbox_attr.compare `Drafts `Drafts);
  Alcotest.(check bool)
    "distinct attributes compare nonzero" true
    (Mailbox_attr.compare `Drafts `Trash <> 0)

let () =
  Alcotest.run "Mailbox_attr"
    [
      ( "of_string",
        [
          Alcotest.test_case "special-use with backslash" `Quick
            test_of_string_special_use;
          Alcotest.test_case "special-use no backslash" `Quick
            test_of_string_no_backslash;
          Alcotest.test_case "case insensitive" `Quick
            test_of_string_case_insensitive;
          Alcotest.test_case "LIST attributes" `Quick test_of_string_list_attrs;
          Alcotest.test_case "junk/spam alias" `Quick test_of_string_junk_alias;
          Alcotest.test_case "extended attributes" `Quick
            test_of_string_extended;
          Alcotest.test_case "extensions" `Quick test_of_string_extension;
        ] );
      ( "to_string",
        [ Alcotest.test_case "IMAP wire format" `Quick test_to_string ] );
      ( "JMAP roles",
        [
          Alcotest.test_case "to_jmap_role" `Quick test_to_jmap_role;
          Alcotest.test_case "of_jmap_role" `Quick test_of_jmap_role;
          Alcotest.test_case "special-use/role agreement" `Quick
            test_special_use_role_agreement;
        ] );
      ( "predicates",
        [
          Alcotest.test_case "is_special_use" `Quick test_is_special_use;
          Alcotest.test_case "is_selectable" `Quick test_is_selectable;
        ] );
      ( "roundtrip",
        [ Alcotest.test_case "to_string -> of_string" `Quick test_roundtrip ] );
      ( "equality",
        [
          Alcotest.test_case "equal" `Quick test_equal;
          Alcotest.test_case "compare" `Quick test_compare;
        ] );
    ]
