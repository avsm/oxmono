(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type list_attr =
  [ `Noinferiors
  | `Noselect
  | `Marked
  | `Unmarked
  | `Subscribed
  | `HasChildren
  | `HasNoChildren
  | `NonExistent
  | `Remote ]

type special_use =
  [ `All
  | `Archive
  | `Drafts
  | `Flagged
  | `Important
  | `Inbox
  | `Junk
  | `Sent
  | `Subscribed
  | `Trash
  | `Snoozed
  | `Scheduled
  | `Memos ]

type t = [ list_attr | special_use | `Extension of string ]

let normalize s =
  let s = String.lowercase_ascii s in
  if String.length s > 0 && s.[0] = '\\' then
    String.sub s 1 (String.length s - 1)
  else s

let of_string s =
  match normalize s with
  (* LIST attributes *)
  | "noinferiors" -> `Noinferiors
  | "noselect" -> `Noselect
  | "marked" -> `Marked
  | "unmarked" -> `Unmarked
  | "subscribed" -> `Subscribed
  | "haschildren" -> `HasChildren
  | "hasnochildren" -> `HasNoChildren
  | "nonexistent" -> `NonExistent
  | "remote" -> `Remote
  (* Special-use roles *)
  | "all" -> `All
  | "archive" -> `Archive
  | "drafts" -> `Drafts
  | "flagged" -> `Flagged
  | "important" -> `Important
  | "inbox" -> `Inbox
  | "junk" | "spam" -> `Junk
  | "sent" -> `Sent
  | "trash" -> `Trash
  | "snoozed" -> `Snoozed
  | "scheduled" -> `Scheduled
  | "memos" -> `Memos
  | other -> `Extension other

let to_string = function
  (* LIST attributes *)
  | `Noinferiors -> "\\Noinferiors"
  | `Noselect -> "\\Noselect"
  | `Marked -> "\\Marked"
  | `Unmarked -> "\\Unmarked"
  | `Subscribed -> "\\Subscribed"
  | `HasChildren -> "\\HasChildren"
  | `HasNoChildren -> "\\HasNoChildren"
  | `NonExistent -> "\\NonExistent"
  | `Remote -> "\\Remote"
  (* Special-use roles *)
  | `All -> "\\All"
  | `Archive -> "\\Archive"
  | `Drafts -> "\\Drafts"
  | `Flagged -> "\\Flagged"
  | `Important -> "\\Important"
  | `Junk -> "\\Junk"
  | `Sent -> "\\Sent"
  | `Trash -> "\\Trash"
  (* Names registered in the IANA "IMAP Mailbox Name Attributes" registry
     outside RFC 6154 Section 2 carry no implied backslash.
     draft-ietf-mailmaint-messageflag-mailboxattribute Section 4.2: "none of the
     attribute names in this section have an implied backslash.  This sets them
     apart from those specified in Section 2 of [RFC6154]."  RFC 8621
     Section 10.5.1 registers "Inbox" the same way, as a "JMAP only" attribute:
     IMAP has no \Inbox special use, INBOX being a reserved mailbox *name*. *)
  | `Inbox -> "Inbox"
  | `Snoozed -> "Snoozed"
  | `Scheduled -> "Scheduled"
  | `Memos -> "Memos"
  | `Extension s -> if String.length s > 0 && s.[0] = '\\' then s else "\\" ^ s

let to_jmap_role = function
  (* Special-use roles have JMAP equivalents *)
  | `All -> Some "all"
  | `Archive -> Some "archive"
  | `Drafts -> Some "drafts"
  | `Flagged -> Some "flagged"
  | `Important -> Some "important"
  | `Inbox -> Some "inbox"
  | `Junk -> Some "junk"
  | `Sent -> Some "sent"
  | `Trash -> Some "trash"
  | `Snoozed -> Some "snoozed"
  | `Scheduled -> Some "scheduled"
  | `Memos -> Some "memos"
  (* \Subscribed is not a role.  RFC 8621 Section 2 ties "role" to the IMAP
     SPECIAL-USE extension (RFC 6154) and requires that a Mailbox "MUST only
     have a single role, and there MUST NOT be two Mailboxes in the same
     account with the same role".  \Subscribed is a LIST name attribute
     (RFC 9051 Section 7.2.2), not a special use: any number of mailboxes may
     carry it at once, so it cannot satisfy that uniqueness rule.  RFC 8621
     Section 2 models subscription with the separate "isSubscribed" Boolean,
     which "corresponds to IMAP [RFC3501] mailbox subscriptions". *)
  (* Remaining LIST attributes and extensions have no JMAP role *)
  | `Subscribed | `Noinferiors | `Noselect | `Marked | `Unmarked | `HasChildren
  | `HasNoChildren | `NonExistent | `Remote | `Extension _ ->
      None

let of_jmap_role s =
  match String.lowercase_ascii s with
  | "all" -> Some `All
  | "archive" -> Some `Archive
  | "drafts" -> Some `Drafts
  | "flagged" -> Some `Flagged
  | "important" -> Some `Important
  | "inbox" -> Some `Inbox
  | "junk" -> Some `Junk
  | "sent" -> Some `Sent
  | "trash" -> Some `Trash
  | "snoozed" -> Some `Snoozed
  | "scheduled" -> Some `Scheduled
  | "memos" -> Some `Memos
  (* Lenient on input only: {!to_jmap_role} never produces "subscribed", but a
     server that reads RFC 8621 Section 2's "IMAP Mailbox Name Attributes"
     wording literally may send it, and the attribute it names is \Subscribed. *)
  | "subscribed" -> Some `Subscribed
  | _ -> None

let is_special_use = function
  | `All | `Archive | `Drafts | `Flagged | `Important | `Inbox | `Junk | `Sent
  | `Trash | `Snoozed | `Scheduled | `Memos ->
      true
  (* \Subscribed is a LIST name attribute rather than a special use; see
     {!to_jmap_role} for the RFC 8621 Section 2 reasoning. *)
  | `Subscribed | `Noinferiors | `Noselect | `Marked | `Unmarked | `HasChildren
  | `HasNoChildren | `NonExistent | `Remote | `Extension _ ->
      false

let is_selectable = function `Noselect | `NonExistent -> false | _ -> true
let pp ppf attr = Format.pp_print_string ppf (to_string attr)
let equal a b = a = b
let compare a b = Stdlib.compare a b
