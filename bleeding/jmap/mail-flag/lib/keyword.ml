(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type standard = [ `Seen | `Answered | `Flagged | `Draft | `Deleted | `Forwarded ]
type spam = [ `Phishing | `Junk | `NotJunk ]

type extended =
  [ `HasAttachment
  | `HasNoAttachment
  | `Memo
  | `HasMemo
  | `CanUnsubscribe
  | `Unsubscribed
  | `Muted
  | `Followed
  | `AutoSent
  | `Imported
  | `IsTrusted
  | `MaskedEmail
  | `New
  | `Notify ]

type flag_bit = [ `MailFlagBit0 | `MailFlagBit1 | `MailFlagBit2 ]
type t = [ standard | spam | extended | flag_bit | `Custom of string ]

(** Normalize a keyword string for {i lookup only}: drop a leading [$] or [\\]
    sigil and lowercase.  The result is never used to build a [`Custom]
    keyword: an unrecognised keyword keeps its exact wire spelling, so that
    [$important] does not round-trip as the different keyword [important]. *)
let normalize s =
  let s = String.lowercase_ascii s in
  if String.length s > 0 && s.[0] = '$' then String.sub s 1 (String.length s - 1)
  else if String.length s > 0 && s.[0] = '\\' then
    String.sub s 1 (String.length s - 1)
  else s

let of_string s =
  match normalize s with
  | "seen" -> `Seen
  | "answered" -> `Answered
  | "flagged" -> `Flagged
  | "draft" -> `Draft
  | "deleted" -> `Deleted
  | "forwarded" -> `Forwarded
  | "phishing" -> `Phishing
  | "junk" -> `Junk
  | "notjunk" -> `NotJunk
  | "hasattachment" -> `HasAttachment
  | "hasnoattachment" -> `HasNoAttachment
  | "memo" -> `Memo
  | "hasmemo" -> `HasMemo
  | "canunsubscribe" -> `CanUnsubscribe
  | "unsubscribed" -> `Unsubscribed
  | "muted" -> `Muted
  | "followed" -> `Followed
  | "autosent" -> `AutoSent
  | "imported" -> `Imported
  | "istrusted" -> `IsTrusted
  | "maskedemail" -> `MaskedEmail
  | "new" -> `New
  | "notify" -> `Notify
  | "mailflagbit0" -> `MailFlagBit0
  | "mailflagbit1" -> `MailFlagBit1
  | "mailflagbit2" -> `MailFlagBit2
  (* Unrecognised: keep the caller's exact spelling. JMAP keywords are opaque
     strings, so [$important] and [important] are distinct and stripping the
     [$] would silently rewrite the wire value on the next {!to_string}. A
     leading backslash marks an IMAP system flag this module does not model;
     RFC 8621 Section 4.1.1 forbids [\] in a JMAP keyword, so only that is
     dropped. *)
  | _ ->
      if String.length s > 0 && s.[0] = '\\' then
        `Custom (String.sub s 1 (String.length s - 1))
      else `Custom s

let to_string = function
  | `Seen -> "$seen"
  | `Answered -> "$answered"
  | `Flagged -> "$flagged"
  | `Draft -> "$draft"
  | `Deleted -> "$deleted"
  | `Forwarded -> "$forwarded"
  | `Phishing -> "$phishing"
  | `Junk -> "$junk"
  | `NotJunk -> "$notjunk"
  | `HasAttachment -> "$hasattachment"
  | `HasNoAttachment -> "$hasnoattachment"
  | `Memo -> "$memo"
  | `HasMemo -> "$hasmemo"
  | `CanUnsubscribe -> "$canunsubscribe"
  | `Unsubscribed -> "$unsubscribed"
  | `Muted -> "$muted"
  | `Followed -> "$followed"
  | `AutoSent -> "$autosent"
  | `Imported -> "$imported"
  | `IsTrusted -> "$istrusted"
  | `MaskedEmail -> "$maskedemail"
  | `New -> "$new"
  | `Notify -> "$notify"
  (* The Apple flag bits are registered with this exact mixed-case spelling in
     draft-ietf-mailmaint-messageflag-mailboxattribute Section 4.1.14-4.1.16, so
     that is what we emit.  RFC 8621 Section 4.1.1's "servers MUST return
     keywords in lowercase" binds servers; {!of_string} is case-insensitive, so
     either spelling interoperates. *)
  | `MailFlagBit0 -> "$MailFlagBit0"
  | `MailFlagBit1 -> "$MailFlagBit1"
  | `MailFlagBit2 -> "$MailFlagBit2"
  (* Verbatim: [`Custom] holds the exact spelling {!of_string} was given. *)
  | `Custom s -> s

let to_imap_string = function
  | `Seen -> "\\Seen"
  | `Answered -> "\\Answered"
  | `Flagged -> "\\Flagged"
  | `Draft -> "\\Draft"
  | `Deleted -> "\\Deleted"
  | `Forwarded -> "$Forwarded"
  | `Phishing -> "$Phishing"
  | `Junk -> "$Junk"
  | `NotJunk -> "$NotJunk"
  | `HasAttachment -> "$HasAttachment"
  | `HasNoAttachment -> "$HasNoAttachment"
  | `Memo -> "$Memo"
  | `HasMemo -> "$HasMemo"
  | `CanUnsubscribe -> "$CanUnsubscribe"
  | `Unsubscribed -> "$Unsubscribed"
  | `Muted -> "$Muted"
  | `Followed -> "$Followed"
  | `AutoSent -> "$AutoSent"
  | `Imported -> "$Imported"
  | `IsTrusted -> "$IsTrusted"
  | `MaskedEmail -> "$MaskedEmail"
  | `New -> "$New"
  | `Notify -> "$Notify"
  | `MailFlagBit0 -> "$MailFlagBit0"
  | `MailFlagBit1 -> "$MailFlagBit1"
  | `MailFlagBit2 -> "$MailFlagBit2"
  | `Custom s -> s

let is_standard = function
  | `Seen | `Answered | `Flagged | `Draft | `Deleted -> true
  | _ -> false

let is_mutually_exclusive k1 k2 =
  match (k1, k2) with
  | `HasAttachment, `HasNoAttachment | `HasNoAttachment, `HasAttachment -> true
  | `Junk, `NotJunk | `NotJunk, `Junk -> true
  | `Muted, `Followed | `Followed, `Muted -> true
  | _ -> false

let pp ppf k = Format.pp_print_string ppf (to_string k)

let equal k1 k2 =
  match (k1, k2) with
  | `Custom s1, `Custom s2 ->
      String.equal (String.lowercase_ascii s1) (String.lowercase_ascii s2)
  | _ -> k1 = k2

let compare k1 k2 =
  match (k1, k2) with
  | `Custom s1, `Custom s2 ->
      String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
  | `Custom _, _ -> 1
  | _, `Custom _ -> -1
  | _ -> Stdlib.compare k1 k2

type flag_color = [ `Red | `Orange | `Yellow | `Green | `Blue | `Purple | `Gray ]
(** Apple Mail flag colors *)

let flag_color_of_keywords keywords =
  let bits =
    List.filter_map
      (function
        | (`MailFlagBit0 | `MailFlagBit1 | `MailFlagBit2) as k -> Some k
        | _ -> None)
      keywords
  in
  Flag_color.of_keywords_default_red bits

let flag_color_to_keywords color = (Flag_color.to_keywords color :> t list)
