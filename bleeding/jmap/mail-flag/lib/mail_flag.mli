@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unified mail flags for IMAP and JMAP.

    A unified representation of message flags and mailbox attributes works
    across both IMAP
    ({{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051}) and JMAP
    ({{:https://datatracker.ietf.org/doc/html/rfc8621}RFC 8621}).

    {2 Overview}

    The library defines three main concepts:

    - {!Keyword}: message keywords and flags, such as [`Seen], [`Flagged] and
      [`Junk].
    - {!Mailbox_attr}: mailbox attributes and special-use roles, such as
      [`Drafts] and [`Inbox].
    - {!Flag_color}: Apple Mail flag color encoding.

    {2 Protocol Mapping}

    {b IMAP system flags} ([\Seen], [\Answered], etc.) map to
    {!Keyword.standard} keywords. Use {!Keyword.to_imap_string} for wire format
    conversion.

    {b JMAP keywords} ([$seen], [$answered], etc.) are the canonical form. Use
    {!Keyword.to_string} for JMAP format.

    {b Mailbox roles} work similarly, with {!Mailbox_attr.to_string} for IMAP
    and {!Mailbox_attr.to_jmap_role} for JMAP.

    {2 References}

    - {{:https://www.rfc-editor.org/rfc/rfc9051}RFC 9051} - IMAP4rev2.
    - {{:https://www.rfc-editor.org/rfc/rfc8621}RFC 8621} - JMAP for Mail.
    - {{:https://www.rfc-editor.org/rfc/rfc6154}RFC 6154} - IMAP Special-Use
      Mailboxes.
    - {{:https://datatracker.ietf.org/doc/draft-ietf-mailmaint-messageflag-mailboxattribute}
       draft-ietf-mailmaint} - extended keywords and attributes. *)

(** {1 Modules} *)

module Keyword = Keyword
(** Message keywords and flags, including standard IMAP flags, JMAP keywords,
    extension keywords and Apple Mail flag bits. *)

module Mailbox_attr = Mailbox_attr
(** IMAP LIST response attributes and special-use mailbox roles, with JMAP role
    conversion. *)

module Flag_color = Flag_color
(** Apple Mail flag colors and conversion to and from flag-bit keywords. *)
