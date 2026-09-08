@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unified message keywords for IMAP and JMAP.

    A keyword is a unified representation of message flags that works across
    both IMAP ({{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051}) and
    JMAP ({{:https://datatracker.ietf.org/doc/html/rfc8621}RFC 8621}).

    {2 Keyword Types}

    Keywords are organized into categories based on their specification:
    - {!standard}: core flags from RFC 8621 Section 4.1.1 that map to IMAP
      system flags.
    - {!spam}: spam-related keywords for junk mail handling.
    - {!extended}: extended keywords from draft-ietf-mailmaint.
    - {!flag_bit}: Apple Mail flag color bits.

    {2 Protocol Mapping}

    IMAP system flags ([\Seen], [\Answered], etc.) map to JMAP keywords
    ([$seen], [$answered], etc.). {!to_string} and {!to_imap_string} convert
    between the two. *)

(** {1 Keyword Types} *)

type standard =
  [ `Seen  (** The message has been read. It maps to IMAP [\Seen]. *)
  | `Answered
    (** The message has been answered. It maps to IMAP [\Answered]. *)
  | `Flagged
    (** The message is flagged, or starred. It maps to IMAP [\Flagged]. *)
  | `Draft  (** The message is a draft. It maps to IMAP [\Draft]. *)
  | `Deleted
    (** The message is marked for deletion. It is IMAP only and maps to
        [\Deleted]. *)
  | `Forwarded
    (** The message has been forwarded. It is the JMAP [$forwarded] keyword. *)
  ]
(** Standard keywords per
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.1.1}RFC 8621
     Section 4.1.1}. These map to the IMAP system flags of
    {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.2}RFC 9051
     Section 2.3.2}. *)

type spam =
  [ `Phishing  (** The message is a phishing attempt. It is JMAP [$phishing]. *)
  | `Junk  (** The message is spam. It is JMAP [$junk]. *)
  | `NotJunk
    (** The message is explicitly marked as not junk. It is JMAP [$notjunk]. *)
  ]
(** Spam-related keywords for junk mail handling. *)

type extended =
  [ `HasAttachment  (** The message has attachments. *)
  | `HasNoAttachment
    (** The message has no attachments. It is mutually exclusive with
        [`HasAttachment]. *)
  | `Memo  (** The message is a memo. *)
  | `HasMemo  (** The message has an associated memo. *)
  | `CanUnsubscribe
    (** The message carries unsubscribe capability, via a List-Unsubscribe
        header. *)
  | `Unsubscribed  (** The user has unsubscribed from this sender. *)
  | `Muted
    (** The thread is muted. It is mutually exclusive with [`Followed]. *)
  | `Followed
    (** The thread is followed. It is mutually exclusive with [`Muted]. *)
  | `AutoSent  (** The message was sent automatically. *)
  | `Imported  (** The message was imported from another source. *)
  | `IsTrusted  (** The sender is trusted. *)
  | `MaskedEmail  (** The message was sent to a masked email address. *)
  | `New  (** The message is new: not yet processed by the client. *)
  | `Notify  (** The user should be notified about this message. *) ]
(** Extended keywords per draft-ietf-mailmaint. *)

type flag_bit =
  [ `MailFlagBit0  (** Bit 0 of the Apple Mail flag color encoding. *)
  | `MailFlagBit1  (** Bit 1 of the Apple Mail flag color encoding. *)
  | `MailFlagBit2  (** Bit 2 of the Apple Mail flag color encoding. *) ]
(** Apple Mail flag color bits. See {!flag_color_of_keywords} for how bits
    combine into a color. *)

type t = [ standard | spam | extended | flag_bit | `Custom of string ]
(** The unified keyword type, combining all the categories above.

    [`Custom s] holds a server-specific or application-specific keyword not
    covered by the standard categories. *)

(** {1 Conversion Functions} *)

val of_string : string -> t
(** [of_string s] is the keyword named by [s].

    Matching is case-insensitive for known keywords, and a leading [$] sigil, or
    the IMAP backslash of ["\\Seen"], is ignored when matching them. Both JMAP
    format ([$seen]) and bare format ([seen]) are accepted.

    An {i unrecognised} keyword is [`Custom s] holding [s] {b verbatim}: the
    sigil is kept and the case is not folded, so that {!to_string} is a left
    inverse of [of_string] on custom keywords. JMAP keywords are opaque strings,
    so [$important] and [important] are two distinct keywords and stripping the
    [$] would rewrite the value on the wire. The one exception is a leading
    backslash, which marks an IMAP system flag and is a character RFC 8621
    Section 4.1.1 forbids in a JMAP keyword: it is dropped.

    Examples:
    - ["$seen"] is [`Seen].
    - ["seen"] is [`Seen].
    - ["SEEN"] is [`Seen].
    - ["\\Seen"] is [`Seen], the IMAP system flag format.
    - ["my-custom-flag"] is [`Custom "my-custom-flag"].
    - ["$important"] is [`Custom "$important"].
    - ["\\Recent"] is [`Custom "Recent"]. *)

val to_string : t -> string
(** [to_string k] is the canonical JMAP format of [k].

    Standard and extended keywords carry a [$] prefix and are lowercase. Apple
    Mail flag bits preserve the mixed-case spelling under which they are
    registered in draft-ietf-mailmaint-messageflag-mailboxattribute Section
    4.1.14-4.1.16; RFC 8621 Section 4.1.1's "servers MUST return keywords in
    lowercase" binds servers, and {!of_string} accepts either spelling. A custom
    keyword is exactly what {!of_string} received, so
    [of_string s |> to_string = s] for any keyword this module does not know.

    [`Deleted] is ["$deleted"], which RFC 8621 Section 4.1.1 says is {i not}
    present in JMAP, since "IMAP uses a delete+expunge model, which JMAP does
    not". Filter it out before building a JMAP [keywords] object.

    Examples:
    - [`Seen] is ["$seen"].
    - [`MailFlagBit0] is ["$MailFlagBit0"].
    - [`Custom "foo"] is ["foo"].
    - [`Custom "$important"] is ["$important"]. *)

val to_imap_string : t -> string
(** [to_imap_string k] is the IMAP wire format of [k].

    Standard keywords that map to IMAP system flags carry a backslash prefix.
    Other known keywords carry a [$] prefix with appropriate casing. A [`Custom]
    keyword is verbatim: in IMAP a flag keyword is an atom, so [my-label] and
    [$my-label] are different flags and no sigil is added.

    Examples:
    - [`Seen] is ["\\Seen"].
    - [`Deleted] is ["\\Deleted"].
    - [`Forwarded] is ["$Forwarded"].
    - [`MailFlagBit0] is ["$MailFlagBit0"].
    - [`Custom "$important"] is ["$important"].
    - [`Custom "my-label"] is ["my-label"]. *)

(** {1 Predicates} *)

val is_standard : t -> bool
(** [is_standard k] is [true] if [k] maps to an IMAP system flag.

    The standard keywords are [`Seen], [`Answered], [`Flagged], [`Draft] and
    [`Deleted]. [`Forwarded] is {i not} an IMAP system flag. *)

val is_mutually_exclusive : t -> t -> bool
(** [is_mutually_exclusive k1 k2] is [true] if [k1] and [k2] cannot both be set
    on the same message.

    The mutually exclusive pairs are [`HasAttachment]/[`HasNoAttachment],
    [`Junk]/[`NotJunk] and [`Muted]/[`Followed]. *)

(** {1 Comparison and Pretty Printing} *)

val equal : t -> t -> bool
(** [equal k1 k2] is [true] if [k1] and [k2] are the same keyword. *)

val compare : t -> t -> int
(** [compare k1 k2] is a total order over keywords. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf k] prints [k] in JMAP format to [ppf]. *)

(** {1 Apple Mail Flag Colors} *)

type flag_color = [ `Red | `Orange | `Yellow | `Green | `Blue | `Purple | `Gray ]
(** Apple Mail flag colors, encoded as 3-bit values. Identical to
    {!Flag_color.t}; see that module for the bit pattern each color encodes. *)

val flag_color_of_keywords : t list -> flag_color option
(** [flag_color_of_keywords keywords] is the Apple Mail flag color encoded by
    the [`MailFlagBit*] members of [keywords]. Keywords other than the flag bits
    are ignored. It is [Red] if [keywords] sets none of the three bits, and
    [None] if the bits set form the undefined pattern 111. See
    {!Flag_color.of_keywords_default_red}, which this delegates to. *)

val flag_color_to_keywords : flag_color -> t list
(** [flag_color_to_keywords color] is the list of [`MailFlagBit*] keywords
    needed to represent [color]. See {!Flag_color.to_keywords}, which this
    delegates to. *)
