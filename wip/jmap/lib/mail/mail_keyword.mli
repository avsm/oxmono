@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Email keywords.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.1.1} RFC 8621
     Section 4.1.1} gives an Email a [keywords] property, a [String[Boolean]]
    map whose keys are keywords. A keyword registered by a specification has a
    constructor of its own and any other keyword is [`Custom]. Keywords are case
    insensitive, so {!equal} and {!compare} fold case.

    @canonical Jmap.Proto.Keyword *)

(** {1 Keywords} *)

type standard =
  [ `Seen
  | `Flagged
  | `Answered
  | `Draft
  | `Forwarded
  | `Phishing
  | `Junk
  | `NotJunk ]
(** The type for the keywords RFC 8621 Section 4.1.1 registers. It is
    {!Mail_flag.Keyword.standard} together with {!Mail_flag.Keyword.spam} and
    without [`Deleted]. The IMAP [\Recent] and [\Deleted] flags are not among
    them, since "IMAP uses a delete+expunge model, which JMAP does not". *)

type extended = Mail_flag.Keyword.extended
(** The type for the keywords registered by
    {{:https://datatracker.ietf.org/doc/draft-ietf-mailmaint-messageflag-mailboxattribute}
     draft-ietf-mailmaint-messageflag-mailboxattribute}. It is an alias of
    {!Mail_flag.Keyword.extended}, so the two libraries cannot register
    different sets. *)

type flag_bits = Mail_flag.Keyword.flag_bit
(** The type for the three keywords that carry an Apple Mail flag colour as a
    bit pattern. It is an alias of {!Mail_flag.Keyword.flag_bit}, so the two
    libraries cannot register different sets. See {!flag_color_of_keywords}. *)

type t = [ standard | extended | flag_bits | `Custom of string ]
(** The type for keywords. A [`Custom] keyword holds its wire spelling verbatim,
    including any [$] sigil. *)

val of_string : string -> t
(** [of_string s] is the keyword [s]. A keyword this module does not know is
    [`Custom s], holding [s] verbatim, since ["$important"] and ["important"]
    are two distinct JMAP keywords. A registered keyword is recognised only with
    its [$] prefix, so [of_string "seen"] is [`Custom "seen"]. RFC 8621 Section
    4.1.1 keeps [\Deleted] out of JMAP, so ["$deleted"] is a [`Custom] keyword
    too. *)

val to_string : t -> string
(** [to_string k] is the wire spelling of [k]. A registered keyword is spelled
    as its specification registers it, which is lowercase with a leading [$] for
    every keyword but the three of {!flag_bits}. A [`Custom] keyword is returned
    unchanged, so [to_string (of_string s)] is [s] for any [s] this module does
    not know. *)

val validate : t -> (t, string) result
(** [validate k] is [Ok k] if [k] is a well formed keyword and [Error msg]
    otherwise, where [msg] is a human readable message.

    RFC 8621 Section 4.1.1 makes a keyword "a case-insensitive string of 1-255
    characters in the ASCII subset %x21-%x7e (excludes control chars and space)"
    that "MUST NOT include" the eight characters IMAP reserves, the open and
    close parenthesis, the open brace, the close bracket, the percent sign, the
    asterisk, the double quote and the backslash. Only a [`Custom] keyword can
    fail the check, the registered ones being well formed by construction.

    The check belongs where a client builds a keyword. Decoding never applies
    it, so a server that sends a malformed keyword still yields a usable Email.
*)

val equal : t -> t -> bool
(** [equal k k'] is [true] if [k] and [k'] are the same keyword. Comparison is
    on {!to_string} and ignores case. *)

val compare : t -> t -> int
(** [compare k k'] is a total order on keywords, the octet order of {!to_string}
    with case folded. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf k] prints {!to_string} of [k] on [ppf], with control bytes escaped.
    This matters for malformed custom keywords retained from a server response.
*)

(** {1 Keyword maps} *)

val of_list : t list -> (t * bool) list
(** [of_list ks] is the [keywords] map holding exactly [ks], every keyword
    mapped to [true].

    @raise Stdlib.exception-Invalid_argument
      if a keyword of [ks] fails {!validate} or duplicates another keyword
      without regard to case. *)

val of_mail_flag : Mail_flag.Keyword.t -> t option
(** [of_mail_flag k] is the JMAP form of the IMAP keyword [k], or [None] if [k]
    has none. RFC 8621 Section 4.1.1 keeps [\Deleted] out of JMAP, so [`Deleted]
    is dropped rather than emitted as [$deleted]. *)

val of_mail_flag_list : Mail_flag.Keyword.t list -> t list
(** [of_mail_flag_list ks] is [ks] with every keyword that has no JMAP form
    removed. *)

val to_mail_flag : t -> Mail_flag.Keyword.t
(** [to_mail_flag k] is [k] as an IMAP keyword. *)

(** {1 Codecs} *)

val jsont : t Jsont.t
(** [jsont] is the codec for a keyword as a JSON string. Decoding is
    {!of_string}. Encoding is {!to_string} and raises [Jsont.Error] for a
    keyword that fails {!validate}. *)

val map_jsont : (t * bool) list Jsont.t
(** [map_jsont] is the codec for the [String[Boolean]] map of the [keywords]
    property. In both directions it rejects a keyword mapped to [false] and a
    key that duplicates another without regard to case, which covers a key
    repeated verbatim. Decoding orders the list by key. Encoding raises
    [Jsont.Error] for a keyword that fails {!validate}. *)

(** {1 Flag colours} *)

type flag_color = Mail_flag.Flag_color.t
(** The type for Apple Mail flag colours, which are [`Red], [`Orange],
    [`Yellow], [`Green], [`Blue], [`Purple] and [`Gray]. It is an alias of
    {!Mail_flag.Flag_color.t}, so the two libraries cannot register different
    sets. A colour is carried by the three {!flag_bits} keywords read as a three
    bit number, bit 0 being [$MailFlagBit0]. It is meaningful only on an Email
    that also has the [$flagged] keyword. *)

val flag_color_of_keywords : t list -> flag_color option
(** [flag_color_of_keywords ks] is the flag colour the {!flag_bits} keywords of
    [ks] carry, or [None] if [ks] holds none of them or holds all three, a
    pattern the specification leaves undefined. An Email with no bit set is
    therefore [None] rather than [`Red], since the two cannot be told apart. *)

val flag_color_to_keywords : flag_color -> t list
(** [flag_color_to_keywords c] is the {!flag_bits} keywords to set for the
    colour [c]. It is the empty list for [`Red], whose bit pattern is zero. *)
