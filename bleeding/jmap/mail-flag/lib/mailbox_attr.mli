@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unified mailbox attributes and roles.

    A mailbox attribute is a unified representation spanning IMAP LIST response
    attributes
    ({{:https://www.rfc-editor.org/rfc/rfc9051#section-7.2.2}RFC 9051 Section
      7.2.2}), special-use mailbox flags
    ({{:https://www.rfc-editor.org/rfc/rfc6154}RFC 6154}), and JMAP mailbox
    roles ({{:https://www.rfc-editor.org/rfc/rfc8621}RFC 8621}).

    {2 References}
    - {{:https://www.rfc-editor.org/rfc/rfc9051}RFC 9051} - IMAP4rev2.
    - {{:https://www.rfc-editor.org/rfc/rfc6154}RFC 6154} - IMAP LIST Extension
      for Special-Use Mailboxes.
    - {{:https://www.rfc-editor.org/rfc/rfc5258}RFC 5258} - IMAP4 LIST Command
      Extensions.
    - {{:https://www.rfc-editor.org/rfc/rfc8457}RFC 8457} - IMAP \$Important
      Keyword and \Important Special-Use Attribute.
    - {{:https://www.rfc-editor.org/rfc/rfc8621}RFC 8621} - JMAP for Mail. *)

(** {1 IMAP LIST Attributes}

    Attributes returned in IMAP LIST responses per
    {{:https://www.rfc-editor.org/rfc/rfc9051#section-7.2.2}RFC 9051 Section
     7.2.2}. *)

type list_attr =
  [ `Noinferiors
    (** [\Noinferiors] marks that no child mailboxes are possible under this
        mailbox, either because the underlying storage does not support them or
        because the mailbox is at the hierarchy depth limit. *)
  | `Noselect
    (** [\Noselect] marks that the mailbox cannot be selected. It exists only to
        hold child mailboxes and is not a valid destination for messages. *)
  | `Marked
    (** [\Marked] marks that the server considers the mailbox interesting,
        typically because it holds messages new since it was last selected. *)
  | `Unmarked
    (** [\Unmarked] marks that the mailbox holds no messages new since it was
        last selected. *)
  | `Subscribed
    (** [\Subscribed] marks that the mailbox is subscribed. It is returned when
        the SUBSCRIBED selection option is specified or implied. *)
  | `HasChildren
    (** [\HasChildren] marks that the mailbox has child mailboxes. It is part of
        the CHILDREN return option
        ({{:https://www.rfc-editor.org/rfc/rfc5258}RFC 5258}). *)
  | `HasNoChildren
    (** [\HasNoChildren] marks that the mailbox has no child mailboxes. It is
        part of the CHILDREN return option
        ({{:https://www.rfc-editor.org/rfc/rfc5258}RFC 5258}). *)
  | `NonExistent
    (** [\NonExistent] marks that the mailbox name is part of the hierarchy but
        does not refer to an existing mailbox
        ({{:https://www.rfc-editor.org/rfc/rfc5258}RFC 5258}). It implies
        [\Noselect]. *)
  | `Remote
    (** [\Remote] marks that the mailbox is located on a remote server
        ({{:https://www.rfc-editor.org/rfc/rfc5258}RFC 5258}). *) ]

(** {1 Special-Use Roles}

    Special-use mailbox roles per
    {{:https://www.rfc-editor.org/rfc/rfc6154}RFC 6154} and
    {{:https://www.rfc-editor.org/rfc/rfc8621}RFC 8621}, identifying mailboxes
    with a specific purpose. *)

type special_use =
  [ `All
    (** [\All] is a virtual mailbox containing every message in the user's
        message store. An implementation may omit some messages. *)
  | `Archive
    (** [\Archive] holds archived messages. What counts as archived may vary by
        server. *)
  | `Drafts
    (** [\Drafts] holds draft messages, typically ones being composed but not
        yet sent. *)
  | `Flagged
    (** [\Flagged] is a virtual mailbox containing every message marked with the
        [\Flagged] flag. *)
  | `Important
    (** [\Important] holds messages deemed important to the user
        ({{:https://www.rfc-editor.org/rfc/rfc8457}RFC 8457}). *)
  | `Inbox
    (** [Inbox] is the user's inbox. It is registered by
        {{:https://www.rfc-editor.org/rfc/rfc8621#section-10.5.1}RFC 8621
         Section 10.5.1} as a "JMAP only" attribute; it has no IMAP special-use
        equivalent, since INBOX is a reserved mailbox {i name} in IMAP, so
        {!to_string} emits it without a backslash. *)
  | `Junk  (** [\Junk] holds messages identified as spam. *)
  | `Sent  (** [\Sent] holds copies of messages that have been sent. *)
  | `Subscribed
    (** [\Subscribed] is retained in this union only because it is shared with
        {!list_attr}. It is a LIST name attribute, {b not} a special use;
        {!is_special_use} and {!to_jmap_role} both reject it. See
        {!to_jmap_role} for the RFC 8621 Section 2 reasoning. *)
  | `Trash
    (** [\Trash] holds messages that have been deleted or marked for deletion.
    *)
  | `Snoozed
    (** [snoozed] holds messages snoozed until a later time
        (draft-ietf-mailmaint-special-use-extensions). *)
  | `Scheduled
    (** [scheduled] holds messages scheduled to be sent at a future time
        (draft-ietf-mailmaint-special-use-extensions). *)
  | `Memos
    (** [memos] holds memo or note messages
        (draft-ietf-mailmaint-special-use-extensions). *) ]

(** {1 Unified Attribute Type} *)

type t = [ list_attr | special_use | `Extension of string ]
(** The unified mailbox attribute type, combining LIST attributes, special-use
    roles, and server-specific extensions. An extension holds its original
    string form, without a leading backslash if it had one. *)

(** {1 Conversion Functions} *)

val of_string : string -> t
(** [of_string s] is the mailbox attribute named by [s], an IMAP wire format
    string. [s] may optionally carry a leading backslash. Matching is
    case-insensitive. An unrecognised attribute is [`Extension s].

    ["spam"] (and ["\\Spam"]) is an alias for [`Junk]; that spelling does not
    survive an [of_string]/{!to_string} round trip.

    Examples:
    - [of_string "\\Drafts"] is [`Drafts].
    - [of_string "drafts"] is [`Drafts].
    - [of_string "\\X-Custom"] is [`Extension "X-Custom"].
    - [of_string "\\Spam"] is [`Junk]. *)

val to_string : t -> string
(** [to_string attr] is the IMAP wire format of [attr].

    An attribute name carries a leading backslash exactly when the specification
    defining it gives it an implied one:

    - the LIST name attributes of
      {{:https://www.rfc-editor.org/rfc/rfc9051#section-7.2.2}RFC 9051 Section
       7.2.2} and {{:https://www.rfc-editor.org/rfc/rfc5258}RFC 5258}, namely
      [`Noinferiors], [`Noselect], [`Marked], [`Unmarked], [`Subscribed],
      [`HasChildren], [`HasNoChildren], [`NonExistent] and [`Remote];
    - the special-use attributes of
      {{:https://www.rfc-editor.org/rfc/rfc6154#section-2}RFC 6154 Section 2},
      namely [`All], [`Archive], [`Drafts], [`Flagged], [`Junk], [`Sent] and
      [`Trash], and [`Important] from
      {{:https://www.rfc-editor.org/rfc/rfc8457}RFC 8457};
    - [`Extension] names, standing in for server-specific IMAP attributes.

    The remaining names are registered in the IANA "IMAP Mailbox Name
    Attributes" registry {i without} an implied backslash and are emitted bare:

    - [`Snoozed], [`Scheduled] and [`Memos], because
      draft-ietf-mailmaint-messageflag-mailboxattribute Section 4.2 states that
      "none of the attribute names in this section have an implied backslash.
      This sets them apart from those specified in Section 2 of [RFC 6154]";
    - [`Inbox], registered by
      {{:https://www.rfc-editor.org/rfc/rfc8621#section-10.5.1}RFC 8621 Section
       10.5.1} as attribute name ["Inbox"] with usage notes "JMAP only". There
      is no [\\Inbox] IMAP special use to spell.

    {!of_string} ignores a leading backslash, so every name still round trips.

    Examples:
    - [to_string `Drafts] is ["\\Drafts"].
    - [to_string `HasChildren] is ["\\HasChildren"].
    - [to_string `Snoozed] is ["Snoozed"].
    - [to_string `Inbox] is ["Inbox"].
    - [to_string (`Extension "X-Custom")] is ["\\X-Custom"]. *)

val to_jmap_role : t -> string option
(** [to_jmap_role attr] is the lowercase JMAP role string for [attr], or [None]
    for a LIST attribute with no JMAP role.

    [to_jmap_role] is [Some] for exactly the attributes {!is_special_use}
    accepts.

    [`Subscribed] is {b not} among them, even though [\\Subscribed] does appear
    in the IANA "IMAP Mailbox Name Attributes" registry that
    {{:https://www.rfc-editor.org/rfc/rfc8621#section-2}RFC 8621 Section 2}
    draws role names from. That section ties [role] to the IMAP SPECIAL-USE
    extension ({{:https://www.rfc-editor.org/rfc/rfc6154}RFC 6154}) and requires
    that a Mailbox "MUST only have a single role, and there MUST NOT be two
    Mailboxes in the same account with the same role". [\\Subscribed] is a LIST
    name attribute (RFC 9051 Section 7.2.2) rather than a special use, and any
    number of mailboxes may carry it at once, so it cannot satisfy that
    uniqueness rule. RFC 8621 Section 2 models subscription separately, with the
    per-user ["isSubscribed"] boolean that "corresponds to IMAP [RFC3501]
    mailbox subscriptions".

    Examples:
    - [to_jmap_role `Drafts] is [Some "drafts"].
    - [to_jmap_role `Inbox] is [Some "inbox"].
    - [to_jmap_role `Subscribed] is [None].
    - [to_jmap_role `Noselect] is [None]. *)

val of_jmap_role : string -> special_use option
(** [of_jmap_role s] is the special-use attribute named by the JMAP role string
    [s], or [None] if [s] names no role. [s] should be lowercase, as per JMAP
    convention.

    [of_jmap_role] is deliberately more permissive than {!to_jmap_role} on one
    name: ["subscribed"] is [Some `Subscribed], because a server reading RFC
    8621 Section 2's registry wording literally may send it, even though this
    library never emits it as a role.

    Examples:
    - [of_jmap_role "drafts"] is [Some `Drafts].
    - [of_jmap_role "inbox"] is [Some `Inbox].
    - [of_jmap_role "subscribed"] is [Some `Subscribed].
    - [of_jmap_role "unknown"] is [None]. *)

(** {1 Predicates} *)

val is_special_use : t -> bool
(** [is_special_use attr] is [true] if [attr] is a special-use role, as opposed
    to a LIST attribute or an extension. It is [true] for exactly the attributes
    for which {!to_jmap_role} is [Some].

    Examples:
    - [is_special_use `Drafts] is [true].
    - [is_special_use `Noselect] is [false].
    - [is_special_use `Subscribed] is [false]; see {!to_jmap_role}.
    - [is_special_use (`Extension "x")] is [false]. *)

val is_selectable : t -> bool
(** [is_selectable attr] is [false] if [attr] marks that the mailbox cannot be
    selected: [`Noselect] and [`NonExistent]. It is [true] for every other
    attribute.

    A mailbox may carry several attributes; check that none of them is
    [`Noselect] or [`NonExistent] to determine whether the mailbox as a whole is
    selectable.

    Examples:
    - [is_selectable `Noselect] is [false].
    - [is_selectable `NonExistent] is [false].
    - [is_selectable `Drafts] is [true].
    - [is_selectable `HasChildren] is [true]. *)

(** {1 Comparison and Pretty Printing} *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same attribute. *)

val compare : t -> t -> int
(** [compare a b] is a total order over attributes. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf attr] prints [attr] in IMAP wire format to [ppf]. *)
