@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mailboxes.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-2} RFC 8621 Section
     2} defines the Mailbox object, a named set of Emails that may have a parent
    Mailbox and a role.

    @canonical Jmap.Proto.Mailbox *)

(** {1 Properties} *)

type property =
  [ `Id
  | `Name
  | `Parent_id
  | `Role
  | `Sort_order
  | `Total_emails
  | `Unread_emails
  | `Total_threads
  | `Unread_threads
  | `My_rights
  | `Is_subscribed ]
(** The type for the properties a [Mailbox/get] may ask for. *)

val property_to_string : [< property ] -> string
(** [property_to_string p] is the wire name of [p], such as ["parentId"]. *)

val property_of_string : string -> property option
(** [property_of_string s] is the property whose wire name is [s], or [None] if
    there is none. The comparison is by octet. *)

(** {1 Rights} *)

(** The rights the user has on a Mailbox. *)
module Rights : sig
  type t = {
    may_read_items : bool;
        (** [true] if the user may read the Emails of the Mailbox. *)
    may_add_items : bool;
        (** [true] if the user may add Emails to the Mailbox. *)
    may_remove_items : bool;
        (** [true] if the user may take Emails out of the Mailbox. *)
    may_set_seen : bool;
        (** [true] if the user may set the [$seen] keyword of an Email of the
            Mailbox. *)
    may_set_keywords : bool;
        (** [true] if the user may set any keyword other than [$seen]. *)
    may_create_child : bool;
        (** [true] if the user may create a Mailbox under this one. *)
    may_rename : bool;
        (** [true] if the user may rename the Mailbox or move it under another
            parent. *)
    may_delete : bool;  (** [true] if the user may destroy the Mailbox. *)
    may_submit : bool;
        (** [true] if the user may submit an Email of the Mailbox for delivery.
        *)
  }
  (** The type for MailboxRights objects. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a MailboxRights. *)
end

(** {1 Roles} *)

type special_use = Mail_flag.Mailbox_attr.special_use
(** The type for the IMAP special-use attributes of RFC 6154, from which RFC
    8621 Section 2 takes the Mailbox roles. *)

type role =
  [ `All
  | `Archive
  | `Drafts
  | `Flagged
  | `Important
  | `Inbox
  | `Junk
  | `Sent
  | `Trash
  | `Snoozed
  | `Scheduled
  | `Memos
  | `Other of string ]
(** The type for Mailbox roles, the names RFC 8621 Section 2 takes from the IANA
    IMAP mailbox name attributes registry. [`Snoozed], [`Scheduled] and [`Memos]
    come from draft-ietf-mailmaint. A name outside them is [`Other]. The IMAP
    [\Subscribed] attribute is not a role and is [`Other "subscribed"]. *)

val role_to_string : role -> string
(** [role_to_string r] is the wire name of [r], which is the IMAP attribute name
    in lowercase. *)

val role_of_string : string -> role
(** [role_of_string s] is the role named [s], or [`Other s] if [s] is not a role
    name. The comparison folds case, so [role_of_string "INBOX"] is [`Inbox] and
    {!role_to_string} of it is ["inbox"], which rewrites the wire value of a
    server that spells a role otherwise. Decoding is lenient here because RFC
    8621 Section 2 names the roles in lowercase and a role the client cannot
    read is worth less than one it can. *)

val role_jsont : role Jsont.t
(** [role_jsont] is the codec for a role. *)

val role_of_special_use : special_use -> role option
(** [role_of_special_use su] is the role matching the IMAP special-use attribute
    [su], or [None] if [su] is not a role. Only [`Subscribed] is [None], RFC
    8621 Section 2 making it a name attribute rather than a role. *)

val special_use_of_role : role -> special_use option
(** [special_use_of_role r] is the IMAP special-use attribute matching [r], or
    [None] for an [`Other] role that names no attribute. *)

(** {1 Mailboxes} *)

type t = {
  id : Proto_id.t option;  (** The server assigned id of the Mailbox. *)
  name : string option;
      (** The name of the Mailbox, unique among its siblings. *)
  parent_id : Proto_id.t option;
      (** The Mailbox this one is under. [None] means the top level, or that the
          property was not asked for. *)
  role : role option;
      (** The role of the Mailbox. [None] means no role, or that the property
          was not asked for. *)
  sort_order : int64 option;
      (** The position of the Mailbox among its siblings when they are shown,
          lower first. *)
  total_emails : int64 option;  (** The number of Emails in the Mailbox. *)
  unread_emails : int64 option;
      (** The number of Emails in the Mailbox without the [$seen] keyword. *)
  total_threads : int64 option;
      (** The number of Threads with at least one Email in the Mailbox. *)
  unread_threads : int64 option;
      (** The number of such Threads with at least one unread Email in the
          Mailbox. *)
  my_rights : Rights.t option;  (** The rights the user has on the Mailbox. *)
  is_subscribed : bool option;
      (** [true] if the user has subscribed to the Mailbox. *)
}
(** The type for Mailbox objects. A property is [None] when the [Mailbox/get]
    did not ask for it. *)

val empty : t
(** [empty] is the Mailbox with every property unset. *)

val id : t -> Proto_id.t option
(** [id m] is the id of the Mailbox [m], or [None] if the [Mailbox/get] did not
    ask for it. *)

val creation : string -> t Proto_id.creation
(** [creation s] is {!Jmap.Proto.Id.val-creation} [s] as the creation id of an
    Mailbox. Binding it here rather than through [Id.creation] fixes the type of
    the record it names at the binding, so a creation id defined before the
    [/set] that uses it needs no annotation. *)

val create :
  name:string ->
  ?parent_id:Proto_id.t ->
  ?role:role ->
  ?sort_order:int64 ->
  ?is_subscribed:bool ->
  unit ->
  (t, string) result
(** [create ~name ()] is the object of a [Mailbox/set] [create] entry. Only the
    client settable properties of RFC 8621 Section 2 are taken, the server set
    counts and [my_rights] being left unset. [parent_id] may be a creation
    reference naming a Mailbox created earlier in the same call.

    The error holds a human readable message when the arguments break Section 2,
    which requires [name] to "be a Net-Unicode string of at least 1 character in
    length" and [sort_order] to "be an integer in the range 0 <= sortOrder <
    2^31". [name] is checked for being non-empty and for being valid UTF-8 free
    of Unicode noncharacters, which is as much of Net-Unicode as the I-JSON
    profile of RFC 8620 Section 1.5 also demands. The maximum length of a name
    is server policy, [maxSizeMailboxName] in the mail capability, and is not
    checked here. *)

val create_exn :
  name:string ->
  ?parent_id:Proto_id.t ->
  ?role:role ->
  ?sort_order:int64 ->
  ?is_subscribed:bool ->
  unit ->
  t
(** [create_exn ~name ()] is {!create}.

    @raise Invalid_argument if the arguments break RFC 8621 Section 2. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a Mailbox. [parentId] and [role] are always present
    and encode as an explicit [null] when they are [None], RFC 8621 Section 2
    typing them [Id|null] and [String|null] and giving [null] the meanings "at
    the top level" and "no role". *)

(** {1 Queries} *)

(** Filter conditions for a [Mailbox/query]. *)
module Filter_condition : sig
  type t = {
    parent_id : Proto_id.t option option;
        (** Keep the Mailboxes under this one. [Some None] keeps the top level
            Mailboxes. *)
    name : string option;
        (** Keep the Mailboxes whose name holds this string. *)
    role : role option option;
        (** Keep the Mailboxes with this role. [Some None] keeps the Mailboxes
            with no role. *)
    has_any_role : bool option;
        (** Keep the Mailboxes that have a role, or those that have none. *)
    is_subscribed : bool option;
        (** Keep the Mailboxes the user has subscribed to, or those it has not.
        *)
  }
  (** The type for the FilterCondition of a [Mailbox/query] (RFC 8621 Section
      2.3). A field of [None] does not filter. *)

  val empty : t
  (** [empty] is the condition with every field unset, which Section 2.3 makes
      true for every Mailbox. Build a condition from it with record update
      syntax, as in [{ empty with role = Some (Some `Inbox) }]. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Mailbox FilterCondition. *)
end

type filter = Filter_condition.t Proto_filter.filter
(** The type for the [filter] argument of a [Mailbox/query]. *)

val filter_jsont : filter Jsont.t
(** [filter_jsont] is the codec for the [filter] argument of a [Mailbox/query].
*)

val filter :
  ?parent_id:Proto_id.t option ->
  ?name:string ->
  ?role:role option ->
  ?has_any_role:bool ->
  ?is_subscribed:bool ->
  unit ->
  filter
(** [filter ()] is the {!type-filter} of one {!Filter_condition} keeping the
    Mailboxes that satisfy every argument given, as one condition of RFC 8621
    Section 2.3. An argument left out sets no field and so filters nothing.
    [filter ()] keeps every Mailbox. [~parent_id:None] keeps the top level
    Mailboxes and [~role:None] keeps the Mailboxes with no role, RFC 8621
    Section 2.3 typing both properties [T|null]. *)

type sort_property = [ `Sort_order | `Name ]
(** The type for the properties a [Mailbox/query] sorts on, which are the two
    RFC 8621 Section 2.3 requires a server to support. *)

val sort :
  ?ascending:bool ->
  ?collation:string ->
  sort_property ->
  Proto_filter.comparator
(** [sort p] is the comparator ordering a [Mailbox/query] on [p]. [ascending]
    defaults to [true], and only [false] makes the comparator descending.
    [collation] is left to the server unless given. *)

(** {1 Mailbox/changes} *)

type changes_response = {
  changes : Proto_method.changes_response;
      (** The standard [/changes] response arguments. *)
  updated_properties : string list option;
      (** The properties that may have changed on the updated Mailboxes, or
          [None] if any property may have. RFC 8621 Section 2.2: "If only the
          [totalEmails], [unreadEmails], [totalThreads], and/or [unreadThreads]
          Mailbox properties have changed since the old state, this will be the
          list of properties that may have changed. If the server is unable to
          tell if only counts have changed, it MUST just be null." *)
}
(** The type for the response arguments of a [Mailbox/changes] call. RFC 8621
    Section 2.2 makes it a standard [/changes] response "but with one extra
    argument to the response", which the generic
    {!Jmap.Proto.Method.changes_response} has no field for. *)

val changes_response_jsont : changes_response Jsont.t
(** [changes_response_jsont] is the codec for the response arguments of a
    [Mailbox/changes] call. [updatedProperties] is always present and encodes as
    an explicit [null] when it is [None]. *)
