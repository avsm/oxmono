(** Typed IMAP server responses.

    {!parse_parts} parses the {!Wire} events of exactly one framed
    response, and {!parse} parses one line that carries no literal. Every
    number is checked against its protocol range and every flag,
    identifier and UID set against its grammar, so a malformed value is an
    error, never a default. An untagged response this module does not type
    is [Other] with its text.

    In a FETCH response only PREVIEW, ENVELOPE and BODYSTRUCTURE literals
    are retained, inlined into the row's [raw] as quoted strings. Every
    other FETCH literal, such as a message body, is streamed by the caller
    and never retained. In the control responses listed at {!parse_parts}
    every literal is retained. *)

(** {1 Response codes} *)

type compound_object_id = {
  account_id : string option;
  mailbox_id : string option;
  email_id : string option;
  thread_id : string option;
  unknown : (string * string) list;
      (** Unrecognised keys, in uppercase, with their identifiers in wire
          order. *)
}
(** The type for draft OBJECTID+ compound identities. Each identifier is
    an RFC 8474 [objectid] and each key appears at most once. *)

type code =
  | Uidvalidity of int64  (** From 1 to 4294967295. *)
  | Uidnext of int64  (** From 1 to 4294967296. *)
  | Highestmodseq of int64  (** A positive MODSEQ. *)
  | Appenduid of int64 * int64  (** The UIDVALIDITY and the one new UID. *)
  | Appenduid_set of int64 * string
      (** The UIDVALIDITY and the UID set of a MULTIAPPEND, holding more
          than one UID. *)
  | Copyuid of int64 * string * string
      (** The UIDVALIDITY, the source UID set and the target UID set, which
          have the same number of UIDs. *)
  | Modified of string
      (** The UID set a conditional STORE did not change. *)
  | Permanentflags of string list
      (** The flags, with [\*] for new keywords. *)
  | Mailboxid of string
  | Objectid of compound_object_id
  | Messagelimit of int64 * int64 option
      (** The limit and, when given, the lowest UID processed. *)
  | Uidrequired
  | Uidnotsticky
  | Expungeissued
  | Overquota
  | Metadata_longentries of int64
  | Metadata_maxsize of int64
  | Metadata_toomany
  | Metadata_noprivate
  | Notificationoverflow
  | Badevent of string list
  | Unseen of int64  (** A sequence number. *)
  | Read_only
  | Read_write
  | Nomodseq
  | Closed
  | Alert
  | Capability of Capability.t list
      (** A [[CAPABILITY ...]] code, deduplicated in {!Capability.compare}
          order. *)
  | Unavailable
  | Authenticationfailed
  | Authorizationfailed
  | Expired
  | Privacyrequired
  | Contactadmin
  | Noperm
  | Inuse
  | Corruption
  | Serverbug
  | Clientbug
  | Cannot
  | Limit
  | Alreadyexists
  | Nonexistent
  | Unknown_cte
  | Trycreate
  | Compressionactive
  | Other_code of string
      (** An unknown code, with the text between the brackets. *)
(** The type for response codes. A known code whose arguments are
    malformed makes the whole response an error. *)

val response_code_name : code -> string option
(** [response_code_name c] is the constant wire token of [c], such as
    [UIDNEXT] or [METADATA], without its arguments or any server text. It
    is [None] for [Other_code], so the result is safe to log. *)

(** {1 FETCH data} *)

type fetch = {
  seq : int64;
      (** The message sequence number, or the UID in a [Uidfetch]. *)
  uid : int64 option;  (** Always [Some seq] in a [Uidfetch]. *)
  flags : string list option;
  modseq : int64 option;
  size : int64 option;  (** The RFC822.SIZE. *)
  internal_date : Internal_date.t option;
  email_id : string option;
  thread_id : string option option;
      (** [None] when absent, and [Some None] for an RFC 8474 THREADID of
          NIL. *)
  preview : string option option;
      (** The RFC 8970 PREVIEW. [None] when absent, [Some None] for NIL,
          otherwise a UTF-8 string of at most 256 characters and 1024
          bytes, possibly empty. *)
  literals : (string * int64) list;
      (** The name and length of each streamed literal item, in wire
          order. *)
  raw : string;
      (** The response from the [FETCH] or [UIDFETCH] keyword on, without
          the final CRLF and otherwise unaltered. A retained literal appears
          as a quoted string. A streamed literal keeps its [{n}] marker and
          CRLF but not its payload. *)
}
(** The type for FETCH rows. An item appears at most once per row. The
    extractors below decode the items the record does not type. *)

type envelope_address = {
  name : string option;
  route : string option;
  mailbox : string option;
  host : string option;
}
(** The type for ENVELOPE addresses. [host = None] marks the start of an
    RFC 5322 group and [mailbox = None] its end. Strings are the decoded
    IMAP strings. *)

type envelope = {
  date : string option;
  subject : string option;
  from : envelope_address list option;
  sender : envelope_address list option;
  reply_to : envelope_address list option;
  to_ : envelope_address list option;
  cc : envelope_address list option;
  bcc : envelope_address list option;
  in_reply_to : string option;
  message_id : string option;
}
(** The type for ENVELOPE structures, RFC 9051 §7.5.2. [None] stands for
    NIL. *)

type binary =
  | Nil
  | Inline of string
  | Literal of int64
      (** A streamed payload of the given length, which is not
          retained. *)
(** The type for BINARY payloads. *)

val fetch_binary : fetch -> section:int list -> offset:int64 option ->
  (binary option, string) result
(** [fetch_binary row ~section ~offset] is the BINARY payload of [row] for
    the part [section] at the partial [offset], or [None] if [row] has
    none. BINARY.SIZE items are ignored. The error covers a duplicate
    payload, a BINARY item for another section or offset, an inline
    string holding NUL, CR or LF, a [raw] over 1 MiB, and a [section] of
    more than 100 parts or a part outside 1 to 4294967295. *)

val fetch_binary_size : fetch -> section:int list ->
  (int64 option, string) result
(** [fetch_binary_size row ~section] is the BINARY.SIZE of the part
    [section] in [row], or [None] if [row] has none. Items for other
    sections are ignored. The size is any non-negative signed 64-bit
    number, since IMAP4rev2 allows body parts over 4 GiB. The error covers
    a duplicate or malformed item. *)

val fetch_envelope : fetch -> (envelope option, string) result
(** [fetch_envelope row] is the ENVELOPE of [row], or [None] if [row] has
    none. The error covers a duplicate or malformed item, a string over
    64 KiB, an address list of more than 1024 addresses and a [raw] over
    1 MiB. {!parse_parts} retains ENVELOPE literals of up to 64 KiB each
    and 256 KiB in total. *)

type body_extension =
  | Ext_nil
  | Ext_string of string
  | Ext_number of int64
  | Ext_list of body_extension list
(** The type for BODYSTRUCTURE extension values. *)

type bodystructure =
  | Single_part of {
      media_type : string;
      subtype : string;
      parameters : (string * string) list option;
      content_id : string option;
      description : string option;
      encoding : string;
      octets : int64;
      lines : int64 option;  (** Present for TEXT parts. *)
      enclosed : (envelope * bodystructure * int64) option;
          (** For MESSAGE/RFC822 and MESSAGE/GLOBAL, the enclosed
              envelope, structure and line count. *)
      extensions : body_extension list;
    }
  | Multipart of {
      parts : bodystructure list;
      subtype : string;
      extensions : body_extension list;
    }
(** The type for BODYSTRUCTURE trees. [extensions] holds the extension
    fields in RFC order, including ones this module does not know. *)

val fetch_bodystructure : fetch -> (bodystructure option, string) result
(** [fetch_bodystructure row] is the BODYSTRUCTURE of [row], or [None] if
    [row] has none. Unknown extension fields are kept. The error covers a
    duplicate item, malformed standard extension fields, a string over
    64 KiB, more than 256 body parts, more than 256 parameters in one list,
    more than 4096 extension values, deep nesting and a [raw] over 1 MiB.
    {!parse_parts} retains BODYSTRUCTURE literals of up to 64 KiB each and
    256 KiB in total. *)

val fetch_objectid : fetch -> (compound_object_id option, string) result
(** [fetch_objectid row] is the draft OBJECTID+ item of [row], or [None]
    if [row] has none. The error covers a duplicate or malformed item, an
    unbalanced quote and a [raw] over 1 MiB. *)

(** {1 Mailbox and server data} *)

type list_result = {
  subscribed : bool;
      (** [true] for an LSUB response. For LIST (SUBSCRIBED), check for
          the [\Subscribed] attribute instead, since LSUB may include
          unsubscribed parents. *)
  attributes : string list;
      (** Every attribute, in its received spelling. *)
  delimiter : string option;  (** [None] for NIL. *)
  mailbox : string;  (** The wire name. *)
  old_name : string option;  (** The wire name from OLDNAME. *)
  childinfo : string list option;
  children : [ `Has_children | `Has_no_children | `Unknown ];
      (** [`Has_no_children] also covers [\Noinferiors]. *)
  selectable : bool;
      (** [false] under [\Noselect] or [\NonExistent], which RFC 5258
          makes imply [\Noselect]. *)
  special_use : string list;
      (** The special-use attributes, such as [\Sent] and [\Trash]. *)
  raw : string;
}
(** The type for LIST and LSUB rows. Extended data other than OLDNAME and
    CHILDINFO is available only through [raw]. *)

type namespace_entry = {
  prefix : string;  (** The wire name prefix. *)
  delimiter : string option;
  extensions : (string * string list) list;
}
(** The type for namespace descriptions. *)

type namespace = {
  personal : namespace_entry list option;
  other_users : namespace_entry list option;
  shared : namespace_entry list option;
  raw : string;
}
(** The type for NAMESPACE responses. [None] stands for NIL, and a present
    class has at least one entry. Prefixes are wire names, to be decoded
    in the mailbox mode in effect. *)

type esearch = {
  tag : string option;
  uid : bool;
  min : int64 option;  (** From 1 to 4294967295. *)
  max : int64 option;  (** From 1 to 4294967295. *)
  count : int64 option;  (** From 0 to 4294967295. *)
  all : string option;  (** The sequence set as received. *)
  modseq : int64 option;  (** A positive signed 64-bit number. *)
  partial : (string * string option) option;
      (** The RFC 9394 requested range and the returned set, or [None] for
          NIL, both as received. *)
  raw : string;
}
(** The type for ESEARCH responses. A return field appears at most once.
    MIN and MAX of an ESORT are sort positions, so MIN may exceed MAX. [all]
    and the results of [partial] keep their wire span order. *)

type uidbatches = {
  tag : string;
  ranges : (int64 * int64) list;
      (** Boundaries [(high, low)] in server order, descending and
          disjoint. A boundary does not imply that its UIDs exist. *)
  raw : string;
}
(** The type for UIDBATCHES responses. *)

type acl = {
  mailbox : string;
  entries : (string * string) list;
      (** Identifiers with their RFC 4314 rights. *)
  raw : string;
}
(** The type for ACL responses. *)

type list_rights = {
  mailbox : string;
  identifier : string;
  required : string;
  optional : string list;
  raw : string;
}
(** The type for LISTRIGHTS responses. No right appears twice across
    [required] and [optional]. *)

type my_rights = { mailbox : string; rights : string; raw : string }
(** The type for MYRIGHTS responses. *)

type quota = {
  root : string;
  resources : (string * int64 * int64) list;
      (** Each resource name with its usage and limit. *)
  raw : string;
}
(** The type for QUOTA responses. Unknown resource names are kept. A usage
    or limit above the largest signed 64-bit number is an error. *)

type quota_root = { mailbox : string; roots : string list; raw : string }
(** The type for QUOTAROOT responses. *)

type metadata_payload =
  | Metadata_values of (string * string option) list
      (** Entry names with their values, [None] for NIL. *)
  | Metadata_changed of string list
      (** The entry names of an unsolicited change notice. *)
(** The type for METADATA payloads. *)

type metadata = { mailbox : string; payload : metadata_payload; raw : string }
(** The type for METADATA responses. Entry names start with [/] and hold
    no [*], [%] or control character. *)

type mailbox_status = {
  mailbox : string;
  messages : int64 option;
  unseen : int64 option;
  uidnext : int64 option;
  uidvalidity : int64 option;
  highestmodseq : int64 option;
  mailbox_id : string option;
  objectid : compound_object_id option;
  size : int64 option;
  deleted : int64 option;
  deleted_storage : int64 option;
  raw : string;
}
(** The type for STATUS responses. [mailbox] must be an atom, a quoted
    string or a literal that {!parse_parts} retained. An item appears at
    most once, unknown items are skipped, and UIDVALIDITY and UIDNEXT are
    range-checked. *)

type thread = { number : int64 option; children : thread list }
(** The type for THREAD nodes. [number] is a message sequence number, or a
    UID for UID THREAD, and [None] stands for a dummy parent. A run of
    message numbers becomes a chain of single-child nodes. *)

(** {1 Responses} *)

type untagged =
  | Ok of code option * string
  | No of code option * string
  | Bad of code option * string
  | Bye of code option * string
  | Preauth of code option * string
  | Capability of Capability.t list
      (** The tokens, deduplicated in {!Capability.compare} order. *)
  | Enabled of Capability.t list
      (** The tokens, deduplicated in {!Capability.compare} order. *)
  | Flags of string list
  | Exists of int64
  | Recent of int64
  | Expunge of int64
  | Fetch of fetch
  | Uidfetch of fetch
      (** An RFC 9586 UIDFETCH, whose [seq] is the leading UID. A UID item
          in the row must match it. *)
  | List of list_result
  | Namespace of namespace
  | Status of mailbox_status
  | Jmapaccess of string  (** The JMAP session URL. *)
  | Uidbatches of uidbatches
  | Acl of acl
  | List_rights of list_rights
  | My_rights of my_rights
  | Quota of quota
  | Quota_root of quota_root
  | Metadata of metadata
  | Vanished of { earlier : bool; uids : string }
      (** The RFC 7162 VANISHED UID set as received. *)
  | Search of int64 list
      (** An RFC 7162 [(MODSEQ n)] suffix is validated and dropped. *)
  | Sort of int64 list
      (** Numbers in server order without repeats. An RFC 7162
          [(MODSEQ n)] suffix is validated and dropped. *)
  | Thread of thread list
      (** RFC 5256 threads in server order. The response is bounded to
          100 levels of parenthesised nesting and 2 MiB, and the node count
          is left to the caller. A chain of members is one level however
          long it is. A repeated
          message number is an error. An empty THREAD may end in one
          space. *)
  | Esearch of esearch
  | Other of string  (** A response this module does not type. *)
(** The type for untagged responses. Counts, sequence numbers and UIDs
    are checked against their protocol ranges. *)

type t =
  | Tagged of { tag : string; status : [ `Ok | `No | `Bad ];
                code : code option; text : string }
  | Untagged of untagged
  | Continuation of string  (** The text after [+]. *)
(** The type for server responses. *)

(** {1 SELECT} *)

type select_metadata = {
  exists : int64;
  recent : int64 option;
  uidvalidity : int64;
  uidnext : int64;
  highestmodseq : int64 option;
  nomodseq : bool;
  flags : string list option;
  permanentflags : string list option;
  mailbox_id : string option;
  objectid : compound_object_id option;
  readonly : bool option;
      (** From a READ-ONLY or READ-WRITE code, when one was sent. *)
  uidnotsticky : bool;
      (** An untagged NO carried UIDNOTSTICKY, so UIDs are unsafe for a
          persistent mirror. *)
}
(** The type for the state a SELECT or EXAMINE reports. *)

val select_metadata : t list -> (select_metadata, string) result
(** [select_metadata rs] is the state reported by [rs], the responses of a
    SELECT or EXAMINE including its tagged completion. A later value
    replaces an earlier one. The error covers a missing tagged OK, a
    missing EXISTS, UIDVALIDITY or UIDNEXT, and NOMODSEQ together with
    HIGHESTMODSEQ. For a tagged NO or BAD it names the status, the
    response code and the text. *)

(** {1 Parsing} *)

val parse : string -> (t, string) result
(** [parse line] is the response in [line], one response line with or
    without its final CRLF. A literal marker in [line] is not followed, so
    a line that introduces a literal must go through {!parse_parts}. *)

val parse_parts : ?max_control_literal:int -> Wire.event list ->
  (t, string) result
(** [parse_parts ~max_control_literal events] is the response framed by
    [events], which must hold exactly one complete response.
    [max_control_literal] bounds each retained literal and also their
    combined size, and defaults to 16 MiB. The literals of LIST, LSUB,
    STATUS, NAMESPACE, ACL, LISTRIGHTS, MYRIGHTS, QUOTA, QUOTAROOT,
    METADATA, ESEARCH and LANGUAGE responses are retained. So are the FETCH
    items named in the module description, a PREVIEW literal up to 1024
    bytes and an ENVELOPE or BODYSTRUCTURE literal up to 64 KiB, each kind
    up to 256 KiB in total. When several limits are exceeded the error
    names the first.

    @raise Invalid_argument if [max_control_literal] is negative. *)
