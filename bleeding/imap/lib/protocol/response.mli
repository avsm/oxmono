(** Typed response metadata. [parse_parts] accepts all events from exactly one
    framed response, including any streamed literals. In [Fetch] it retains
    only PREVIEW, ENVELOPE and BODYSTRUCTURE literals, as quoted strings in
    [raw]. Message body literals are streamed and never retained. *)

type compound_object_id = {
  account_id : string option;
  mailbox_id : string option;
  email_id : string option;
  thread_id : string option;
  unknown : (string * string) list;
}
(** Draft OBJECTID+ -06 compound identity. Every known identifier is optional;
    unrecognized key-value pairs are retained for forward compatibility. *)

type code =
  | Uidvalidity of int64
  | Uidnext of int64
  | Highestmodseq of int64
  | Appenduid of int64 * int64
  | Appenduid_set of int64 * string
  | Copyuid of int64 * string * string
  | Modified of string
  | Permanentflags of string list
  | Mailboxid of string
  | Objectid of compound_object_id
  | Messagelimit of int64 * int64 option
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
  | Unseen of int64
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

val response_code_name : code -> string option
(** Canonical constant wire token only, never argument values or server text.
    Unknown [Other_code] returns [None]. Useful for sanitized diagnostics. *)

type fetch = {
  seq : int64;
  uid : int64 option;
  flags : string list option;
  modseq : int64 option;
  size : int64 option;
  internal_date : Internal_date.t option;
  email_id : string option;
  thread_id : string option option;
  (** [None] means absent; [Some None] represents RFC 8474 THREADID NIL. *)
  preview : string option option;
  (** RFC 8970: absent, LAZY NIL, or a UTF-8 string (including empty).
      Preview strings are bounded to 256 Unicode characters and 1024 bytes. *)
  literals : (string * int64) list;
  raw : string;
  (** [raw] is the response line from the [FETCH] or [UIDFETCH] keyword on,
      without the final CRLF and otherwise unaltered. A retained literal
      appears as a quoted string. A streamed literal keeps its [{n}] marker
      and CRLF but not its payload. *)
}

type envelope_address = {
  name : string option;
  route : string option;
  mailbox : string option;
  host : string option;
}
(** [host = None] represents an RFC 5322 group marker; [mailbox = None]
    marks the end of a group. Strings are exact decoded IMAP strings. *)

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

type binary = Nil | Inline of string | Literal of int64
val fetch_binary : fetch -> section:int list -> offset:int64 option ->
  (binary option, string) result
(** Extract the exact requested BINARY section and response offset. [None]
    means absent; [Some Nil] is an explicit NIL; [Literal n] describes streamed
    bytes, not retained content. Duplicate or mismatched BINARY payload fields
    are errors; unrelated FLAGS and BINARY.SIZE metadata are ignored. *)
val fetch_binary_size : fetch -> section:int list -> (int64 option, string) result
(** Exact BINARY.SIZE section, as a nonnegative signed-int64 number, including
    IMAP4rev2 body sizes exceeding uint32. *)
val fetch_envelope : fetch -> (envelope option, string) result
(** Decode the RFC 3501/9051 ENVELOPE FETCH data item. [None] means absent;
    duplicate or malformed data is an error. Envelope literals are retained
    by [parse_parts], bounded to 64 KiB each and 256 KiB combined. *)

type body_extension =
  | Ext_nil
  | Ext_string of string
  | Ext_number of int64
  | Ext_list of body_extension list
(** Extension fields are retained in RFC order, including unknown future
    fields and nested values. *)

type bodystructure =
  | Single_part of {
      media_type : string;
      subtype : string;
      parameters : (string * string) list option;
      content_id : string option;
      description : string option;
      encoding : string;
      octets : int64;
      lines : int64 option;
      enclosed : (envelope * bodystructure * int64) option;
      extensions : body_extension list;
    }
  | Multipart of {
      parts : bodystructure list;
      subtype : string;
      extensions : body_extension list;
    }

val fetch_bodystructure : fetch -> (bodystructure option, string) result
(** Decode RFC 3501/9051 BODYSTRUCTURE. [None] means absent. The parser
    rejects malformed standard extension fields, duplicate values, excessive
    nesting and oversized structures. Unknown future extensions are retained.
    Literals are retained by [parse_parts] up to 64 KiB each and 256 KiB
    combined. *)

val fetch_objectid : fetch -> (compound_object_id option, string) result
(** Decode a draft OBJECTID+ FETCH data item from a parsed FETCH row.
    [None] means no such item, while malformed or duplicate data is an error. *)

type list_result = {
  subscribed : bool;
  (** True for an LSUB response. For LIST (SUBSCRIBED), inspect the exact
      [\\Subscribed] attribute; LSUB may include unsubscribed parents. *)
  attributes : string list;
  delimiter : string option;
  mailbox : string;
  old_name : string option;
  childinfo : string list option;
  children : [ `Has_children | `Has_no_children | `Unknown ];
  selectable : bool;
  special_use : string list;
  raw : string;
}
(** [mailbox] and [old_name] are exact wire names. [selectable] includes
    RFC 5258's NonExistent => NoSelect implication. Unknown attributes and
    complete extended data remain available through [attributes]/[raw]. *)

type namespace_entry = {
  prefix : string;
  delimiter : string option;
  extensions : (string * string list) list;
}
type namespace = {
  personal : namespace_entry list option;
  other_users : namespace_entry list option;
  shared : namespace_entry list option;
  raw : string;
}
(** [None] denotes NIL. A present namespace class has at least one entry.
    Prefixes are exact wire names and should be decoded using the negotiated
    mode. *)

(** ESEARCH return fields cannot repeat. MIN/MAX are positive uint32 values;
    COUNT is uint32 and MODSEQ is a nonnegative signed int64. MIN/MAX describe
    sort positions for ESORT, so MIN may exceed MAX numerically. ALL and PARTIAL
    preserve wire span order: ESORT expands each range ascending, including a
    reversed range such as [4:3], without reordering the comma-separated spans. *)
type esearch = {
  tag : string option;
  uid : bool;
  min : int64 option;
  max : int64 option;
  count : int64 option;
  all : string option;
  modseq : int64 option;
  partial : (string * string option) option;
  (** Requested RFC 5267/9394 range and returned finite set, or NIL. *)
  raw : string;
}

type uidbatches = {
  tag : string;
  ranges : (int64 * int64) list;
  (** Server order: descending, nonoverlapping high:low UID boundaries.
      Message existence inside each boundary is not guaranteed. *)
  raw : string;
}

type acl = { mailbox : string; entries : (string * string) list; raw : string }
type list_rights = {
  mailbox : string; identifier : string; required : string;
  optional : string list; raw : string
}
type my_rights = { mailbox : string; rights : string; raw : string }
type quota = { root : string; resources : (string * int64 * int64) list;
               raw : string }
(** Unknown resource names are retained. RFC number64 values above signed
    int64 are rejected by this parser rather than silently truncated. *)
type quota_root = { mailbox : string; roots : string list; raw : string }
type metadata_payload =
  | Metadata_values of (string * string option) list
  | Metadata_changed of string list
type metadata = { mailbox : string; payload : metadata_payload; raw : string }

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

type thread = { uid : int64 option; children : thread list }
(** A THREAD node. [None] preserves a dummy parent. A sequence of message
    numbers becomes a chain of single-child nodes. Response numbers are UIDs
    only when the command was UID THREAD. *)
type untagged =
  | Ok of code option * string
  | No of code option * string
  | Bad of code option * string
  | Bye of code option * string
  | Preauth of code option * string
  | Capability of Capability.t list
  | Enabled of Capability.t list
      (** [Capability] and [Enabled] tokens are deduplicated and in
          {!Capability.compare} order. *)
  | Flags of string list
  | Exists of int64
  | Recent of int64
  | Expunge of int64
  | Fetch of fetch
  | Uidfetch of fetch
  (** RFC 9586: [fetch.seq] is the leading UID, never a sequence number.
      If the UID data item is present, the parser verifies it matches. *)
  | List of list_result
  | Namespace of namespace
  | Status of mailbox_status
  | Jmapaccess of string
  | Uidbatches of uidbatches
  | Acl of acl
  | List_rights of list_rights
  | My_rights of my_rights
  | Quota of quota
  | Quota_root of quota_root
  | Metadata of metadata
  | Vanished of { earlier : bool; uids : string }
  | Search of int64 list
  | Sort of int64 list
  (** An RFC 7162 [(MODSEQ n)] suffix on SEARCH or SORT is validated and
      dropped. *)
  | Thread of thread list
  (** RFC 5256 ordered results, bounded to 100000 nodes, 100 levels of
      parenthesised nesting, and 2 MiB of syntax. A chain of members is one
      level however long it is. Duplicate message numbers are rejected. An
      empty THREAD may have one trailing space for interoperability. *)
  | Esearch of esearch
  | Other of string

type t =
  | Tagged of { tag : string; status : [ `Ok | `No | `Bad ];
                code : code option; text : string }
  | Untagged of untagged
  | Continuation of string

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
  uidnotsticky : bool;
}

val select_metadata : t list -> (select_metadata, string) result
(** Extract a completed SELECT/EXAMINE prelude, including its tagged OK.
    Mandatory EXISTS, UIDVALIDITY and UIDNEXT data must be present. A tagged
    NO or BAD yields an error naming its response code and text.
    [uidnotsticky] records the untagged NO response code that makes UIDs
    unsafe for a persistent mirror. *)

val parse : string -> (t, string) result
val parse_parts : ?max_control_literal:int -> Wire.event list ->
  (t, string) result
(** [parse_parts events] parses one framed response. [max_control_literal]
    bounds each retained literal and also their combined size, and defaults
    to 16 MiB. Retained literals are those of LIST, LSUB, STATUS, NAMESPACE,
    ACL, LISTRIGHTS, MYRIGHTS, QUOTA, QUOTAROOT, METADATA, ESEARCH and
    LANGUAGE responses, and the FETCH items named in the module header.
    Message body literals are streamed and not retained here. When several
    limits are exceeded the error names the first.

    @raise Invalid_argument if [max_control_literal] is negative. *)
