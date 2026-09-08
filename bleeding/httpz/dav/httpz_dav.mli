@@ portable

(** Bounded RFC 4918 protocol codecs. See [SPEC.md] for scope and policy. *)

type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE
                | `ISO_8859_1 | `ISO_8859_15 | `US_ASCII ]
val ns_xml : string
val ns_xmlns : string
(** XML and XML namespace-declaration namespace names. *)

type name = string * string
val dav : string -> name

type xml = Text of string | Element of element
and element = { name : name; attrs : (name * string) list; children : xml list }
(** Parsed fragments include inherited namespace declarations and xml:lang.
    Text is UTF-8. Lexical details such as comments and prefixes on element
    names are not retained. *)

type limits = { max_bytes : int; max_depth : int; max_nodes : int }
val default_limits : limits
val validate_limits : limits -> unit
val element : ?attrs:(name * string) list -> name -> xml list -> element
val text : element -> (string, string) result
val children : name -> element -> element list
val parse_xml : ?limits:limits -> ?encoding:encoding -> string -> (element, string) result
exception Output_too_large
val encode_xml : ?max_bytes:int -> element -> string
(** [encode_xml] raises [Output_too_large] before exceeding [max_bytes],
    including during the namespace prepass. It raises [Invalid_argument]
    for invalid XML data or names.
    Inputs are application data and must be bounded by the caller. *)

type propstat = {
  properties : element list;
  status : int;
  errors : element list;
  description : string option;
}
type outcome = Status of int | Properties of propstat list
type response = {
  hrefs : string list;
  outcome : outcome;
  errors : element list;
  description : string option;
  location : string option;
}
type multistatus = { responses : response list; description : string option }
val multistatus : ?lenient:bool -> element -> (multistatus, string) result
(** With [lenient], an href that is not a valid URL or absolute path is
    repaired by percent-encoding the characters it may not hold, for a
    server that writes member names into hrefs unencoded. Off by default. *)

val property_results : name -> response -> (element, int) result list
(** All reported occurrences in response order, including repeated PROPPATCH
    instructions. Servers may group results by status, so this order need not
    match the request's instruction order. Inspect every result for failures;
    successful PROPPATCH results do not contain the final property value.
    A failed whole-resource status yields one [Error]; a successful
    whole-resource status or an unreported property yields an empty list. *)

val property : name -> response -> (element, int) result option
(** [None] means unreported; [Error status] means reported but failed.
    Raises [Invalid_argument] for multiple occurrences instead of choosing one
    and hiding another result. Use {!property_results} for repeated reports. *)

val resolve_href : base:string -> string -> (string, string) result
(** Hrefs must be absolute HTTP(S) URLs or absolute paths. No network access. *)

type propfind = Allprop of name list | Propname | Prop of name list
type update = Set of element list | Remove of name list
val propfind : propfind -> string
val proppatch : update list -> string
(** Instructions are serialized in order; empty operations are rejected. *)

type depth = [ `Zero | `One | `Infinity ]
type tree_depth = [ `Zero | `Infinity ]
val encode_depth : depth -> string
val decode_depth : string -> depth option
type timeout = Infinite | Seconds of int64
val encode_timeout : timeout -> string
val decode_timeout : string -> timeout option

module Token : sig
  type t : immutable_data
  val of_string : string -> (t, string) result
  val to_string : t -> string
  val encode : t -> string
  val decode : string -> t option
end

type term = Token of Token.t | Etag of string
type condition = Is of term | Not of term
type if_condition =
  | Untagged of condition list list
  | Tagged of (string * condition list list) list
val encode_if : if_condition -> string
(** Raises [Invalid_argument] for empty lists, invalid resource tags or ETags. *)

val valid_etag : string -> bool
val strong_etag : string -> bool

type scope = Exclusive | Shared
type lock = {
  scope : scope;
  depth : tree_depth;
  timeout : timeout option;
  token : Token.t option;
  root : string option;
  owner : element option;
}
val lockinfo : ?owner:xml list -> scope -> string
val locks : element -> (lock list, string) result
(** Decode DAV:prop containing DAV:lockdiscovery, including empty discovery. *)

(** {1 Fragments}

    Readers for property values that tolerate mixed content and unknown
    children, and constructors for the elements request bodies are made of. *)

val attr : name -> element -> string option
val elements : element -> element list
(** Element children, in order; text is skipped. *)

val find : name -> element -> element option
(** The first child named [name]. *)

val content : element -> string
(** The concatenated text of the element and its descendants, trimmed. *)

val leaf : name -> string -> element
val empty : name -> element
val href_path : string -> string
(** The path of an href without scheme, authority, query or fragment, with
    its escapes decoded except [%2F], for display and comparison. Not a
    resource identity: see {!resolve_href}. *)

val same_href : string -> string -> bool
(** Equal {!href_path} values, ignoring one trailing slash. *)

val basename : string -> string
(** The last path segment of an href, decoded, ignoring a trailing slash. *)

(** {1 Live properties}

    The names of RFC 4918 Section 15, RFC 3253 Section 3.1.5, RFC 3744
    Sections 4 and 5, RFC 5397, RFC 5995 and RFC 6578, and readers for their
    values. A reader returns [None] or the empty list for another property. *)
module Prop : sig
  val creationdate : name
  val displayname : name
  val getcontentlanguage : name
  val getcontentlength : name
  val getcontenttype : name
  val getetag : name
  val getlastmodified : name
  val resourcetype : name
  val lockdiscovery : name
  val supportedlock : name
  val supported_report_set : name
  val principal_url : name
  val alternate_uri_set : name
  val group_membership : name
  val owner : name
  val current_user_privilege_set : name
  val principal_collection_set : name
  val current_user_principal : name
  val add_member : name
  val sync_token : name
  val hrefs : element -> string list
  (** The [DAV:href] children, trimmed. *)

  val etag : element -> string option
  (** The value of a [DAV:getetag], as a validator. *)

  val resource_types : element -> name list
  val is_collection : element -> bool
  val reports : element -> name list
  (** The reports a [DAV:supported-report-set] advertises. *)

  val privileges : element -> name list
  (** The privileges a [DAV:current-user-privilege-set] grants. *)

  val principal : element -> [ `Href of string | `Unauthenticated ] option
  (** The value of a [DAV:current-user-principal]. *)
end

(** {1 Responses} *)

val href : response -> string
(** The first href of a response. *)

val find_response : multistatus -> string -> response option
(** The response whose first href is {!same_href} as the argument. *)

val response_status : response -> int
(** The whole-resource status, or the status of the first group, or 200. *)

val succeeded : response -> element list
(** The properties reported with a success status. *)

val find_property : name -> response -> element option
(** The first property [name] reported with a success status. *)

val property_status : name -> response -> int option
val is_collection : response -> bool
val etag : response -> string option

val failures : multistatus -> (int * element list * name list) list
(** Every failed whole-resource status and property group, with its DAV error
    children and the names of the properties it reports. A 424 Failed
    Dependency is omitted: it follows another failure, RFC 4918 Section 9.2.1. *)

(** {1 Extensions} *)

val mkcol : element list -> string
(** RFC 5689 extended MKCOL body setting the given properties. *)

val mkcalendar : element list -> string
(** RFC 4791 Section 5.3.1 MKCALENDAR body setting the given properties. *)

val mkcol_response : element -> (propstat list, string) result
(** Decode a [DAV:mkcol-response] or [CALDAV:mkcalendar-response]. *)

(** RFC 6578 collection synchronization. *)
module Sync : sig
  type level = [ `One | `Infinite ]
  val request : ?token:string -> ?level:level -> ?limit:int -> name list -> string
  (** A [DAV:sync-collection] report body. No token requests an initial
      synchronization. [level] defaults to [`One]. *)

  type change =
    | Changed of response  (** Added or modified, with the requested properties. *)
    | Removed of string  (** A member reported 404. *)
    | Unsupported of string * element list
        (** A member collection reported 403, with its DAV error children. *)
  type t = { token : string option; changes : change list; truncated : bool }
  val decode : ?lenient:bool -> base:string -> element -> (t, string) result
  (** Decode a report response. [truncated] records the 507 the collection
      [base] itself reports when results were left out, Section 3.6. *)
end

(** RFC 4918 Section 16, RFC 5995, RFC 6578 and RFC 3744 condition names. *)
module Condition : sig
  val propfind_finite_depth : name
  val cannot_modify_protected_property : name
  val preserved_live_properties : name
  val no_external_entities : name
  val lock_token_submitted : name
  val no_conflicting_lock : name
  val allow_client_defined_uri : name
  val valid_sync_token : name
  val number_of_matches_within_limits : name
  val supported_report : name
  val sync_traversal_supported : name
  val need_privileges : name
  val has : name -> element list -> bool
  val hrefs : name -> element list -> string list
  (** The [DAV:href] children of the named condition. *)
end

(** RFC 6764 service discovery names. *)
module Discovery : sig
  type service = [ `Caldav | `Carddav ]
  val well_known : service -> string
  (** The well-known path, such as ["/.well-known/carddav"]. *)

  val srv_name : secure:bool -> service -> string -> string
  (** The SRV record name for a domain, such as ["_carddavs._tcp.example.com"]. *)

  val txt_path : string -> string option
  (** The [path] key of a TXT record. *)

  val mailbox : string -> (string * string) option
  (** The local part and domain of an email address. *)

  val principal_query : propfind
  (** [DAV:current-user-principal] and [DAV:principal-URL]. *)
end

(** Server request decoding and response encoding. *)
module Server : sig
  val propfind : element -> (propfind, string) result
  val proppatch : element -> (update list, string) result
  val lockinfo : element -> (scope * element option, string) result
  val if_condition : string -> (if_condition, string) result
  (** Bounds header bytes, resources, lists and terms before evaluation. *)

  val multistatus : ?max_bytes:int -> multistatus -> string
  val error : name list -> string
  val lockdiscovery : lock list -> element
end
