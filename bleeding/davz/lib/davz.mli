@@ portable

(** Bounded RFC 4918 protocol codecs. See [../SPEC.md] for scope and policy. *)
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
val parse_xml : ?limits:limits -> ?encoding:Xmlm.encoding -> string -> (element, string) result
val encode_xml : element -> string
(** [encode_xml] raises [Invalid_argument] for invalid XML data or names.
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
val multistatus : element -> (multistatus, string) result
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
