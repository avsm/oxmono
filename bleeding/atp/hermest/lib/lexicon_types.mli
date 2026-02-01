(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(** AT Protocol Lexicon type definitions.

    This module defines the OCaml representation of AT Protocol Lexicon schemas
    as specified in the
    {{:https://atproto.com/specs/lexicon}Lexicon specification}.

    Lexicons define the schema for data and RPC methods in the AT Protocol. Each
    lexicon document has a unique NSID (Namespaced Identifier) and contains type
    definitions.

    {2 Type System}

    The lexicon type system includes:

    - {b Primitives}: string, integer, boolean, bytes
    - {b Complex}: blob, cid-link, array, object, ref, union
    - {b Special}: token, unknown
    - {b Methods}: query, procedure, subscription
    - {b Data}: record
    - {b Auth}: permission-set (OAuth scopes)

    {2 Example Lexicon}

    A simple lexicon JSON:
    {v
    {
      "lexicon": 1,
      "id": "app.bsky.feed.post",
      "defs": {
        "main": {
          "type": "record",
          "key": "tid",
          "record": {
            "type": "object",
            "required": ["text", "createdAt"],
            "properties": {
              "text": {"type": "string", "maxLength": 300},
              "createdAt": {"type": "string", "format": "datetime"}
            }
          }
        }
      }
    }
    v} *)

(** {1 Primitive Type Specifications} *)

type string_spec = {
  format : string option;
      (** Format hint: "at-uri", "at-identifier", "did", "handle", "nsid",
          "cid", "datetime", "uri", "language". *)
  min_length : int option;  (** Minimum string length in UTF-8 bytes. *)
  max_length : int option;  (** Maximum string length in UTF-8 bytes. *)
  min_graphemes : int option;
      (** Minimum length in Unicode grapheme clusters. *)
  max_graphemes : int option;
      (** Maximum length in Unicode grapheme clusters. *)
  known_values : string list option;  (** Suggested values (open enum). *)
  enum : string list option;  (** Allowed values (closed enum). *)
  const : string option;  (** Fixed constant value. *)
  default : string option;  (** Default value if not provided. *)
  description : string option;  (** Human-readable description. *)
}
(** String type specification with validation constraints. *)

type integer_spec = {
  minimum : int option;  (** Minimum allowed value. *)
  maximum : int option;  (** Maximum allowed value. *)
  enum : int list option;  (** Allowed values (closed enum). *)
  const : int option;  (** Fixed constant value. *)
  default : int option;  (** Default value if not provided. *)
  description : string option;  (** Human-readable description. *)
}
(** Integer type specification with validation constraints. *)

type boolean_spec = {
  const : bool option;  (** Fixed constant value. *)
  default : bool option;  (** Default value if not provided. *)
  description : string option;  (** Human-readable description. *)
}
(** Boolean type specification. *)

type bytes_spec = {
  min_length : int option;  (** Minimum byte length. *)
  max_length : int option;  (** Maximum byte length. *)
  description : string option;  (** Human-readable description. *)
}
(** Byte string specification. *)

type blob_spec = {
  accept : string list option;
      (** Accepted MIME types (e.g., ["image/png"; "image/jpeg"]). *)
  max_size : int option;  (** Maximum blob size in bytes. *)
  description : string option;  (** Human-readable description. *)
}
(** Blob (binary large object) specification for file uploads. *)

type cid_link_spec = {
  description : string option;  (** Human-readable description. *)
}
(** CID link specification for content-addressed references. *)

(** {1 Complex Type Specifications} *)

type array_spec = {
  items : type_def;  (** Element type definition. *)
  min_length : int option;  (** Minimum array length. *)
  max_length : int option;  (** Maximum array length. *)
  description : string option;  (** Human-readable description. *)
}
(** Array type specification. *)

and property = {
  type_def : type_def;  (** The property's type. *)
  description : string option;  (** Human-readable description. *)
}
(** Object property definition. *)

and object_spec = {
  properties : (string * property) list;  (** Named properties. *)
  required : string list option;  (** Property names that must be present. *)
  nullable : string list option;  (** Property names that may be null. *)
  description : string option;  (** Human-readable description. *)
}
(** Object type specification with named properties. *)

and ref_spec = {
  ref_ : string;
      (** Reference target: "#localDef" for same-file or
          "com.example.defs#someDef" for cross-file references. *)
  description : string option;  (** Human-readable description. *)
}
(** Reference to another type definition. *)

and union_spec = {
  refs : string list;  (** List of allowed type references. *)
  closed : bool option;
      (** If [true], only listed types allowed. If [false] or absent, union is
          open. *)
  description : string option;  (** Human-readable description. *)
}
(** Union type (discriminated by [$type] field). *)

and token_spec = {
  description : string option;  (** Human-readable description. *)
}
(** Token type - a unique identifier string. *)

and unknown_spec = {
  description : string option;  (** Human-readable description. *)
}
(** Unknown type - accepts any valid data. *)

(** {1 Method Specifications} *)

and params_spec = {
  properties : (string * property) list;  (** Query/procedure parameters. *)
  required : string list option;  (** Required parameter names. *)
  description : string option;  (** Human-readable description. *)
}
(** HTTP query parameters specification. *)

and body_def = {
  encoding : string;
      (** MIME type: "application/json", "application/cbor", etc. *)
  schema : type_def option;  (** Body schema (usually an object). *)
  description : string option;  (** Human-readable description. *)
}
(** Request/response body definition. *)

and error_def = {
  name : string;  (** Error name (e.g., "InvalidRequest"). *)
  description : string option;  (** Human-readable description. *)
}
(** Error response definition. *)

and query_spec = {
  parameters : params_spec option;  (** URL query parameters. *)
  output : body_def option;  (** Response body schema. *)
  errors : error_def list option;  (** Possible error responses. *)
  description : string option;  (** Human-readable description. *)
}
(** HTTP GET query method specification. *)

and procedure_spec = {
  parameters : params_spec option;  (** URL query parameters. *)
  input : body_def option;  (** Request body schema. *)
  output : body_def option;  (** Response body schema. *)
  errors : error_def list option;  (** Possible error responses. *)
  description : string option;  (** Human-readable description. *)
}
(** HTTP POST procedure method specification. *)

and subscription_spec = {
  parameters : params_spec option;  (** Connection parameters. *)
  message : body_def option;  (** Message schema for subscription events. *)
  errors : error_def list option;  (** Possible error responses. *)
  description : string option;  (** Human-readable description. *)
}
(** WebSocket subscription specification. *)

(** {1 Record Specification} *)

and record_spec = {
  key : string;  (** Record key type: "tid" (timestamp ID), "nsid", or "any". *)
  record : object_spec;  (** The record's object schema. *)
  description : string option;  (** Human-readable description. *)
}
(** Repository record specification. *)

(** {1 Permission Specifications}

    OAuth permission scopes for AT Protocol authorization. *)

and permission_spec = {
  resource : string;
      (** Resource type: "rpc" for API calls or "repo" for repository
          operations. *)
  inherit_aud : bool option;
      (** Whether to inherit audience from parent token. *)
  lxm : string list option;  (** For "rpc" resources: allowed method NSIDs. *)
  action : string list option;
      (** For "repo" resources: allowed actions ("create", "update", "delete").
      *)
  collection : string list option;
      (** For "repo" resources: allowed collection NSIDs. *)
}
(** Individual permission grant within a permission set. *)

and permission_set_spec = {
  title : string;  (** Human-readable title for the permission set. *)
  detail : string option;
      (** Detailed description of what this permission allows. *)
  permissions : permission_spec list;  (** List of permission grants. *)
  description : string option;  (** Human-readable description. *)
}
(** OAuth permission set (scope) specification. *)

(** {1 Type Definition Sum Type} *)

and type_def =
  | String of string_spec
  | Integer of integer_spec
  | Boolean of boolean_spec
  | Bytes of bytes_spec
  | Blob of blob_spec
  | CidLink of cid_link_spec
  | Array of array_spec
  | Object of object_spec
  | Ref of ref_spec
  | Union of union_spec
  | Token of token_spec
  | Unknown of unknown_spec
  | Query of query_spec
  | Procedure of procedure_spec
  | Subscription of subscription_spec
  | Record of record_spec
  | PermissionSet of permission_set_spec  (** A lexicon type definition. *)

(** {1 Document Structure} *)

type def_entry = {
  name : string;
      (** Definition name within the lexicon (e.g., "main", "view"). *)
  type_def : type_def;  (** The type definition. *)
}
(** A named definition within a lexicon document. *)

type lexicon_doc = {
  lexicon : int;  (** Lexicon version (always 1). *)
  id : string;  (** Lexicon NSID (e.g., "app.bsky.feed.post"). *)
  revision : int option;  (** Schema revision number. *)
  description : string option;
      (** Human-readable description of the lexicon. *)
  defs : def_entry list;  (** Type definitions in this lexicon. *)
}
(** A complete lexicon document. *)

type parse_result = (lexicon_doc, string) result
(** Result of parsing a lexicon JSON file. *)

(** {1 Jsont Codecs}

    These codecs can be used to parse and serialize lexicon documents using the
    jsont library. *)

val string_spec_jsont : string_spec Jsont.t
(** JSON codec for string specifications. *)

val integer_spec_jsont : integer_spec Jsont.t
(** JSON codec for integer specifications. *)

val boolean_spec_jsont : boolean_spec Jsont.t
(** JSON codec for boolean specifications. *)

val bytes_spec_jsont : bytes_spec Jsont.t
(** JSON codec for bytes specifications. *)

val blob_spec_jsont : blob_spec Jsont.t
(** JSON codec for blob specifications. *)

val cid_link_spec_jsont : cid_link_spec Jsont.t
(** JSON codec for CID link specifications. *)

val ref_spec_jsont : ref_spec Jsont.t
(** JSON codec for reference specifications. *)

val union_spec_jsont : union_spec Jsont.t
(** JSON codec for union specifications. *)

val token_spec_jsont : token_spec Jsont.t
(** JSON codec for token specifications. *)

val unknown_spec_jsont : unknown_spec Jsont.t
(** JSON codec for unknown type specifications. *)

val error_def_jsont : error_def Jsont.t
(** JSON codec for error definitions. *)

val type_def_jsont : type_def Jsont.t
(** JSON codec for type definitions (discriminated union). *)

val property_jsont : property Jsont.t
(** JSON codec for object properties. *)

val object_spec_jsont : object_spec Jsont.t
(** JSON codec for object specifications. *)

val array_spec_jsont : array_spec Jsont.t
(** JSON codec for array specifications. *)

val params_spec_jsont : params_spec Jsont.t
(** JSON codec for parameter specifications. *)

val body_def_jsont : body_def Jsont.t
(** JSON codec for body definitions. *)

val query_spec_jsont : query_spec Jsont.t
(** JSON codec for query specifications. *)

val procedure_spec_jsont : procedure_spec Jsont.t
(** JSON codec for procedure specifications. *)

val subscription_spec_jsont : subscription_spec Jsont.t
(** JSON codec for subscription specifications. *)

val record_spec_jsont : record_spec Jsont.t
(** JSON codec for record specifications. *)

val def_entry_jsont : def_entry Jsont.t
(** JSON codec for definition entries. *)

val lexicon_doc_jsont : lexicon_doc Jsont.t
(** JSON codec for lexicon documents. *)
