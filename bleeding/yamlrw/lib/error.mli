(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** {1 Error Handling}

    Comprehensive error reporting for YAML parsing and emission.

    This module provides detailed error types that correspond to various failure
    modes in YAML processing, as specified in the
    {{:https://yaml.org/spec/1.2.2/}YAML 1.2.2 specification}. *)

(** {2 Error Classification} *)

type kind =
  (* Scanner errors *)
  | Unexpected_character of char
  | Unexpected_eof
  | Invalid_escape_sequence of string
  | Invalid_unicode_escape of string
  | Invalid_hex_escape of string
  | Invalid_tag of string
  | Invalid_anchor of string
  | Invalid_alias of string
  | Invalid_comment
  | Unclosed_single_quote
  | Unclosed_double_quote
  | Unclosed_flow_sequence
  | Unclosed_flow_mapping
  | Invalid_indentation of int * int
  | Invalid_flow_indentation
  | Tab_in_indentation
  | Invalid_block_scalar_header of string
  | Invalid_quoted_scalar_indentation of string
  | Invalid_directive of string
  | Invalid_yaml_version of string
  | Invalid_tag_directive of string
  | Reserved_directive of string
  | Illegal_flow_key_line
  | Block_sequence_disallowed
  (* Parser errors *)
  | Unexpected_token of string
  | Expected_document_start
  | Expected_document_end
  | Expected_block_entry
  | Expected_key
  | Expected_value
  | Expected_node
  | Expected_scalar
  | Expected_sequence_end
  | Expected_mapping_end
  | Duplicate_anchor of string
  | Undefined_alias of string
  | Alias_cycle of string
  | Multiple_documents
  | Mapping_key_too_long
  (* Loader errors *)
  | Invalid_scalar_conversion of string * string
  | Type_mismatch of string * string
  | Unresolved_alias of string
  | Key_not_found of string
  | Alias_expansion_node_limit of int
  | Alias_expansion_depth_limit of int
  (* Emitter errors *)
  | Invalid_encoding of string
  | Scalar_contains_invalid_chars of string
  | Anchor_not_set
  | Invalid_state of string
  (* Generic *)
  | Custom of string

(** {2 Error Value} *)

type t = {
  kind : kind;
  span : Span.t option;
  context : string list;
  source : string option;
}

(** {2 Exception} *)

exception Yamlrw_error of t
(** The main exception type raised by all yamlrw operations. *)

(** {2 Error Construction} *)

val make : ?span:Span.t -> ?context:string list -> ?source:string -> kind -> t
(** Construct an error value. *)

val raise : ?span:Span.t -> ?context:string list -> ?source:string -> kind -> 'a
(** Construct and raise an error. *)

val raise_at : Position.t -> kind -> 'a
(** Raise an error at a specific position. *)

val raise_span : Span.t -> kind -> 'a
(** Raise an error at a specific span. *)

val with_context : string -> (unit -> 'a) -> 'a
(** Execute a function and add context to any raised error. *)

(** {2 Error Formatting} *)

val kind_to_string : kind -> string
(** Convert an error kind to a human-readable string. *)

val to_string : t -> string
(** Convert an error to a human-readable string. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print an error. *)

val pp_with_source : source:string -> Format.formatter -> t -> unit
(** Pretty-print an error with source context. *)
