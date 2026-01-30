(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** {1 Error Handling}

    Comprehensive error reporting for YAML parsing and emission.

    This module provides detailed error types that correspond to various failure
    modes in YAML processing, as specified in the
    {{:https://yaml.org/spec/1.2.2/}YAML 1.2.2 specification}.

    Each error includes:
    - A classification of the error type ({!type:kind})
    - Optional source location information ({!type:Span.t})
    - A context stack showing where the error occurred
    - Optional source text for error display

    See also
    {{:https://yaml.org/spec/1.2.2/#31-processes}Section 3.1 (Processes)} for
    background on the YAML processing model. *)

(** {2 Error Classification}

    Error kinds are organized by the processing stage where they occur:
    - Scanner errors: Lexical analysis failures (character-level)
    - Parser errors: Syntax errors in event stream
    - Loader errors: Semantic errors during representation construction
    - Emitter errors: Failures during YAML generation *)
type kind =
  (* Scanner errors - see {{:https://yaml.org/spec/1.2.2/#51-character-set}Section 5.1} *)
  | Unexpected_character of char
      (** Invalid character in input. See
          {{:https://yaml.org/spec/1.2.2/#51-character-set}Section 5.1
           (Character Set)}. *)
  | Unexpected_eof  (** Premature end of input. *)
  | Invalid_escape_sequence of string
      (** Invalid escape in double-quoted string. See
          {{:https://yaml.org/spec/1.2.2/#57-escaped-characters}Section 5.7
           (Escaped Characters)}. *)
  | Invalid_unicode_escape of string
      (** Invalid Unicode escape sequence (\uXXXX or \UXXXXXXXX). *)
  | Invalid_hex_escape of string
      (** Invalid hexadecimal escape sequence (\xXX). *)
  | Invalid_tag of string
      (** Malformed tag syntax. See
          {{:https://yaml.org/spec/1.2.2/#681-node-tags}Section 6.8.1 (Node
           Tags)}. *)
  | Invalid_anchor of string
      (** Malformed anchor name. See
          {{:https://yaml.org/spec/1.2.2/#3222-anchors-and-aliases}Section
           3.2.2.2 (Anchors and Aliases)}. *)
  | Invalid_alias of string
      (** Malformed alias reference. See
          {{:https://yaml.org/spec/1.2.2/#3222-anchors-and-aliases}Section
           3.2.2.2 (Anchors and Aliases)}. *)
  | Invalid_comment
      (** Comment not properly separated from content. See
          {{:https://yaml.org/spec/1.2.2/#62-comments}Section 6.2 (Comments)}.
      *)
  | Unclosed_single_quote
      (** Unterminated single-quoted scalar. See
          {{:https://yaml.org/spec/1.2.2/#72-single-quoted-style}Section 7.2
           (Single-Quoted Style)}. *)
  | Unclosed_double_quote
      (** Unterminated double-quoted scalar. See
          {{:https://yaml.org/spec/1.2.2/#73-double-quoted-style}Section 7.3
           (Double-Quoted Style)}. *)
  | Unclosed_flow_sequence
      (** Missing closing bracket \] for flow sequence. See
          {{:https://yaml.org/spec/1.2.2/#742-flow-sequences}Section 7.4.2 (Flow
           Sequences)}. *)
  | Unclosed_flow_mapping
      (** Missing closing brace \} for flow mapping. See
          {{:https://yaml.org/spec/1.2.2/#743-flow-mappings}Section 7.4.3 (Flow
           Mappings)}. *)
  | Invalid_indentation of int * int
      (** Incorrect indentation level (expected, got). See
          {{:https://yaml.org/spec/1.2.2/#61-indentation-spaces}Section 6.1
           (Indentation Spaces)}. *)
  | Invalid_flow_indentation
      (** Content in flow collection must be indented. See
          {{:https://yaml.org/spec/1.2.2/#74-flow-styles}Section 7.4 (Flow
           Styles)}. *)
  | Tab_in_indentation
      (** Tab character used for indentation (only spaces allowed). See
          {{:https://yaml.org/spec/1.2.2/#61-indentation-spaces}Section 6.1
           (Indentation Spaces)}. *)
  | Invalid_block_scalar_header of string
      (** Malformed block scalar header (| or >). See
          {{:https://yaml.org/spec/1.2.2/#81-block-scalar-styles}Section 8.1
           (Block Scalar Styles)}. *)
  | Invalid_quoted_scalar_indentation of string
      (** Incorrect indentation in quoted scalar. *)
  | Invalid_directive of string
      (** Malformed directive. See
          {{:https://yaml.org/spec/1.2.2/#68-directives}Section 6.8
           (Directives)}. *)
  | Invalid_yaml_version of string
      (** Unsupported YAML version in %YAML directive. See
          {{:https://yaml.org/spec/1.2.2/#681-yaml-directives}Section 6.8.1
           (YAML Directives)}. *)
  | Invalid_tag_directive of string
      (** Malformed %TAG directive. See
          {{:https://yaml.org/spec/1.2.2/#682-tag-directives}Section 6.8.2 (TAG
           Directives)}. *)
  | Reserved_directive of string
      (** Reserved directive name. See
          {{:https://yaml.org/spec/1.2.2/#683-reserved-directives}Section 6.8.3
           (Reserved Directives)}. *)
  | Illegal_flow_key_line
      (** Key and colon must be on same line in flow context. See
          {{:https://yaml.org/spec/1.2.2/#743-flow-mappings}Section 7.4.3 (Flow
           Mappings)}. *)
  | Block_sequence_disallowed
      (** Block sequence entries not allowed in this context. See
          {{:https://yaml.org/spec/1.2.2/#82-block-collection-styles}Section 8.2
           (Block Collection Styles)}. *)
  (* Parser errors - see {{:https://yaml.org/spec/1.2.2/#3-processing-yaml-information}Section 3 (Processing)} *)
  | Unexpected_token of string  (** Unexpected token in event stream. *)
  | Expected_document_start
      (** Expected document start marker (---). See
          {{:https://yaml.org/spec/1.2.2/#912-document-markers}Section 9.1.2
           (Document Markers)}. *)
  | Expected_document_end
      (** Expected document end marker (...). See
          {{:https://yaml.org/spec/1.2.2/#912-document-markers}Section 9.1.2
           (Document Markers)}. *)
  | Expected_block_entry
      (** Expected block sequence entry marker (-). See
          {{:https://yaml.org/spec/1.2.2/#821-block-sequences}Section 8.2.1
           (Block Sequences)}. *)
  | Expected_key
      (** Expected mapping key. See
          {{:https://yaml.org/spec/1.2.2/#822-block-mappings}Section 8.2.2
           (Block Mappings)}. *)
  | Expected_value
      (** Expected mapping value after colon. See
          {{:https://yaml.org/spec/1.2.2/#822-block-mappings}Section 8.2.2
           (Block Mappings)}. *)
  | Expected_node  (** Expected a YAML node. *)
  | Expected_scalar  (** Expected a scalar value. *)
  | Expected_sequence_end
      (** Expected closing bracket \] for flow sequence. See
          {{:https://yaml.org/spec/1.2.2/#742-flow-sequences}Section 7.4.2 (Flow
           Sequences)}. *)
  | Expected_mapping_end
      (** Expected closing brace \} for flow mapping. See
          {{:https://yaml.org/spec/1.2.2/#743-flow-mappings}Section 7.4.3 (Flow
           Mappings)}. *)
  | Duplicate_anchor of string
      (** Anchor name defined multiple times. See
          {{:https://yaml.org/spec/1.2.2/#3222-anchors-and-aliases}Section
           3.2.2.2 (Anchors and Aliases)}. *)
  | Undefined_alias of string
      (** Alias references non-existent anchor. See
          {{:https://yaml.org/spec/1.2.2/#3222-anchors-and-aliases}Section
           3.2.2.2 (Anchors and Aliases)}. *)
  | Alias_cycle of string
      (** Circular reference in alias chain. See
          {{:https://yaml.org/spec/1.2.2/#3222-anchors-and-aliases}Section
           3.2.2.2 (Anchors and Aliases)}. *)
  | Multiple_documents
      (** Multiple documents found when single document expected. See
          {{:https://yaml.org/spec/1.2.2/#912-document-markers}Section 9.1.2
           (Document Markers)}. *)
  | Mapping_key_too_long
      (** Mapping key exceeds maximum length (1024 characters). *)
  (* Loader errors - see {{:https://yaml.org/spec/1.2.2/#31-processes}Section 3.1 (Processes)} *)
  | Invalid_scalar_conversion of string * string
      (** Cannot convert scalar value to target type (value, target type). See
          {{:https://yaml.org/spec/1.2.2/#103-core-schema}Section 10.3 (Core
           Schema)}. *)
  | Type_mismatch of string * string
      (** Value has wrong type for operation (expected, got). *)
  | Unresolved_alias of string
      (** Alias encountered during conversion but not resolved. See
          {{:https://yaml.org/spec/1.2.2/#3222-anchors-and-aliases}Section
           3.2.2.2 (Anchors and Aliases)}. *)
  | Key_not_found of string  (** Mapping key not found. *)
  | Alias_expansion_node_limit of int
      (** Alias expansion exceeded maximum node count (protection against
          billion laughs attack). See
          {{:https://yaml.org/spec/1.2.2/#321-processes}Section 3.2.1
           (Processes)}.

          The "billion laughs attack" (also known as an XML bomb) is a
          denial-of-service attack where a small YAML document expands to
          enormous size through recursive alias expansion. This limit prevents
          such attacks. *)
  | Alias_expansion_depth_limit of int
      (** Alias expansion exceeded maximum nesting depth (protection against
          deeply nested aliases). See
          {{:https://yaml.org/spec/1.2.2/#321-processes}Section 3.2.1
           (Processes)}. *)
  (* Emitter errors *)
  | Invalid_encoding of string
      (** Invalid character encoding specified. See
          {{:https://yaml.org/spec/1.2.2/#51-character-set}Section 5.1
           (Character Set)}. *)
  | Scalar_contains_invalid_chars of string
      (** Scalar contains characters invalid for chosen style. *)
  | Anchor_not_set  (** Attempted to emit alias before anchor was defined. *)
  | Invalid_state of string
      (** Emitter in invalid state for requested operation. *)
  (* Generic *)
  | Custom of string  (** Custom error message. *)

type t = {
  kind : kind;  (** The specific error classification. *)
  span : Span.t option;
      (** Source location where the error occurred (if available). *)
  context : string list;
      (** Context stack showing the processing path leading to the error. *)
  source : string option;
      (** Source text for displaying the error in context. *)
}
(** {2 Error Value}

    Full error information including classification, location, and context. *)

exception Yamlrw_error of t
(** {2 Exception}

    The main exception type raised by all yamlrw operations.

    All parsing, loading, and emitting errors are reported by raising this
    exception with detailed error information. *)

let () =
  Printexc.register_printer (function
    | Yamlrw_error e ->
        let loc =
          match e.span with
          | None -> ""
          | Some span -> " at " ^ Span.to_string span
        in
        Some
          (Printf.sprintf "Yamlrw_error: %s%s"
             (match e.kind with Custom s -> s | _ -> "error")
             loc)
    | _ -> None)

(** {2 Error Construction} *)

(** [make ?span ?context ?source kind] constructs an error value.

    @param span Source location
    @param context Context stack (defaults to empty)
    @param source Source text
    @param kind Error classification *)
let make ?span ?(context = []) ?source kind = { kind; span; context; source }

(** [raise ?span ?context ?source kind] constructs and raises an error.

    This is the primary way to report errors in yamlrw.

    @param span Source location
    @param context Context stack
    @param source Source text
    @param kind Error classification
    @raise Yamlrw_error *)
let raise ?span ?context ?source kind =
  Stdlib.raise (Yamlrw_error (make ?span ?context ?source kind))

(** [raise_at pos kind] raises an error at a specific position.

    @param pos Source position
    @param kind Error classification
    @raise Yamlrw_error *)
let raise_at pos kind =
  let span = Span.point pos in
  raise ~span kind

(** [raise_span span kind] raises an error at a specific span.

    @param span Source span
    @param kind Error classification
    @raise Yamlrw_error *)
let raise_span span kind = raise ~span kind

(** [with_context ctx f] executes [f ()] and adds [ctx] to any raised error's
    context.

    This is useful for tracking the processing path through nested structures.

    @param ctx Context description (e.g., "parsing mapping key")
    @param f Function to execute *)
let with_context ctx f =
  try f ()
  with Yamlrw_error e ->
    Stdlib.raise (Yamlrw_error { e with context = ctx :: e.context })

(** {2 Error Formatting} *)

(** [kind_to_string kind] converts an error kind to a human-readable string. *)
let kind_to_string = function
  | Unexpected_character c -> Printf.sprintf "unexpected character %C" c
  | Unexpected_eof -> "unexpected end of input"
  | Invalid_escape_sequence s -> Printf.sprintf "invalid escape sequence: %s" s
  | Invalid_unicode_escape s -> Printf.sprintf "invalid unicode escape: %s" s
  | Invalid_hex_escape s -> Printf.sprintf "invalid hex escape: %s" s
  | Invalid_tag s -> Printf.sprintf "invalid tag: %s" s
  | Invalid_anchor s -> Printf.sprintf "invalid anchor: %s" s
  | Invalid_alias s -> Printf.sprintf "invalid alias: %s" s
  | Invalid_comment ->
      "comments must be separated from other tokens by whitespace"
  | Unclosed_single_quote -> "unclosed single quote"
  | Unclosed_double_quote -> "unclosed double quote"
  | Unclosed_flow_sequence -> "unclosed flow sequence '['"
  | Unclosed_flow_mapping -> "unclosed flow mapping '{'"
  | Invalid_indentation (expected, got) ->
      Printf.sprintf "invalid indentation: expected %d, got %d" expected got
  | Invalid_flow_indentation -> "invalid indentation in flow construct"
  | Tab_in_indentation -> "tab character in indentation"
  | Invalid_block_scalar_header s ->
      Printf.sprintf "invalid block scalar header: %s" s
  | Invalid_quoted_scalar_indentation s -> Printf.sprintf "%s" s
  | Invalid_directive s -> Printf.sprintf "invalid directive: %s" s
  | Invalid_yaml_version s -> Printf.sprintf "invalid YAML version: %s" s
  | Invalid_tag_directive s -> Printf.sprintf "invalid TAG directive: %s" s
  | Reserved_directive s -> Printf.sprintf "reserved directive: %s" s
  | Illegal_flow_key_line ->
      "key and ':' must be on the same line in flow context"
  | Block_sequence_disallowed ->
      "block sequence entries are not allowed in this context"
  | Unexpected_token s -> Printf.sprintf "unexpected token: %s" s
  | Expected_document_start -> "expected document start '---'"
  | Expected_document_end -> "expected document end '...'"
  | Expected_block_entry -> "expected block entry '-'"
  | Expected_key -> "expected mapping key"
  | Expected_value -> "expected mapping value"
  | Expected_node -> "expected node"
  | Expected_scalar -> "expected scalar"
  | Expected_sequence_end -> "expected sequence end ']'"
  | Expected_mapping_end -> "expected mapping end '}'"
  | Duplicate_anchor s -> Printf.sprintf "duplicate anchor: &%s" s
  | Undefined_alias s -> Printf.sprintf "undefined alias: *%s" s
  | Alias_cycle s -> Printf.sprintf "alias cycle detected: *%s" s
  | Multiple_documents -> "multiple documents found when single expected"
  | Mapping_key_too_long -> "mapping key too long (max 1024 characters)"
  | Invalid_scalar_conversion (value, typ) ->
      Printf.sprintf "cannot convert %S to %s" value typ
  | Type_mismatch (expected, got) ->
      Printf.sprintf "type mismatch: expected %s, got %s" expected got
  | Unresolved_alias s -> Printf.sprintf "unresolved alias: *%s" s
  | Key_not_found s -> Printf.sprintf "key not found: %s" s
  | Alias_expansion_node_limit n ->
      Printf.sprintf "alias expansion exceeded node limit (%d nodes)" n
  | Alias_expansion_depth_limit n ->
      Printf.sprintf "alias expansion exceeded depth limit (%d levels)" n
  | Invalid_encoding s -> Printf.sprintf "invalid encoding: %s" s
  | Scalar_contains_invalid_chars s ->
      Printf.sprintf "scalar contains invalid characters: %s" s
  | Anchor_not_set -> "anchor not set"
  | Invalid_state s -> Printf.sprintf "invalid state: %s" s
  | Custom s -> s

(** [to_string t] converts an error to a human-readable string.

    Includes error kind, source location (if available), and context stack. *)
let to_string t =
  let loc =
    match t.span with None -> "" | Some span -> " at " ^ Span.to_string span
  in
  let ctx =
    match t.context with
    | [] -> ""
    | ctxs -> " (in " ^ String.concat " > " (List.rev ctxs) ^ ")"
  in
  kind_to_string t.kind ^ loc ^ ctx

(** [pp fmt t] pretty-prints an error to a formatter. *)
let pp fmt t = Format.fprintf fmt "Yamlrw error: %s" (to_string t)

(** [pp_with_source ~source fmt t] pretty-prints an error with source context.

    Shows the error message followed by the relevant source line with a caret
    (^) pointing to the error location.

    @param source The source text
    @param fmt Output formatter
    @param t The error to display *)
let pp_with_source ~source fmt t =
  let extract_line source line_num =
    let lines = String.split_on_char '\n' source in
    if line_num >= 1 && line_num <= List.length lines then
      Some (List.nth lines (line_num - 1))
    else None
  in

  pp fmt t;
  match t.span with
  | None -> ()
  | Some span -> (
      match extract_line source span.start.line with
      | None -> ()
      | Some line ->
          Format.fprintf fmt "\n  %d | %s\n" span.start.line line;
          let padding = String.make (span.start.column - 1) ' ' in
          Format.fprintf fmt "    | %s^" padding)
