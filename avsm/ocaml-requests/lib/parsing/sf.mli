(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 8941 Structured Fields (minimal subset for HTTP Signatures)

    This module implements the subset of
    {{:https://datatracker.ietf.org/doc/html/rfc8941}RFC 8941} Structured Fields
    required for HTTP Message Signatures (RFC 9421).

    Only Dictionary and Inner List types are fully supported, as these are
    used by the Signature-Input and Signature headers. *)

(** {1 Bare Items} *)

type item =
  | String of string      (** Quoted string: "value" *)
  | Token of string       (** Unquoted token: value *)
  | Integer of int64      (** Integer: 123 *)
  | Decimal of float      (** Decimal: 1.23 *)
  | Boolean of bool       (** Boolean: ?1 or ?0 *)
  | Byte_seq of string    (** Byte sequence: :base64: *)

val item_to_string : item -> string
(** Serialize an item to its Structured Field representation. *)

(** {1 Parameters} *)

type parameters = (string * item) list
(** Parameters are key-value pairs attached to items. *)

val parameters_to_string : parameters -> string
(** Serialize parameters (e.g., [;key=value;key2]). *)

(** {1 Inner Lists} *)

type inner_list = (item * parameters) list * parameters
(** An inner list is a list of parameterized items with list-level parameters.
    Format: [(item1;param item2);listparam=value] *)

val inner_list_to_string : inner_list -> string
(** Serialize an inner list. *)

val parse_inner_list : string -> (inner_list, string) result
(** Parse an inner list from its string representation. *)

(** {1 List Members} *)

type list_member =
  | Item of item * parameters
  | Inner_list of inner_list

(** {1 Dictionaries} *)

type dictionary = (string * list_member) list
(** A dictionary is an ordered map of string keys to list members. *)

val dictionary_to_string : dictionary -> string
(** Serialize a dictionary to its Structured Field representation. *)

val parse_dictionary : string -> (dictionary, string) result
(** Parse a dictionary from its string representation. *)

(** {1 Convenience Functions} *)

val string_item : string -> item
(** Create a string item. *)

val token_item : string -> item
(** Create a token item. *)

val integer_item : int64 -> item
(** Create an integer item. *)

val byte_seq_item : string -> item
(** Create a byte sequence item (raw bytes, will be base64 encoded). *)

val bool_item : bool -> item
(** Create a boolean item. *)
