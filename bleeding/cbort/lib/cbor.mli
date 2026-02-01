(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CBOR data items.

    This module defines the CBOR (Concise Binary Object Representation) data
    model as specified in {{:https://www.rfc-editor.org/rfc/rfc8949}RFC 8949}.

    A CBOR data item is one of:
    - Unsigned integer (major type 0)
    - Negative integer (major type 1)
    - Byte string (major type 2)
    - Text string (major type 3)
    - Array of data items (major type 4)
    - Map of data item pairs (major type 5)
    - Tagged data item (major type 6)
    - Simple value or float (major type 7)

    {2 Example}

    {[
      let person =
        Cbor.Map
          [
            (Cbor.Text "name", Cbor.Text "Alice"); (Cbor.Text "age", Cbor.int 30);
          ]
    ]} *)

(** {1:types CBOR Data Items} *)

(** The type of CBOR data items. *)
type t =
  | Int of Z.t
      (** Signed integer. Positive values encode as major type 0, negative as
          major type 1. Small values (fitting in int64) are encoded directly;
          larger values use bignum tags (2 for positive, 3 for negative). Uses
          zarith for arbitrary precision. *)
  | Bytes of string  (** Byte string (major type 2). Raw binary data. *)
  | Text of string  (** Text string (major type 3). Must be valid UTF-8. *)
  | Array of t list  (** Array of data items (major type 4). *)
  | Map of (t * t) list
      (** Map of key-value pairs (major type 5). Keys can be any CBOR type,
          though string keys are most common for JSON interoperability. *)
  | Tag of int * t
      (** Tagged data item (major type 6). The tag number provides semantic
          information about the enclosed item. Common tags include:
          - 0: Standard date/time string
          - 1: Epoch-based date/time
          - 2: Positive bignum
          - 3: Negative bignum
          - 21: Expected base64url encoding
          - 22: Expected base64 encoding
          - 23: Expected base16 encoding *)
  | Bool of bool
      (** Boolean simple value. [true] is simple value 21, [false] is 20. *)
  | Null  (** Null simple value (22). Represents absence of a value. *)
  | Undefined  (** Undefined simple value (23). Distinct from null in CBOR. *)
  | Simple of int
      (** Other simple values (0-19, 24-255 excluding reserved). Simple values
          in range 0-23 are encoded in a single byte; 24-255 require two bytes.
          Values 24-31 are reserved for special encodings. *)
  | Float of float
      (** Floating-point number. May be encoded as half (16-bit), single
          (32-bit), or double (64-bit) precision IEEE 754. *)

(** {1:constructors Constructors}

    Convenience constructors for common cases. *)

val int : int -> t
(** [int n] creates an integer from an OCaml [int]. *)

val int64 : int64 -> t
(** [int64 n] creates an integer from an [int64]. *)

val bigint : Z.t -> t
(** [bigint n] creates an integer from a zarith integer. *)

val string : string -> t
(** [string s] creates a text string. Alias for [Text s]. *)

val bytes : string -> t
(** [bytes s] creates a byte string. Alias for [Bytes s]. *)

val array : t list -> t
(** [array items] creates an array. Alias for [Array items]. *)

val map : (t * t) list -> t
(** [map pairs] creates a map. Alias for [Map pairs]. *)

val tag : int -> t -> t
(** [tag n item] creates a tagged item. Alias for [Tag (n, item)]. *)

val bool : bool -> t
(** [bool b] creates a boolean. Alias for [Bool b]. *)

val null : t
(** The null value. Alias for [Null]. *)

val undefined : t
(** The undefined value. Alias for [Undefined]. *)

val float : float -> t
(** [float f] creates a float. Alias for [Float f]. *)

(** {1:maps Map Operations}

    CBOR maps can have any type as keys. These functions provide convenient
    access patterns for the common case of text string keys. *)

val find : t -> t -> t option
(** [find key map] finds the value associated with [key] in [map]. Returns
    [None] if [map] is not a map or [key] is not found. *)

val find_text : string -> t -> t option
(** [find_text key map] finds the value for text key [key] in [map]. Equivalent
    to [find (Text key) map]. *)

val mem : t -> t -> bool
(** [mem key map] returns [true] if [key] exists in [map]. *)

val mem_text : string -> t -> bool
(** [mem_text key map] returns [true] if text key [key] exists in [map]. *)

(** {1:arrays Array Operations} *)

val nth : int -> t -> t option
(** [nth i arr] returns the [i]th element of array [arr], or [None] if [arr] is
    not an array or [i] is out of bounds. Zero-indexed. *)

val length : t -> int option
(** [length v] returns the length of array or map [v], or [None] if [v] is
    neither. For maps, returns the number of key-value pairs. *)

(** {1:predicates Type Predicates} *)

val is_int : t -> bool
(** [is_int v] is [true] iff [v] is an [Int]. *)

val is_bytes : t -> bool
(** [is_bytes v] is [true] iff [v] is a [Bytes]. *)

val is_text : t -> bool
(** [is_text v] is [true] iff [v] is a [Text]. *)

val is_array : t -> bool
(** [is_array v] is [true] iff [v] is an [Array]. *)

val is_map : t -> bool
(** [is_map v] is [true] iff [v] is a [Map]. *)

val is_tag : t -> bool
(** [is_tag v] is [true] iff [v] is a [Tag]. *)

val is_bool : t -> bool
(** [is_bool v] is [true] iff [v] is a [Bool]. *)

val is_null : t -> bool
(** [is_null v] is [true] iff [v] is [Null]. *)

val is_undefined : t -> bool
(** [is_undefined v] is [true] iff [v] is [Undefined]. *)

val is_simple : t -> bool
(** [is_simple v] is [true] iff [v] is a [Simple]. *)

val is_float : t -> bool
(** [is_float v] is [true] iff [v] is a [Float]. *)

(** {1:accessors Type-Safe Accessors}

    These functions extract values with type checking. *)

val to_int : t -> Z.t option
(** [to_int v] extracts the integer value, or [None] if not an [Int]. *)

val to_int_exn : t -> Z.t
(** [to_int_exn v] extracts the integer value.
    @raise Invalid_argument if [v] is not an [Int]. *)

val to_int64 : t -> int64 option
(** [to_int64 v] extracts the integer as int64, or [None] if not an [Int] or if
    the value doesn't fit in int64. *)

val to_int64_exn : t -> int64
(** [to_int64_exn v] extracts the integer as int64.
    @raise Invalid_argument if [v] is not an [Int] or doesn't fit in int64. *)

val to_bytes : t -> string option
(** [to_bytes v] extracts the byte string, or [None] if not [Bytes]. *)

val to_bytes_exn : t -> string
(** [to_bytes_exn v] extracts the byte string.
    @raise Invalid_argument if [v] is not [Bytes]. *)

val to_text : t -> string option
(** [to_text v] extracts the text string, or [None] if not [Text]. *)

val to_text_exn : t -> string
(** [to_text_exn v] extracts the text string.
    @raise Invalid_argument if [v] is not [Text]. *)

val to_array : t -> t list option
(** [to_array v] extracts the array elements, or [None] if not [Array]. *)

val to_array_exn : t -> t list
(** [to_array_exn v] extracts the array elements.
    @raise Invalid_argument if [v] is not [Array]. *)

val to_map : t -> (t * t) list option
(** [to_map v] extracts the map pairs, or [None] if not [Map]. *)

val to_map_exn : t -> (t * t) list
(** [to_map_exn v] extracts the map pairs.
    @raise Invalid_argument if [v] is not [Map]. *)

val to_tag : t -> (int * t) option
(** [to_tag v] extracts the tag number and content, or [None] if not [Tag]. *)

val to_tag_exn : t -> int * t
(** [to_tag_exn v] extracts the tag number and content.
    @raise Invalid_argument if [v] is not [Tag]. *)

val to_bool : t -> bool option
(** [to_bool v] extracts the boolean, or [None] if not [Bool]. *)

val to_bool_exn : t -> bool
(** [to_bool_exn v] extracts the boolean.
    @raise Invalid_argument if [v] is not [Bool]. *)

val to_float : t -> float option
(** [to_float v] extracts the float, or [None] if not [Float]. *)

val to_float_exn : t -> float
(** [to_float_exn v] extracts the float.
    @raise Invalid_argument if [v] is not [Float]. *)

(** {1:numeric Numeric Conversions}

    These functions provide flexible numeric access, converting between integer
    and float representations as needed. *)

val to_number : t -> float option
(** [to_number v] extracts a numeric value as float. Returns [Some f] if [v] is
    [Int n] (converted to float) or [Float f]. Returns [None] otherwise. *)

val to_int_of_float : t -> Z.t option
(** [to_int_of_float v] extracts an integer, converting floats if they represent
    whole numbers. Returns [None] if [v] is not numeric or if the float has a
    fractional part. *)

(** {1:comparison Comparison and Equality} *)

val equal : t -> t -> bool
(** [equal a b] is structural equality. For floats, uses IEEE 754 equality (NaN
    ≠ NaN). For maps, order of pairs matters. *)

val compare : t -> t -> int
(** [compare a b] provides total ordering. Comparison order follows CBOR
    deterministic encoding: by major type first, then by value. *)

(** {1:output Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf v] pretty-prints [v] in diagnostic notation as defined in RFC 8949
    Section 8. *)

val to_diagnostic : t -> string
(** [to_diagnostic v] returns [v] in diagnostic notation. *)
