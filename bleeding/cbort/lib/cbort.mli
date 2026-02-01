(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CBOR codecs.

    This module provides type-safe CBOR encoding and decoding using a
    combinator-based approach. Define codecs once and use them for both encoding
    and decoding.

    The design follows the jsont pattern: codecs are values of type ['a t] that
    describe how to convert between OCaml values of type ['a] and CBOR data
    items.

    {2 Quick Start}

    {[
      (* Define a codec for a record type *)
      type person = { name : string; age : int }

      let person_codec =
        let open Cbort.Obj in
        let* name = mem "name" Cbort.string in
        let* age = mem "age" Cbort.int in
        return { name; age } ~enc:(fun p ->
            field "name" p.name @@ field "age" p.age @@ empty)

      (* Encode to CBOR bytes *)
      let cbor_bytes =
        Cbort.encode_string person_codec { name = "Alice"; age = 30 }

      (* Decode from CBOR bytes *)
      let person = Cbort.decode_string person_codec cbor_bytes
    ]}

    {2 Data Model}

    This codec library maps OCaml types to CBOR types:

    | OCaml type | CBOR type | |------------|-----------| | [unit] | null
    (simple value 22) | | [bool] | true/false (simple values 21/20) | | [int],
    [int32], [int64] | integer (major types 0/1) | | [float] | float (major type
    7) | | [string] | text string (major type 3) | | [bytes] | byte string
    (major type 2) | | ['a list] | array (major type 4) | | [('k, 'v) list] |
    map (major type 5) | | records | map with text keys | | variants | tagged or
    keyed encoding | *)

open Bytesrw

(** {1:errors Errors} *)

(** Error handling for codec operations. *)
module Error : sig
  (** {1:paths Paths} *)

  type path = segment list
  (** Path to a location in a CBOR structure. *)

  (** A segment of a path. *)
  and segment =
    | Root  (** The root of the structure. *)
    | Index of int  (** An array index. *)
    | Key of string  (** A map key (text string). *)
    | Key_cbor of Cbor.t  (** A map key (arbitrary CBOR). *)
    | Tag of int  (** Inside a tagged value. *)

  val pp_path : Format.formatter -> path -> unit
  (** [pp_path ppf path] pretty-prints [path]. *)

  val path_to_string : path -> string
  (** [path_to_string path] returns [path] as a string. *)

  (** {1:errors Errors} *)

  (** The kind of error. *)
  type kind =
    | Type_mismatch of { expected : string; got : string }
        (** Expected one CBOR type but got another. *)
    | Missing_member of string  (** Required map member not found. *)
    | Unknown_member of string  (** Unexpected map member (when strict). *)
    | Duplicate_member of string  (** Map contains duplicate key. *)
    | Out_of_range of { value : string; range : string }
        (** Value outside acceptable range. *)
    | Invalid_value of string  (** Value doesn't satisfy a constraint. *)
    | Parse_error of string  (** Low-level parsing error. *)
    | Custom of string  (** User-defined error. *)

  type t = {
    path : path;  (** Location in the structure. *)
    kind : kind;  (** What went wrong. *)
  }
  (** A decode error with location context. *)

  val make : path -> kind -> t
  (** [make path kind] creates an error. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] pretty-prints error [e]. *)

  val to_string : t -> string
  (** [to_string e] returns [e] as a string. *)

  exception Decode of t
  (** Exception raised by [_exn] decoding functions. *)
end

(** {1:codec Codecs} *)

type 'a t
(** The type of codecs for values of type ['a]. A codec knows how to both encode
    ['a] to CBOR and decode CBOR to ['a]. *)

(** {1:base Base Codecs}

    Codecs for primitive types. *)

val null : unit t
(** [null] is a codec for the CBOR null value. Encodes [()] as null. *)

val bool : bool t
(** [bool] is a codec for CBOR booleans. *)

val int : int t
(** [int] is a codec for OCaml [int] as CBOR integer.
    @raise Error.Decode if the CBOR integer is out of [int] range. *)

val int32 : int32 t
(** [int32] is a codec for [int32] as CBOR integer.
    @raise Error.Decode if the CBOR integer is out of [int32] range. *)

val int64 : int64 t
(** [int64] is a codec for [int64] as CBOR integer. *)

val float : float t
(** [float] is a codec for CBOR floating-point numbers. Also accepts CBOR
    integers, converting them to float. *)

val string : string t
(** [string] is a codec for CBOR text strings (UTF-8). *)

val bytes : string t
(** [bytes] is a codec for CBOR byte strings. The OCaml [string] type is used
    since it can hold arbitrary bytes. *)

val any : Cbor.t t
(** [any] is a codec that accepts any CBOR value. Useful for dynamic content or
    when preserving unknown fields. *)

(** {1:nullable Nullable Values} *)

val nullable : 'a t -> 'a option t
(** [nullable c] creates a codec for optional values. Encodes [None] as null,
    [Some x] as [c] would encode [x]. *)

val option : default:'a -> 'a t -> 'a t
(** [option ~default c] creates a codec that uses [default] when decoding null
    instead of failing. *)

(** {1:numbers Numeric Variants} *)

val uint : int t
(** [uint] is like {!int} but only accepts non-negative integers. *)

val uint32 : int32 t
(** [uint32] is like {!int32} but only accepts non-negative integers. *)

val uint64 : int64 t
(** [uint64] is like {!int64} but only accepts non-negative integers. *)

val number : float t
(** [number] accepts both integers and floats, converting to float. Alias for
    {!float}. *)

(** {1:arrays Arrays} *)

val array : 'a t -> 'a list t
(** [array c] is a codec for arrays where each element uses codec [c]. *)

val array_of : len:int -> 'a t -> 'a list t
(** [array_of ~len c] is like {!array} but requires exactly [len] elements. *)

val tuple2 : 'a t -> 'b t -> ('a * 'b) t
(** [tuple2 ca cb] is a codec for 2-element arrays as pairs. *)

val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [tuple3 ca cb cc] is a codec for 3-element arrays as triples. *)

val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [tuple4 ca cb cc cd] is a codec for 4-element arrays as quadruples. *)

(** {1:maps Maps}

    Codecs for CBOR maps with uniform key and value types. For records and
    heterogeneous maps, see {!module:Obj}. *)

val assoc : 'k t -> 'v t -> ('k * 'v) list t
(** [assoc kc vc] is a codec for maps as association lists. Keys are decoded
    using [kc], values using [vc]. *)

val string_map : 'v t -> (string * 'v) list t
(** [string_map vc] is a codec for maps with text string keys. Equivalent to
    [assoc string vc]. *)

val int_map : 'v t -> (int * 'v) list t
(** [int_map vc] is a codec for maps with integer keys. Common in COSE and other
    binary protocols. *)

(** {1:objects Object Codecs}

    Build codecs for records and objects from CBOR maps with text string keys.
    Uses a monadic interface for composing member codecs. *)
module Obj : sig
  type ('o, 'a) mem
  (** A member specification. ['o] is the object type being built, ['a] is the
      decoded value at this step. *)

  val ( let* ) : ('o, 'a) mem -> ('a -> ('o, 'b) mem) -> ('o, 'b) mem
  (** Monadic bind for sequencing member decoders. *)

  val mem : string -> ('o -> 'a) -> 'a t -> ('o, 'a) mem
  (** [mem name get c] declares a required member with key [name] decoded by
      [c]. The [get] function extracts the field value from the object for
      encoding. *)

  val mem_opt : string -> ('o -> 'a option) -> 'a t -> ('o, 'a option) mem
  (** [mem_opt name get c] declares an optional member. Returns [None] if the
      key is absent or the value is null. *)

  val mem_default : string -> ('o -> 'a) -> default:'a -> 'a t -> ('o, 'a) mem
  (** [mem_default name get ~default c] declares a member with a default value
      used when the key is absent. *)

  val return : 'o -> ('o, 'o) mem
  (** [return v] completes the object codec, returning the built value. *)

  val finish : ('o, 'o) mem -> 'o t
  (** [finish m] converts the member specification to a codec. *)
end

(** {1:int_objects Integer-Keyed Objects}

    Build codecs for maps with integer keys. Common in COSE, CWT, and other
    space-efficient binary protocols. *)
module Obj_int : sig
  type ('o, 'a) mem
  (** A member specification. ['o] is the object type being built, ['a] is the
      decoded value at this step. *)

  val ( let* ) : ('o, 'a) mem -> ('a -> ('o, 'b) mem) -> ('o, 'b) mem
  (** Monadic bind for sequencing member decoders. *)

  val mem : int -> ('o -> 'a) -> 'a t -> ('o, 'a) mem
  (** [mem key get c] declares a required member with integer key [key]. The
      [get] function extracts the field value for encoding. *)

  val mem_opt : int -> ('o -> 'a option) -> 'a t -> ('o, 'a option) mem
  (** [mem_opt key get c] declares an optional member with integer key. *)

  val mem_default : int -> ('o -> 'a) -> default:'a -> 'a t -> ('o, 'a) mem
  (** [mem_default key get ~default c] declares a member with default value. *)

  val return : 'o -> ('o, 'o) mem
  (** [return v] completes the codec. *)

  val finish : ('o, 'o) mem -> 'o t
  (** [finish m] converts to a codec. *)
end

(** {1:tags Tagged Values}

    CBOR tags provide semantic information about data items. *)

val tag : int -> 'a t -> 'a t
(** [tag n c] wraps codec [c] with tag number [n]. On encoding, outputs the tag;
    on decoding, expects and strips the tag. *)

val tag_opt : int -> 'a t -> 'a t
(** [tag_opt n c] is like {!tag} but the tag is optional when decoding. Useful
    for accepting both tagged and untagged input. *)

(** {1:transforms Transformations}

    Convert between types using codecs. *)

val map : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
(** [map decode encode c] transforms codec [c]. The [decode] function is applied
    after decoding, [encode] before encoding. *)

val conv : ('a -> ('b, string) result) -> ('b -> 'a) -> 'a t -> 'b t
(** [conv decode encode c] is like {!map} but [decode] may fail. *)

val const : 'a -> unit t -> 'a t
(** [const v c] is a codec that always decodes to [v] and encodes [v] using [c].
*)

(** {1:variants Variants}

    Encode sum types using either tags or key-based discrimination. *)

(** Tag-based variant encoding. Each constructor is assigned a unique CBOR tag
    number. *)
module Variant : sig
  type 'a case
  (** A variant case specification. *)

  val case : int -> 'a t -> ('a -> 'b) -> ('b -> 'a option) -> 'b case
  (** [case tag c inject project] defines a case:
      - [tag] is the CBOR tag number for this case
      - [c] is the codec for the payload
      - [inject] wraps decoded payload into the variant type
      - [project] extracts payload if this case matches *)

  val case0 : int -> 'a -> ('a -> bool) -> 'a case
  (** [case0 tag v is_v] defines a case with no payload. Encodes as an empty
      tag. [is_v x] should return [true] iff [x] equals [v]. *)

  val variant : 'a case list -> 'a t
  (** [variant cases] builds a codec from a list of cases. Cases are tried in
      order during decoding. *)
end

(** Key-based variant encoding. Each constructor is identified by a string key
    in a singleton map. *)
module Variant_key : sig
  type 'a case
  (** A variant case specification. *)

  val case : string -> 'a t -> ('a -> 'b) -> ('b -> 'a option) -> 'b case
  (** [case key c inject project] defines a case identified by text key [key].
  *)

  val case0 : string -> 'a -> ('a -> bool) -> 'a case
  (** [case0 key v is_v] defines a case with no payload. Encodes as
      [{key: null}]. *)

  val variant : 'a case list -> 'a t
  (** [variant cases] builds a codec from cases. *)
end

(** {1:recursive Recursive Types} *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] creates a recursive codec. The function [f] receives a codec that
    can be used for self-reference.

    {[
      type tree = Leaf of int | Node of tree * tree

      let tree_codec =
        Cbort.fix @@ fun self ->
        Cbort.Variant.(
          variant
            [
              case 0 Cbort.int
                (fun x -> Leaf x)
                (function Leaf x -> Some x | _ -> None);
              case 1 (Cbort.tuple2 self self)
                (fun (l, r) -> Node (l, r))
                (function Node (l, r) -> Some (l, r) | _ -> None);
            ])
    ]} *)

(** {1:decode Decoding} *)

val decode : 'a t -> Bytes.Reader.t -> ('a, Error.t) result
(** [decode c r] decodes a value from CBOR reader [r] using codec [c]. *)

val decode_string : 'a t -> string -> ('a, Error.t) result
(** [decode_string c s] decodes from CBOR bytes [s]. *)

val decode_exn : 'a t -> Bytes.Reader.t -> 'a
(** [decode_exn c r] is like {!val-decode} but raises {!Error.Decode}. *)

val decode_string_exn : 'a t -> string -> 'a
(** [decode_string_exn c s] is like {!decode_string} but raises. *)

val decode_cbor : 'a t -> Cbor.t -> ('a, Error.t) result
(** [decode_cbor c v] decodes from a CBOR value [v]. Useful when you already
    have parsed CBOR. *)

val decode_cbor_exn : 'a t -> Cbor.t -> 'a
(** [decode_cbor_exn c v] is like {!decode_cbor} but raises. *)

(** {1:encode Encoding} *)

val encode : 'a t -> 'a -> eod:bool -> Bytes.Writer.t -> unit
(** [encode c v ~eod w] encodes [v] to writer [w] using codec [c]. If [eod] is
    true, signals end-of-data after encoding. *)

val encode_string : 'a t -> 'a -> string
(** [encode_string c v] encodes [v] to a CBOR byte string. *)

val encode_cbor : 'a t -> 'a -> Cbor.t
(** [encode_cbor c v] encodes [v] to a {!Cbor.t} value. *)

(** {1:low_level Low-level Access}

    Direct access to the underlying reader/writer primitives. *)

module Rw = Cbor_rw
module Cbor = Cbor
