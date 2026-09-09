(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Streaming TOML parser and encoder.

    Internal module implementing the TOML 1.1 parser and encoder on top of
    [bytesrw] readers/writers. This layer produces and consumes {!Value.t}
    without involving codecs; the top-level {!Toml.of_string} and friends
    combine it with the codec layer. *)

open Bytesrw

(** {1 Parsing} *)

val of_string : string -> (Value.t, Loc.Error.t) result
(** [of_string s] parses [s] as a TOML document. *)

val of_reader : ?file:string -> Bytes.Reader.t -> (Value.t, Loc.Error.t) result
(** [of_reader r] parses a TOML document from reader [r].
    @param file Optional filename for error messages. *)

val parse : ?max_depth:int -> ?max_nodes:int -> string -> Value.t
(** [parse s] parses [s] as a TOML document.
    @param max_depth caps nesting of arrays and inline tables (default 100).
    @param max_nodes caps total parsed values (default 10_000_000).
    @raise Loc.exception-Error on parse errors and depth/node limit violations.
*)

val parse_reader :
  ?file:string -> ?max_depth:int -> ?max_nodes:int -> Bytes.Reader.t -> Value.t
(** [parse_reader r] parses a TOML document from reader [r].
    @param file Optional filename for error messages.
    @param max_depth caps nesting of arrays and inline tables (default 100).
    @param max_nodes caps total parsed values (default 10_000_000).
    @raise Loc.exception-Error on parse errors and depth/node limit violations.
*)

(** {1 Encoding} *)

val to_string : ?indent:int -> ?preserve:bool -> Value.t -> string
(** [to_string t] encodes [t] as a TOML-formatted string. [preserve] writes a
    parsed document back as the bytes it was read from, and [indent] lays out a
    value that has none; see {!Toml.to_string} for what each of them keeps.
    @raise Invalid_argument if [t] is not a [Table]. *)

val to_writer :
  ?indent:int -> ?preserve:bool -> Bytes.Writer.t -> Value.t -> unit
(** [to_writer w t] writes [t] as TOML to writer [w]. See {!Toml.to_writer} for
    the semantics of [indent] and [preserve].
    @raise Invalid_argument if [t] is not a [Table]. *)

(** {1 Tagged JSON}

    Functions for interoperating with the
    {{:https://github.com/toml-lang/toml-test}toml-test} suite's tagged JSON
    format. *)

module Tagged_json : sig
  val encode : Value.t -> string
  (** [encode t] converts TOML value [t] to tagged JSON format.

      The tagged JSON format wraps each value with type information:
      - Strings: [{"type": "string", "value": "..."}]
      - Integers: [{"type": "integer", "value": "..."}]
      - Floats: [{"type": "float", "value": "..."}]
      - Booleans: [{"type": "bool", "value": "true"|"false"}]
      - Datetimes: [{"type": "datetime", "value": "..."}]
      - Arrays: [[...]]
      - Tables: [{...}]. *)

  val decode : string -> Value.t
  (** [decode s] parses tagged JSON string [s] into a TOML value.
      @raise Failure if the JSON is malformed or has invalid types. *)

  val decode_and_encode_toml : string -> (string, string) result
  (** [decode_and_encode_toml json] decodes tagged JSON and encodes as TOML.
      Used by the toml-test encoder harness. *)

  val float_to_tagged_json_str : float -> string
  (** [float_to_tagged_json_str f] is the ["value"] string a tagged float
      carries: the shortest spelling [float_of_string] maps back to [f], and
      ["nan"], ["inf"], ["-inf"], ["-0"] for the values that have no digits.
      This is the house spelling, so anything else encoding a TOML float to
      tagged JSON takes it from here rather than choosing again. *)
end
