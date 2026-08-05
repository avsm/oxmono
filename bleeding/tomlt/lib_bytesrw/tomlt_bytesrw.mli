(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bytesrw integration for {{:https://toml.io/en/v1.1.0}TOML 1.1} parsing
    and encoding.

    This module provides I/O operations for TOML values and codecs using
    {{:https://erratique.ch/software/bytesrw}Bytesrw} for efficient streaming.

    {2 Quick Start}

    Parse a TOML string:
    {[
      let config = Tomlt_bytesrw.of_string {|
        [server]
        host = "localhost"
        port = 8080
      |} in
      match config with
      | Ok t ->
          let server = Tomlt.Toml.find "server" t in
          let host = Tomlt.Toml.to_string (Tomlt.Toml.find "host" server) in
          let port = Tomlt.Toml.to_int (Tomlt.Toml.find "port" server) in
          Printf.printf "Server: %s:%Ld\n" host port
      | Error e -> prerr_endline (Tomlt.Toml.Error.to_string e)
    ]}

    Use with codecs:
    {[
      type config = { host : string; port : int }

      let config_codec = Tomlt.(Table.(
        obj (fun host port -> { host; port })
        |> mem "host" string ~enc:(fun c -> c.host)
        |> mem "port" int ~enc:(fun c -> c.port)
        |> finish
      ))

      let config = Tomlt_bytesrw.decode_string config_codec toml_string
    ]}

    {2 Module Overview}

    - {!section:parse} - Parsing TOML from strings and readers
    - {!section:encode} - Encoding TOML to strings and writers
    - {!section:codec_io} - Codec I/O operations
    - {!section:tagged_json} - Tagged JSON for toml-test compatibility *)

open Bytesrw

(** {1:parse Parsing (Decoding)}

    Parse TOML from various sources. *)

val of_string : string -> (Tomlt.Toml.t, Tomlt.Toml.Error.t) result
(** [of_string s] parses [s] as a TOML document. *)

val of_reader : ?file:string -> Bytes.Reader.t -> (Tomlt.Toml.t, Tomlt.Toml.Error.t) result
(** [of_reader r] parses a TOML document from reader [r].
    @param file Optional filename for error messages. *)

val parse : string -> Tomlt.Toml.t
(** [parse s] parses [s] as a TOML document.
    @raise Tomlt.Toml.Error.Error on parse errors. *)

val parse_reader : ?file:string -> Bytes.Reader.t -> Tomlt.Toml.t
(** [parse_reader r] parses a TOML document from reader [r].
    @param file Optional filename for error messages.
    @raise Tomlt.Toml.Error.Error on parse errors. *)

(** {1:encode Encoding}

    Encode TOML values to strings and writers. *)

val to_string : Tomlt.Toml.t -> string
(** [to_string t] encodes [t] as a TOML-formatted string.
    @raise Invalid_argument if [t] is not a [Table]. *)

val to_writer : Bytes.Writer.t -> Tomlt.Toml.t -> unit
(** [to_writer w t] writes [t] as TOML to writer [w].

    Use with {!Bytesrw.Bytes.Writer} to write to various destinations:
    {[
      (* To buffer *)
      let buf = Buffer.create 256 in
      Tomlt_bytesrw.to_writer (Bytes.Writer.of_buffer buf) value;
      Buffer.contents buf

      (* To channel *)
      Tomlt_bytesrw.to_writer (Bytes.Writer.of_out_channel oc) value
    ]}

    @raise Invalid_argument if [t] is not a [Table]. *)

(** {1:codec_io Codec I/O Operations}

    Convenience functions that combine parsing/encoding with codec
    operations. *)

val decode_string : 'a Tomlt.t -> string -> ('a, Tomlt.Toml.Error.t) result
(** [decode_string c s] parses TOML string [s] and decodes with codec [c]. *)

val decode_string_exn : 'a Tomlt.t -> string -> 'a
(** [decode_string_exn c s] is like [decode_string] but raises on error.
    @raise Tomlt.Toml.Error.Error on parse or decode failure. *)

val encode_string : 'a Tomlt.t -> 'a -> string
(** [encode_string c v] encodes [v] using codec [c] to a TOML-formatted string. *)

val decode_reader : ?file:string -> 'a Tomlt.t -> Bytes.Reader.t ->
  ('a, Tomlt.Toml.Error.t) result
(** [decode_reader c r] parses TOML from reader [r] and decodes with codec [c].
    @param file Optional filename for error messages. *)

val encode_writer : 'a Tomlt.t -> 'a -> Bytes.Writer.t -> unit
(** [encode_writer c v w] encodes [v] using codec [c] and writes TOML to
    writer [w]. *)

(** {1:tagged_json Tagged JSON}

    Functions for interoperating with the
    {{:https://github.com/toml-lang/toml-test}toml-test} suite's tagged JSON
    format. These functions are primarily for testing and validation. *)

module Tagged_json : sig
  val encode : Tomlt.Toml.t -> string
  (** [encode t] converts TOML value [t] to tagged JSON format.

      The tagged JSON format wraps each value with type information:
      - Strings: [{"type": "string", "value": "..."}]
      - Integers: [{"type": "integer", "value": "..."}]
      - Floats: [{"type": "float", "value": "..."}]
      - Booleans: [{"type": "bool", "value": "true"|"false"}]
      - Datetimes: [{"type": "datetime", "value": "..."}]
      - Arrays: [[...]]
      - Tables: [{...}] *)

  val decode : string -> Tomlt.Toml.t
  (** [decode s] parses tagged JSON string [s] into a TOML value.
      @raise Failure if the JSON is malformed or has invalid types. *)

  val decode_and_encode_toml : string -> (string, string) result
  (** [decode_and_encode_toml json] decodes tagged JSON and encodes as TOML.
      Used by the toml-test encoder harness. *)
end
