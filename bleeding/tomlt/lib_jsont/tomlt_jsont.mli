(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Jsont codecs for TOML tagged JSON format.

    This module provides bidirectional codecs between TOML values and
    the tagged JSON format used by {{:https://github.com/toml-lang/toml-test}
    toml-test}.

    {2 Tagged JSON Format}

    The toml-test suite uses a "tagged JSON" format where each TOML value
    is represented as a JSON object with type information:
    - Scalars: [{"type": "string", "value": "hello"}]
    - Arrays: [[tagged_value, ...]]
    - Tables: [{"key": tagged_value, ...}]

    {2 Quick Start}

    Using the native encoder (recommended for compatibility):
    {v
    let json = Tomlt_jsont.encode toml_value
    let toml = Tomlt_jsont.decode json_string
    v}

    Using jsont codecs (for integration with jsont pipelines):
    {v
    let json = Tomlt_jsont.encode_jsont toml_value
    let toml = Tomlt_jsont.decode_jsont json_string
    v}

    {2 Module Overview}

    - {!section:native} - Native encode/decode using Tomlt.Toml.Tagged_json
    - {!section:jsont} - Jsont codec for tagged JSON format
    - {!section:conv} - Convenience functions *)

module Toml = Tomlt.Toml
(** Re-exported TOML module for convenience. *)

(** {1:native Native Encode/Decode}

    These functions use Tomlt's built-in tagged JSON encoder/decoder,
    which is highly optimized for the toml-test format. *)

val encode : Toml.t -> string
(** [encode v] encodes TOML value [v] to tagged JSON format.
    This uses [Toml.Tagged_json.encode] directly. *)

val decode : string -> Toml.t
(** [decode s] decodes tagged JSON string [s] to a TOML value.
    This uses [Toml.Tagged_json.decode] directly.
    @raise Failure on malformed JSON or unknown types. *)

val decode_result : string -> (Toml.t, string) result
(** [decode_result s] is like [decode] but returns a result. *)

(** {1:jsont Jsont Codec}

    The [toml] codec provides a jsont-based implementation of the
    tagged JSON format. This allows integration with jsont pipelines
    and other jsont-based tooling. *)

val toml : Toml.t Jsont.t
(** [toml] is a jsont codec for TOML values in tagged JSON format.

    This codec can decode and encode the tagged JSON format used by
    toml-test. On decode, it distinguishes between:
    - Tagged scalars: [{"type": "T", "value": "V"}] (exactly these two keys)
    - Tables: Other JSON objects
    - Arrays: JSON arrays

    On encode, TOML values are converted to appropriate tagged JSON. *)

(** {1:conv Convenience Functions}

    These functions use the jsont codec with [Jsont_bytesrw] for
    string-based encoding/decoding. *)

val encode_jsont : Toml.t -> (string, string) result
(** [encode_jsont v] encodes TOML value [v] using the jsont codec.
    Returns an error string on failure. *)

val decode_jsont : string -> (Toml.t, string) result
(** [decode_jsont s] decodes tagged JSON [s] using the jsont codec.
    Returns an error string on failure. *)

val decode_jsont' : string -> (Toml.t, Jsont.Error.t) result
(** [decode_jsont' s] is like [decode_jsont] but preserves the error. *)

val decode_jsont_exn : string -> Toml.t
(** [decode_jsont_exn s] is like [decode_jsont'] but raises on error.
    @raise Jsont.exception-Error on decode failure. *)

(** {1:internal Internal Types}

    These are exposed for advanced use cases but may change between versions. *)

type tagged_value = {
  typ : string;
  value : string;
}
(** A tagged scalar value with type and value strings. *)

val tagged_jsont : tagged_value Jsont.t
(** Jsont codec for tagged scalar values. *)

val tagged_to_toml : tagged_value -> Toml.t
(** Convert a tagged value to its TOML representation. *)

val toml_to_tagged : Toml.t -> tagged_value
(** Convert a TOML scalar to a tagged value.
    @raise Failure if the value is not a scalar. *)
