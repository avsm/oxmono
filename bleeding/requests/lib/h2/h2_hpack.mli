(*---------------------------------------------------------------------------
  Copyright (c) 2019 Antonio Nuno Monteiro.
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  SPDX-License-Identifier: BSD-3-Clause
 ---------------------------------------------------------------------------*)

(** HPACK Header Compression per RFC 7541.

    This module implements header compression for HTTP/2 as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc7541}RFC 7541}.

    {2 Overview}

    HPACK compresses HTTP headers by:
    - Using static and dynamic tables to index frequently used headers
    - Huffman encoding string literals
    - Variable-length integer encoding

    {2 Usage}

    Create encoder/decoder contexts for each HTTP/2 connection:
    {[
      let encoder = Encoder.create 4096 in
      let decoder = Decoder.create 4096 in

      (* Encode headers *)
      let headers = [
        { name = ":method"; value = "GET"; sensitive = false };
        { name = ":path"; value = "/"; sensitive = false };
      ] in
      let buf = Cstruct.create 1024 in
      let len = Encoder.encode_headers encoder buf headers in

      (* Decode headers *)
      let header_block = Cstruct.sub buf 0 len in
      match Decoder.decode decoder header_block with
      | Ok headers -> (* process headers *)
      | Error Decoding_error -> (* handle error *)
    ]}
*)

(** {1 Types} *)

type header = {
  name : string;
  (** Header field name (lowercase, or pseudo-header with colon prefix). *)
  value : string;
  (** Header field value. *)
  sensitive : bool;
  (** If true, this header should never be indexed.
      Used for sensitive values like cookies and authorization. *)
}
(** A header field with optional sensitivity flag. *)

type error = Decoding_error
(** HPACK decoding error. *)

val pp_error : Format.formatter -> error -> unit
(** Pretty printer for errors. *)

(** {1 Decoder}

    HPACK decoder with dynamic table context. *)
module Decoder : sig
  type t
  (** Decoder context maintaining dynamic table state. *)

  val create : int -> t
  (** [create max_capacity] creates a decoder with the given maximum
      dynamic table capacity in bytes (default 4096). *)

  val set_capacity : t -> int -> (unit, error) result
  (** [set_capacity t capacity] updates the dynamic table capacity.
      Returns [Error Decoding_error] if capacity exceeds max_capacity. *)

  val decode : t -> Cstruct.t -> (header list, error) result
  (** [decode t buf] decodes a header block fragment.
      Returns headers in transmission order.

      Per RFC 7541, decoding updates the dynamic table state, so the
      same decoder must be used for all header blocks on a connection. *)
end

(** {1 Encoder}

    HPACK encoder with dynamic table context. *)
module Encoder : sig
  type t
  (** Encoder context maintaining dynamic table state. *)

  val create : int -> t
  (** [create capacity] creates an encoder with the given dynamic
      table capacity in bytes (default 4096). *)

  val set_capacity : t -> int -> unit
  (** [set_capacity t capacity] updates the dynamic table capacity.
      This may evict entries if the new capacity is smaller. *)

  val encode_headers : t -> Cstruct.t -> header list -> int
  (** [encode_headers t buf headers] encodes headers into [buf].
      Returns the number of bytes written.

      The buffer must be large enough for the encoded output.
      A safe estimate is 16KB for typical header blocks.

      Per RFC 7541, encoding updates the dynamic table state, so the
      same encoder must be used for all header blocks on a connection. *)
end

(** {1 Huffman Encoding}

    Low-level Huffman encoding/decoding per RFC 7541 Appendix B. *)
module Huffman : sig
  val encoded_length : string -> int
  (** [encoded_length s] returns the Huffman-encoded length in bytes. *)

  val encode : Cstruct.t -> int -> string -> int
  (** [encode buf off s] encodes [s] using Huffman coding.
      Returns the number of bytes written. *)

  val decode : string -> (string, error) result
  (** [decode s] decodes a Huffman-encoded string. *)
end

(** {1 Constants} *)

val default_table_size : int
(** Default dynamic table size: 4096 bytes. *)
