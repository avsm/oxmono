(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Response limits for HTTP protocol handling

    Configurable limits for response body size, header count, and header length
    to prevent DoS attacks. *)

type t = {
  max_response_body_size: int64;
  max_header_size: int;
  max_header_count: int;
  max_decompressed_size: int64;
  max_compression_ratio: float;
}

let default = {
  max_response_body_size = 104_857_600L;  (* 100MB *)
  max_header_size = 16_384;               (* 16KB *)
  max_header_count = 100;
  max_decompressed_size = 104_857_600L;   (* 100MB *)
  max_compression_ratio = 100.0;          (* 100:1 *)
}

let make
    ?(max_response_body_size = 104_857_600L)
    ?(max_header_size = 16_384)
    ?(max_header_count = 100)
    ?(max_decompressed_size = 104_857_600L)
    ?(max_compression_ratio = 100.0)
    () =
  { max_response_body_size; max_header_size; max_header_count;
    max_decompressed_size; max_compression_ratio }

let max_response_body_size t = t.max_response_body_size
let max_header_size t = t.max_header_size
let max_header_count t = t.max_header_count
let max_decompressed_size t = t.max_decompressed_size
let max_compression_ratio t = t.max_compression_ratio

let pp fmt t =
  Format.fprintf fmt "@[<v 2>Response_limits {@ \
    max_response_body_size: %Ld bytes@ \
    max_header_size: %d bytes@ \
    max_header_count: %d@ \
    max_decompressed_size: %Ld bytes@ \
    max_compression_ratio: %.1f:1@ \
    }@]"
    t.max_response_body_size
    t.max_header_size
    t.max_header_count
    t.max_decompressed_size
    t.max_compression_ratio

let to_string t = Format.asprintf "%a" pp t
