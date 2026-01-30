(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Response limits for HTTP protocol handling

    Configurable limits for response body size, header count, and header length
    to prevent DoS attacks. *)

type t
(** Abstract type representing HTTP response limits. *)

val default : t
(** Default limits:
    - max_response_body_size: 100MB
    - max_header_size: 16KB
    - max_header_count: 100
    - max_decompressed_size: 100MB
    - max_compression_ratio: 100:1 *)

val make :
  ?max_response_body_size:int64 ->
  ?max_header_size:int ->
  ?max_header_count:int ->
  ?max_decompressed_size:int64 ->
  ?max_compression_ratio:float ->
  unit -> t
(** Create custom response limits. All parameters are optional and default
    to the values in {!default}. *)

val max_response_body_size : t -> int64
(** Maximum response body size in bytes. *)

val max_header_size : t -> int
(** Maximum size of a single header line in bytes. *)

val max_header_count : t -> int
(** Maximum number of headers allowed. *)

val max_decompressed_size : t -> int64
(** Maximum decompressed size in bytes. *)

val max_compression_ratio : t -> float
(** Maximum compression ratio allowed (e.g., 100.0 means 100:1). *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for response limits. *)

val to_string : t -> string
(** Convert response limits to a human-readable string. *)
