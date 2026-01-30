(*---------------------------------------------------------------------------
  Copyright (c) 2019 Antonio Nuno Monteiro.
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>.

  SPDX-License-Identifier: BSD-3-Clause
 ---------------------------------------------------------------------------*)

(** {b Internal module - not part of the public API.}

    HPACK static table and token indices per RFC 7541 Appendix A.
    These tables are used internally by {!H2_hpack}. Direct use is
    not recommended and the API may change without notice. *)

val static_table_size : int
(** Size of the HPACK static table (61 entries). *)

val static_table : (string * string) array
(** The HPACK static table as (name, value) pairs. *)

(** Token indices for commonly accessed headers.
    These correspond to 0-based indices into {!static_table}. *)
module TokenIndices : sig
  val authority : int
  val _method : int
  val path : int
  val scheme : int
  val status : int
  val accept_charset : int
  val accept_encoding : int
  val accept_language : int
  val accept_ranges : int
  val accept : int
  val access_control_allow_origin : int
  val age : int
  val allow : int
  val authorization : int
  val cache_control : int
  val content_disposition : int
  val content_encoding : int
  val content_language : int
  val content_length : int
  val content_location : int
  val content_range : int
  val content_type : int
  val cookie : int
  val date : int
  val etag : int
  val expect : int
  val expires : int
  val from : int
  val host : int
  val if_match : int
  val if_modified_since : int
  val if_none_match : int
  val if_range : int
  val if_unmodified_since : int
  val last_modified : int
  val link : int
  val location : int
  val max_forwards : int
  val proxy_authenticate : int
  val proxy_authorization : int
  val range : int
  val referer : int
  val refresh : int
  val retry_after : int
  val server : int
  val set_cookie : int
  val strict_transport_security : int
  val transfer_encoding : int
  val user_agent : int
  val vary : int
  val via : int
  val www_authenticate : int
end

val lookup_token_index : string -> int
(** [lookup_token_index name] returns the static table token index for [name],
    or -1 if not found. *)
