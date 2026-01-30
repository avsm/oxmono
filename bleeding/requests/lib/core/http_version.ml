(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP protocol version identification and ALPN support.

    Implements protocol identification per
    {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3.1}RFC 9113 Section 3.1}. *)

type t =
  | Http_1_0
  | Http_1_1
  | Http_2

let to_string = function
  | Http_1_0 -> "HTTP/1.0"
  | Http_1_1 -> "HTTP/1.1"
  | Http_2 -> "HTTP/2"

let pp ppf v = Format.pp_print_string ppf (to_string v)

let equal v1 v2 =
  match v1, v2 with
  | Http_1_0, Http_1_0 -> true
  | Http_1_1, Http_1_1 -> true
  | Http_2, Http_2 -> true
  | _ -> false

let compare v1 v2 =
  let to_int = function
    | Http_1_0 -> 0
    | Http_1_1 -> 1
    | Http_2 -> 2
  in
  Int.compare (to_int v1) (to_int v2)

(* ALPN Protocol Identifiers per RFC 9113 Section 3.1 *)

let alpn_h2 = "h2"
let alpn_http_1_1 = "http/1.1"

let alpn_of_version = function
  | Http_1_0 -> None  (* HTTP/1.0 has no ALPN identifier *)
  | Http_1_1 -> Some alpn_http_1_1
  | Http_2 -> Some alpn_h2

let version_of_alpn = function
  | "h2" -> Some Http_2
  | "http/1.1" -> Some Http_1_1
  | _ -> None

let alpn_protocols ~preferred =
  List.filter_map alpn_of_version preferred

(* Version capability detection *)

let supports_multiplexing = function
  | Http_2 -> true
  | Http_1_0 | Http_1_1 -> false

let supports_server_push = function
  | Http_2 -> true
  | Http_1_0 | Http_1_1 -> false

let supports_header_compression = function
  | Http_2 -> true
  | Http_1_0 | Http_1_1 -> false
