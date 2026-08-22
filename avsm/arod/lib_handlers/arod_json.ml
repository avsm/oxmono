(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON responses. *)

let encode codec v =
  match Jsont_bytesrw.encode_string codec v with
  | Ok s -> s
  | Error _ -> {|{"error":"JSON encoding failed"}|}
