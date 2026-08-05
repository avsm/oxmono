(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP feed fetching via Requests.

    Supports conditional GET with ETag and Last-Modified headers
    to avoid re-downloading unchanged feeds. *)

type fetch_result = {
  body : string;
  etag : string option;
  last_modified : string option;
}

val fetch :
  session:Requests.t ->
  ?etag:string ->
  ?last_modified:string ->
  string ->
  (fetch_result, [`Not_modified | `Error of string]) result
