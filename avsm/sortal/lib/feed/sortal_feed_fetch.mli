(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP feed fetching via Fetch.

    Supports conditional GET with ETag and Last-Modified headers
    to avoid re-downloading unchanged feeds. *)

type fetch_result = {
  body : string;
  etag : string option;
  last_modified : string option;
  content_type : string option;
      (** the response's [Content-Type] media type (e.g. ["text/html"]),
          without parameters such as [charset]. Not to be trusted for
          identifying the feed's format: use {!Sortal_feed_sniff} on
          [body] instead. Kept only for error messages. *)
}

val fetch :
  session:Fetch.plain ->
  ?etag:string ->
  ?last_modified:string ->
  string ->
  (fetch_result, [`Not_modified | `Error of string]) result
