(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Persistent metadata for one sparse cached object. *)
type t = {
  url : string; (** Complete upstream URL, including its query. *)
  absent : bool; (** Whether the origin returned 404 or 410. *)
  generation : string; (** Data-file ID: 32 lowercase hexadecimal digits. *)
  size : int; (** Full object length in bytes, including uncovered holes. *)
  content_type : string; (** Upstream media type. *)
  etag : string option; (** Upstream entity tag, when supplied. *)
  modified : string option; (** Upstream Last-Modified field, when supplied. *)
  ranges : Coverage.t list; (** Synced bytes, in sorted, coalesced intervals. *)
}

val jsont : t Jsont.t
(** [jsont] encodes and decodes version 1 manifests. Missing [version] means
    version 1 for compatibility with earlier caches. Unknown versions and
    fields are rejected.

    Both directions validate nonempty URLs, safe generation IDs, exact
    nonnegative JSON integer sizes and offsets, and nonempty, sorted,
    disjoint, coalesced coverage within the object. Absent entries have
    zero size and no coverage. Offsets cannot exceed [2^53 - 1] or [max_int].
    Invalid values produce Jsont errors. Data-file existence and length
    are checked separately by {!Cache}. *)
