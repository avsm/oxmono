(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unknown fields for capturing extra JSON object members.

    This module provides a type and utilities for preserving unknown/extra
    fields when parsing JSON objects with jsont. Use with
    [Jsont.Object.keep_unknown] to capture fields not explicitly defined in your
    codec. *)

type t = Jsont.json
(** The type of unknown fields - stored as raw JSON. *)

val empty : t
(** An empty unknown fields value (empty JSON object). *)

val is_empty : t -> bool
(** [is_empty t] returns [true] if there are no unknown fields. *)

val jsont : t Jsont.t
(** Codec for unknown fields. *)
