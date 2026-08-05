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

(** An empty unknown fields value (empty JSON object). *)
let empty = Jsont.Object ([], Jsont.Meta.none)

(** [is_empty t] returns [true] if there are no unknown fields. *)
let is_empty = function Jsont.Object ([], _) -> true | _ -> false

(** Codec for unknown fields. *)
let jsont = Jsont.json
