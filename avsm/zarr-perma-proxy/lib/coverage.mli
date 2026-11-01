(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Half-open byte intervals and persistent coverage. *)
type t = private { start : int; stop : int }
val v : int -> int -> t
(** [v start stop] is the interval [start, stop). Negative or reversed
    bounds raise [Invalid_argument]. *)
val merge : t list -> t -> t list
(** [merge ranges r] inserts [r] into sorted disjoint [ranges], coalescing
    overlaps and adjacent intervals. *)
val missing : t list -> t -> t list
(** [missing ranges r] is the sorted set of uncovered intervals in [r]. *)
val covers : t list -> t -> bool
(** [covers ranges r] says whether all of [r] is covered. *)
val offset_jsont : int Jsont.t
(** [offset_jsont] encodes exact nonnegative byte offsets. Fractions, strings
    and values above the smaller of [max_int] and [2^53 - 1] are rejected. *)
val jsont : t Jsont.t
(** [jsont] encodes interval bounds and validates them in both directions. *)
