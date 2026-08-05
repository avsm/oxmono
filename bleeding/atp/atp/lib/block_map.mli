(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** In-memory CID to block mapping.

    A block map is an immutable mapping from CIDs to block data (bytes). Used
    for batch operations and caching in blockstores. *)

(** {1 Types} *)

type t
(** Immutable map from CID to block bytes. *)

type with_missing = { blocks : t; missing : Cid.t list }
(** Result of batch get with missing CIDs. *)

(** {1 Construction} *)

val empty : t
(** Empty block map. *)

val singleton : Cid.t -> string -> t
(** [singleton cid data] creates a map with single entry. *)

val of_list : (Cid.t * string) list -> t
(** [of_list pairs] creates a map from list of (CID, data) pairs. *)

val of_seq : (Cid.t * string) Seq.t -> t
(** [of_seq seq] creates a map from sequence. *)

(** {1 Queries} *)

val is_empty : t -> bool
(** [is_empty t] is [true] if map is empty. *)

val size : t -> int
(** [size t] returns number of entries. *)

val get : Cid.t -> t -> string option
(** [get cid t] returns block data for [cid], if present. *)

val get_exn : Cid.t -> t -> string
(** [get_exn cid t] returns block data for [cid].
    @raise Not_found if [cid] not in map. *)

val has : Cid.t -> t -> bool
(** [has cid t] is [true] if [cid] is in map. *)

val get_many : Cid.t list -> t -> with_missing
(** [get_many cids t] returns found blocks and list of missing CIDs. *)

(** {1 Modification} *)

val set : Cid.t -> string -> t -> t
(** [set cid data t] adds or replaces entry. *)

val set_many : (Cid.t * string) list -> t -> t
(** [set_many pairs t] adds or replaces entries. *)

val remove : Cid.t -> t -> t
(** [remove cid t] removes entry if present. *)

val merge : t -> t -> t
(** [merge a b] combines maps. [b] entries take precedence. *)

(** {1 Iteration} *)

val to_list : t -> (Cid.t * string) list
(** [to_list t] returns all entries as list. *)

val to_seq : t -> (Cid.t * string) Seq.t
(** [to_seq t] returns all entries as sequence. *)

val iter : (Cid.t -> string -> unit) -> t -> unit
(** [iter f t] calls [f] on each entry. *)

val fold : (Cid.t -> string -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f t acc] folds over entries. *)
