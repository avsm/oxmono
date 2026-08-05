(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Statistics for connection pool endpoints *)

(** {1 Type} *)

type t
(** Statistics snapshot for a specific endpoint *)

(** {1 Construction} *)

val make :
  active:int ->
  idle:int ->
  total_created:int ->
  total_reused:int ->
  total_closed:int ->
  errors:int ->
  t
(** Create a statistics snapshot. *)

(** {1 Accessors} *)

val active : t -> int
(** Number of connections currently in use. *)

val idle : t -> int
(** Number of connections in pool waiting to be reused. *)

val total_created : t -> int
(** Total connections created over the endpoint's lifetime. *)

val total_reused : t -> int
(** Total number of times connections were reused from the pool. *)

val total_closed : t -> int
(** Total connections that have been closed. *)

val errors : t -> int
(** Total connection errors encountered. *)

(** {1 Pretty-printing} *)

val pp : t Fmt.t
(** Pretty-printer for statistics. *)
