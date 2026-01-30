(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Network endpoint representation *)

(** {1 Logging} *)

val src : Logs.Src.t
(** Logs source for endpoint operations. Configure logging with:
    {[
      Logs.Src.set_level Conpool.Endpoint.src (Some Logs.Debug)
    ]} *)

(** {1 Type} *)

type t
(** Network endpoint identified by host and port *)

(** {1 Construction} *)

val make : host:string -> port:int -> t
(** Create an endpoint from a hostname and port. *)

(** {1 Accessors} *)

val host : t -> string
(** Get the hostname from an endpoint. *)

val port : t -> int
(** Get the port number from an endpoint. *)

(** {1 Comparison and Hashing} *)

val equal : t -> t -> bool
(** Compare two endpoints for equality. *)

val hash : t -> int
(** Hash an endpoint for use in hash tables. *)

(** {1 Pretty-printing} *)

val pp : t Fmt.t
(** Pretty-printer for endpoints. Formats as "host:port". *)
