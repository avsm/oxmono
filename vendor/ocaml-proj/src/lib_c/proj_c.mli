(*---------------------------------------------------------------------------
   Copyright (c) 2024 The ocaml-proj programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)
type ctx

val ctx : unit -> ctx
(** Create a new PROJ context. Note that unless multiple threads are involved,
    you shouldn't need to pass in this context. *)

type area
(** A Proj area *)

module CRS : sig
  type t
  (** A CRS object *)

  val of_string : ?ctx:ctx -> string -> t
  (** Create a CRS object from a general string definition *)

  val of_wkt : ?options:string list -> ?ctx:ctx -> string -> t
  (** Create a CRS object from a WKT string *)

  val identify : ?ctx:ctx -> t -> string -> (string * int) list
  (** Attempt to identify a CRS as belonging to a particular naming authority.
      Returns a list of names and confidence values. *)

  val name : t -> string option
  (** Get a human readable name for the CRS *)

  val id_code : t -> int -> string option
  (** Get the code identifier *)

  val id_auth_name : t -> int -> string option
  (** Get the authority name *)
end

module Coord : sig
  type t
  (** A proj coordinate object *)

  val make : x:float -> y:float -> z:float -> t:float -> t
  (** Make a new coordinate using [xyzt] *)

  val v : t -> float array
  (** The values of the coordinate as an array *)

  val x : t -> float
  (** [x t] is [Array.get (v t) 0] *)

  val y : t -> float
  (** [y t] is [Array.get (v t) 1] *)
end

type direction =
  | Forward
  | Inverse
  | Ident  (** The direction to apply a {! transform} *)

module Transformation : sig
  type t
  (** A transformation object *)

  val of_string : ?area:area -> ?ctx:ctx -> src:string -> string -> t
  (** Create a transformation object from [src] to [tgt] using strings *)

  val of_crs :
    ?area:area -> ?options:string list -> ?ctx:ctx -> src:CRS.t -> CRS.t -> t
  (** Create a transformation object from [src] to [tgt] using CRS.t values *)

  val normalize_for_visualization : ?ctx:ctx -> t -> t
  (** This will change the given {! t} into one whose axis order is the one
      expected for visualization purposes. *)

  val transform : ?direction:direction -> t -> Coord.t -> Coord.t
  (** [transform ?direction trans c] uses [trans] to transform [c]. You can
      optionally use the [direction] argument to invert the transformation. *)
end
