(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** References extension for JSON Feed items.

    This implements the references extension that allows items to cite sources.
    Each reference represents a cited resource with optional DOI and CiTO
    annotations.

    @see <https://github.com/egonw/JSONFeed-extensions/blob/main/references.md>
      References Extension Specification
    @see <https://purl.archive.org/spar/cito> Citation Typing Ontology *)

type t
(** The type representing a reference to a cited source. *)

(** {1 Unknown Fields} *)

module Unknown : sig
  type t = Jsont.json
  (** Unknown/unrecognized JSON object members as a generic JSON object. Useful
      for preserving fields from custom extensions or future spec versions. *)

  val empty : t
  (** [empty] is the empty list of unknown fields. *)

  val is_empty : t -> bool
  (** [is_empty u] returns [true] if there are no unknown fields. *)
end

(** {1 Jsont Type} *)

val jsont : t Jsont.t
(** Declarative JSON type for references.

    Maps JSON objects with "url" (required) and optional "doi" and "cito"
    fields. *)

(** {1 Construction} *)

val create :
  url:string ->
  ?doi:string ->
  ?cito:Cito.t list ->
  ?unknown:Unknown.t ->
  unit ->
  t
(** [create ~url ?doi ?cito ?unknown ()] creates a reference.

    @param url
      Unique URL for the reference (required). A URL based on a persistent
      unique identifier (like DOI) is recommended.
    @param doi Digital Object Identifier for the reference
    @param cito Citation Typing Ontology intent annotations
    @param unknown Unknown/custom fields for extensions (default: empty)

    {b Examples:}
    {[
      (* Simple reference with just a URL *)
      let ref1 =
        Reference.create ~url:"https://doi.org/10.5281/zenodo.16755947" ()

      (* Reference with DOI *)
      let ref2 =
        Reference.create ~url:"https://doi.org/10.5281/zenodo.16755947"
          ~doi:"10.5281/zenodo.16755947" ()

      (* Reference with CiTO annotations *)
      let ref3 =
        Reference.create ~url:"https://doi.org/10.5281/zenodo.16755947"
          ~doi:"10.5281/zenodo.16755947"
          ~cito:[ `CitesAsRecommendedReading; `UsesMethodIn ]
          ()
    ]} *)

(** {1 Accessors} *)

val url : t -> string
(** [url t] returns the reference's URL. *)

val doi : t -> string option
(** [doi t] returns the reference's DOI, if set. *)

val cito : t -> Cito.t list option
(** [cito t] returns the reference's CiTO annotations, if set. *)

val unknown : t -> Unknown.t
(** [unknown t] returns unrecognized fields from the JSON. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests equality between two references.

    References are considered equal if they have the same URL. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] pretty prints a reference to the formatter.

    {b Example output:}
    {v https://doi.org/10.5281/zenodo.16755947 [DOI: 10.5281/zenodo.16755947] v}
*)
