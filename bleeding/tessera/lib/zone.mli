(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** UTM zone routing.

    The Tessera store holds one group per UTM zone, [utm01] to [utm60],
    and routes a query by longitude alone. The rule is the plain
    six-degree division with no Norway or Svalbard exceptions, matching
    the Python reference implementation.

    Both hemispheres of a zone live in the northern group, so the code a
    zone is stored under is always [32600 + zone]. Southern data sits at
    negative northings on a continuous axis rather than in an [EPSG:327xx]
    code. *)

val for_lon : float -> int
(** [for_lon lon] is the UTM zone holding the WGS84 longitude [lon],
    clamped to [1 .. 60]. Longitudes at or below [-180.] give zone 1 and
    [180.] gives zone 60. *)

val canonical_epsg : int -> int
(** [canonical_epsg z] is [32600 + z], the northern UTM code the store
    files zone [z] under in both hemispheres. *)

val centre_lon : int -> float
(** [centre_lon z] is the central meridian of zone [z], that is
    [-180. +. (z - 0.5) * 6.]. *)

val seam_degrees : float
(** [seam_degrees] is [0.1], how close to a zone boundary a longitude
    must be before the neighbouring zone is worth consulting.

    A tile is 0.1 degrees and belongs to the zone containing its centre,
    so a point can only be covered by the zone next door if it is within
    half a tile of the boundary. This is that bound doubled, which
    absorbs the sub-pixel spread of a tile's curved UTM footprint. *)

val seam_neighbours : float -> int list
(** [seam_neighbours lon] is the zones to try after [for_lon lon] when a
    read there fails, empty away from a seam.

    A longitude within {!seam_degrees} of the western boundary of its
    zone yields the zone to the west, one within {!seam_degrees} of the
    eastern boundary yields the zone to the east, and zones 1 and 60 wrap
    into each other across the antimeridian. A longitude exactly on a
    seam belongs to the eastern zone, so it yields the western one
    alone. *)

val spanned : float list -> centre_lon:float -> int list
(** [spanned lons ~centre_lon] is the contiguous inclusive run of zones
    covering [lons], listed west to east.

    The run is walked the short way round the ring from the zone of
    [centre_lon], so a patch straddling the antimeridian yields
    [[60; 1]] rather than the fifty-nine zones between them.

    @raise Invalid_argument if [lons] is empty. *)
