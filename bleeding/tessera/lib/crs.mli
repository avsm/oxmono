(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Projections between WGS84 and the store's grids.

    Two shapes are needed to read a Tessera store: the northern UTM zone
    a group is filed under, and a transverse Mercator grid centred on a
    patch, used when a patch straddles a zone seam. Both project from
    and to [EPSG:4326], always in longitude then latitude order.

    A value holds the PROJ transformation it needs, built on first use
    and kept. PROJ objects are not thread safe, so a value belongs to one
    domain. Share nothing, build one value per domain.

    EPSG codes are resolved against the PROJ database, so
    [/usr/share/proj/proj.db] must be present at run time. The transverse
    Mercator strings of {!patch} carry their own datum and need no
    lookup. *)

type t
(** A projection from [EPSG:4326] to one target grid. *)

val utm_north : zone:int -> t
(** [utm_north ~zone] projects to [EPSG:{32600 + zone}], the northern UTM
    code the store files [zone] under in both hemispheres. Southern
    points come back with a negative northing on a continuous axis rather
    than the [EPSG:327xx] false northing.

    @raise Invalid_argument if [zone] is outside [1 .. 60]. *)

val patch : lon:float -> lat:float -> t
(** [patch ~lon ~lat] projects to a transverse Mercator grid whose
    central meridian is [lon], namely

    {[
      +proj=tmerc +lat_0=0 +lon_0=<lon> +k=0.9996 +x_0=500000
      +y_0=<0 north, 10000000 south> +datum=WGS84 +units=m +no_defs
    ]}

    with [lon] printed to eight decimal places. This is UTM's projection
    centred on the patch rather than on a six-degree zone, which keeps
    distortion small and symmetric whatever the patch spans. The false
    northing follows the sign of [lat], so a southern patch has a
    positive northing. *)

val forward : t -> lon:float -> lat:float -> float * float
(** [forward t ~lon ~lat] is the easting and northing of the WGS84 point
    [(lon, lat)] in [t].

    @raise Invalid_argument if PROJ rejects the target grid or the point,
    with the message naming the CRS. *)

val inverse : t -> e:float -> n:float -> float * float
(** [inverse t ~e ~n] is the WGS84 longitude and latitude of the easting
    and northing [(e, n)] in [t]. It is the inverse of {!forward}.

    @raise Invalid_argument if PROJ rejects the target grid or the point,
    with the message naming the CRS. *)

val name : t -> string
(** [name t] is the string [t] was built from, an [EPSG:] code for
    {!utm_north} and a proj string for {!patch}. *)
