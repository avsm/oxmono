(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Fixed size square patches centred on a point.

    A patch is exactly [(size_px, size_px, bands)] float32 with the
    requested point on the centre of pixel [(size_px / 2, size_px / 2)],
    [NaN] wherever the store holds nothing. {!Tessera.read_patch} is how
    a caller reaches it. It is the port of [GeoTesseraZarr.read_patch]
    in the Python reference, less that function's [dst_crs], so a patch
    comes back on one of the two grids below and never on a
    caller-chosen one.

    {2 The two paths}

    A patch inside one UTM zone is sliced off that zone's grid
    unresampled and comes back in the zone's own CRS. A patch that
    straddles a seam is merged onto a transverse Mercator grid centred
    on the patch, {!Crs.patch}, since no one zone's grid holds it all.
    Which path runs is decided by projecting the four corners of the
    patch out of the centre zone's grid and asking {!Zone.spanned} what
    they cover.

    The merged path relocates pixels whole, nearest neighbour, so a
    vector is never blended with its neighbours. Ownership settles the
    overlap: a pixel is taken from the zone owning its longitude, and
    any other zone only fills pixels the owner has nothing for. *)

type crs = [ `Epsg of int | `Proj of string ]
(** The type for the CRS a patch is on. [`Epsg] is a UTM zone of the
    store, [`Proj] the proj string of a patch-centred grid.
    {!Dataset.Region.t} carries an [epsg] code instead, which cannot
    name the second. *)

type t = {
  data : Zarrz.Slab.t;
      (** Float32 of shape [[size_px; size_px; bands]], C order, so one
          pixel's vector is contiguous. *)
  transform : Affine.t;
      (** The affine of the patch, whose [c] and [f] are the world
          coordinate of the upper-left {b corner} of the first pixel. *)
  crs : crs;  (** The grid the pixels are on. *)
}
(** The type for a patch. *)

val crs_name : crs -> string
(** [crs_name c] is ["EPSG:32631"] for a zone code and the proj string
    itself for a patch grid, which is what {!Crs.name} gives for the
    same grid. *)

val read :
  zone:(int -> Dataset.t option) ->
  lon:float ->
  lat:float ->
  year:int ->
  size_px:int ->
  t
(** [read ~zone ~lon ~lat ~year ~size_px] is the patch of [year] centred
    on the WGS84 point [(lon, lat)]. [zone] resolves a UTM zone number
    to its dataset, or to [None] when the store does not hold it:
    {!Tessera.zone_opt} is that function.

    The zone of [lon] must be present, since it fixes the pixel size and
    the band count of the patch. A contributing zone that is absent is
    skipped, and its pixels stay [NaN].

    Cost. The native path is one region read. The merged path is one
    region read per contributing zone, plus one inverse projection per
    output pixel and one forward projection per output pixel per zone.
    At the sizes a patch is asked for, tens of thousands of projections,
    that is well under the cost of the reads.

    @raise Invalid_argument if [size_px] is not positive.
    @raise Zarrz.Error.E [(Store _)] when the zone of [lon] is not in
    the store.
    @raise Invalid_argument if [year] is not in a contributing zone's
    {!Dataset.years}. *)
