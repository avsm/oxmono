(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A reader for Tessera geospatial embedding stores.

    Tessera publishes one Zarr V3 group per UTM zone, [utm01] to
    [utm60], each holding [int8] embeddings, the per pixel [float32]
    scales that dequantise them, and the years those embeddings cover.
    This module is the layer that takes longitude and latitude: it
    routes a query to the zone that holds it, projects the point into
    that zone's grid and hands back the pixels on their native grid,
    never resampled. {!Dataset} is the layer below, taking eastings and
    northings in a zone's own CRS.

    Everything reads through a {!Zarrz.Store.t}, so the same code runs
    over HTTP, a directory or a store in memory. {!url} is the public
    store.

    It is a port of the Zarr read path of the Python [geotessera]
    package, and {!probe} matches its [GeoTesseraZarr.probe] decision
    for decision.

    {2 Cost}

    {!of_store} reads the root [zarr.json] once. A store written by
    zarr-python carries the metadata of every node inside it, so
    {!zones} is answered from memory and opening a zone costs no
    request at all. Without that, {!zones} probes all sixty zone paths
    and each zone open fetches four documents. Datasets and the answer
    to {!zones} are cached in the handle. *)

module Affine = Affine
module Consolidated = Consolidated
module Crs = Crs
module Dataset = Dataset
module Npy = Npy
module Patch = Patch
module Zone = Zone

type status = Dataset.status = Valid | Water | Nodata | Outside
(** The type for the outcome of a point read. See {!Dataset.status}. *)

type t
(** The type for open stores. *)

val url : string
(** [url] is ["https://data.source.coop/tessera/tessera/zarr/v1"], the
    public store. *)

val of_store : Zarrz.Store.t -> t
(** [of_store store] reads the root group of [store] and binds it.

    @raise Zarrz.Error.E [(Store _)] when [store] has no root
    [zarr.json], and [(Metadata _)] when the root group does not
    declare the geo-embeddings convention or its attributes do not
    satisfy that convention's schema. *)

val store : t -> Zarrz.Store.t
(** [store t] is the store [t] reads. *)

val geoemb : t -> Zarrz_geoemb.t
(** [geoemb t] is the geo-embeddings convention block of the root
    group, which names the model, the build version and the length of
    an embedding vector. *)

val consolidated : t -> Consolidated.t option
(** [consolidated t] is the root group's inline node map, when it has
    one. *)

val zones : t -> int list
(** [zones t] are the UTM zones the store holds, ascending.

    With consolidated metadata this is free. Without it the first call
    fetches the metadata key of each of the sixty zone paths and the
    answer is kept, so it is sixty requests once. *)

val years : t -> int list
(** [years t] are the years of the first zone present in the store. The
    zones share a time axis, so one zone answers for all of them. *)

val zone : t -> int -> Dataset.t
(** [zone t z] is the dataset of zone [z], opened once per handle and
    kept.

    @raise Zarrz.Error.E [(Store _)] when [z] is not in the store, with
    a message naming the zone. *)

val zone_opt : t -> int -> Dataset.t option
(** [zone_opt t z] is {!zone} but [None] when [z] is not in the store.

    With consolidated metadata the node map decides, as it does for
    zarr-python: a zone the map does not list is absent whatever the
    store holds under its path. Without it the store is asked for the
    zone's metadata key, so a store that fails rather than reporting the
    key absent raises instead of answering [None]. *)

val probe :
  t ->
  lon:float ->
  lat:float ->
  year:int ->
  ?cross_zone:bool ->
  ?search_px:int ->
  unit ->
  float array option * status
(** [probe t ~lon ~lat ~year ()] samples the embedding at the WGS84
    point [(lon, lat)] and says why when there is none.

    The point's own zone is tried first. With [cross_zone], the default,
    a point near a zone seam then falls back to the neighbouring zone:
    a tile belongs to the zone holding its centre, so a point on a seam
    is often covered by the zone next door. A zone absent from the store
    is skipped, not an error.

    The first zone answering [Valid] wins. Otherwise the statuses seen
    are ranked [Water] over [Nodata] over [Outside], since water is a
    real answer about the location and [Outside] only says no grid
    reached the point. A point no zone covers is [Outside].

    [search_px] is passed to {!Dataset.probe} and defaults to 1.

    @raise Invalid_argument if [year] is not in the zone's
    {!Dataset.years}. *)

val sample :
  t ->
  lon:float ->
  lat:float ->
  year:int ->
  ?cross_zone:bool ->
  ?search_px:int ->
  unit ->
  float array option
(** [sample t ~lon ~lat ~year ()] is the vector {!probe} finds, or
    [None] for any status but [Valid]. The Python [sample_at] returns a
    row of [NaN] there instead, which cannot be told from a real vector
    of [NaN]. [None] can. *)

val sample_points :
  t ->
  (float * float) array ->
  year:int ->
  ?cross_zone:bool ->
  ?search_px:int ->
  unit ->
  float array option array
(** [sample_points t pts ~year ()] is {!sample} at each [(lon, lat)] of
    [pts], in the order given.

    The points are visited grouped by the zone they route to, so a run
    of neighbouring points reuses one dataset's tile cache instead of
    thrashing it. Each point still falls back across a seam on its own,
    exactly as {!sample} would. *)

val read_region :
  t -> bbox:float * float * float * float -> year:int -> Dataset.Region.t
(** [read_region t ~bbox ~year] is every pixel of [year] inside the
    WGS84 box [(min_lon, min_lat, max_lon, max_lat)], dequantised.

    The zone holding the centre of the box serves the whole request, as
    in the Python reference. A box spanning a seam is therefore answered
    from one zone alone and is short on the far side of it.

    Pixels come back on that zone's grid in its own CRS, untouched. The
    box is projected to pick the window, so the region is the enclosing
    easting and northing extent of the projected corners rather than a
    box in longitude and latitude.

    @raise Zarrz.Error.E [(Store _)] when the centre's zone is not in
    the store. *)

val read_patch :
  t -> lon:float -> lat:float -> year:int -> size_px:int -> Patch.t
(** [read_patch t ~lon ~lat ~year ~size_px] is the square patch of
    [year] centred on the WGS84 point [(lon, lat)], exactly
    [(size_px, size_px, bands)] float32 with the point on the centre of
    pixel [(size_px / 2, size_px / 2)].

    A patch inside one zone keeps that zone's grid and CRS untouched.
    One straddling a seam is merged onto a patch-centred transverse
    Mercator grid. See {!Patch.read}, which this calls with
    {!zone_opt}. *)
