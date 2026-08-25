(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One UTM zone of a Tessera store.

    A dataset binds the three arrays of a [utm{NN}] group, [embeddings],
    [scales] and [time], to the group's affine transform, and reads
    points and regions off them. Every coordinate here is an easting and
    a northing in the zone's own CRS: nothing in this module projects.
    {!Tessera.probe} and its neighbours are the layer that takes
    longitude and latitude.

    {2 Pixel states}

    A [scales] value says what a pixel is. Finite and positive is real
    data and dequantises the [embeddings] column beneath it. [NaN] is
    open water or nodata inside a written tile. The array's fill value
    is [+inf], so a region no tile ever covered reads as [+inf] without
    a chunk being stored. {!probe} reports the three apart.

    {2 Reads}

    Point reads go through a cache of inner-chunk-aligned 32 by 32
    tiles, one per array per time index, in an {!Lru}. A point workload
    revisits a tile constantly, and a hit costs no request. Region reads
    fetch their own subsets and leave the cache alone, since a region is
    read once and is far larger than a tile. *)

type status = Valid | Water | Nodata | Outside
(** The type for the outcome of a point read. [Outside] is a point more
    than one pixel from the nearest pixel centre of this grid,
    [Nodata] a neighbourhood no tile ever wrote, [Water] a pixel the
    producer marked as having no embedding. *)

(** Dequantised rectangles. *)
module Region : sig
  type t = {
    data : Zarrz.Slab.t;
        (** Float32 of shape [[h; w; bands]], C order, so one pixel's
            vector is contiguous. *)
    transform : Affine.t;
        (** The affine of the window, whose [c] and [f] are the world
            coordinate of the upper-left {b corner} of the first
            pixel. *)
    epsg : int;  (** The code of the zone the pixels are on. *)
  }
  (** The type for a dequantised region. *)
end

type t
(** The type for open zone datasets. *)

val open_ :
  ?cache_capacity:int ->
  ?consolidated:Consolidated.t ->
  Zarrz.Store.t ->
  zone:int ->
  t
(** [open_ store ~zone] binds the group [utm{zone}] of [store] and its
    three arrays.

    [consolidated] is the root group's node map, which supplies the
    metadata of any node it holds so that opening the zone costs no
    request. A node it lacks is fetched. [cache_capacity] is the tile
    cache bound and defaults to 256, about 33 MiB at the store's 128
    bands.

    @raise Zarrz.Error.E [(Store _)] when the group or an array is
    absent, and [(Metadata _)] when the group has no [proj:code] or
    [spatial:transform], when [proj:code] is not the zone's canonical
    code, or when an array has an unexpected rank, shape or data
    type. *)

val zone : t -> int
(** [zone t] is the UTM zone number of [t]. *)

val epsg : t -> int
(** [epsg t] is the [proj:code] of the group, always [32600 + zone t].
    Both hemispheres of a zone are filed under the northern code, with
    southern data at negative northings, so this is checked at
    {!open_} rather than trusted. *)

val transform : t -> Affine.t
(** [transform t] is the group's [spatial:transform], mapping pixel
    corners to eastings and northings. *)

val shape : t -> int * int
(** [shape t] is the height and width of the grid in pixels. *)

val bands : t -> int
(** [bands t] is the length of one embedding vector. *)

val pixel_size : t -> float
(** [pixel_size t] is the width of a pixel in metres, [10.] in the
    published store. *)

val crs : t -> Crs.t
(** [crs t] projects WGS84 to this zone's grid. It is built from
    {!zone}, not from {!epsg}: the store's canonical code is the
    northern one for both hemispheres and {!Crs.utm_north} is what
    reproduces it. *)

val years : t -> int list
(** [years t] are the values of the [time] array, in its own order.
    Time is looked up by value, never by index. The array is read on the
    first call, one small chunk, and kept. *)

val proj : t -> lon:float -> lat:float -> float * float
(** [proj t ~lon ~lat] is the easting and northing of the WGS84 point
    [(lon, lat)] on this zone's grid. *)

val probe :
  t ->
  e:float ->
  n:float ->
  year:int ->
  ?search_px:int ->
  unit ->
  float array option * status
(** [probe t ~e ~n ~year ()] samples the embedding at [(e, n)] and says
    why when there is none. It is the OCaml of [TesseraAccessor.probe]
    in the Python reference, decision for decision.

    The column and row are the nearest pixel centres, ties going to the
    lower index as [numpy.argmin] does, clamped to the grid. A residual
    above one pixel in either axis is [Outside], which is what stops a
    distant point from snapping onto an edge pixel.

    The [(2 * search_px + 1)] squared window of [scales] around that
    pixel then decides. A [NaN] centre is [Water] and is never searched
    past, so a repair can never report land for a sea location. A finite
    centre is the pixel itself. Otherwise the nearest finite scale in
    the window wins by squared pixel distance, ties going to the first
    in row-major order, and a window with no finite scale is [Nodata].

    The winner's [embeddings] column is dequantised into a fresh array
    of {!bands} doubles, each the float32 product of the stored [int8]
    and the scale. [search_px] defaults to 1 and 0 disables the search.

    @raise Invalid_argument if [year] is not in {!years}, with a message
    listing them. *)

val sample :
  t -> e:float -> n:float -> year:int -> ?search_px:int -> unit ->
  float array option
(** [sample t ~e ~n ~year ()] is the vector {!probe} finds, or [None]
    for any status but [Valid]. The Python [sample_at] returns a row of
    [NaN] there instead, which cannot be told from a real vector of
    [NaN]. [None] can. *)

val read_region :
  t ->
  e_min:float ->
  e_max:float ->
  n_min:float ->
  n_max:float ->
  year:int ->
  Region.t
(** [read_region t ~e_min ~e_max ~n_min ~n_max ~year] is every pixel of
    [year] whose centre lies in the closed box, dequantised.

    The bounds are taken as given and swapped if reversed. Selection is
    inclusive on pixel centres, matching the [xarray] label slice the
    Python reference uses, so the first column is
    [ceil (Affine.col_of_x t e_min)] and the last is
    [floor (Affine.col_of_x t e_max)], clamped to the grid. Rows run
    north to south, so [n_max] picks the first.

    A box that selects nothing is a region of shape [[0; w; bands]] or
    [[h; 0; bands]] whose transform is that of the clamped corner. A
    pixel whose scale is not finite gets a row of [NaN].

    The two subsets are read straight from the arrays, so a region read
    neither fills nor disturbs the tile cache. *)
