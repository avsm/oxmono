(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** zarr-python consolidated metadata.

    {!Zarrz.Consolidated} with the one lookup the Tessera layout needs.
    A store written by zarr-python carries every descendant node's
    [zarr.json] inside the root group's own document, so reading that
    document describes the whole store and opening a node below it
    costs no request. The Tessera store holds 871 nodes this way. *)

include module type of struct
  include Zarrz.Consolidated
end

val zones : t -> int list
(** [zones t] are the UTM zone numbers of the groups directly below the
    root of [t], ascending.

    A child counts when its name is exactly ["utm"] followed by two
    digits naming a zone in \[1;60\], and when it is a group. The zone
    arrays are keyed ["utm30/embeddings"] and the like, so they are not
    root children at all, and the group test is what keeps a same-named
    array out. *)
