(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** zarr-python consolidated metadata.

    A store written by zarr-python carries every descendant node's
    [zarr.json] inside the root group's own document, under a
    [consolidated_metadata] member that Zarr V3 marks
    [must_understand: false]:

    {[
      { "kind": "inline", "must_understand": false,
        "metadata": { "utm30": { ... }, "utm30/embeddings": { ... } } }
    ]}

    Reading the root document therefore describes the whole store, and
    opening a node below it costs no request at all. The TESSERA store
    holds 871 nodes this way, so the map is kept as raw JSON and each
    node is parsed only when it is opened.

    The keys are node paths relative to the root with no leading slash.
    {!node} accepts either spelling. *)

type t
(** The type for a consolidated node map. *)

val of_group : Zarrz.Metadata.group_meta -> t option
(** [of_group m] is the node map inside the [consolidated_metadata]
    member of [m], or [None] when [m] has no such member, when its
    [kind] is not ["inline"], or when it carries no [metadata] object.
    Nothing below [metadata] is parsed. *)

val node : t -> string -> Jsont.json option
(** [node t path] is the [zarr.json] document of the node at [path], or
    [None] when [t] does not hold one. [path] is relative to the root,
    with a leading or trailing slash tolerated, as in ["utm30"] or
    ["/utm30/embeddings"]. *)

val paths : t -> string list
(** [paths t] are the node paths [t] holds, in document order and
    without the leading slash. *)

val zones : t -> int list
(** [zones t] are the UTM zone numbers of the groups [t] holds,
    ascending.

    A key counts when it is exactly ["utm"] followed by two digits, and
    when the document under it says [node_type] ["group"]. The zone
    arrays are keyed ["utm30/embeddings"] and the like, so the two-digit
    rule alone already excludes them, and the [node_type] test is what
    keeps a same-named array out. *)
