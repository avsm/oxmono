(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Consolidated metadata.

    The map of node documents that a writer such as zarr-python puts
    into the root group under a [consolidated_metadata] member. One
    document then describes the whole hierarchy, so a reader that has
    it opens any node without a request of its own.

    The member carries [must_understand] [false], so
    {!Metadata.group_jsont} keeps it among the [group_unknown] members
    of {!Metadata.group_meta} rather than rejecting it. This module
    gives that member a type.

    A node document is kept exactly as it was parsed and is decoded
    only when a caller asks for it, so a map of hundreds of nodes costs
    one pass over a member list to hold. Open an array from one with
    {!Arr.of_json}, which reaches no further metadata. *)

type t
(** The type for a consolidated node map. *)

val of_group : Metadata.group_meta -> t option
(** [of_group m] is the node map [m] carries, or [None] when [m] has no
    [consolidated_metadata] member, when that member is not an object,
    when its [kind] is not ["inline"], or when it has no [metadata]
    object. ["inline"] is the only kind the specification defines, so
    any other one names a map this module cannot read rather than an
    empty one.

    A path that the [metadata] object repeats keeps its first
    document. *)

val paths : t -> string list
(** [paths t] are the paths of the nodes of [t], in the document order
    of the [metadata] object. Each is relative to the root and carries
    no leading ['/']. The root is not among them, since the document
    holding [t] is the root's own. *)

val node : t -> string -> Jsont.json option
(** [node t path] is the [zarr.json] document of the node at [path], or
    [None] when [t] holds no node there. Leading and trailing ['/'] are
    ignored, so ["a/b"], ["/a/b"] and ["/a/b/"] name the same node. *)

val children : t -> string -> (string * [ `Array | `Group ]) list
(** [children t path] are the nodes one path component below [path],
    each by its own name rather than its path, in the document order of
    {!paths}. An empty [path] and ["/"] both name the root.

    A child is reported whether or not [path] is itself a node of [t],
    so a map that skipped an intermediate group still enumerates below
    it.

    The kind comes from the [node_type] member of the child's document,
    read here rather than at {!of_group}, so a caller that walks one
    branch of a hierarchy does not pay for the rest. A document that
    does not say ["array"] is a group, which is what a document-less
    implicit group is too. *)
