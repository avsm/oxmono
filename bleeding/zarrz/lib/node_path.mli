(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Node names and node paths.

    A path names a node in a hierarchy and is its node names joined by
    ['/']. The specification writes one with a leading ['/'], as in
    ["/foo/bar"], and the root as ["/"]. This library accepts that
    spelling and the one without, so ["foo/bar"] and ["/foo/bar"] name
    one node, and [""] and ["/"] both name the root.

    Creating a node checks its path with {!check}. Opening one does
    not: the specification says nothing a reader must do about a name
    it would not have written, and a hierarchy another writer produced
    is worth reading. *)

val is_valid_name : string -> bool
(** [is_valid_name n] is [true] when [n] is a node name the
    specification allows. Such a name is not the empty string, holds no
    ['/'], is not composed only of period characters, does not start
    with the reserved prefix ["__"], and is not ["zarr.json"]. *)

val check : string -> unit
(** [check path] returns on the root, spelled ["/"] or [""], and on a
    path whose every name satisfies {!is_valid_name}.

    A trailing ['/'], a repeated one and a leading ["//"] each leave an
    empty name, so each is refused. So is a ["."] or [".."] component,
    as periods alone.

    @raise Error.E [(Metadata _)] naming the whole path and the one
    name that failed. *)
