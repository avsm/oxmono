(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Nodes of unknown kind.

    A [zarr.json] says whether the node it describes is an array or a
    group. Use this module when a path may be either. *)

type t = [ `Array of Arr.t | `Group of Group.t ]
(** The type for open nodes. *)

val open_ : ?codecs:Codec.resolver -> Store.t -> path:string -> t
(** [open_ store ~path] reads [<path>/zarr.json] once, dispatches on its
    [node_type] member and opens the node with {!Arr.of_json} or
    {!Group.of_json}. [codecs] is passed on to {!Arr.of_json}.

    @raise Error.E [(Store _)] when there is no metadata document at
    [path], with a message naming the key as not found. [(Metadata _)]
    when the document is not a JSON object, has no [node_type] member,
    has one that is not a string, has one that is neither ["array"] nor
    ["group"], or fails the checks of the module that opens it.
    [(Codec _)] as {!Arr.open_} does. *)
