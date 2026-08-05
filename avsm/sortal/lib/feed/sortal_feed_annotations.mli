(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Per-entry annotations for feed entries.

    Stores custom metadata (key-value pairs) for feed entries that
    persists across feed syncs. Annotations are keyed by entry URL
    and stored in a JSON file alongside the feed data. *)

type entry_annotation = { slugs : string list }
(** Annotation data for a single feed entry. *)

type t = (string, entry_annotation) Hashtbl.t
(** Annotations keyed by entry URL. *)

val empty : unit -> t
(** [empty ()] returns an empty annotations table. *)

val add_slug : t -> url:string -> slug:string -> unit
(** [add_slug t ~url ~slug] adds [slug] to the annotation for [url].
    Does nothing if [slug] is already present. *)

val slugs_for_url : t -> string -> string list
(** [slugs_for_url t url] returns the slugs annotated for [url],
    or the empty list if none. *)

val load : Eio.Fs.dir_ty Eio.Path.t -> t
(** [load path] reads annotations from [path]. Returns [empty ()]
    if the file does not exist or cannot be parsed. *)

val save : Eio.Fs.dir_ty Eio.Path.t -> t -> unit
(** [save path t] writes annotations to [path] as JSON. *)
