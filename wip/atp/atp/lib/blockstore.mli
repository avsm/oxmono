(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Content-addressed block storage with Eio capabilities.

    Blockstores provide read and write access to content-addressed blocks. Each
    block is identified by its CID (Content Identifier). *)

(** {1 Errors} *)

type error = [ `Block_not_found of Cid.t | `Block_io_error of string ]
(** Blockstore errors. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

type Eio.Exn.err +=
  | E of error  (** Eio exception wrapper for blockstore errors. *)

(** {1 Capabilities} *)

class type readable = object
  method get : Cid.t -> string option
  (** [get cid] returns block data for [cid], or [None] if not found. *)

  method get_exn : Cid.t -> string
  (** [get_exn cid] returns block data for [cid].
      @raise Eio.Io if not found. *)

  method has : Cid.t -> bool
  (** [has cid] is [true] if block exists. *)

  method get_many : Cid.t list -> Block_map.with_missing
  (** [get_many cids] returns found blocks and list of missing CIDs. *)
end

class type writable = object
  inherit readable

  method put : Cid.t -> string -> unit
  (** [put cid data] stores block. Overwrites if exists. *)

  method put_many : Block_map.t -> unit
  (** [put_many blocks] stores multiple blocks. *)

  method delete : Cid.t -> unit
  (** [delete cid] removes block if present. No-op if missing. *)

  method delete_many : Cid.t list -> unit
  (** [delete_many cids] removes blocks if present. *)

  method sync : unit
  (** [sync] flushes pending writes to storage. *)
end

(** {1 Implementations} *)

val memory : ?init:Block_map.t -> unit -> writable
(** [memory ?init ()] creates an in-memory blockstore.

    Fast but not persistent. Useful for testing.

    @param init Initial contents. *)

val filesystem : _ Eio.Path.t -> writable
(** [filesystem dir] creates a file-backed blockstore.

    Each block is stored as a separate file. Directory structure:
    {v
      <dir>/
        <prefix>/       (first 2 chars of base32 CID)
          <cid>.block   (full CID as filename)
    v}

    Thread-safe via Eio's file operations. *)

val overlay : top:#readable -> bottom:#readable -> readable
(** [overlay ~top ~bottom] creates a read-only overlay.

    Reads from [top] first, falls back to [bottom] if not found. *)

val cached : ?capacity:int -> #writable -> writable
(** [cached ?capacity store] wraps [store] with LRU read cache.

    @param capacity Maximum cached blocks (default: 1000). *)

val read_only : #readable -> readable
(** [read_only store] returns a read-only view. *)
