(** Zarr Eio-based async store implementations

    This module provides effect-based async store implementations
    for Zarr using Eio, including in-memory and filesystem stores.
*)

(** In-memory store with Eio mutex for thread safety *)
module Memory_store = Memory_store

(** Filesystem store using Eio for async I/O *)
module Filesystem_store = Filesystem_store

(** Array operations for memory store *)
module Memory_array = Zarr.Array.Make(Memory_store)

(** Array operations for filesystem store *)
module Filesystem_array = Zarr.Array.Make(Filesystem_store)

(** Group operations for memory store *)
module Memory_group = Zarr.Group.Make(struct
  include Memory_store
  let list_dir = Memory_store.list_dir
end)

(** Group operations for filesystem store *)
module Filesystem_group = Zarr.Group.Make(struct
  include Filesystem_store
  let list_dir = Filesystem_store.list_dir
end)

(** Hierarchy operations for memory store *)
module Memory_hierarchy = Zarr.Group.Hierarchy.Make(struct
  include Memory_store
  let list_prefix = Memory_store.list_prefix
  let erase_prefix = Memory_store.erase_prefix
end)

(** Hierarchy operations for filesystem store *)
module Filesystem_hierarchy = Zarr.Group.Hierarchy.Make(struct
  include Filesystem_store
  let list_prefix = Filesystem_store.list_prefix
  let erase_prefix = Filesystem_store.erase_prefix
end)

(** Run Zarr operations within an Eio environment *)
let run f =
  Eio_main.run @@ fun env ->
  f ~fs:(Eio.Stdenv.fs env)
