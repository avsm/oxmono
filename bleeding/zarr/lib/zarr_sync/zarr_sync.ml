(** Zarr synchronous store implementations

    This module provides synchronous (blocking) store implementations
    for Zarr, including in-memory and filesystem stores.
*)

(** In-memory store *)
module Memory_store = Memory_store

(** Filesystem store using Unix I/O *)
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
