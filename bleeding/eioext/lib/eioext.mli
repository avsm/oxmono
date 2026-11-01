(** Unix filesystem and subprocess extensions for Eio. *)

module Path = Path
module Process = Process

module Native = Native
(** Synchronous compatibility for stores that perform their own worker-thread
    dispatch. Prefer [Path] in Eio applications. *)
