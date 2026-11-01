(** Verified sources and immutable prepared trees. *)
val safe_relative : string -> string
(** [safe_relative path] rejects absolute paths and parent traversal. *)

val prepare :
  refresh:bool ->
  Support.proc ->
  D10.Config.t ->
  Solve.package ->
  string * string
(** [prepare ~refresh proc cache package] fetches sources, checks declared
    checksums, and copies repository files and extra sources. It returns a
    cached source directory and its content hash. Callers must copy it before
    building. [refresh] fetches mutable references again. *)
