val ( / ) : string -> string -> string
(** Filesystem and process helpers for the single-domain runner. *)

val fail : ('a, unit, string, 'b) format4 -> 'a
val read : string -> string
val exists : string -> bool
val mkdir : string -> unit
val write : string -> string -> unit
val atomic_write : string -> string -> unit
val lines : string -> string list
val nul_lines : string -> string list
val sorted_dir : string -> string list
val hash : string -> string
val hash_file : string -> string
val hash_fields : string list -> string
val getenv : string -> string -> string
val home : unit -> string
val data_dir : unit -> string
val cache_dir : unit -> string
val opam_file : string -> 'a OpamFile.t
val read_opam : string -> OpamFile.OPAM.t
val write_opam : string -> OpamFile.OPAM.t -> unit
val replace_env : string array -> (string * string) list -> string array
val clean_env : unit -> string array

type proc = Eio_unix.Process.mgr_ty Eio.Resource.t

val capture : ?env:string array -> proc -> string list -> string
val command : ?env:string array -> proc -> string list -> unit
val git : proc -> string -> string list -> string
val log : ('a, unit, string, unit) format4 -> 'a
val tree_hash : string -> string
val remove_tree : string -> unit

val refresh_checkout : proc -> string -> unit
(** [refresh_checkout proc path] fetches and advances an owned, clean clone to
    its origin's default branch. Local changes cause failure. *)

val copy_files : src:string -> dst:string -> unit
