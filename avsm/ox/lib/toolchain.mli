type t = {
  prefix : string;
  fingerprint : string;
  packages : OpamPackage.t list;
  tools : string list;
}
(** Import an explicitly supplied OxCaml compiler into a day10 layer. *)

val inspect : Support.proc -> string -> t
(** [inspect proc prefix] verifies OxCaml and fingerprints compiler artifacts. *)

val write_repository : t -> guards:bool -> string -> unit
(** [write_repository compiler ~guards path] creates metadata for supplied
    components. Optional package guards are not compiler artifacts. *)

val install : Support.proc -> t -> prefix:string -> unit
(** [install proc compiler ~prefix] copies compiler tools and standard library.
    The original compiler prefix remains part of the cache identity. *)
