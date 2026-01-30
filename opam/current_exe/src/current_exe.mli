open! Core

(** Get the path to the current executable. By default, this is /proc/PID/exe, but some
    environments may choose to override it via one of the [Expert] functions. For example,
    it could be set to a wrapper script, instead. *)
val get_path : unit -> string

module Expert : sig
  val set_path_getter : (unit -> string) -> unit
  val set_path : string -> unit
end
