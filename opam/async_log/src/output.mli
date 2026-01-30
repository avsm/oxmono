open! Core
open! Async_kernel
open! Import
include module type of Async_log_kernel.Output

(** [stdout] defaults to [format=`Text] *)
val stdout : ?format:Format.t -> unit -> t

(** [stderr] defaults to [format=`Text] *)
val stderr : ?format:Format.t -> unit -> t

(** The writer output type takes no responsibility over the Writer.t it is given. In
    particular it makes no attempt to ever close it. *)
val writer : Format.t -> Writer.t -> t

(** The [perm] argument is passed through to [Writer.open_file], and so has the default
    behavior described there. *)
val file : ?perm:Unix.file_perm -> Format.t -> filename:string -> t

val rotating_file
  :  ?perm:Unix.file_perm
  -> ?time_source:Synchronous_time_source.t
  -> ?log_on_rotation:(unit -> Message.t list)
  -> Format.t
  -> basename:string
  -> ?suffix:string (** defaults to [".log"] *)
  -> Rotation.t
  -> t

(** Returns a tail of the filenames. When [rotate] is called, the previous filename is put
    on the tail *)
val rotating_file_with_tail
  :  ?perm:Unix.file_perm
  -> ?time_source:Synchronous_time_source.t
  -> ?log_on_rotation:(unit -> Message.t list)
  -> Format.t
  -> basename:string
  -> ?suffix:string (** defaults to [".log"] *)
  -> Rotation.t
  -> t * string Tail.t

(** See {!Log_extended} for syslog and colorized console output. *)
