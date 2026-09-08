(** Eio raising wrapper for {!Matrix_client.Thread_paginator}. *)

type state = Matrix_client.Thread_paginator.state =
  | Start
  | Loading
  | Next of string
  | End
  | Failed of Matrix_client.Error.t

type t = Matrix_client.Thread_paginator.t

val create : client:Client.t -> room_id:Matrix_proto.Id.Room_id.t -> unit -> t
val set_filter : t -> Matrix_client.Relations.thread_filter -> unit
val reset : t -> unit
val state : t -> state
val roots : t -> Matrix_proto.Event.Raw_event.t list
val loaded_pages : t -> int
val is_at_last_page : t -> bool
val subscribe : t -> (state -> unit) -> unit -> unit
val next_page : t -> ?limit:int -> unit -> unit
