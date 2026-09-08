(** Raising wrappers for room previews. *)

type t = Matrix_client.Room_preview.t

val get :
  Client.t ->
  store:Matrix_client.Store.t ->
  room_id_or_alias:Directory.room_id_or_alias ->
  ?via:string list ->
  unit ->
  t
