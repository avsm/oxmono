(** Interactive SAS verification for the profile's existing Matrix device. *)

val affirmative : string -> bool
(** [affirmative text] accepts only [y] or [yes], ignoring case and space. *)

val prompt : Matrix_eio.Verification_service.prompt -> string
(** [prompt p] displays the exact peer, device, seven emoji and three numbers.
*)

val run :
  env:Eio_unix.Stdenv.base ->
  client:Matrix_eio.Client.t ->
  encryption:Matrix_eio.Encryption.t ->
  ?private_identity:Matrix_client.Cross_signing.private_identity ->
  target:Matrix_proto.Id.User_id.t ->
  listen:bool ->
  ?room:Matrix_proto.Id.Room_id.t ->
  ask:(string -> bool) ->
  unit ->
  Matrix_eio.Verification_service.result
(** [run ~env ~client ~encryption ~target ~listen ~ask ()] requests SAS or
    accepts a request from [target] after asking the operator. [ask] must use a
    trusted channel and yield while waiting. [room] selects an in-room request
    or restricts listening to that room. The workflow times out after ten
    minutes and never changes Crow's access list. The caller persists crypto
    state, including on failure. *)
