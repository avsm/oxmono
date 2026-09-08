(** Room-scoped identity warnings. *)

type violation = Verification_violation | Pin_violation

type member = { user_id : Matrix_proto.Id.User_id.t; violation : violation }
(** A room member whose current cross-signing identity is in violation. *)

type t
(** The room identity warning projection. *)

val create :
  own_user:Matrix_proto.Id.User_id.t ->
  ?encryption:Matrix_eio.Encryption.t ->
  unit ->
  t
(** [create ~own_user ?encryption ()] builds an initially empty projection.
    Without [encryption], every room list remains empty. *)

val members : t -> Matrix_proto.Id.Room_id.t -> member Observable.List.t
(** [members t room] is the current non-self room members whose identity has a
    verification violation, sorted by user id. *)

val refresh : t -> Matrix_client.Base_client.state -> unit
(** [refresh t state] recomputes every room projection from [state] and the
    encryption identity statuses. *)

val user_id : member -> Matrix_proto.Id.User_id.t
(** [user_id member] is the member's user id. *)

val violation : member -> violation
