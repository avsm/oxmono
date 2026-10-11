(** Private transport for the profile's admin onboarding messages. *)

type t
(** A transport bound to one Matrix account and Crow profile. *)

val create :
  client:Matrix_eio.Client.t -> ?encryption:Matrix_eio.Encryption.t ->
  store:Store.t -> state:(unit -> Matrix_client.Base_client.state option) ->
  unit -> t
(** [create ~client ~store ~state ()] creates a transport. [encryption] must
    belong to [client]. [state] supplies the running sync's room projection. *)

val room : t -> Matrix_proto.Id.Room_id.t
(** [room t] reuses an eligible admin DM or creates an invite-only DM. Current
    membership is fetched before reuse and must contain exactly Crow and the
    admin, including invitations. Existing rooms must match the transport's
    encryption setting. New rooms use that setting. The room is remembered in
    Crow's store and marked as a DM in remote account data when possible.
    Matrix errors propagate. The caller must bound network operations. *)

val send : t -> string -> unit
(** [send t text] sends an admin notice after checking {!room}'s membership.
    Notices are encrypted when [create] received an encryption machine.
    Delivery failures propagate. Logs contain room and event IDs only. *)
