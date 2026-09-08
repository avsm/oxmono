(** verification — the device verification state machines, plus putting their
    events on the wire.

    The machines in {!Matrix_client.Verification} are pure. They are re-exported
    here, with the one piece that does I/O added, which raises [Eio.Io] carrying
    an {!Error.type-err} rather than returning an error. {!Verification_service}
    is the driver that runs a flow to completion against the sync loop.

    @see <https://spec.matrix.org/v1.11/client-server-api/#device-verification>
      Device verification *)

(** {1 The state machines} *)

module Cancel_code = Matrix_client.Verification.Cancel_code
(** The codes a flow can end with. *)

module Method = Matrix_client.Verification.Method
(** The verification methods a flow can offer. *)

module Transaction = Matrix_client.Verification.Transaction
(** How a flow is addressed. *)

module Message = Matrix_client.Verification.Message
(** A verification event, addressed to a transaction. *)

module Sas = Matrix_client.Verification.Sas
(** The Short Authentication String protocol. *)

module Qr = Matrix_client.Verification.Qr
(** The QR code payload and the reciprocation handshake. *)

module Flow = Matrix_client.Verification.Flow
(** A table of in-flight verifications. *)

module Cross_signing = Matrix_client.Cross_signing
(** The key hierarchy a completed verification feeds. *)

(** {1 Sending} *)

val send_to_devices :
  Client.t ->
  their_user_id:Matrix_proto.Id.User_id.t ->
  devices:To_device.recipient list ->
  Message.t ->
  unit
(** [send_to_devices c ~their_user_id ~devices msg] sends [msg] to each of
    [their_user_id]'s [devices] as its own event type, under a transaction
    identifier drawn from the client's randomness. A request is fanned out over
    every device of a user until one answers it, which is
    [[Matrix_client.To_device.All]]. A [msg] that will not encode, and a failed
    send, raise [Eio.Io]. *)
