(** verification — deciding that a device belongs to the person it claims to.

    A verification flow runs over to-device events or inside a room. Both forms
    share the framework below, which is how a flow is addressed, the events it
    is made of and the codes that end it. {!Sas} and {!Qr} are the two
    comparison methods, and {!Flow} runs several verifications at once and
    routes incoming events to them.

    Everything here is pure. A transition takes a state and an incoming event
    and returns the next state with the events to send, and the clock and the
    randomness are always explicit arguments. {!Matrix_eio.Verification} puts
    the results on the wire. The key hierarchy a completed verification feeds is
    {!Cross_signing}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#device-verification>
      Device verification *)

include module type of struct
  include Verification_base
end

module Sas = Verification_sas
(** The Short Authentication String protocol.

    @canonical Matrix_client.Verification.Sas *)

module Qr = Verification_qr
(** The QR code payload and the reciprocation handshake.

    @canonical Matrix_client.Verification.Qr *)

module Flow = Verification_flow
(** A table of in-flight verifications.

    @canonical Matrix_client.Verification.Flow *)
