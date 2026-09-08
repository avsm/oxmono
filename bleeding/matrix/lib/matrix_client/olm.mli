(** olm — the Olm double ratchet for to-device messages and Megolm for room
    messages.

    Both wire formats are version [0x03] and are byte-compatible with libolm and
    vodozemac. Everything needing unpredictable bytes takes an explicit
    {!Random.t}, so no function here reads a global generator.

    The event bodies these ratchets are carried in are
    {!Matrix_proto.Event.Encrypted}, {!Matrix_proto.Event.Olm_plaintext},
    {!Matrix_proto.Event.Room_key_content} and
    {!Matrix_proto.Event.Forwarded_room_key_content}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#end-to-end-encryption>
      End-to-end encryption *)

type error = Olm_error.t =
  | Bad_base64 of string
  | Bad_key of string
  | Bad_message_format of string
  | Bad_message_version of int
  | Bad_mac
  | Bad_signature
  | Bad_padding
  | Key_exchange_failed of string
  | Identity_key_mismatch
  | Session_id_mismatch
  | Unknown_one_time_key
  | Unknown_message_index of { index : int; first_known : int }
  | Message_gap_too_large of { gap : int; max : int }
  | No_message_key of int
  | No_session
      (** What every operation here reports on failure. See {!Olm_error.t} for
          what each one means. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints a one-line description of [e] on [ppf]. *)

module Account = Olm_account
(** A device's long-lived Olm keys.

    @canonical Matrix_client.Olm.Account *)

module Session = Olm_session
(** One double ratchet with one other device.

    @canonical Matrix_client.Olm.Session *)

module Megolm = Megolm
(** The forward-only hash ratchet that encrypts room messages.

    @canonical Matrix_client.Olm.Megolm *)

module Machine = Olm_machine
(** One device's account and its sessions with other devices.

    @canonical Matrix_client.Olm.Machine *)
