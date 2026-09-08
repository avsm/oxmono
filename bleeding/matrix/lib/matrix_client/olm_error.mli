(** olm_error — what can go wrong in the Olm and Megolm ratchets.

    Every failure the ratchets report is one of these. None of them is
    actionable by retrying the same input. A caller that has to show a person
    something shows {!pp}. *)

type t =
  | Bad_base64 of string  (** The named value is not base64. *)
  | Bad_key of string  (** The named value is not a key of its kind. *)
  | Bad_message_format of string  (** What the encoding got wrong. *)
  | Bad_message_version of int  (** The version byte the message carried. *)
  | Bad_mac
  | Bad_signature
  | Bad_padding
  | Key_exchange_failed of string
  | Identity_key_mismatch
      (** The message names a sender other than the expected one. *)
  | Session_id_mismatch
      (** The session identifier does not match the key it is claimed for. *)
  | Unknown_one_time_key
      (** No one-time key of this account matches the pre-key message. *)
  | Unknown_message_index of { index : int; first_known : int }
      (** The session has been ratcheted past [index]. *)
  | Message_gap_too_large of { gap : int; max : int }
      (** Accepting the message would mean deriving [gap] message keys. *)
  | No_message_key of int  (** No key was kept for this chain index. *)
  | No_session  (** No session for this peer would decrypt the message. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints a one-line description of [t] on [ppf]. *)
