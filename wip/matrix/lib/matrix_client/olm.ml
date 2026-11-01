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

let pp_error = Olm_error.pp

module Account = Olm_account
module Session = Olm_session
module Megolm = Megolm
module Machine = Olm_machine
