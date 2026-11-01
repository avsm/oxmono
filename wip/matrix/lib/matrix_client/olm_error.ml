type t =
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

let pp ppf = function
  | Bad_base64 what -> Format.fprintf ppf "%s is not base64" what
  | Bad_key what -> Format.fprintf ppf "%s is not a valid key" what
  | Bad_message_format what -> Format.fprintf ppf "malformed message: %s" what
  | Bad_message_version v ->
      Format.fprintf ppf "unsupported message version %d" v
  | Bad_mac -> Format.pp_print_string ppf "the message MAC does not match"
  | Bad_signature -> Format.pp_print_string ppf "the signature does not verify"
  | Bad_padding -> Format.pp_print_string ppf "the plaintext padding is invalid"
  | Key_exchange_failed m -> Format.fprintf ppf "key exchange failed: %s" m
  | Identity_key_mismatch ->
      Format.pp_print_string ppf
        "the message names a different sender identity key"
  | Session_id_mismatch ->
      Format.pp_print_string ppf "the session id does not match the session key"
  | Unknown_one_time_key ->
      Format.pp_print_string ppf
        "no one-time key of this account matches the message"
  | Unknown_message_index { index; first_known } ->
      Format.fprintf ppf
        "the session has been ratcheted past message index %d, first known %d"
        index first_known
  | Message_gap_too_large { gap; max } ->
      Format.fprintf ppf "the message is %d ahead of the chain, at most %d" gap
        max
  | No_message_key index ->
      Format.fprintf ppf "no message key was kept for chain index %d" index
  | No_session ->
      Format.pp_print_string ppf "no session for this sender decrypts it"
