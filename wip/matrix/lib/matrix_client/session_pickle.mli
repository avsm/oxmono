(** session_pickle — Olm and Megolm ratchet state as a string.

    {!Session} stores ratchet state as an opaque string and these are the only
    functions that make or read one. {!Crypto_store} goes through them. Key
    material and raw ratchet bytes travel as unpadded base64, since JSON strings
    hold text rather than octets.

    A pickle carries private keys in the clear. It is only as safe as the file
    it is written to. *)

type error = [ `Msg of string ]
(** The type for pickling failures. The message names what would not encode or
    decode. *)

val pickle_account : Olm.Account.t -> (string, error) result
(** [pickle_account a] is [a]'s identity keys and one-time key pool as a string.
*)

val unpickle_account : string -> (Olm.Account.t, error) result
(** [unpickle_account s] is the account [pickle_account] wrote as [s]. *)

val pickle_session : Olm.Session.t -> (string, error) result
(** [pickle_session s] is the Olm ratchet [s] as a string. *)

val unpickle_session : string -> (Olm.Session.t, error) result
(** [unpickle_session s] is the session [pickle_session] wrote as [s]. *)

val pickle_megolm_inbound : Olm.Megolm.Inbound.t -> (string, error) result
(** [pickle_megolm_inbound s] is the inbound Megolm session [s] as a string. *)

val unpickle_megolm_inbound : string -> (Olm.Megolm.Inbound.t, error) result
(** [unpickle_megolm_inbound s] is the session [pickle_megolm_inbound] wrote as
    [s]. *)

val pickle_megolm_outbound : Olm.Megolm.Outbound.t -> (string, error) result
(** [pickle_megolm_outbound s] is the outbound Megolm session [s] as a string.
*)

val unpickle_megolm_outbound : string -> (Olm.Megolm.Outbound.t, error) result
(** [unpickle_megolm_outbound s] is the session [pickle_megolm_outbound] wrote
    as [s]. *)
