(** The legacy libolm account pickle used by dehydrated devices.

    This is deliberately only the account-pickle codec. It does not pickle Olm
    sessions and it does not implement the dehydrated-device manager. *)

type error = [ `Msg of string ]

type decoded = {
  account : Olm.Account.t;
  published_one_time_keys : Crypto_key.Key_id.t list;
  published_fallback_keys : Crypto_key.Key_id.t list;
}

val pickle :
  ?is_published:(Crypto_key.Key_id.t -> bool) ->
  device_id:Matrix_proto.Id.Device_id.t ->
  pickle_key:string ->
  Olm.Account.t ->
  (string, error) result
(** [pickle ~device_id ~pickle_key account] encrypts the account in the
    vodozemac/libolm account-pickle format. [pickle_key] is the raw 32-byte
    dehydrated-device key. [is_published] supplies the publication state that
    [Olm.Account] intentionally does not retain; it defaults to [false]. *)

val unpickle :
  device_id:Matrix_proto.Id.Device_id.t ->
  pickle_key:string ->
  string ->
  (decoded, error) result
(** [unpickle ~device_id ~pickle_key value] authenticates and restores an
    account, also returning the one-time and fallback IDs whose records were
    marked published. The device ID and raw key are both required because the
    dehydrated-device key is derived from the device ID. *)
