(** to_device — send-to-device messaging.

    A send-to-device event is addressed to individual devices rather than to a
    room and never appears in a timeline. These events carry the Olm payloads
    that bootstrap end-to-end encryption. Only the sending half is here.
    Received events arrive in the [to_device] section of a [/sync] response.

    @see <https://spec.matrix.org/v1.11/client-server-api/#send-to-device-messaging>
      Send-to-Device messaging *)

(** Which of a user's devices a message is for. *)
type recipient =
  | All  (** Every device the user has, now and at delivery time. *)
  | Device of Matrix_proto.Id.Device_id.t  (** One named device. *)

type messages = (Matrix_proto.Id.User_id.t * (recipient * Jsont.json) list) list
(** The type for message bodies by user and then by recipient. A user whose list
    is empty is not sent. *)

val send :
  Client.t ->
  event_type:string ->
  txn_id:string ->
  messages ->
  (unit, Error.t) result
(** [send client ~event_type ~txn_id messages] delivers [messages] to their
    devices. [txn_id] makes the request idempotent, so retrying a failed send
    with the same identifier does not deliver the events twice.

    Uses [PUT /_matrix/client/v3/sendToDevice/{eventType}/{txnId}]. *)

val send_with_new_txn :
  Client.t -> event_type:string -> messages -> (unit, Error.t) result
(** [send_with_new_txn client ~event_type messages] is {!send} with a
    transaction identifier drawn from {!Client.random}. Use {!send} when the
    send may need retrying, so that the retry reuses the identifier.

    Uses [PUT /_matrix/client/v3/sendToDevice/{eventType}/{txnId}]. *)
