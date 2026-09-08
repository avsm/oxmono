(** to_device — send-to-device messaging, raising instead of returning a result.

    A to-device event goes straight to named devices rather than into a room,
    and is never persisted in a timeline. Every function here raises [Eio.Io]
    carrying an {!Error.type-err} where its {!Matrix_client.To_device}
    counterpart returns an error. *)

(** Which of a user's devices a message is for. It is
    {!Matrix_client.To_device.type-recipient}. *)
type recipient = Matrix_client.To_device.recipient =
  | All
  | Device of Matrix_proto.Id.Device_id.t

type messages = Matrix_client.To_device.messages
(** The type for message bodies by user and then by recipient. It is
    {!Matrix_client.To_device.messages}. *)

val send : Client.t -> event_type:string -> txn_id:string -> messages -> unit
(** [send c ~event_type ~txn_id messages] is {!Matrix_client.To_device.send}
    with the result unwrapped. *)

val send_with_new_txn : Client.t -> event_type:string -> messages -> unit
(** [send_with_new_txn c ~event_type messages] is
    {!Matrix_client.To_device.send_with_new_txn} with the result unwrapped. Use
    {!send} when the send may need retrying, so that the retry reuses the
    transaction identifier. *)
