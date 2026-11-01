(** Resolve and locally evaluate one push-notification event.

    The shared event cache is consulted first. A [/context?limit=0] fetch is
    retained in a private table rather than spliced into the room timeline,
    because an isolated event says nothing about chunk adjacency or the room's
    history edges. When a later sync carries that event, the shared cached
    record wins and the private copy is discarded. *)

type decrypt =
  Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Event.Raw_event.t ->
  ( Matrix_proto.Event.Raw_event.t,
    Matrix_client.Encryption.decrypt_error )
  result
(** A dependency which decrypts one room event into a plaintext event with the
    same envelope identity. *)

val decrypt_with : Matrix_eio.Encryption.t -> decrypt
(** Adapt the ordinary Matrix Eio encryption machine into a {!decrypt}
    dependency. *)

type notification_event = {
  event : Event_cache.event;
  notification : Matrix_client.Push_evaluator.notification;
}

type status =
  | Event of notification_event
  | Event_filtered_out of Event_cache.event
      (** No local push rule notifies, or the sender is ignored. *)
  | Event_redacted of Event_cache.event
  | Event_not_found
  | Unable_to_decrypt of {
      event : Event_cache.event;
      error : Matrix_client.Encryption.decrypt_error option;
          (** [None] means no decryptor was configured. *)
    }

type t

val create :
  client:Matrix_client.Client.t ->
  cache:Event_cache.t ->
  state:(unit -> Matrix_client.Base_client.state) ->
  ?decrypt:decrypt ->
  unit ->
  t
(** [state] is read for every evaluation, so synced push rules, room members,
    the user's room display name, power levels and ignored users take effect
    without reconstructing this client. *)

val fetch :
  t ->
  Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Id.Event_id.t ->
  (status, Matrix_client.Error.t) result
(** Resolve and evaluate the event. Matrix [M_NOT_FOUND] is {!Event_not_found};
    other transport, policy, HTTP and decoding failures are preserved in the
    result. Repeated calls before sync reuse the private fetched record and make
    no extra request. *)
