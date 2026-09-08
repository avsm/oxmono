(** Durable unable-to-decrypt (UTD) notifications. *)

type report = {
  event_id : Matrix_proto.Id.Event_id.t;
  cause : Matrix_client.Encryption.utd_cause;
  time_to_decrypt : float option;
      (** Seconds from the first UTD observation, for a late decryption. *)
  event_age : int64 option;
      (** The server-reported event age, when the event carried [unsigned.age].
      *)
  event_local_age : int64 option;
      (** Event timestamp relative to local device creation, when known. *)
  event_local_age_millis : int64 option;
      (** Alias using the Rust SDK's millisecond naming. *)
  user_trusts_own_identity : bool;
  sender : Matrix_proto.Id.User_id.t;
  sender_homeserver : string;
  own_homeserver : string option;
}
(** A report delivered to the callback passed to {!create}. *)

type t
(** A UTD hook manager. *)

val create :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  ?store:Matrix_client.Store.t ->
  ?max_delay:float ->
  ?own_homeserver:string ->
  ?device_created_at:Ptime.t ->
  on_utd:(report -> unit) ->
  unit ->
  t
(** [create] makes a manager. [max_delay], when supplied, holds a first UTD
    notification pending for that many seconds; a late decryption reports it
    immediately with [time_to_decrypt]. Without a delay, reports are immediate.

    The reported event-id ring is restored from [store] and persisted and
    flushed after each report. Persistence failures are logged and do not stop
    delivery to [on_utd]. A memory-only manager works with no store.
    [device_created_at] enables [event_local_age]; if absent that field is
    [None]. *)

val on_utd :
  t ->
  event:Matrix_proto.Event.Raw_event.t ->
  cause:Matrix_client.Encryption.utd_cause ->
  ?event_local_age:int64 ->
  user_trusts_own_identity:bool ->
  unit ->
  unit
(** [on_utd] observes an event which could not be decrypted. Duplicate calls for
    one event-id collapse, including calls while a delayed report is pending.
    Events without an id are ignored. *)

val on_late_decrypt : t -> Matrix_proto.Id.Event_id.t -> unit
(** [on_late_decrypt t id] reports a pending event as late-decrypted. It is a
    no-op for an unknown event or one already reported definitely. *)
