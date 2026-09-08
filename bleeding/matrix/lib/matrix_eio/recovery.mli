(** recovery — account-data markers and plans for recovery, raising instead of
    returning a result.

    {!disable_account_data} applies only the ordered account-data cleanup plan.
    {!disable} also deletes the exact active server backup first. *)

type state = Matrix_client.Recovery.state =
  | Unknown
  | Enabled
  | Disabled
  | Incomplete

type marker = Matrix_client.Recovery.marker =
  | Absent
  | Valid of bool
  | Malformed of string

type markers = Matrix_client.Recovery.markers = {
  stable : marker;
  unstable : marker;
}

type account_data_write = Matrix_client.Recovery.account_data_write = {
  event_type : string;
  content : Jsont.json;
}

type inputs = Matrix_client.Recovery.inputs = {
  secret_storage_enabled : bool option;
  cross_signing_complete : bool option;
  backup_enabled : bool option;
  stable_marker : marker;
  unstable_marker : marker;
}

val key_backup_event_type : string
val backup_disabled_event_type : string
val key_backup_jsont : bool Jsont.t
val backup_disabled_jsont : bool Jsont.t
val key_backup_marker : Jsont.json option -> marker
val backup_disabled_marker : Jsont.json option -> marker
val state : inputs -> state

val check_state :
  Client.t ->
  encryption:Encryption.t ->
  private_identity:Matrix_client.Cross_signing.private_identity ->
  state
(** Probes default SSSS, cross-signing, backup, and marker state in Rust's
    short-circuit order, raising on transport/server errors. *)

val known_secret_event_types : string list
(** The recovery secret event types cleared when disabling recovery. *)

val mark_enabled_writes : unit -> account_data_write list
val disable_writes : ?default_key_id:string -> unit -> account_data_write list

val fetch_markers : Client.t -> markers
(** Fetches both markers in stable-then-unstable order. *)

val apply_writes : Client.t -> account_data_write list -> unit
(** Applies writes in order and raises on the first failure. *)

val mark_backup_enabled : Client.t -> unit
(** Writes the enabled markers in Rust's order. *)

val disable_account_data : ?default_key_id:string -> Client.t -> unit
(** Applies account-data cleanup only. The caller must disable/delete the active
    backup first. *)

val disable : Client.t -> encryption:Encryption.t -> unit
(** Deletes and locally disables the exact active backup, persists the
    encryption state, then clears the known recovery account data. Raises on the
    first non-idempotent failure. *)

val disable_and_delete_backups : Client.t -> encryption:Encryption.t -> unit
(** Aggressively GETs and deletes every server backup version, including when no
    backup was enabled locally, then disables and persists local backup state.
    Remote GET/DELETE failures are raised before local state changes; a final
    persistence failure is raised after the in-memory disable. *)

type recovered = Matrix_client.Recovery.recovered = {
  store : Matrix_client.Secrets.store;
  private_identity : Matrix_client.Cross_signing.private_identity;
}
(** The validated SSSS store and private cross-signing identity returned by
    {!recover}. *)

val recover :
  Client.t -> encryption:Encryption.t -> credential:string -> recovered
(** Opens SSSS, imports cross-signing, persists the driver, and then validates
    the optional current room-key backup. It enables backup metadata only; it
    does not download room keys. Raises after a failed step. *)

val recover_and_fix_backup :
  Client.t -> encryption:Encryption.t -> credential:string -> recovered
(** Recovers the identity and repairs an unreadable or inconsistent backup by
    replacing server backups and exporting into the opened SSSS store. An absent
    secret or server backup is an unconfigured no-op. *)

type backup_upload = Matrix_client.Recovery.backup_upload =
  | Not_waited
  | Uploaded of int
  | Upload_failed of Matrix_client.Error.t

type enabled = Matrix_client.Recovery.enabled = {
  store : Matrix_client.Secrets.store;
  key_id : string;
  recovery_key : string;
  backup_version : string;
  backup_upload : backup_upload;
}
(** The created recovery store and key information returned by {!enable}. *)

val enable :
  Client.t ->
  encryption:Encryption.t ->
  private_identity:Matrix_client.Cross_signing.private_identity ->
  ?passphrase:string ->
  ?wait_for_backups_to_upload:bool ->
  unit ->
  enabled
(** Enables recovery, raising after a partial account-data, backup, or store
    write. It does not roll back successful earlier writes. *)

val reset_key :
  Client.t ->
  encryption:Encryption.t ->
  private_identity:Matrix_client.Cross_signing.private_identity ->
  ?passphrase:string ->
  unit ->
  Matrix_client.Secrets.created_store
(** Creates a fresh recovery store and exports the supplied identity and any
    local backup key. It does not change backup state or recovery markers. *)

type recovered_and_reset = Matrix_client.Recovery.recovered_and_reset = {
  store : Matrix_client.Secrets.store;
  key_id : string;
  recovery_key : string;
  private_identity : Matrix_client.Cross_signing.private_identity;
}
(** The new recovery store/key and identity recovered before the reset. *)

val recover_and_reset :
  Client.t ->
  encryption:Encryption.t ->
  credential:string ->
  ?passphrase:string ->
  unit ->
  recovered_and_reset
(** Recovers using the old store, then publishes a new default recovery store.
    Earlier successful writes are not rolled back after a later failure. *)

(** A raising Eio facade over {!Matrix_client.Recovery.Manager}. Pure state and
    subscription operations do not perform I/O. The remaining operations unwrap
    client errors and can additionally raise [Eio.Io] or cancellation from the
    transport or persistent encryption store. *)
module Manager : sig
  type t = Matrix_client.Recovery.Manager.t
  type subscription = Matrix_client.Recovery.Manager.subscription

  val create :
    ?private_identity:Matrix_client.Cross_signing.private_identity ->
    ?base:Matrix_client.Base_client.state ->
    Client.t ->
    encryption:Encryption.t ->
    t

  val base_state : t -> Matrix_client.Base_client.state

  val private_identity :
    t -> Matrix_client.Cross_signing.private_identity option

  val set_private_identity :
    t -> Matrix_client.Cross_signing.private_identity option -> state

  val state : t -> state
  val refresh_from_base : t -> Matrix_client.Base_client.state -> state
  val subscribe : t -> (state -> unit) -> subscription
  val unsubscribe : t -> subscription -> unit
  val watch : t -> (state -> unit) -> unit -> unit
  val refresh : t -> state
  val disable : t -> unit
  val disable_and_delete_backups : t -> unit
  val recover : t -> credential:string -> recovered
  val recover_and_fix_backup : t -> credential:string -> recovered

  val enable :
    t ->
    ?private_identity:Matrix_client.Cross_signing.private_identity ->
    ?passphrase:string ->
    ?wait_for_backups_to_upload:bool ->
    unit ->
    enabled

  val reset_key :
    t ->
    ?private_identity:Matrix_client.Cross_signing.private_identity ->
    ?passphrase:string ->
    unit ->
    Matrix_client.Secrets.created_store

  val recover_and_reset :
    t -> credential:string -> ?passphrase:string -> unit -> recovered_and_reset

  val reset_identity :
    t ->
    auth_callback:
      (Matrix_client.Uiaa.uiaa_response -> Matrix_client.Uiaa.auth_data option) ->
    unit ->
    Matrix_client.Cross_signing.private_identity Matrix_client.Uiaa.uiaa_result
  (** [reset_identity] deletes remote backups and uploads a replacement
      cross-signing identity. [Uiaa_auth_required] is returned when the callback
      declines a stage or one retry is insufficient; transport and persistence
      failures raise [Eio.Io] with [Error.E], following the other manager
      mutations. Repeating the call retries the retained upload body after
      external approval; use {!cancel_pending_identity_reset} to abandon it. The
      callback can answer an [m.oauth] challenge with
      {!Matrix_client.Uiaa.oauth_auth}. *)

  val cancel_pending_identity_reset : t -> unit
  (** Discard a generated identity retained for a UIAA continuation. If a
      replacement was already installed while backup recreation failed, this
      does not revert it; it only suppresses further retry. *)
end
