(** Recovery account-data state, marker writes, and backup lifecycle.

    Marker decoding and write plans are pure. {!disable_account_data} applies
    only account-data writes; {!disable} additionally deletes the exact active
    server backup before applying the cleanup plan. *)

type state =
  | Unknown
  | Enabled
  | Disabled
  | Incomplete  (** The high-level recovery state. *)

type marker =
  | Absent
  | Valid of bool
  | Malformed of string
      (** The result of inspecting one recovery account-data marker. *)

type markers = { stable : marker; unstable : marker }
(** The two recovery account-data markers, fetched independently. *)

val key_backup_event_type : string
(** The stable recovery marker, [m.key_backup]. *)

val backup_disabled_event_type : string
(** The unstable recovery marker, [m.org.matrix.custom.backup_disabled]. *)

val key_backup_jsont : bool Jsont.t
(** Codec for [m.key_backup], whose content is [{"enabled": bool}]. *)

val backup_disabled_jsont : bool Jsont.t
(** Codec for the unstable marker, whose content is [{"disabled": bool}]. *)

val key_backup_marker : Jsont.json option -> marker
(** Decode an optional stable marker. Absent and malformed data are explicit. *)

val backup_disabled_marker : Jsont.json option -> marker
(** Decode an optional unstable marker. Absent and malformed data are explicit.
*)

type inputs = {
  secret_storage_enabled : bool option;
  cross_signing_complete : bool option;
  backup_enabled : bool option;
  stable_marker : marker;
  unstable_marker : marker;
}

val state : inputs -> state
(** [state inputs] derives recovery state. A valid stable marker controls the
    disabled fallback; the unstable marker is consulted only when the stable
    marker is absent. [cross_signing_complete] means that all three private
    cross-signing keys are locally available. Recovery is enabled when those
    keys exist and either key backup is locally enabled or account data
    explicitly disables it. [None] represents an observation not made yet. *)

val check_state :
  Client.t ->
  encryption:Encryption_driver.t ->
  private_identity:Cross_signing.private_identity ->
  (state, Error.t) result
(** [check_state client ~encryption ~private_identity] probes recovery in Rust's
    order. It validates the three users before I/O, fetches the default SSSS key
    first, treats an absent or malformed default event as [Disabled], and stops
    at [Incomplete] when any private cross-signing key is absent. A complete
    identity with an active backup is [Enabled] without marker requests;
    otherwise stable then unstable markers determine whether the backup was
    explicitly disabled. Other transport/server errors propagate. *)

type account_data_write = { event_type : string; content : Jsont.json }
(** One targeted global account-data replacement. *)

val known_secret_event_types : string list
(** The four recovery secrets Rust clears when disabling recovery. *)

val mark_enabled_writes : unit -> account_data_write list
(** Writes, in Rust order, the stable enabled marker and unstable false marker.
*)

val disable_writes : ?default_key_id:string -> unit -> account_data_write list
(** Writes, in Rust order, empty known recovery metadata only. When
    [default_key_id] is present its [m.secret_storage.key.<id>] description is
    cleared first, followed by the default-key event, stable and unstable backup
    markers, and the four known secret events. Secret events use the valid empty
    shape [{"encrypted":{}}]. No unrelated account data is targeted. The caller
    must disable/delete the active backup before applying this plan, as Rust
    does. *)

val fetch_markers : Client.t -> (markers, Error.t) result
(** Fetch both recovery markers in stable-then-unstable order.

    A Matrix [M_NOT_FOUND] response or a bare HTTP [404] is [Absent]. Other
    transport/server errors are returned. A successful but malformed body is
    retained as [Malformed], rather than being returned as a transport error. *)

val apply_writes : Client.t -> account_data_write list -> (unit, Error.t) result
(** Apply account-data replacements strictly in list order, stopping at the
    first error. *)

val mark_backup_enabled : Client.t -> (unit, Error.t) result
(** Apply [mark_enabled_writes]. *)

val disable_account_data :
  ?default_key_id:string -> Client.t -> (unit, Error.t) result
(** Apply [disable_writes]. Despite its deliberately narrow name, this only
    changes recovery account data; the caller must disable/delete any active
    backup before invoking it. *)

val disable :
  Client.t -> encryption:Encryption_driver.t -> (unit, Error.t) result
(** [disable client ~encryption] follows the pinned Rust recovery order: delete
    the exact locally active server backup, disable it in the encryption
    machine, persist that machine, best-effort read the current SSSS key id,
    then apply {!disable_writes}. A missing server version is accepted when the
    local machine still names it; any other delete error leaves the local backup
    enabled. With no locally active backup this returns {!Error.Policy_denied}
    without making a request.

    As in Rust, failures after the server deletion are not rolled back. *)

val disable_and_delete_backups :
  Client.t -> encryption:Encryption_driver.t -> (unit, Error.t) result
(** [disable_and_delete_backups client ~encryption] is the aggressive Rust
    [Backups::disable_and_delete] operation. It first validates that the
    authenticated session and encryption machine belong to the same user, before
    making any request. It then repeatedly GETs the current server backup
    version and DELETEs exactly that version until the GET reports [M_NOT_FOUND]
    or bare HTTP [404]. A [404]/[M_NOT_FOUND] from DELETE is treated as already
    deleted and followed by another GET.

    Local backup state is not changed while a GET or DELETE can still fail. Once
    the server reports exhaustion, local backup state is disabled and the
    encryption driver is persisted, even when no local backup version was
    enabled. If that final save fails, the error is returned after the in-memory
    disable and successful remote deletions; no rollback is attempted.
    Account-data markers and secrets are not changed by this operation. *)

type recovered = {
  store : Secrets.store;
  private_identity : Cross_signing.private_identity;
}
(** The validated SSSS store and private cross-signing identity recovered by
    {!recover}. *)

val recover :
  Client.t ->
  encryption:Encryption_driver.t ->
  credential:string ->
  (recovered, Error.t) result
(** [recover client ~encryption ~credential] follows the Rust recovery order:
    open the default SSSS store, import and validate the three private
    cross-signing seeds through the driver's machine, persist the driver, then
    validate and enable the optional current room-key backup. The driver is
    already persisted if backup import later fails. Successful recovery enables
    backup metadata only; it does not download room keys. *)

val recover_and_fix_backup :
  Client.t ->
  encryption:Encryption_driver.t ->
  credential:string ->
  (recovered, Error.t) result
(** [recover_and_fix_backup] recovers the cross-signing identity into the
    supplied machine, then imports the optional backup. When an encrypted
    backup-secret copy is unreadable or its public key is inconsistent, it
    deletes every server backup, creates one fresh backup, and exports all
    available recovery seeds into the same opened SSSS store. An absent secret
    or absent server backup is an unconfigured no-op, matching Rust. Other
    failures are propagated. The ordered remote/account-data writes are not
    transactional. *)

type backup_upload =
  | Not_waited
  | Uploaded of int
  | Upload_failed of Error.t
      (** The optional synchronous room-key upload outcome. An upload failure is
          reported but does not make {!enable} fail, matching Rust. *)

type enabled = {
  store : Secrets.store;
  key_id : string;
  recovery_key : string;
  backup_version : string;
  backup_upload : backup_upload;
}
(** The created recovery store, its user-facing recovery key, the exact
    server-side backup version, and the optional upload outcome. *)

val enable :
  Client.t ->
  encryption:Encryption_driver.t ->
  private_identity:Cross_signing.private_identity ->
  ?passphrase:string ->
  ?wait_for_backups_to_upload:bool ->
  unit ->
  (enabled, Error.t) result
(** [enable client ~encryption ~private_identity ?passphrase ()] follows Rust's
    enable order. It validates all three users before I/O; refuses to create a
    second server backup; marks and creates a new backup when none exists;
    otherwise reuses the locally persisted backup key. It then publishes the
    cross-signing and backup secrets before the new default-key event. When
    [wait_for_backups_to_upload] is true, pending room keys are uploaded
    synchronously; an upload failure is returned as [Upload_failed] while
    recovery still succeeds. Otherwise [backup_upload] is [Not_waited] and
    pending work remains for the normal driver. Account-data, backup, and store
    writes are not transactional and no rollback is attempted after a partial
    failure. *)

val reset_key :
  Client.t ->
  encryption:Encryption_driver.t ->
  private_identity:Cross_signing.private_identity ->
  ?passphrase:string ->
  unit ->
  (Secrets.created_store, Error.t) result
(** [reset_key client ~encryption ~private_identity ?passphrase ()] creates a
    fresh recovery SSSS store after validating all three users before I/O. It
    exports the supplied identity and, when present, the locally persisted
    backup decryption key. It does not create, delete, or change a backup or
    either recovery marker. Account-data writes are ordered but
    non-transactional; earlier writes remain after a later failure. *)

type recovered_and_reset = {
  store : Secrets.store;
  key_id : string;
  recovery_key : string;
  private_identity : Cross_signing.private_identity;
}
(** The new recovery store/key and the identity recovered before resetting it.
*)

val recover_and_reset :
  Client.t ->
  encryption:Encryption_driver.t ->
  credential:string ->
  ?passphrase:string ->
  unit ->
  (recovered_and_reset, Error.t) result
(** [recover_and_reset] first performs {!recover} with the old credential,
    including its backup validation and persistence boundary, then calls
    {!reset_key} to publish a new default recovery store. Earlier successful
    effects are not rolled back if the reset writes fail. *)

(** {1 Reactive recovery state}

    [Manager] is a small state projection for callers which already maintain a
    {!type:Base_client.state}, such as a sync loop. It performs no I/O while
    deriving state from that snapshot. The manager, its encryption driver, and
    its callbacks are intended for one fiber (or one thread); callers must
    serialize calls when sharing one between fibers. *)

module Manager : sig
  type t
  type subscription

  val create :
    ?private_identity:Cross_signing.private_identity ->
    ?base:Base_client.state ->
    Client.t ->
    encryption:Encryption_driver.t ->
    t
  (** [create client ~encryption ?private_identity ~base] creates a manager.
      [base] defaults to an empty state for the client's session when omitted.
      The initial state is derived immediately, without a request. An empty
      state which has not completed a sync remains [Unknown]; a persisted or
      committed state can establish that recovery is [Disabled]. *)

  val client : t -> Client.t
  val encryption : t -> Encryption_driver.t
  val base_state : t -> Base_client.state

  val private_identity : t -> Cross_signing.private_identity option
  (** The private cross-signing identity currently available to the manager. *)

  val set_private_identity : t -> Cross_signing.private_identity option -> state
  (** Replace the locally available identity and recompute/publish state.

      Raises [Invalid_argument] when the identity belongs to another user. *)

  val state : t -> state

  val refresh_from_base : t -> Base_client.state -> state
  (** Replace the latest sync snapshot and derive/publish state without I/O.

      Raises [Invalid_argument] when the snapshot belongs to another user. *)

  val refresh : t -> (state, Error.t) result
  (** Refresh default-key and, when necessary, marker account data from the
      homeserver. A failed refresh leaves the last confirmed state in place,
      then performs the local projection as a best effort. *)

  val subscribe : t -> (state -> unit) -> subscription
  (** Register a callback, calling it immediately with the current state and
      again for each distinct state. Exceptions from later publications are
      logged and isolated from the manager and its other subscribers. *)

  val unsubscribe : t -> subscription -> unit
  (** Idempotently remove a subscription. *)

  val watch : t -> (state -> unit) -> unit -> unit
  (** [watch t f] is [subscribe t f] as an unsubscribe function. *)

  val disable : t -> (unit, Error.t) result
  val disable_and_delete_backups : t -> (unit, Error.t) result
  val recover : t -> credential:string -> (recovered, Error.t) result

  val recover_and_fix_backup :
    t -> credential:string -> (recovered, Error.t) result

  val enable :
    t ->
    ?private_identity:Cross_signing.private_identity ->
    ?passphrase:string ->
    ?wait_for_backups_to_upload:bool ->
    unit ->
    (enabled, Error.t) result

  val reset_key :
    t ->
    ?private_identity:Cross_signing.private_identity ->
    ?passphrase:string ->
    unit ->
    (Secrets.created_store, Error.t) result

  val recover_and_reset :
    t ->
    credential:string ->
    ?passphrase:string ->
    unit ->
    (recovered_and_reset, Error.t) result

  val reset_identity :
    t ->
    auth_callback:(Uiaa.uiaa_response -> Uiaa.auth_data option) ->
    unit ->
    Cross_signing.private_identity Uiaa.uiaa_result
  (** Delete every remote backup, mark the local SSSS default disabled, then
      publish this device's keys first when they have not reached the server,
      upload a freshly generated own cross-signing identity through UIAA, and
      publish its self-signature over the current device. The old identity is
      retained when deletion, device/signing-key upload, or signature upload
      fails. A successful result resets local cross-signing-derived trust and
      installs the new identity; callers may then use {!enable} to create/export
      a new recovery store. A subsequent call after [Uiaa_auth_required] or an
      ambiguous [Uiaa_error] resumes the retained upload body; use
      {!cancel_pending_identity_reset} to abandon it. The callback can answer
      [m.oauth] with {!Uiaa.oauth_auth} after external approval. If a backup was
      active before reset, a replacement backup is created after the replacement
      identity is accepted; SSSS remains disabled until {!enable} because this
      bounded result does not carry a new recovery key. A failed recreation is
      returned while the replacement identity remains installed and the
      continuation stays retryable. *)

  val cancel_pending_identity_reset : t -> unit
  (** Discard a generated identity retained for a UIAA continuation. This is
      pure. If the replacement identity was already installed while a follow-up
      backup recreation failed, cancellation does not revert it; it only
      suppresses further retry. *)
end
