module R = Matrix_client.Recovery

let unwrap context fn =
  Error.with_context context (fun () -> Error.unwrap (fn ()))

type state = R.state = Unknown | Enabled | Disabled | Incomplete
type marker = R.marker = Absent | Valid of bool | Malformed of string
type markers = R.markers = { stable : marker; unstable : marker }

type account_data_write = R.account_data_write = {
  event_type : string;
  content : Jsont.json;
}

type inputs = R.inputs = {
  secret_storage_enabled : bool option;
  cross_signing_complete : bool option;
  backup_enabled : bool option;
  stable_marker : marker;
  unstable_marker : marker;
}

let key_backup_event_type = R.key_backup_event_type
let backup_disabled_event_type = R.backup_disabled_event_type
let key_backup_jsont = R.key_backup_jsont
let backup_disabled_jsont = R.backup_disabled_jsont
let key_backup_marker = R.key_backup_marker
let backup_disabled_marker = R.backup_disabled_marker
let state = R.state

let check_state client ~encryption ~private_identity =
  unwrap "checking Matrix recovery state" (fun () ->
      R.check_state (Client.base client) ~encryption ~private_identity)

let known_secret_event_types = R.known_secret_event_types
let mark_enabled_writes = R.mark_enabled_writes
let disable_writes = R.disable_writes

let fetch_markers client =
  unwrap "fetching Matrix recovery markers" (fun () ->
      R.fetch_markers (Client.base client))

let apply_writes client writes =
  unwrap "applying Matrix recovery account data" (fun () ->
      R.apply_writes (Client.base client) writes)

let mark_backup_enabled client =
  unwrap "marking Matrix key backup enabled" (fun () ->
      R.mark_backup_enabled (Client.base client))

let disable_account_data ?default_key_id client =
  unwrap "disabling Matrix recovery account data" (fun () ->
      R.disable_account_data ?default_key_id (Client.base client))

let disable client ~encryption =
  unwrap "disabling Matrix recovery" (fun () ->
      R.disable (Client.base client) ~encryption)

let disable_and_delete_backups client ~encryption =
  unwrap "disabling Matrix recovery and deleting key backups" (fun () ->
      R.disable_and_delete_backups (Client.base client) ~encryption)

type recovered = R.recovered = {
  store : Matrix_client.Secrets.store;
  private_identity : Matrix_client.Cross_signing.private_identity;
}

let recover client ~encryption ~credential =
  unwrap "recovering Matrix secrets" (fun () ->
      R.recover (Client.base client) ~encryption ~credential)

let recover_and_fix_backup client ~encryption ~credential =
  unwrap "recovering Matrix secrets and repairing key backup" (fun () ->
      R.recover_and_fix_backup (Client.base client) ~encryption ~credential)

type backup_upload = R.backup_upload =
  | Not_waited
  | Uploaded of int
  | Upload_failed of Matrix_client.Error.t

type enabled = R.enabled = {
  store : Matrix_client.Secrets.store;
  key_id : string;
  recovery_key : string;
  backup_version : string;
  backup_upload : backup_upload;
}

let enable client ~encryption ~private_identity ?passphrase
    ?wait_for_backups_to_upload () =
  unwrap "enabling Matrix recovery" (fun () ->
      R.enable (Client.base client) ~encryption ~private_identity ?passphrase
        ?wait_for_backups_to_upload ())

let reset_key client ~encryption ~private_identity ?passphrase () =
  unwrap "resetting the Matrix recovery key" (fun () ->
      R.reset_key (Client.base client) ~encryption ~private_identity ?passphrase
        ())

type recovered_and_reset = R.recovered_and_reset = {
  store : Matrix_client.Secrets.store;
  key_id : string;
  recovery_key : string;
  private_identity : Matrix_client.Cross_signing.private_identity;
}

let recover_and_reset client ~encryption ~credential ?passphrase () =
  unwrap "recovering and resetting Matrix secrets" (fun () ->
      R.recover_and_reset (Client.base client) ~encryption ~credential
        ?passphrase ())

module Manager = struct
  type t = R.Manager.t
  type subscription = R.Manager.subscription

  let create ?private_identity ?base client ~encryption =
    R.Manager.create ?private_identity ?base (Client.base client) ~encryption

  let base_state = R.Manager.base_state
  let private_identity = R.Manager.private_identity
  let set_private_identity = R.Manager.set_private_identity
  let state = R.Manager.state
  let refresh_from_base = R.Manager.refresh_from_base
  let subscribe = R.Manager.subscribe
  let unsubscribe = R.Manager.unsubscribe
  let watch = R.Manager.watch

  let refresh t =
    unwrap "refreshing managed Matrix recovery state" (fun () ->
        R.Manager.refresh t)

  let disable t =
    unwrap "disabling managed Matrix recovery" (fun () -> R.Manager.disable t)

  let disable_and_delete_backups t =
    unwrap "disabling managed recovery and deleting key backups" (fun () ->
        R.Manager.disable_and_delete_backups t)

  let recover t ~credential =
    unwrap "recovering managed Matrix secrets" (fun () ->
        R.Manager.recover t ~credential)

  let recover_and_fix_backup t ~credential =
    unwrap "recovering managed secrets and repairing key backup" (fun () ->
        R.Manager.recover_and_fix_backup t ~credential)

  let enable t ?private_identity ?passphrase ?wait_for_backups_to_upload () =
    unwrap "enabling managed Matrix recovery" (fun () ->
        R.Manager.enable t ?private_identity ?passphrase
          ?wait_for_backups_to_upload ())

  let reset_key t ?private_identity ?passphrase () =
    unwrap "resetting the managed Matrix recovery key" (fun () ->
        R.Manager.reset_key t ?private_identity ?passphrase ())

  let recover_and_reset t ~credential ?passphrase () =
    unwrap "recovering and resetting managed Matrix secrets" (fun () ->
        R.Manager.recover_and_reset t ~credential ?passphrase ())

  let reset_identity t ~auth_callback () =
    match R.Manager.reset_identity t ~auth_callback () with
    | Matrix_client.Uiaa.Uiaa_error error ->
        Error.raise_client_error ~context:"resetting the Matrix identity" error
    | result -> result

  let cancel_pending_identity_reset = R.Manager.cancel_pending_identity_reset
end
