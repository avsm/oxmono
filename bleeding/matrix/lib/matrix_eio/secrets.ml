module S = Matrix_client.Secrets

let unwrap context fn =
  Error.with_context context (fun () -> Error.unwrap (fn ()))

let default_key_event_type = S.default_key_event_type
let key_event_type = S.key_event_type

let get_default_key_id client =
  unwrap "fetching the default secret-storage key" (fun () ->
      S.get_default_key_id (Client.base client))

let set_default_key_id client ~key_id =
  unwrap "setting the default secret-storage key" (fun () ->
      S.set_default_key_id (Client.base client) ~key_id)

let get_key_description client ~key_id =
  unwrap "fetching a secret-storage key description" (fun () ->
      S.get_key_description (Client.base client) ~key_id)

let put_key_description client ~key_id description =
  unwrap "storing a secret-storage key description" (fun () ->
      S.put_key_description (Client.base client) ~key_id description)

let get_secret client ~key_id ~key ~name =
  unwrap "fetching and decrypting a Matrix secret" (fun () ->
      S.get_secret (Client.base client) ~key_id ~key ~name)

let get_secret_opt client ~key_id ~key ~name =
  unwrap "optionally fetching and decrypting a Matrix secret" (fun () ->
      S.get_secret_opt (Client.base client) ~key_id ~key ~name)

let store_secret client ~random ~key_id ~key ~name secret =
  unwrap "encrypting and storing a Matrix secret" (fun () ->
      S.store_secret (Client.base client) ~random ~key_id ~key ~name secret)

type store = S.store
type created_store = S.created_store

let create_secret_store client ~random ?passphrase ?secrets () =
  unwrap "creating Matrix secret storage" (fun () ->
      S.create_secret_store (Client.base client) ~random ?passphrase ?secrets ())

let open_secret_store client ~credential =
  unwrap "opening Matrix secret storage" (fun () ->
      S.open_secret_store (Client.base client) ~credential)

let create_recovery_store client ~random ?passphrase ~private_identity
    ?backup_key () =
  unwrap "creating Matrix recovery secret storage" (fun () ->
      S.create_recovery_store (Client.base client) ~random ?passphrase
        ~private_identity ?backup_key ())

let store_key_id = S.store_key_id
let store_user_id = S.store_user_id
let store_homeserver = S.store_homeserver

let get_store_secret t ~name =
  unwrap "fetching a secret from an open Matrix store" (fun () ->
      S.get_store_secret t ~name)

let put_store_secret t ~random ~name secret =
  unwrap "storing a secret in an open Matrix store" (fun () ->
      S.put_store_secret t ~random ~name secret)

let import_backup t ~encryption =
  unwrap "importing key backup from Matrix secret storage" (fun () ->
      S.import_backup t ~encryption)

let export_recovery_secrets t ~random ~private_identity ?backup_key () =
  unwrap "exporting Matrix recovery secrets" (fun () ->
      S.export_recovery_secrets t ~random ~private_identity ?backup_key ())

let import_cross_signing t ~encryption =
  let identity =
    unwrap "importing cross-signing secrets" (fun () ->
        S.import_cross_signing t ~encryption:(Encryption.machine encryption))
  in
  (* A successful import also refreshed the validated public identity. *)
  Encryption.save encryption;
  identity
