(** Crow's account cross-signing keys, held in the private Matrix profile. *)

val setup :
  dir:Eio.Fs.dir_ty Eio.Path.t -> client:Matrix_eio.Client.t ->
  encryption:Matrix_eio.Encryption.t -> random:Matrix_client.Random.t ->
  auth_callback:(Matrix_client.Uiaa.uiaa_response ->
                 Matrix_client.Uiaa.auth_data option) ->
  Matrix_client.Cross_signing.private_identity
(** [setup ~dir ~client ~encryption ~random ~auth_callback] creates missing
    account keys, signs the current device and returns the account identity.
    It saves seeds with mode 0600 before publishing. Retry resumes saved keys.
    Invalid keys or an identity mismatch raise without replacing keys. An existing remote identity
    without matching local keys is never replaced. Hold the profile lock while
    calling this function. *)
