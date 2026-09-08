(** cross_signing — the key hierarchy that says which devices a user owns.

    A user has three signing keys. The master key is the user's identity. It
    signs a self-signing key, which signs that user's own devices, and a
    user-signing key, which signs other users' master keys. Verifying one device
    of another user therefore vouches for every device that user has signed.

    The public halves are published through {!Keys}. The private halves live
    here as {!Crypto_key.Ed25519.Private.t} and never leave the device except
    through secret storage.

    Nothing here does any I/O. A caller signs and checks in memory and hands the
    results to {!Keys.upload_signing_keys} and {!Keys.upload_signatures}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#cross-signing>
      Cross-signing *)

(** {1 Trust states} *)

(** What this client was told about a device, independently of cross-signing. *)
type local_trust =
  | Verified  (** Verified by the user of this client. *)
  | Blacklisted  (** Explicitly distrusted. *)
  | Ignored  (** Trust deliberately not tracked. *)
  | Unset  (** Nothing known. *)

(** How far this user's own identity has been verified. *)
type own_identity_state =
  | Never_verified
  | Verification_violation  (** Was verified, then the master key changed. *)
  | Identity_verified

(** {1 Public keys} *)

(** What a cross-signing key is for. It is the [usage] the key is published
    under. *)
type role = Keys.key_usage =
  | Master
  | Self_signing
  | User_signing
  | Other of string
      (** A usage this library does not model, which takes part in no check
          here. *)

type key
(** The type for a published cross-signing key together with the role it plays.
*)

val key : role:role -> Keys.cross_signing_key -> key
(** [key ~role published] is [published] in the role [role]. [role] must be one
    of the usages [published] declares, since nothing but the caller says which
    of them this value stands for. *)

val role : key -> role
(** [role t] is the role [t] was built with. *)

val published : key -> Keys.cross_signing_key
(** [published t] is the wire form of [t], as [/keys/query] returns it. *)

val key_user_id : key -> Matrix_proto.Id.User_id.t
(** [key_user_id t] is the user [t] belongs to. *)

val key_ed25519 : key -> Crypto_key.Ed25519.Public.t option
(** [key_ed25519 t] is the Ed25519 public key [t] publishes, and [None] when it
    publishes none or when what it publishes is not a key. *)

val verify_key : signer:key -> signed:key -> bool
(** [verify_key ~signer ~signed] is [true] when [signed] carries a valid
    signature made by [signer]'s Ed25519 key.

    The verifying key is taken from [signer] rather than from the key identifier
    in the signature block, so a server that rewrote that identifier cannot
    choose which key the signature is checked against. *)

(** {1 Private keys} *)

type private_identity
(** The type for the three private cross-signing keys of one user. Any of them
    may be missing, on a device that has not generated or fetched it. *)

val create_private_identity :
  user_id:Matrix_proto.Id.User_id.t -> private_identity
(** [create_private_identity ~user_id] is an identity for [user_id] with no keys
    in it. *)

val generate_private_keys : random:Random.t -> private_identity -> unit
(** [generate_private_keys ~random t] draws all three keys from [random],
    replacing any [t] already held. *)

val identity_user_id : private_identity -> Matrix_proto.Id.User_id.t
(** [identity_user_id t] is the user [t] belongs to. *)

val master_secret : private_identity -> Crypto_key.Ed25519.Private.t option
(** [master_secret t] is the private master key of [t]. *)

val self_signing_secret :
  private_identity -> Crypto_key.Ed25519.Private.t option
(** [self_signing_secret t] is the private self-signing key of [t]. *)

val user_signing_secret :
  private_identity -> Crypto_key.Ed25519.Private.t option
(** [user_signing_secret t] is the private user-signing key of [t]. *)

val set_user_signing_secret :
  private_identity -> Crypto_key.Ed25519.Private.t option -> unit
(** [set_user_signing_secret t secret] replaces the held user-signing secret.
    Passing [None] clears it. This is intended for secret-storage restore and
    rotation; callers must ensure it matches the published identity. *)

val master_public : private_identity -> Crypto_key.Ed25519.Public.t option
(** [master_public t] is the public half of the private master key, when held.
    It is a capability helper for protocols which authenticate this device's
    cross-signing identity. *)

(** Why a secret-storage cross-signing seed could not be imported. The role is
    included so callers can report which of the three secrets was bad. *)
type private_identity_import_error =
  | Invalid_secret of role * string
      (** The secret was not Base64 or was not a 32-byte Ed25519 seed. *)
  | Public_key_mismatch of role
      (** The seed's public key differs from the freshly queried public
          identity. *)

val pp_private_identity_import_error :
  Format.formatter -> private_identity_import_error -> unit
(** [pp_private_identity_import_error ppf error] prints a concise explanation of
    an import failure. *)

val private_identity_of_secrets :
  user_id:Matrix_proto.Id.User_id.t ->
  expected_master:Crypto_key.Ed25519.Public.t ->
  expected_self_signing:Crypto_key.Ed25519.Public.t ->
  expected_user_signing:Crypto_key.Ed25519.Public.t ->
  master:string option ->
  self_signing:string option ->
  user_signing:string option ->
  (private_identity, private_identity_import_error) result
(** [private_identity_of_secrets] decodes the supplied standard-Base64,
    unexpanded 32-byte Ed25519 seeds and checks each derived public key against
    the published identity. Padded and unpadded Base64 are accepted. Missing
    secrets remain missing. The operation is atomic: a malformed or mismatched
    supplied secret returns an error and never returns a usable partial
    identity. *)

val private_identity_of_secrets_unchecked :
  user_id:Matrix_proto.Id.User_id.t ->
  master:string ->
  self_signing:string ->
  user_signing:string ->
  (private_identity, private_identity_import_error) result
(** [private_identity_of_secrets_unchecked] atomically decodes all three
    standard-Base64, unexpanded 32-byte Ed25519 seeds without comparing their
    public halves with an independently obtained public identity. Padded and
    unpadded Base64 are accepted.

    This is only safe when the complete set of secrets came through an
    authenticated, trusted channel, such as an established MSC4108 QR-login
    channel. Recovery from server-side secret storage must use
    {!private_identity_of_secrets}, which performs the public-key checks. *)

(** {1 Signing} *)

val sign_cross_signing_key :
  signer:Crypto_key.Ed25519.Private.t ->
  signer_user_id:Matrix_proto.Id.User_id.t ->
  Keys.cross_signing_key ->
  Keys.cross_signing_key
(** [sign_cross_signing_key ~signer ~signer_user_id k] is [k] with [signer]'s
    signature over it added to its signature block, replacing any signature that
    key had already made. It is how a master key signs the self- and
    user-signing keys, and how a user-signing key signs another user's master
    key. *)

type upload = {
  master_key : Keys.cross_signing_key;
  self_signing_key : Keys.cross_signing_key;
  user_signing_key : Keys.cross_signing_key;
}
(** The type for the three public keys [/keys/device_signing/upload] takes. *)

val build_upload : private_identity -> upload option
(** [build_upload t] is the publishable form of [t], with the self- and
    user-signing keys signed by the master key. It is [None] when any of the
    three private keys is missing. *)

(** {1 Devices} *)

type device
(** The type for one device and what is known about its trustworthiness. *)

val create_device : Keys.device_keys -> device
(** [create_device keys] is the device [keys] describes, with its local trust
    {!Unset} and not trusted through cross-signing until {!update_device_trust}
    says otherwise. *)

val device_keys : device -> Keys.device_keys
(** [device_keys t] is the wire form of [t], as [/keys/query] returned it. *)

val device_id : device -> Matrix_proto.Id.Device_id.t
(** [device_id t] is the identifier of [t]. *)

val device_user_id : device -> Matrix_proto.Id.User_id.t
(** [device_user_id t] is the owner of [t]. *)

val device_algorithms : device -> string list
(** [device_algorithms t] is the encryption algorithms [t] claims to speak. *)

val device_public_keys : device -> (Crypto_key.Key_id.t * string) list
(** [device_public_keys t] is the public keys [t] published, as unpadded base64.
*)

val device_signatures : device -> Keys.signatures
(** [device_signatures t] is the signatures over [t]'s key object. *)

val device_ed25519 : device -> Crypto_key.Ed25519.Public.t option
(** [device_ed25519 t] is the [ed25519:] key [t] published under its own device
    identifier, and [None] when it published none or when what it published is
    not a key. *)

val device_local_trust : device -> local_trust
(** [device_local_trust t] is what this client was told about [t]. *)

val set_device_local_trust : device -> local_trust -> unit
(** [set_device_local_trust t trust] records [trust] for [t]. It is the only way
    local trust is written. *)

val device_cross_signing_trusted : device -> bool
(** [device_cross_signing_trusted t] is what {!update_device_trust} last
    concluded about [t], and [false] until it has run. *)

val is_device_verified : device -> bool
(** [is_device_verified t] is [true] when [t] is trusted locally or through
    cross-signing. An explicit local {!Blacklisted} always wins: no
    cross-signing signature can make a blacklisted device verified. *)

val verify_device_signature : self_signing_key:key -> device:device -> bool
(** [verify_device_signature ~self_signing_key ~device] is [true] when
    [self_signing_key] has signed [device]'s key object. It is [false] when the
    two belong to different users, so one user's self-signing key cannot vouch
    for another user's device. *)

val update_device_trust : self_signing_key:key -> device -> bool
(** [update_device_trust ~self_signing_key t] runs {!verify_device_signature},
    records the answer in [t] and returns it. It is the only way
    {!device_cross_signing_trusted} is written. *)

val sign_device :
  signer:Crypto_key.Ed25519.Private.t ->
  signer_user_id:Matrix_proto.Id.User_id.t ->
  device ->
  device
(** [sign_device ~signer ~signer_user_id t] is [t] with [signer]'s signature
    over its key object added, as a self-signing key adds when a device is
    verified. The local and cross-signing trust of the result are those of [t].
*)

val sign_device_keys :
  signer:Crypto_key.Ed25519.Private.t ->
  signer_user_id:Matrix_proto.Id.User_id.t ->
  Keys.device_keys ->
  Keys.device_keys
(** [sign_device_keys] signs the exact wire device-key object supplied. *)

(** {1 User identities} *)

type own_identity
(** The type for this user's own identity, for which all three public keys are
    known. *)

val own_identity :
  user_id:Matrix_proto.Id.User_id.t ->
  master_key:key ->
  self_signing_key:key ->
  user_signing_key:key ->
  ?state:own_identity_state ->
  unit ->
  own_identity
(** [own_identity ~user_id ~master_key ~self_signing_key ~user_signing_key ()]
    is the identity of [user_id]. [state] defaults to {!Never_verified}. *)

val own_user_id : own_identity -> Matrix_proto.Id.User_id.t
(** [own_user_id t] is the user [t] belongs to. *)

val own_master_key : own_identity -> key
(** [own_master_key t] is the master key of [t]. *)

val own_self_signing_key : own_identity -> key
(** [own_self_signing_key t] is the self-signing key of [t]. *)

val own_user_signing_key : own_identity -> key
(** [own_user_signing_key t] is the user-signing key of [t]. *)

val own_identity_state : own_identity -> own_identity_state
(** [own_identity_state t] is how far [t] has been verified. *)

val is_own_identity_verified : own_identity -> bool
(** [is_own_identity_verified t] is [true] when [own_identity_state t] is
    {!Identity_verified}. *)

type other_identity
(** The type for another user's identity. A homeserver never publishes anybody
    else's user-signing key, so only two of the three are known. *)

val other_identity :
  user_id:Matrix_proto.Id.User_id.t ->
  master_key:key ->
  self_signing_key:key ->
  ?pinned_master_key:key ->
  unit ->
  other_identity
(** [other_identity ~user_id ~master_key ~self_signing_key ()] is the identity
    of [user_id] with no master key pinned.

    A fresh [/keys/query] response builds a new [other_identity], so a caller
    that wants {!has_identity_changed} to keep working across a refresh must
    pass [~pinned_master_key] itself, read from the {!val-other_identity} it is
    replacing with {!pinned_master_key}. [pinned_master_key] defaults to absent.
*)

val other_user_id : other_identity -> Matrix_proto.Id.User_id.t
(** [other_user_id t] is the user [t] belongs to. *)

val other_master_key : other_identity -> key
(** [other_master_key t] is the master key of [t]. *)

val other_self_signing_key : other_identity -> key
(** [other_self_signing_key t] is the self-signing key of [t]. *)

val is_other_identity_verified :
  our_user_signing_key:key -> other_identity -> bool
(** [is_other_identity_verified ~our_user_signing_key t] is [true] when
    [our_user_signing_key] has signed [t]'s master key. *)

val pin_master_key : other_identity -> unit
(** [pin_master_key t] records [t]'s current master key as the one
    {!has_identity_changed} compares against. *)

val pinned_master_key : other_identity -> key option
(** [pinned_master_key t] is the master key {!pin_master_key} last recorded for
    [t], or [None] if none was. Pass it as [~pinned_master_key] to
    {!val-other_identity} when building the replacement for [t] after a
    [/keys/query] refresh, so the pin survives the refresh. *)

val has_identity_changed : other_identity -> bool
(** [has_identity_changed t] is [true] when [t]'s master key differs from the
    one {!pin_master_key} last recorded. It is [false] when none was ever
    pinned. *)

val verify_master_trust : ours:own_identity -> theirs:other_identity -> bool
(** [verify_master_trust ~ours ~theirs] walks the whole chain the specification
    requires before another user's keys may be trusted. The master key of [ours]
    must have signed its user-signing key, that user-signing key must have
    signed the master key of [theirs], and that master key must have signed the
    self-signing key of [theirs]. *)

val verify_device_trust_chain :
  ours:own_identity -> theirs:other_identity -> device:device -> bool
(** [verify_device_trust_chain ~ours ~theirs ~device] is {!verify_master_trust}
    and, on top of it, the signature of [theirs]'s self-signing key on [device].
*)
