(** QR-login wire boundaries.

    The legacy MSC4388 boundary below deliberately covers only its QR payload
    and rendezvous capability probe; its secure channel remains unsupported. The
    {!Msc4108} module is the separate MSC4108 implementation and provides the
    typed rendezvous/ECIES handshake, but not OAuth or login handover. *)

type intent =
  | Login
  | Reciprocate
      (** Which device displayed the QR code. [Login] means the new device
          displayed it; [Reciprocate] means the existing device did. *)

type t = {
  intent : intent;
  public_key : Crypto_key.Curve25519.Public.t;
  rendezvous_id : string;
  base_url : Uriz.t;
}
(** An MSC4388 QR payload. *)

type codec_error =
  | Not_enough_data
  | Invalid_prefix
  | Invalid_type of int
  | Invalid_intent of int
  | Invalid_utf8 of string
  | Invalid_base_url of string
  | Field_too_long of string
  | Invalid_base64 of string
      (** Why an MSC4388 payload could not be decoded or encoded. *)

val pp_codec_error : Format.formatter -> codec_error -> unit
(** [pp_codec_error ppf e] renders [e] on one line. *)

val make :
  intent:intent ->
  public_key:Crypto_key.Curve25519.Public.t ->
  rendezvous_id:string ->
  base_url:Uriz.t ->
  (t, codec_error) result
(** [make] validates the two length-prefixed UTF-8 fields and the homeserver
    base URL. *)

val of_bytes : string -> (t, codec_error) result
(** [of_bytes bytes] decodes the [IO_ELEMENT_MSC4388] binary format. *)

val to_bytes : t -> (string, codec_error) result
(** [to_bytes t] encodes the [IO_ELEMENT_MSC4388] binary format. A base URL
    whose path is exactly [/] is encoded without that trailing slash, matching
    the Rust SDK. *)

val of_base64 : string -> (t, codec_error) result
(** [of_base64 value] decodes standard padded or unpadded Base64 and then
    applies {!of_bytes}. *)

val to_base64 : t -> (string, codec_error) result
(** [to_base64 t] is the unpadded standard-Base64 form of {!to_bytes}. *)

val rendezvous_path : string
(** The unstable MSC4388 rendezvous discovery endpoint. *)

val rendezvous_server_supported : Client.t -> (bool, Error.t) result
(** [rendezvous_server_supported client] returns the endpoint's
    [create_available] flag. HTTP 403 and 404 (including their ordinary Matrix
    error spellings) are capability outcomes and return [false]; other errors
    are preserved. The discovery request is unauthenticated. *)

type secure_channel_error =
  | Invalid_channel_intent
  | Unsupported_qr_code_type
      (** The currently reachable secure-channel outcomes at this boundary. *)

val establish_secure_channel :
  expected_intent:intent -> t -> (unit, secure_channel_error) result
(** [establish_secure_channel ~expected_intent qr] mirrors the pinned Rust
    boundary. Scanning a QR with the same intent is {!Invalid_channel_intent};
    an opposite-intent MSC4388 payload reaches {!Unsupported_qr_code_type},
    since its HPKE/rendezvous channel is not implemented upstream at the pinned
    revision either. *)

(** The MSC4108 QR payload used by the cross-device login protocol.

    This is deliberately separate from the older MSC4388 payload above: the two
    formats have different magic, intent values, and intent data. *)
module Msc4108 : sig
  type intent =
    | Login
    | Reciprocate of Uriz.t
        (** [Reciprocate server_name] includes the existing device's
            homeserver/server name in the QR payload. *)

  type code = {
    intent : intent;
    public_key : Crypto_key.Curve25519.Public.t;
    rendezvous_url : Uriz.t;
  }

  type t = code

  type codec_error =
    | Not_enough_data
    | Invalid_prefix
    | Invalid_version of int
    | Invalid_intent of int
    | Invalid_public_key
    | Invalid_utf8 of string
    | Invalid_url of string
    | Field_too_long of string
    | Trailing_data
    | Invalid_base64 of string

  val pp_codec_error : Format.formatter -> codec_error -> unit

  val make :
    intent:intent ->
    public_key:Crypto_key.Curve25519.Public.t ->
    rendezvous_url:Uriz.t ->
    (code, codec_error) result

  val of_bytes : string -> (code, codec_error) result
  val to_bytes : code -> (string, codec_error) result
  val of_base64 : string -> (code, codec_error) result
  val to_base64 : code -> (string, codec_error) result

  val rendezvous_path : string
  (** The unstable MSC4108 rendezvous creation endpoint. *)

  (** Typed MSC4108 OAuth/login messages exchanged after the secure channel is
      established. Unknown protocol and failure-reason values are retained as
      custom values, so newer peers remain round-trippable. *)
  module Messages : sig
    type login_protocol =
      | Device_authorization_grant
      | Custom_protocol of string

    type protocol = login_protocol

    type login_failure_reason =
      | Authorization_expired
      | Device_already_exists
      | Device_not_found
      | Unexpected_message_received
      | Unsupported_protocol
      | User_cancelled
      | Custom_failure of string

    type failure_reason = login_failure_reason

    type authorization_grant = {
      verification_uri : Uriz.t;
      verification_uri_complete : Uriz.t option;
    }

    type grant = authorization_grant

    type login_protocols = {
      protocols : login_protocol list;
      homeserver : Uriz.t;
    }

    type login_protocol_message = {
      device_authorization_grant : authorization_grant;
      protocol : login_protocol;
      device_id : string;
    }

    type cross_signing_secrets = {
      master_key : string;
      user_signing_key : string;
      self_signing_key : string;
    }

    type backup_secrets = {
      algorithm : string;
      backup_version : string;
      key : string;
    }

    type backup_secret = backup_secrets

    type secrets_bundle = {
      cross_signing : cross_signing_secrets;
      backup : backup_secrets option;
    }

    type secret_bundle = secrets_bundle

    type t =
      | Login_protocols of login_protocols
      | Login_protocol of login_protocol_message
      | Login_protocol_accepted
      | Login_success
      | Login_declined
      | Login_failure of {
          reason : login_failure_reason;
          homeserver : Uriz.t option;
        }
      | Login_secrets of secrets_bundle

    val login_protocol_of_string : string -> login_protocol
    val login_protocol_to_string : login_protocol -> string
    val login_failure_reason_of_string : string -> login_failure_reason
    val login_failure_reason_to_string : login_failure_reason -> string

    val authorization_grant_login_protocol :
      authorization_grant -> device_key:Crypto_key.Curve25519.Public.t -> t
    (** Construct the standard device-authorization message and encode the
        Curve25519 device identity in Matrix's canonical unpadded Base64. *)

    val jsont : t Jsont.t
    val message_jsont : t Jsont.t
    val auth_message_jsont : t Jsont.t
    val of_json : Jsont.json -> (t, string) result
    val to_json : t -> (Jsont.json, string) result
    val of_string : string -> (t, string) result
    val to_string : t -> (string, string) result
  end

  module Auth_message : module type of Messages
  module Qr_auth_message : module type of Messages

  (** Conversion between the MSC4108 wire bundle and the client's typed
      cross-signing and backup secrets. These functions only transform secret
      material; callers remain responsible for persisting the imported values.

      A bundle must only be imported from an authenticated, trusted MSC4108
      secure channel. Its cross-signing seeds are intentionally not compared
      with server-provided public keys, matching the QR-login flow in the pinned
      Rust SDK. *)
  module Secrets : sig
    type backup = {
      backup_version : string;
      decryption_key : Backup.Decryption_key.t;
    }
    (** A room-key backup version and its private decryption key. Keeping the
        pair together prevents exporting a key without the version it opens. *)

    type imported = {
      private_identity : Cross_signing.private_identity;
      backup : backup option;
    }
    (** The fully decoded values in an incoming secret bundle. *)

    type error =
      | Missing_cross_signing_secret of Cross_signing.role
      | Invalid_cross_signing_secret of
          Cross_signing.private_identity_import_error
      | Unsupported_backup_algorithm of string
      | Invalid_backup_key of string

    val pp_error : Format.formatter -> error -> unit
    (** [pp_error ppf error] prints a concise, non-secret description. *)

    val export :
      private_identity:Cross_signing.private_identity ->
      ?backup:backup ->
      unit ->
      (Messages.secrets_bundle, error) result
    (** [export ~private_identity ?backup ()] creates the MSC4108 wire bundle.
        All three private cross-signing seeds are required and are encoded as
        canonical unpadded Base64. When present, [backup] is encoded with
        {!Backup.backup_algorithm}. *)

    val import :
      user_id:Matrix_proto.Id.User_id.t ->
      Messages.secrets_bundle ->
      (imported, error) result
    (** [import ~user_id bundle] validates and decodes the complete bundle
        atomically. Unsupported backup algorithms and malformed cross-signing or
        backup keys are rejected without returning a partial result. *)
  end

  module Rendezvous = Qr_login_rendezvous
  (** The conditional, unauthenticated MSC4108 HTTP channel. It carries opaque
      messages; {!Secure_channel} composes it with {!Ecies} for the typed
      handshake. *)

  module Ecies = Qr_login_ecies
  (** The MSC4108 ECIES cryptographic primitive. The typed rendezvous handshake
      is exposed by {!Secure_channel}; OAuth, login approval, and secret
      handover remain outside this boundary. *)

  module Secure_channel : sig
    (** Typed MSC4108 rendezvous and ECIES handshake. OAuth, login approval, and
        secret handover are intentionally outside this boundary. *)
    type error =
      | Rendezvous_error of Rendezvous.error
      | Ecies_error of Ecies.error
      | Invalid_qr_code of codec_error
      | Invalid_intent
      | Secure_channel_message of { expected : string; received : string }
      | Invalid_utf8 of string
      | Invalid_auth_message of string
      | Invalid_check_code
      | Busy
      | Closed
      | Consumed

    val pp_error : Format.formatter -> error -> unit

    type displayed
    type almost_established
    type established

    val create :
      transport:Rendezvous.transport ->
      rendezvous_server:Uriz.t ->
      random:Random.t ->
      intent:intent ->
      unit ->
      (displayed, error) result
    (** [create] creates the rendezvous session and an ephemeral ECIES key, then
        returns the QR payload that the other device must scan.

        [rendezvous_server] is the absolute MSC4108 rendezvous creation
        endpoint, not merely the homeserver base URL. *)

    val login :
      transport:Rendezvous.transport ->
      rendezvous_server:Uriz.t ->
      random:Random.t ->
      unit ->
      (displayed, error) result
    (** [login] is [create] with a [Login] intent. *)

    val reciprocate :
      transport:Rendezvous.transport ->
      rendezvous_server:Uriz.t ->
      random:Random.t ->
      server_name:Uriz.t ->
      unit ->
      (displayed, error) result
    (** [reciprocate] is [create] with a [Reciprocate server_name] intent. *)

    val qr_code : displayed -> code

    val qr_code_base64 : displayed -> string
    (** The payload to render or its unpadded Base64 encoding. These accessors
        raise [Invalid_argument] after the displayed state has been consumed. *)

    val connect : displayed -> (almost_established, error) result
    (** Wait for and authenticate [MATRIX_QR_CODE_LOGIN_INITIATE], then send
        [MATRIX_QR_CODE_LOGIN_OK]. A failed handshake is consumed when cleanup
        succeeds; if deletion fails, {!cancel_displayed} can retry it. *)

    val check_code : almost_established -> int

    val confirm :
      almost_established -> check_code:int -> (established, error) result
    (** [confirm] consumes the almost-established channel. A wrong check code
        invalidates it and attempts to delete its rendezvous. *)

    val cancel_displayed : displayed -> (unit, error) result

    val cancel_almost : almost_established -> (unit, error) result
    (** Explicit, idempotent cancellation before confirmation. Failed deletion
        retains the state so cancellation can be retried. *)

    val from_qr_code :
      transport:Rendezvous.transport ->
      random:Random.t ->
      expected_intent:intent ->
      code ->
      (established, error) result
    (** [from_qr_code] consumes the one-shot rendezvous handshake. If a
        post-accept failure cannot be cleaned up, the server's expiry remains
        the final fallback for retiring that rendezvous session. *)

    val check_code_established : established -> int

    val send : established -> string -> (unit, error) result
    (** Encrypt and send one UTF-8 message. Overlapping operations return
        {!Busy}. Once encryption advances the nonce, any transport failure is
        delivery-ambiguous and poisons the channel; subsequent operations return
        {!Closed} rather than risk nonce reuse or desynchronization. *)

    val receive : established -> (string, error) result
    (** Receive, authenticate and UTF-8 validate one message. Overlapping
        operations return {!Busy}. An authentication failure or authenticated
        non-UTF-8 payload poisons the channel because its receive nonce has
        already advanced. *)

    val send_json : established -> Messages.t -> (unit, error) result
    (** Encode and send one typed MSC4108 authentication message. *)

    val receive_json : established -> (Messages.t, error) result
    (** Receive and decode one typed MSC4108 authentication message. *)

    val send_message : established -> Messages.t -> (unit, error) result
    val receive_message : established -> (Messages.t, error) result

    val close : established -> (unit, error) result
    (** Delete the rendezvous and close the channel. Idempotent. *)
  end

  (** Transport-independent MSC4108 application protocol.

      This layer only sequences typed messages on an already established
      channel. It does not close or cancel that channel. OAuth, device
      creation/lookup, authorization UI, and secret storage are injected by the
      hook records below. *)
  module Application : sig
    type 'e channel = {
      send : Messages.t -> (unit, 'e) result;
      receive : unit -> (Messages.t, 'e) result;
    }

    val secure_channel :
      Secure_channel.established -> Secure_channel.error channel

    type login_start = Await_protocols | Homeserver_known of Uriz.t
    type token_failure = Access_denied | Expired | Token_error of string

    type 'token authorization = {
      grant : Messages.authorization_grant;
      device_id : string;
      user_code : string;
      await_token : unit -> ('token, token_failure) result;
    }

    type login_progress =
      | Starting
      | Waiting_for_token of { user_code : string }
      | Syncing_secrets
      | Done

    type 'token login_hooks = {
      prepare :
        homeserver:Uriz.t option -> ('token authorization, string) result;
      activate : device_id:string -> 'token -> (unit, string) result;
      import_secrets : Messages.secrets_bundle -> (unit, string) result;
      on_progress : login_progress -> unit;
    }

    type grant_start = Advertise_protocols of Uriz.t | Protocols_already_known
    type grant_decision = Confirm | Cancel

    type grant_progress =
      | Grant_starting
      | Waiting_for_authorization of { verification_uri : Uriz.t }
      | Grant_syncing_secrets
      | Grant_done

    type grant_hooks = {
      export_secrets : unit -> (Messages.secrets_bundle, string) result;
      device_exists : string -> (bool, string) result;
      authorize : Uriz.t -> (grant_decision, string) result;
      await_device : string -> (bool, string) result;
      on_progress : grant_progress -> unit;
    }

    type 'e error =
      | Channel_error of 'e
      | Unexpected_message of { expected : string; received : Messages.t }
      | Peer_failure of {
          reason : Messages.login_failure_reason;
          homeserver : Uriz.t option;
        }
      | Unsupported_protocol of Messages.login_protocol
      | No_supported_protocol
      | Device_already_exists
      | Device_not_found
      | User_cancelled
      | Authorization_denied
      | Authorization_expired
      | Local_error of string

    val pp_error :
      (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e error -> unit

    val run_login :
      start:login_start ->
      channel:'e channel ->
      hooks:'token login_hooks ->
      (unit, 'e error) result
    (** Run the new-device side. [Await_protocols] receives the existing
        device's protocol advertisement; [Homeserver_known uri] starts directly
        with [uri]. The callbacks perform OAuth and secret handling. *)

    val run_grant :
      start:grant_start ->
      channel:'e channel ->
      hooks:grant_hooks ->
      (unit, 'e error) result
    (** Run the existing-device side. Secrets are exported before any protocol
        exchange. [Advertise_protocols uri] sends the protocol advertisement;
        [Protocols_already_known] skips it. *)
  end
end
