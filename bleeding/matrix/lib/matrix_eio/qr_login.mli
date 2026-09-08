(** Raising Eio facade for {!Matrix_client.Qr_login}. *)

type intent = Matrix_client.Qr_login.intent = Login | Reciprocate

type t = Matrix_client.Qr_login.t = {
  intent : intent;
  public_key : Matrix_client.Crypto_key.Curve25519.Public.t;
  rendezvous_id : string;
  base_url : Uriz.t;
}

type codec_error = Matrix_client.Qr_login.codec_error =
  | Not_enough_data
  | Invalid_prefix
  | Invalid_type of int
  | Invalid_intent of int
  | Invalid_utf8 of string
  | Invalid_base_url of string
  | Field_too_long of string
  | Invalid_base64 of string

val pp_codec_error : Format.formatter -> codec_error -> unit

val make :
  intent:intent ->
  public_key:Matrix_client.Crypto_key.Curve25519.Public.t ->
  rendezvous_id:string ->
  base_url:Uriz.t ->
  (t, codec_error) result

val of_bytes : string -> (t, codec_error) result
val to_bytes : t -> (string, codec_error) result
val of_base64 : string -> (t, codec_error) result
val to_base64 : t -> (string, codec_error) result
val rendezvous_path : string

val rendezvous_server_supported : Client.t -> bool
(** The raising form of {!Matrix_client.Qr_login.rendezvous_server_supported}.
*)

type secure_channel_error = Matrix_client.Qr_login.secure_channel_error =
  | Invalid_channel_intent
  | Unsupported_qr_code_type

val establish_secure_channel :
  expected_intent:intent -> t -> (unit, secure_channel_error) result

module Msc4108 : sig
  include module type of Matrix_client.Qr_login.Msc4108

  module Application_eio : sig
    (** Eio orchestration for the MSC4108 application protocol.

        Both operations take an already authenticated secure channel. They
        always close it before returning or raising; the caller remains
        responsible for creating, displaying or scanning the QR code and for
        confirming the channel check code. *)

    type login_progress =
      Matrix_client.Qr_login.Msc4108.Application.login_progress

    type login_start = Matrix_client.Qr_login.Msc4108.Application.login_start =
      | Await_protocols
      | Homeserver_known of Uriz.t
          (** How the new device enters the application exchange.
              [Await_protocols] receives the existing device's homeserver
              advertisement. [Homeserver_known uri] skips that message and
              requires [uri] to share the configured client's origin. *)

    type grant_progress =
      Matrix_client.Qr_login.Msc4108.Application.grant_progress

    type grant_decision =
          Matrix_client.Qr_login.Msc4108.Application.grant_decision =
      | Confirm
      | Cancel
          (** The application-controlled response after presenting the OAuth
              verification URI on the existing device. *)

    type login = {
      client : Client.t;
      session : Matrix_client.Client.session;
      client_id : string;
      expires_at : Ptime.t option;
      encryption : Encryption.t;
      private_identity : Matrix_client.Cross_signing.private_identity;
      backup : Matrix_client.Qr_login.Msc4108.Secrets.backup option;
    }
    (** A completed new-device login. The client and encryption machine share
        the Curve25519-derived device ID. The imported private identity and
        optional backup are returned explicitly because private cross-signing
        seeds are not persisted by [Crypto_store]; the application must retain
        them using its credential-storage policy. *)

    type Eio.Exn.err +=
      | Application_error of
          Matrix_client.Qr_login.Msc4108.Secure_channel.error
          Matrix_client.Qr_login.Msc4108.Application.error
            (** A secure-channel or MSC4108 application-protocol failure. *)

    val pp_application_error :
      Format.formatter ->
      Matrix_client.Qr_login.Msc4108.Secure_channel.error
      Matrix_client.Qr_login.Msc4108.Application.error ->
      unit

    val login :
      env:< mono_clock : _ Eio.Time.Mono.t ; .. > ->
      ?http:Fetch.plain ->
      ?client_id:string ->
      ?client_metadata:Oauth.Registration.client_metadata ->
      ?scope:string list ->
      ?allow_insecure:bool ->
      ?timeout:float ->
      ?crypto_store:Matrix_client.Crypto_store.t ->
      ?start:login_start ->
      ?on_progress:(login_progress -> unit) ->
      ?on_authenticated:
        (Matrix_client.Client.session ->
        client_id:string ->
        expires_at:Ptime.t option ->
        unit) ->
      Matrix_client.Qr_login.Msc4108.Secure_channel.established ->
      Client.t ->
      unit ->
      login
    (** [login ~env channel client ()] runs the new-device role. [client] must
        be unauthenticated. It performs OAuth device authorization, creates an
        Olm account whose Curve25519 key is the requested device ID, activates
        the returned session, imports the peer's secrets, uploads the device
        keys with their account and self-signing signatures, verifies the
        server's copy, and enables an advertised backup.

        [client_id] avoids dynamic registration; otherwise [client_metadata]
        defaults to {!Oauth.default_device_client_metadata}. [http] reaches the
        OAuth server and defaults to the client's transport. [timeout] bounds
        token polling and is also capped by the authorization response's expiry.
        [crypto_store] persists the new encryption machine's public and session
        state. [on_authenticated], when supplied, is called exactly once
        synchronously immediately after [Oauth.Token.finish_login] yields the
        authenticated session, before encryption state is created or any further
        network work. It receives the resolved [client_id] and the absolute
        [expires_at] from the OAuth token. Exceptions (including Eio
        cancellation) raised by the callback propagate and abort the login; the
        callback is not retried. [on_progress] runs synchronously and
        cancellation propagates.

        Transport and SDK errors raise [Eio.Io]; application-protocol failures
        raise {!Application_error}. *)

    val grant :
      env:< mono_clock : _ Eio.Time.Mono.t ; .. > ->
      ?advertise_protocols:bool ->
      ?timeout:float ->
      ?backup:Matrix_client.Qr_login.Msc4108.Secrets.backup ->
      authorize:(Uriz.t -> grant_decision) ->
      ?on_progress:(grant_progress -> unit) ->
      private_identity:Matrix_client.Cross_signing.private_identity ->
      encryption:Encryption.t ->
      Matrix_client.Qr_login.Msc4108.Secure_channel.established ->
      Client.t ->
      unit ->
      unit
    (** [grant ~env ~authorize ~private_identity ~encryption channel client ()]
        runs the existing-device role. The authenticated [client], [encryption],
        and [private_identity] must all belong to the same user and device.
        [authorize uri] presents the OAuth verification URI and explicitly
        confirms or cancels the grant. After confirmation, the operation waits
        up to [timeout] for the new device and sends it the cross-signing and
        optional backup secrets.

        [advertise_protocols] defaults to [true], pairing with a login using
        [Await_protocols]. Set it to [false] when the QR payload already gave
        the new device the homeserver and it uses [Homeserver_known]. The
        operation has the same exception, cleanup, callback, and cancellation
        behavior as {!val-login}. *)

    module Session : sig
      (** A complete caller-owned MSC4108 rendezvous session. QR rendering and
          scanning remain callbacks: this layer deliberately has no image codec
          or UI dependency. *)

      type progress =
        | Establishing_channel
        | Awaiting_check_code of int
        | OAuth of login_progress
        | Grant_oauth of grant_progress
        | Secrets
        | Trust_and_backup
        | Done

      type persistence =
        Matrix_client.Cross_signing.private_identity ->
        Matrix_client.Qr_login.Msc4108.Secrets.backup option ->
        unit

      type error =
        | Check_code_rejected
        | Secure_channel_error of
            Matrix_client.Qr_login.Msc4108.Secure_channel.error

      type Eio.Exn.err += Session_error of error

      val pp_error : Format.formatter -> error -> unit

      val login :
        env:< mono_clock : _ Eio.Time.Mono.t ; .. > ->
        transport:Matrix_client.Qr_login.Msc4108.Rendezvous.transport ->
        rendezvous_server:Uriz.t ->
        random:Matrix_client.Random.t ->
        display_qr:(Matrix_client.Qr_login.Msc4108.code -> unit) ->
        confirm_check_code:(int -> bool) ->
        ?persist:persistence ->
        ?http:Fetch.plain ->
        ?client_id:string ->
        ?client_metadata:Oauth.Registration.client_metadata ->
        ?scope:string list ->
        ?allow_insecure:bool ->
        ?timeout:float ->
        ?crypto_store:Matrix_client.Crypto_store.t ->
        ?start:login_start ->
        ?on_progress:(progress -> unit) ->
        ?on_authenticated:
          (Matrix_client.Client.session ->
          client_id:string ->
          expires_at:Ptime.t option ->
          unit) ->
        Client.t ->
        unit ->
        login
      (** [login] creates and displays a QR code, establishes the secure
          channel, requires [confirm_check_code], then runs the application
          login flow described above. [on_authenticated] is passed through to
          {!val:Application_eio.login}, with the same exactly-once timing and
          exception/cancellation behavior. The optional [persist] callback is
          the only persistence hook for the imported private seeds; this library
          never writes them. The callback must apply the caller's encrypted
          credential-storage policy before returning. *)

      val grant :
        env:< mono_clock : _ Eio.Time.Mono.t ; .. > ->
        transport:Matrix_client.Qr_login.Msc4108.Rendezvous.transport ->
        random:Matrix_client.Random.t ->
        scan_qr:(unit -> Matrix_client.Qr_login.Msc4108.code) ->
        expected_intent:Matrix_client.Qr_login.Msc4108.intent ->
        confirm_check_code:(int -> bool) ->
        ?advertise_protocols:bool ->
        ?timeout:float ->
        ?backup:Matrix_client.Qr_login.Msc4108.Secrets.backup ->
        authorize:(Uriz.t -> grant_decision) ->
        ?on_progress:(progress -> unit) ->
        private_identity:Matrix_client.Cross_signing.private_identity ->
        encryption:Encryption.t ->
        Client.t ->
        unit ->
        unit
      (** [grant] obtains a QR payload from [scan_qr], establishes the secure
          channel and requires local check-code confirmation before running the
          application grant flow described above. *)
    end
  end
end
