(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 9421 HTTP Message Signatures for {!Fetch}.

    This module implements
    {{:https://datatracker.ietf.org/doc/html/rfc9421}RFC 9421} HTTP Message
    Signatures, enabling cryptographic signing and verification of HTTP request
    and response messages, and offers a {!Middleware.sign} wrapper that signs
    every request a {!Fetch} client makes.

    {2 Overview}

    HTTP Message Signatures provide a mechanism to create and verify
    cryptographic signatures on HTTP messages. The signature covers specific
    message components (headers, derived values) and metadata (timestamps, key
    identifiers).

    Headers are the wire-form {!Http.Header.t} that {!Fetch} uses throughout;
    {!Fetch.Header.of_http} and {!Fetch.Header.to_http} bridge to the typed
    header codecs when a caller prefers those.

    {2 Example: signing by hand}

    {[
      Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      let key =
        Fetch_signature.Key.ed25519
          ~priv:(Base64.decode_exn "private-key-bytes")
          ~pub:(Base64.decode_exn "public-key-bytes")
      in
      let config =
        Fetch_signature.config ~key ~keyid:"my-key-id"
          ~components:
            Fetch_signature.Component.[ method_; authority ]
          ()
      in
      let ctx =
        Fetch_signature.Context.request ~method_:`GET
          ~uri:(Uri.of_string "https://example.com/")
          ~headers
      in
      let signed =
        Fetch_signature.sign ~clock ~config ~context:ctx ~headers
        |> Result.get_ok
      in
      ...
    ]}

    {2 Example: signing every request}

    {[
      Eio.Switch.run @@ fun sw ->
      let t =
        Fetch_curl.v ~sw ()
        |> Fetch_signature.Middleware.sign ~clock:env#clock
             ~keyid:"https://me.example/actor#main-key" ~key
      in
      Fetch.post ~sw t "https://peer.example/inbox"
        ~body:(Fetch.String activity)
    ]}

    {2 References}
    {ul
     {- {{:https://datatracker.ietf.org/doc/html/rfc9421}RFC 9421} - HTTP
        Message Signatures}
     {- {{:https://datatracker.ietf.org/doc/html/rfc9530}RFC 9530} - Digest
        Fields}} *)

(** {1 Algorithms} *)

module Algorithm : sig
  (** Signature algorithms per
      {{:https://datatracker.ietf.org/doc/html/rfc9421#section-3.3}RFC 9421
      Section 3.3}. *)

  type t =
    [ `Rsa_pss_sha512  (** RSASSA-PSS using SHA-512 *)
    | `Rsa_v1_5_sha256  (** RSASSA-PKCS1-v1_5 using SHA-256 *)
    | `Hmac_sha256  (** HMAC using SHA-256 *)
    | `Ecdsa_p256_sha256  (** ECDSA using P-256 curve with SHA-256 *)
    | `Ecdsa_p384_sha384  (** ECDSA using P-384 curve with SHA-384 *)
    | `Ed25519  (** EdDSA using curve25519 *) ]

  val to_string : t -> string
  (** [to_string alg] returns the algorithm identifier string. *)

  val of_string : string -> t option
  (** [of_string s] parses an algorithm identifier string. *)

  val of_string_exn : string -> t
  (** [of_string_exn s] parses an algorithm identifier.
      @raise Invalid_argument if [s] is not an algorithm identifier. *)
end

(** {1 Message Components} *)

module Component : sig
  (** Message components that can be included in signatures per
      {{:https://datatracker.ietf.org/doc/html/rfc9421#section-2}RFC 9421
      Section 2}. *)

  (** {2 Derived Components}

      Derived components are computed from message context, not raw headers.
      Per {{:https://datatracker.ietf.org/doc/html/rfc9421#section-2.2}Section
      2.2}. *)

  type derived =
    [ `Method  (** [@method] - HTTP request method *)
    | `Authority  (** [@authority] - Target host (host:port) *)
    | `Path  (** [@path] - Request target path *)
    | `Query  (** [@query] - Query string with leading [?] *)
    | `Query_param of string  (** [@query-param;name="..."] - One parameter *)
    | `Target_uri  (** [@target-uri] - Full target URI *)
    | `Status  (** [@status] - Response status code (responses only) *)
    | `Request_target  (** [@request-target] - Deprecated form *) ]

  (** {2 Component Parameters}

      Parameters that modify component behavior per
      {{:https://datatracker.ietf.org/doc/html/rfc9421#section-2.1}Section
      2.1}. *)

  type param =
    [ `Sf  (** Strict structured field serialization *)
    | `Key of string  (** Dictionary member selection *)
    | `Bs  (** Byte sequence wrapping *)
    | `Tr  (** Trailer field designation *)
    | `Req  (** Request-bound component (for response signatures) *) ]

  type t = [ `Derived of derived * param list | `Field of string * param list ]
  (** A component identifier, either derived or a header field. *)

  (** {2 Constructors} *)

  val method_ : t
  (** The [@method] derived component. *)

  val authority : t
  (** The [@authority] derived component. *)

  val path : t
  (** The [@path] derived component. *)

  val query : t
  (** The [@query] derived component. *)

  val query_param : string -> t
  (** [query_param name] creates a [@query-param;name="..."] component. *)

  val target_uri : t
  (** The [@target-uri] derived component. *)

  val status : t
  (** The [@status] derived component (for responses). *)

  val request_target : t
  (** The [@request-target] derived component (deprecated). *)

  val field : string -> t
  (** [field name] creates a header field component (lowercased). *)

  val field_sf : string -> t
  (** [field_sf name] creates a header field with strict structured field
      serialization. *)

  val field_bs : string -> t
  (** [field_bs name] creates a header field with byte sequence wrapping. *)

  val field_key : string -> key:string -> t
  (** [field_key name ~key] creates a header field selecting a dictionary
      member. *)

  val field_req : string -> t
  (** [field_req name] creates a request-bound header field (for responses). *)

  (** {2 Common Fields} *)

  val content_type : t
  (** The [content-type] header field. *)

  val content_length : t
  (** The [content-length] header field. *)

  val content_digest : t
  (** The [content-digest] header field (RFC 9530). *)

  val date : t
  (** The [date] header field. *)

  val host : t
  (** The [host] header field. *)

  (** {2 Serialization} *)

  val to_identifier : t -> string
  (** [to_identifier c] returns the component identifier string. *)

  val of_identifier : string -> (t, string) result
  (** [of_identifier s] parses a component identifier. *)
end

(** {1 Signature Parameters} *)

module Params : sig
  (** Signature parameters per
      {{:https://datatracker.ietf.org/doc/html/rfc9421#section-2.3}RFC 9421
      Section 2.3}. *)

  type t
  (** Signature parameters. *)

  val empty : t
  (** Empty parameters. *)

  val created : Ptime.t -> t -> t
  (** [created time params] sets the creation timestamp. *)

  val expires : Ptime.t -> t -> t
  (** [expires time params] sets the expiration timestamp. *)

  val nonce : string -> t -> t
  (** [nonce value params] sets a unique nonce. *)

  val alg : Algorithm.t -> t -> t
  (** [alg algorithm params] sets the algorithm identifier. *)

  val keyid : string -> t -> t
  (** [keyid id params] sets the key identifier. *)

  val tag : string -> t -> t
  (** [tag value params] sets an application-specific tag. *)

  val get_created : t -> Ptime.t option
  val get_expires : t -> Ptime.t option
  val get_nonce : t -> string option
  val get_alg : t -> Algorithm.t option
  val get_keyid : t -> string option
  val get_tag : t -> string option
end

(** {1 Key Material} *)

module Key : sig
  (** Cryptographic key material for signing and verification. *)

  type t
  (** A key (may contain private key, public key, or both). *)

  (** {2 Symmetric Keys} *)

  val symmetric : string -> t
  (** [symmetric secret] creates a symmetric key for HMAC algorithms. *)

  (** {2 Ed25519 Keys} *)

  val ed25519 : priv:string -> pub:string -> t
  (** [ed25519 ~priv ~pub] creates an Ed25519 key pair. Both [priv] and [pub]
      should be raw 32-byte keys. *)

  val ed25519_priv : string -> t
  (** [ed25519_priv priv] creates an Ed25519 private key (for signing only). *)

  val ed25519_pub : string -> t
  (** [ed25519_pub pub] creates an Ed25519 public key (verification only). *)

  (** {2 ECDSA P-256 Keys} *)

  val p256 : priv:Mirage_crypto_ec.P256.Dsa.priv -> t
  (** [p256 ~priv] creates a P-256 key from the private key (public key
      derived automatically). *)

  val p256_pub : Mirage_crypto_ec.P256.Dsa.pub -> t
  (** [p256_pub pub] creates a P-256 public key (verification only). *)

  (** {2 ECDSA P-384 Keys} *)

  val p384 : priv:Mirage_crypto_ec.P384.Dsa.priv -> t
  (** [p384 ~priv] creates a P-384 key from the private key. *)

  val p384_pub : Mirage_crypto_ec.P384.Dsa.pub -> t
  (** [p384_pub pub] creates a P-384 public key (verification only). *)

  (** {2 RSA Keys} *)

  val rsa : priv:Mirage_crypto_pk.Rsa.priv -> t
  (** [rsa ~priv] creates an RSA key from the private key. *)

  val rsa_pub : Mirage_crypto_pk.Rsa.pub -> t
  (** [rsa_pub pub] creates an RSA public key (verification only). *)

  (** {2 Key Properties} *)

  val can_sign : t -> bool
  (** [can_sign key] returns [true] if the key can be used for signing. *)

  val can_verify : t -> bool
  (** [can_verify key] returns [true] if the key can be used for
      verification. *)

  val algorithm : t -> Algorithm.t option
  (** [algorithm key] returns the algorithm associated with the key, if
      known. *)
end

(** {1 Signing Context} *)

type request_ctx = {
  method_ : Http.Method.t;
  uri : Uri.t;
  headers : Http.Header.t;
}
(** Request context for signature computation. Contains the HTTP method,
    request URI, and request headers. *)

type response_ctx = {
  status : int;
  headers : Http.Header.t;
  request : request_ctx option;
}
(** Response context for signature computation. Contains the HTTP status code,
    response headers, and optionally the original request. *)

(** Context for resolving message components. *)
module Context : sig
  type t = [ `Request of request_ctx | `Response of response_ctx ]
  (** Message context (request or response). *)

  val request :
    method_:Http.Method.t -> uri:Uri.t -> headers:Http.Header.t -> t
  (** [request ~method_ ~uri ~headers] creates a request context. *)

  val response :
    status:int -> headers:Http.Header.t -> ?request:t -> unit -> t
  (** [response ~status ~headers ?request ()] creates a response context. The
      optional [request] context is used for request-bound components. *)

  val with_headers : t -> Http.Header.t -> t
  (** [with_headers ctx hs] is [ctx] with its own headers replaced by [hs],
      which is what {!sign_with_digest} uses to keep a context in step with a
      header it has just added. *)
end

(** {1 Content-Digest} *)

module Content_digest : sig
  (** RFC 9530 Content-Digest support.

      {{:https://datatracker.ietf.org/doc/html/rfc9530}RFC 9530} defines the
      Content-Digest header for message body integrity. *)

  type algorithm = [ `Sha256 | `Sha512 ]

  val compute : algorithm:algorithm -> body:string -> string
  (** [compute ~algorithm ~body] returns the Content-Digest header value. *)

  val add : algorithm:algorithm -> body:string -> Http.Header.t -> Http.Header.t
  (** [add ~algorithm ~body headers] adds a Content-Digest header, replacing
      any existing one. *)

  val verify : header:string -> body:string -> (unit, string) result
  (** [verify ~header ~body] verifies a Content-Digest header value. *)
end

(** {1 Signing Configuration} *)

type config
(** Configuration for signing requests. *)

val config :
  key:Key.t ->
  ?keyid:string ->
  ?components:Component.t list ->
  ?tag:string ->
  ?include_created:bool ->
  ?label:string ->
  unit ->
  config
(** [config ~key ?keyid ?components ?tag ?include_created ?label ()] creates a
    signing configuration.

    @param key The signing key.
    @param keyid Key identifier (included in signature parameters).
    @param components Components to sign. Default: {!default_components}.
    @param tag Application-specific tag.
    @param include_created Include creation timestamp. Default: [true].
    @param label Signature label for dictionary key. Default: ["sig1"]. *)

val default_components : Component.t list
(** Default components to sign: [\[@method; @authority; @path\]]. *)

(** {1 Signing} *)

type sign_error =
  [ `Key_algorithm_mismatch of string
  | `Missing_private_key
  | `Component_resolution_error of string
  | `Crypto_error of string ]

val sign_error_to_string : sign_error -> string
(** [sign_error_to_string err] returns a human-readable error message. *)

val sign :
  clock:_ Eio.Time.clock ->
  config:config ->
  context:Context.t ->
  headers:Http.Header.t ->
  (Http.Header.t, sign_error) result
(** [sign ~clock ~config ~context ~headers] signs the message and returns
    headers with [Signature-Input] and [Signature] headers added.

    @param clock Eio clock used for the [created] timestamp. *)

val sign_with_digest :
  clock:_ Eio.Time.clock ->
  config:config ->
  context:Context.t ->
  headers:Http.Header.t ->
  body:string ->
  digest_algorithm:Content_digest.algorithm ->
  (Http.Header.t, sign_error) result
(** [sign_with_digest ~clock ~config ~context ~headers ~body ~digest_algorithm]
    computes Content-Digest, adds it to [headers] {e and} to [context], and
    signs. The [content-digest] component is automatically added to the signed
    components if it is not there already.

    @param clock Eio clock used for the [created] timestamp. *)

(** {1 Verification} *)

type verify_error =
  [ `Missing_signature_header
  | `Missing_signature_input_header
  | `Invalid_signature_input of string
  | `Signature_label_not_found of string
  | `Key_algorithm_mismatch of string
  | `Missing_public_key
  | `Component_resolution_error of string
  | `Signature_mismatch
  | `Signature_expired
  | `Required_component_missing of string
  | `Crypto_error of string ]

val verify_error_to_string : verify_error -> string
(** [verify_error_to_string err] returns a human-readable error message. *)

type verify_result = {
  label : string;
  keyid : string option;
  created : Ptime.t option;
  expires : Ptime.t option;
  verified_components : Component.t list;
}
(** Successful verification result. *)

val verify :
  clock:_ Eio.Time.clock ->
  key:Key.t ->
  ?label:string ->
  ?max_age:Ptime.Span.t ->
  ?required_components:Component.t list ->
  context:Context.t ->
  headers:Http.Header.t ->
  unit ->
  (verify_result, verify_error) result
(** [verify ~clock ~key ?label ?max_age ?required_components ~context ~headers
     ()] verifies a signature on the message.

    Time validation per RFC 9421:
    - If [expires] is present and in the past, verification fails
    - If [created] is present and in the future (with 60s clock skew
      allowance), verification fails
    - If [max_age] is specified and [created] is older than [max_age],
      verification fails

    @param clock Eio clock used for time validation.
    @param key The verification key.
    @param label Signature label to verify. Default: first signature found.
    @param max_age
      Maximum age of signature. If [created] is present and older than
      [max_age], verification fails.
    @param required_components
      Components that must be signed. Verification fails if any are missing
      from the signature.
    @param context Message context for component resolution.
    @param headers Headers containing [Signature] and [Signature-Input]. *)

val verify_all :
  clock:_ Eio.Time.clock ->
  key_resolver:(string -> Key.t option) ->
  ?max_age:Ptime.Span.t ->
  context:Context.t ->
  headers:Http.Header.t ->
  unit ->
  (verify_result list, verify_error) result
(** [verify_all ~clock ~key_resolver ?max_age ~context ~headers ()] verifies
    all signatures on a message.

    @param clock Eio clock used for time validation.
    @param key_resolver
      Function to resolve keys by [keyid]. Called for each signature with its
      [keyid] parameter.
    @param max_age Maximum age for all signatures. *)

(** {1 Utilities} *)

val http_date : Ptime.t -> string
(** [http_date t] is [t] as an IMF-fixdate, the form the [Date] header takes,
    for instance ["Tue, 20 Apr 2021 02:07:55 GMT"]. *)

(** {1:middleware Signing a client} *)

(** A {!Fetch} wrapper that signs every request it passes on. *)
module Middleware : sig
  val sign :
    clock:_ Eio.Time.clock ->
    ?config:config ->
    ?digest_algorithm:Content_digest.algorithm ->
    ?add_date:bool ->
    key:Key.t ->
    _ Fetch.t ->
    Fetch.plain
  (** [sign ~clock ~key t] is [t] with an RFC 9421 signature added to every
      request, built with {!Fetch.Middleware.map_request}.

      [config] (default [config ~key ()]) supplies the label, keyid,
      components and tag; its own [key] field is always replaced by [key], so
      one configuration can be reused across keys. [digest_algorithm] (default
      [`Sha256]) is the {!Content_digest} algorithm. [add_date] (default
      [true]) adds a [Date] header when the request has none, since [date] is
      a common signed component and no backend derives it.

      A request with an [Empty] body is signed with {!val-sign} and one with a
      [String] body with {!sign_with_digest}, which adds [Content-Digest] and
      covers it.

      {b Streaming bodies cannot be signed.} RFC 9421 covers a body only
      through [Content-Digest], which needs the bytes in hand, and a [Stream]
      body is consumable once and may not even declare a length. A request
      whose body is [Fetch.Stream] therefore raises
      [Eio.Io (Fetch.E (Invalid_request _), _)]. Buffer such a body into a
      [Fetch.String] if it must be signed.

      Wrap the backend with this before any wrapper that adds headers of its
      own, so that everything to be covered is already in place:

      {[
        Fetch_curl.v ~sw ()
        |> Fetch_signature.Middleware.sign ~clock ~key
        |> Fetch.with_headers Fetch.Header.[ user_agent, "app/1.0" ]
      ]}

      is wrong — the user agent is added {e outside}, so it reaches the
      signing wrapper and is covered, but a header the {e backend} derives is
      not. [Host], [Content-Length] and [Transfer-Encoding] are set by the
      backend after this wrapper runs, so signing [content-length] or [host]
      fails with a component resolution error; sign [@authority] and
      [content-digest] instead.

      @raise Eio.Io
        with [Fetch.E (Invalid_request _)] if the body is a [Stream], or if
        signing fails (a missing private key, an unresolvable component). *)
end
