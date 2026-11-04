(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The steps of AT Protocol OAuth that every client shares.

    {!Xrpc_auth.Oauth}, a command-line client, and {!Xrpc_auth_web}, a web
    server's client, are both built on these. A client authenticates to the
    authorization server either not at all, as a public client, or with a signed
    [private_key_jwt] assertion, as a confidential one.

    @see <https://atproto.com/specs/oauth> AT Protocol OAuth *)

(** {1 Encoding} *)

val b64 : string -> string
(** [b64 s] is [s] in unpadded base64url. *)

val sha256 : string -> string
(** [sha256 s] is the raw SHA-256 digest of [s]. *)

val random : int -> string
(** [random n] is [n] random bytes in unpadded base64url, from the default
    Mirage RNG, which is started from the system's if it has not been. *)

val equal : string -> string -> bool
(** [equal a b] compares [a] and [b] in time that does not depend on where they
    differ. *)

type fields = (string * [ `S of string | `N of float | `L of string list ]) list
(** The string, number and string-list members of a JSON object. *)

val parse : string -> fields
(** [parse body] is the members of the JSON object [body], or [[]]. *)

val field : fields -> string -> string option

val required : string -> fields -> string -> string
(** [required what fields name] is the string [name] of [fields].

    @raise Failure naming [what] if it is missing. *)

val strip_slash : string -> string
(** [strip_slash s] is [s] without one trailing [/]. *)

val pct_encode : string -> string
(** [pct_encode s] percent-encodes every byte of [s] outside the RFC 3986
    unreserved set. *)

val encode_query : (string * string) list -> string
(** [encode_query pairs] is [pairs] as [application/x-www-form-urlencoded]. *)

val decode_query : string -> (string * string) list
(** [decode_query q] is the pairs of the query string [q], without its [?]. A
    pair without [=] is left out. *)

(** {1 Keys} *)

(** P-256 keys, for DPoP proofs and client assertions. *)
module Key : sig
  type t

  val generate : unit -> t
  (** [generate ()] is a new key. *)

  val to_string : t -> string
  (** [to_string k] is [k] as a JWK that includes its private scalar [d]. Store
      it only where nobody else can read it. *)

  val of_string : string -> t
  (** [of_string s] reads a key written by {!to_string}.

      @raise Invalid_argument if [s] holds no P-256 private key. *)

  val public_jwk : ?kid:string -> t -> Jsont.json
  (** [public_jwk ~kid k] is the public half of [k] as a JWK, with [kid] if
      given. It never includes [d]. *)

  val sign : t -> string -> string
  (** [sign k input] is the ES256 signature of [input], as JWS writes it: [r]
      and [s] in 64 bytes. *)
end

(** {1 DPoP} *)

val proof :
  Key.t ->
  now:float ->
  meth:string ->
  url:string ->
  ?nonce:string ->
  ?token:string ->
  unit ->
  string
(** [proof key ~now ~meth ~url ~nonce ~token ()] is a DPoP proof for a request,
    as RFC 9449 section 4.2 describes. [htu] is [url] without its query or
    fragment, and [token], when given, is bound by [ath]. *)

type dpop
(** A DPoP key and the nonces servers have asked for, one per origin. *)

val dpop : Key.t -> dpop
val dpop_key : dpop -> Key.t

val remember : dpop -> string -> Http.Header.t -> unit
(** [remember d url headers] keeps the [DPoP-Nonce] of a reply from [url]. *)

val nonce : dpop -> string -> string option
(** [nonce d url] is the nonce last given by [url]'s origin. *)

val www_error : Http.Header.t -> string option
(** [www_error headers] is the [error] of a [WWW-Authenticate] field. *)

(** {1 Client authentication} *)

type client =
  | Public of string  (** A public client, by its client ID. *)
  | Confidential of { id : string; key : Key.t; kid : string }
      (** A confidential client, by its client ID, that signs a
          [private_key_jwt] assertion with [key], published in its JWKS under
          [kid]. *)

val client_id : client -> string

val client_assertion :
  key:Key.t ->
  kid:string ->
  client_id:string ->
  issuer:string ->
  now:float ->
  string
(** [client_assertion ~key ~kid ~client_id ~issuer ~now] is an RFC 7523 client
    assertion: an ES256 JWT with [kid], whose [iss] and [sub] are [client_id]
    and whose [aud] is [issuer], with a random [jti], [iat] at [now] and [exp] a
    minute later. *)

val authentication :
  client -> issuer:string -> now:float -> (string * string) list
(** [authentication client ~issuer ~now] is the form fields that authenticate
    [client] to [issuer]: [client_id], and for a confidential client a fresh
    [client_assertion] and its type. *)

(** {1 Authorization servers} *)

val post_form :
  clock:_ Eio.Time.clock ->
  http:_ Fetch.t ->
  dpop ->
  ?client:client * string ->
  string ->
  (string * string) list ->
  fields
(** [post_form ~clock ~http dpop ~client url pairs] posts [pairs] to [url] with
    a DPoP proof, and is the JSON reply. [client], with the server's issuer,
    adds the client's {!authentication}, made afresh for each attempt. A reply
    demanding a DPoP nonce is answered by sending once more with it, as RFC 9449
    section 8 describes. Redirects are not followed.

    @raise Failure if the server refuses. *)

val get_json : http:_ Fetch.t -> string -> string
(** [get_json ~http url] is the body of a [200] answer to [GET url], at most 1
    MiB. Redirects are not followed.

    @raise Failure on any other status. *)

type server = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  par_endpoint : string;
  revocation_endpoint : string option;
}
(** An authorization server's endpoints. *)

val server_jsont : server Jsont.t

val server_of_pds : http:_ Fetch.t -> string -> server
(** [server_of_pds ~http pds] is the authorization server that [pds]'s
    protected-resource metadata names. Its metadata must name itself as [issuer]
    and list the PAR, authorization and token endpoints.

    @raise Failure otherwise. *)

val pds_of : ?plc:string -> http:_ Fetch.t -> string -> string * string
(** [pds_of ~plc ~http id] is the DID and PDS of the handle or DID [id].

    @raise Failure if either cannot be resolved. *)

val check_token_type : fields -> unit
(** [check_token_type token] holds for a DPoP-bound token response.

    @raise Failure otherwise. *)

val account_pds :
  ?plc:string ->
  http:_ Fetch.t ->
  issuer:string ->
  expected:string option ->
  pds:string option ->
  string ->
  string
(** [account_pds ~plc ~http ~issuer ~expected ~pds sub] is the PDS of the
    account [sub] that a token response names. With [expected], [sub] must be
    it, and [pds], which must be given, is returned. Without, [sub]'s own PDS
    must name [issuer] as its authorization server, or any server could claim
    any DID.

    @raise Failure otherwise.
    @raise Invalid_argument if [expected] is given without [pds]. *)
