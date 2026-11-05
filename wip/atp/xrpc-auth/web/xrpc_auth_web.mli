(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Logging people in to a web site with their AT Protocol account.

    The site is a confidential OAuth client. It publishes its client metadata
    and its public keys at HTTPS URLs on its own origin, and signs a
    [private_key_jwt] assertion with every request to an authorization server,
    so that the server knows the request comes from the site.

    A login has two halves. {!start} resolves the account, pushes an
    authorization request and gives the URL to send the browser to, with a
    {!pending} record that the site keeps on the server, keyed by its
    {!pending_state}. The browser comes back to the site's redirect URI, and
    {!finish} checks what it brings against the pending record, exchanges the
    code and is the account that logged in. Nothing secret goes to the browser:
    the PKCE verifier and the DPoP key stay in the pending record.

    The site learns who logged in, then forgets the account's tokens. It should
    give the person a session of its own, such as a cookie, rather than keep the
    tokens.

    {b Fetching what users name.} Resolving an account fetches URLs that the
    person logging in chooses: a handle's host, a DID document and the servers
    it names. Give {!start} and {!finish} a client that cannot reach the site's
    own network, such as one built on {!Public_net.connect}.

    @see <https://atproto.com/specs/oauth> AT Protocol OAuth *)

module Key = Xrpc_auth_protocol.Key
(** P-256 keys. A site makes one with {!Key.generate}, keeps {!Key.to_string} of
    it readable only by itself, and publishes its public half through
    {!Client.jwks}. *)

(** {1 Clients} *)

module Client : sig
  type t
  (** A site's OAuth client. *)

  val v :
    client_id:string ->
    redirect_uri:string ->
    jwks_uri:string ->
    key:Key.t ->
    kid:string ->
    ?scope:string ->
    ?client_name:string ->
    ?client_uri:string ->
    unit ->
    t
  (** [v ~client_id ~redirect_uri ~jwks_uri ~key ~kid ()] is the client whose
      metadata document is served at [client_id], whose authorization responses
      return to [redirect_uri], and whose signing key [key] is published at
      [jwks_uri] under [kid]. [scope] is ["atproto"] by default, which is enough
      to learn who logged in.

      @raise Invalid_argument
        unless the three URLs are HTTPS URLs without a fragment on one origin,
        if [client_uri] is not a parent of [client_id], as Bluesky's
        authorization server requires, or if [scope] lacks [atproto]. *)

  val id : t -> string
  val redirect_uri : t -> string
  val scope : t -> string

  val metadata : t -> Jsont.json
  (** [metadata t] is the client metadata document to serve at [id t]: a web
      application that authenticates with [private_key_jwt] and ES256, wants
      DPoP-bound tokens, and names [jwks_uri]. *)

  val jwks : t -> Jsont.json
  (** [jwks t] is the JWK set to serve at [jwks_uri]. It holds the public key
      alone, never the private scalar. *)
end

(** {1 Logging in} *)

type pending
(** A login started by {!start} and not yet finished. It holds the PKCE verifier
    and the DPoP key, so it must stay on the server. *)

val pending_state : pending -> string
(** [pending_state p] is the [state] that the browser brings back, at least 128
    random bits, by which the site finds [p]. *)

val pending_expires_at : pending -> float
(** [pending_expires_at p] is when [p] stops being accepted, in POSIX seconds.
*)

val pending_jsont : pending Jsont.t
(** [pending_jsont] encodes a pending login for the site's store. *)

type identity = {
  did : string;  (** The account that logged in. *)
  handle : string option;
      (** Its handle, only if the DID document names it and the handle's host
          names the DID in turn. *)
  pds : string;  (** Its PDS. *)
  scope : string;  (** The scope the authorization server granted. *)
}

type error =
  [ `Expired  (** The pending login is too old. *)
  | `Wrong_state  (** The browser brought another login's [state]. *)
  | `Duplicate of string  (** A callback parameter was given twice. *)
  | `Missing of string  (** A callback parameter is missing. *)
  | `Wrong_issuer
    (** The callback names another authorization server than the one the login
        was pushed to. *)
  | `Denied of string * string option
    (** The authorization server refused, with an error code and maybe a
        description. Both come from the callback's query, so a page that shows
        them must escape them. *)
  | `Refused of string
    (** A step failed: the account could not be resolved, a server refused a
        request, or the token response was not acceptable. *) ]

val error_to_string : error -> string

val start :
  clock:_ Eio.Time.clock ->
  http:_ Fetch.t ->
  ?plc:string ->
  ?ttl:float ->
  ?scope:string ->
  Client.t ->
  identifier:string ->
  (pending * string, error) result
(** [start ~clock ~http client ~identifier] begins a login, and is the pending
    login and the URL to redirect the browser to.

    [identifier] is a handle or DID, whose PDS, found through [plc] for a
    [did:plc], selects the authorization server and is passed as [login_hint].
    It may instead be the HTTPS URL of a PDS or entryway, which selects the
    server and lets the person choose the account there. The request is pushed
    with PKCE S256 and a fresh DPoP key, and authenticated with the client's
    assertion. The pending login is accepted for [ttl] seconds, 600 by default.

    [scope] is the client's scope by default. A narrower one asks for less,
    so that a site can log in with [atproto] alone and ask for more only for
    an action that needs it. Each of its words must be in the client's scope,
    and it must include [atproto]. *)

val finish :
  clock:_ Eio.Time.clock ->
  http:_ Fetch.t ->
  ?plc:string ->
  Client.t ->
  pending ->
  query:string ->
  (identity, error) result
(** [finish ~clock ~http client pending ~query] completes [pending] with the
    query string the browser brought to the redirect URI, without its [?].

    The site must take [pending] out of its store before calling [finish], so
    that a login cannot be finished twice, and must not call it after
    {!pending_expires_at}, which [finish] also checks.

    The callback's [state] must be [pending]'s, compared in constant time, and
    its [iss] must be the authorization server's. No parameter may appear twice.
    The code is exchanged with the PKCE verifier, the DPoP key and the client's
    assertion, and the token must be DPoP-bound, carry the [atproto] scope, and
    name the account given to {!start}. When {!start} was given a server, the
    account's own PDS must name the same authorization server. The tokens are
    then revoked, as far as the server allows, since only the identity is
    wanted. *)

type call = ?body:string -> Http.Method.t -> string -> int * string
(** A [call] makes requests to the account's PDS on its behalf. [call ?body
    meth path] sends [meth] to [path], an absolute path with any query on the
    PDS, with [body] as JSON, and is the status and body of the answer, which
    is read up to 1 MiB. It does not follow redirects.

    @raise Invalid_argument if [path] does not start with [/]. *)

val finish_with :
  clock:_ Eio.Time.clock ->
  http:_ Fetch.t ->
  ?plc:string ->
  Client.t ->
  pending ->
  query:string ->
  (identity -> call -> 'a) ->
  (identity * 'a, error) result
(** [finish_with ~clock ~http client pending ~query use] is {!finish} that
    also applies [use] to the identity and a {!call} that carries the access
    token, before the tokens are revoked. [use] must not keep the call. It
    should check that [identity.scope] grants what it needs, since an
    authorization server may grant less than was asked. A [Failure] that
    [use] raises is returned as [`Refused]. *)

(** {1 Reaching only public hosts} *)

module Public_net : sig
  val is_public : Eio.Net.Ipaddr.v4v6 -> bool
  (** [is_public ip] holds for a globally routable unicast address. It is false
      for unspecified, loopback, private, shared, link-local, documentation,
      benchmarking, multicast and reserved IPv4 ranges, and for the IPv6
      unspecified, loopback, unique-local, link-local and multicast ranges, and
      for IPv4-mapped, IPv4-compatible and NAT64 addresses whose embedded IPv4
      address is not public. *)

  val connect : _ Eio.Net.t -> Fetch_httpz.connect
  (** [connect net] connects as {!Fetch_httpz}'s default does, but only to
      addresses that are {!is_public}. A host whose every address is not public
      is refused before any connection is made. Checking the address a name
      resolves to, at connection time, is what stops a name that points at the
      site's own network. *)
end
