(** ATProto ES256K and ES256 service authentication with replay prevention. *)

exception Rejected
(** [Rejected] means a claim, key, signature or replay check failed. *)

val authenticate :
  resolve:(string -> string) ->
  consume:(issuer:string -> jti:string -> expires:float -> bool) ->
  audience:string ->
  meth:string ->
  now:float ->
  string ->
  string
(** [authenticate ~resolve ~consume ~audience ~meth ~now token] returns the
    verified issuer DID. [resolve] retrieves the current DID document through
    the application's bounded trusted resolver. [consume] must atomically
    persist the issuer/nonce until expiry and reject duplicates. It is called
    only after successful signature and claim checks. Transport and storage
    errors propagate. The accepted audiences are [audience] and that DID with
    the explicit [#tangled_spindle] service fragment. Tokens require [typ=JWT],
    [#atproto] key selection, an exact method, a nonempty bounded [jti],
    integral [iat]/[exp], and at most one hour of lifetime. *)
