(** random — cryptographic randomness as a capability.

    Every value in this library that needs unpredictable bytes draws them from a
    {!t} passed in explicitly, so a caller that holds none cannot consume
    randomness. A {!t} wraps an [Eio.Flow.source], in practice
    [env#secure_random], and carries no mutable state of its own, so it is safe
    to share across fibers and domains. *)

type t
(** The type for sources of cryptographically secure random bytes. *)

val of_source : _ Eio.Flow.source -> t
(** [of_source src] reads randomness from the flow [src].

    [src] must be a cryptographically secure source, and nothing here checks
    that. {!of_env} is what supplies one outside a test. *)

val of_env : < secure_random : _ Eio.Flow.source ; .. > -> t
(** [of_env env] is [of_source env#secure_random], the standard Eio capability
    for the system CSPRNG. *)

val generate : t -> int -> string
(** [generate t n] is a fresh string of [n] cryptographically secure random
    bytes, and [""] for [n = 0].

    Each call allocates its own buffer, so concurrent calls on the same [t] from
    different fibers or domains cannot interfere. The temporary read buffer is
    cleared after copying, but the returned immutable OCaml string cannot be
    reliably zeroised; see {!Crypto_key} for the process-memory policy.

    Raises [Invalid_argument] if [n] is negative. *)

val txn_id : t -> string
(** [txn_id t] is a fresh Matrix transaction identifier, namely ["m"] followed
    by the unpadded URL-safe base64 of 16 random bytes. It contains only
    unreserved path-segment characters, and is what every [{txnId}] path segment
    in this library is filled from.

    @see <https://spec.matrix.org/v1.11/client-server-api/#transaction-identifiers>
      Transaction Identifiers *)
