(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP request retry logic with exponential backoff

    This module provides configurable retry logic for HTTP requests,
    including exponential backoff, custom retry predicates, and
    Retry-After header support per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.3}RFC 9110 Section 10.2.3}.

    {2 Custom Retry Predicates}

    Per Recommendation #14: You can define custom predicates to control
    retry behavior beyond the built-in status code and method checks.

    {b Example: Retry on specific error responses}
    {[
      let retry_on_rate_limit method_ status headers =
        status = 429 && Headers.get "x-retry-allowed" headers = Some "true"
      in
      let config = Retry.create_config
        ~retry_response:retry_on_rate_limit
        ()
    ]}

    {b Example: Retry on custom exceptions}
    {[
      let retry_on_network_error = function
        | Unix.Unix_error (Unix.ECONNRESET, _, _) -> true
        | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> true
        | _ -> false
      in
      let config = Retry.create_config
        ~retry_exception:retry_on_network_error
        ()
    ]}
*)

open Eio

(** Log source for retry operations *)
val src : Logs.Src.t

(** {1 Custom Retry Predicates}

    Per Recommendation #14: Allow user-defined retry logic. *)

(** Custom retry predicate for responses.
    Receives (method, status, headers) and returns true to retry.
    This runs in addition to the built-in status_forcelist check. *)
type response_predicate = Method.t -> int -> Headers.t -> bool

(** Custom retry predicate for exceptions.
    Returns true if the exception should trigger a retry. *)
type exception_predicate = exn -> bool

(** {1 Configuration} *)

(** Retry configuration *)
type config = {
  max_retries : int;              (** Maximum number of retry attempts *)
  backoff_factor : float;         (** Exponential backoff multiplier *)
  backoff_max : float;            (** Maximum backoff time in seconds *)
  status_forcelist : int list;    (** HTTP status codes to retry *)
  allowed_methods : Method.t list; (** Methods safe to retry *)
  respect_retry_after : bool;     (** Honor Retry-After response header *)
  jitter : bool;                  (** Add randomness to prevent thundering herd *)
  retry_response : response_predicate option;  (** Custom response retry predicate *)
  retry_exception : exception_predicate option;  (** Custom exception retry predicate *)
  strict_method_semantics : bool;
  (** When true, raise an error if asked to retry a non-idempotent method.
      Per RFC 9110 Section 9.2.2: Non-idempotent methods should not be retried
      automatically as the request may have already been processed. Default is
      false (just log and skip retry). *)
}

(** Default retry configuration *)
val default_config : config

(** Create a custom retry configuration.
    @param retry_response Custom predicate for response-based retry decisions
    @param retry_exception Custom predicate for exception-based retry decisions
    @param strict_method_semantics When true, raise error on non-idempotent retry *)
val create_config :
  ?max_retries:int ->
  ?backoff_factor:float ->
  ?backoff_max:float ->
  ?status_forcelist:int list ->
  ?allowed_methods:Method.t list ->
  ?respect_retry_after:bool ->
  ?jitter:bool ->
  ?retry_response:response_predicate ->
  ?retry_exception:exception_predicate ->
  ?strict_method_semantics:bool ->
  unit -> config

(** {1 Retry Decision Functions} *)

(** Check if a request should be retried based on built-in rules only.
    For full custom predicate support, use [should_retry_response]. *)
val should_retry : config:config -> method_:Method.t -> status:int -> bool

(** Check if a response should be retried, including custom predicates.
    Returns true if either built-in rules or custom predicate says to retry. *)
val should_retry_response : config:config -> method_:Method.t -> status:int -> headers:Headers.t -> bool

(** Check if an exception should trigger a retry using custom predicates. *)
val should_retry_exn : config:config -> exn -> bool

(** Calculate backoff delay for a given attempt *)
val calculate_backoff : config:config -> attempt:int -> float

(** Parse Retry-After header value (seconds or HTTP date).

    Per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.3}RFC 9110 Section 10.2.3},
    Retry-After can be either:
    - A non-negative integer (delay in seconds)
    - An HTTP-date (absolute time to retry after)

    Values are capped to [backoff_max] (default 120s) to prevent DoS
    from malicious servers specifying extremely long delays. *)
val parse_retry_after : ?backoff_max:float -> string -> float option

(** Execute a request with retry logic *)
val with_retry :
  sw:Switch.t ->
  clock:_ Time.clock ->
  config:config ->
  f:(unit -> 'a) ->
  should_retry_exn:(exn -> bool) ->
  'a

(** Pretty print retry configuration *)
val pp_config : Format.formatter -> config -> unit

(** Log retry attempt information *)
val log_retry : attempt:int -> delay:float -> reason:string -> unit