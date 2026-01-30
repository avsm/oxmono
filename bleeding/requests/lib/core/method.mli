(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP request methods per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-9}RFC 9110 Section 9}

    HTTP methods indicate the desired action to be performed on a resource.
    The method token is case-sensitive.

    {2 Safe Methods}

    Methods are considered "safe" if their semantics are read-only (GET, HEAD,
    OPTIONS, TRACE). Per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-9.2.1}RFC 9110 Section 9.2.1}.

    {2 Idempotent Methods}

    A method is "idempotent" if multiple identical requests have the same effect
    as a single request (GET, HEAD, PUT, DELETE, OPTIONS, TRACE).
    Per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-9.2.2}RFC 9110 Section 9.2.2}. *)

(** Log source for method operations *)
val src : Logs.Src.t

(** HTTP method type using polymorphic variants for better composability *)
type t = [
  | `GET      (** Retrieve a resource *)
  | `POST     (** Submit data to be processed *)
  | `PUT      (** Replace a resource *)
  | `DELETE   (** Delete a resource *)
  | `HEAD     (** Retrieve headers only *)
  | `OPTIONS  (** Retrieve allowed methods *)
  | `PATCH    (** Partial resource modification *)
  | `CONNECT  (** Establish tunnel to server *)
  | `TRACE    (** Echo received request *)
  | `Other of string  (** Non-standard or extension method *)
]

(** {1 Conversion Functions} *)

val to_string : t -> string
(** Convert method to uppercase string representation *)

val of_string : string -> t
(** Parse method from string (case-insensitive).
    Returns [`Other s] for unrecognized methods. *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for methods *)

(** {1 Method Properties} *)

val is_safe : t -> bool
(** Returns true for safe methods (GET, HEAD, OPTIONS, TRACE).
    Safe methods should not have side effects. *)

val is_idempotent : t -> bool
(** Returns true for idempotent methods (GET, HEAD, PUT, DELETE, OPTIONS, TRACE).
    Idempotent methods can be called multiple times with the same result. *)

(** Request body semantics per RFC 9110 Section 9.3 *)
type body_semantics =
  | Body_required   (** Method requires a body (POST, PUT, PATCH) *)
  | Body_optional   (** Method MAY have a body (DELETE, OPTIONS, GET) *)
  | Body_forbidden  (** Method MUST NOT have a body (HEAD, TRACE, CONNECT) *)

val request_body_semantics : t -> body_semantics
(** Returns the request body semantics for a method per RFC 9110.

    - {!Body_required}: POST, PUT, PATCH - body is expected
    - {!Body_optional}: DELETE, OPTIONS, GET - body allowed but has no defined semantics
    - {!Body_forbidden}: HEAD, TRACE, CONNECT - body MUST NOT be sent *)

val has_request_body : t -> bool
(** Returns true for methods that typically have a request body (POST, PUT, PATCH).
    @deprecated Use {!request_body_semantics} for more accurate RFC 9110 semantics. *)

val is_cacheable : t -> bool
(** Returns true for methods whose responses are cacheable by default (GET, HEAD, POST).
    Note: POST is only cacheable with explicit cache headers. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** Compare two methods for equality *)

val compare : t -> t -> int
(** Compare two methods for ordering *)