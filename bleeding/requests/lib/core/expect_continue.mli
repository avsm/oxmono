(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP 100-Continue configuration

    Configuration for the HTTP 100-Continue protocol, which allows clients
    to check if the server will accept a request before sending a large body.
    Per RFC 9110 Section 10.1.1 (Expect) and Section 15.2.1 (100 Continue).

    {2 Usage}

    The simplest way to configure 100-continue is with the polymorphic variant:
    {[
      (* Use 100-continue for bodies >= 1MB (default) *)
      let session = Requests.create ~sw ~expect_100_continue:(`Threshold 1_048_576L) env

      (* Always use 100-continue *)
      let session = Requests.create ~sw ~expect_100_continue:`Always env

      (* Disable 100-continue *)
      let session = Requests.create ~sw ~expect_100_continue:`Disabled env
    ]} *)

(** {1 Configuration Types} *)

type config = [
  | `Disabled             (** Never use 100-continue *)
  | `Always               (** Always use 100-continue regardless of body size *)
  | `Threshold of int64   (** Use 100-continue for bodies >= threshold bytes *)
]
(** User-facing configuration as a polymorphic variant.

    - [`Disabled]: Never send Expect: 100-continue header
    - [`Always]: Always send Expect: 100-continue for requests with bodies
    - [`Threshold n]: Send Expect: 100-continue for bodies >= n bytes *)

type t
(** Internal configuration type with timeout. *)

(** {1 Default Values} *)

val default_threshold : int64
(** Default threshold: 1MB (1_048_576 bytes) *)

val default : t
(** Default configuration: [`Threshold 1_048_576L] with 1.0s timeout *)

val disabled : t
(** Configuration with 100-Continue disabled. *)

(** {1 Construction} *)

val of_config : ?timeout:float -> config -> t
(** [of_config ?timeout config] creates internal configuration from
    user-facing config. Timeout defaults to 1.0s. *)

val make :
  ?enabled:bool ->
  ?threshold:int64 ->
  ?timeout:float ->
  unit -> t
(** Create custom 100-Continue configuration. All parameters are optional
    and default to the values in {!default}. *)

(** {1 Accessors} *)

val enabled : t -> bool
(** Whether 100-continue is enabled. *)

val threshold : t -> int64
(** Body size threshold in bytes to trigger 100-continue. *)

val timeout : t -> float
(** Timeout in seconds to wait for 100 response. *)

val should_use : t -> int64 -> bool
(** [should_use t body_size] returns [true] if 100-continue should be used
    for a request with the given [body_size]. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for 100-Continue configuration. *)

val to_string : t -> string
(** Convert configuration to a human-readable string. *)

val pp_config : Format.formatter -> config -> unit
(** Pretty-printer for the user-facing config variant. *)
