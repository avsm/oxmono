(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP 100-Continue configuration

    Configuration for the HTTP 100-Continue protocol, which allows clients
    to check if the server will accept a request before sending a large body.
    Per RFC 9110 Section 10.1.1 (Expect) and Section 15.2.1 (100 Continue). *)

(** User-facing configuration as a polymorphic variant *)
type config = [
  | `Disabled             (** Never use 100-continue *)
  | `Always               (** Always use 100-continue regardless of body size *)
  | `Threshold of int64   (** Use 100-continue for bodies >= threshold bytes *)
]

(** Internal representation *)
type t = {
  enabled : bool;
  threshold : int64;
  timeout : float;
}

let default_threshold = 1_048_576L  (* 1MB *)

let default = {
  enabled = true;
  threshold = default_threshold;
  timeout = 1.0;            (* 1 second *)
}

let of_config ?(timeout = 1.0) (config : config) : t =
  match config with
  | `Disabled -> { enabled = false; threshold = 0L; timeout }
  | `Always -> { enabled = true; threshold = 0L; timeout }
  | `Threshold n -> { enabled = true; threshold = n; timeout }

let make ?(enabled = true) ?(threshold = 1_048_576L) ?(timeout = 1.0) () =
  { enabled; threshold; timeout }

let disabled = { enabled = false; threshold = 0L; timeout = 0.0 }

let enabled t = t.enabled
let threshold t = t.threshold
let timeout t = t.timeout

let should_use t body_size =
  t.enabled && body_size >= t.threshold

let pp fmt t =
  if not t.enabled then
    Format.fprintf fmt "100-continue: disabled"
  else if t.threshold = 0L then
    Format.fprintf fmt "100-continue: always (timeout: %.2fs)" t.timeout
  else
    Format.fprintf fmt "100-continue: threshold %Ld bytes (timeout: %.2fs)"
      t.threshold t.timeout

let to_string t = Format.asprintf "%a" pp t

let pp_config fmt (config : config) =
  match config with
  | `Disabled -> Format.fprintf fmt "`Disabled"
  | `Always -> Format.fprintf fmt "`Always"
  | `Threshold n -> Format.fprintf fmt "`Threshold %Ld" n
