(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Claude AI model identifiers.

    This module provides type-safe model identifiers based on the Python SDK's
    model strings. Use polymorphic variants for known models with a custom
    escape hatch for future or unknown models. *)

type t =
  [ `Sonnet_4_5  (** claude-sonnet-4-5 - Most recent Sonnet model *)
  | `Sonnet_4  (** claude-sonnet-4 - Sonnet 4 model *)
  | `Sonnet_3_5  (** claude-sonnet-3-5 - Sonnet 3.5 model *)
  | `Opus_4_5  (** claude-opus-4-5 - Most recent Opus model *)
  | `Opus_4_1  (** claude-opus-4-1 - Opus 4.1 model *)
  | `Opus_4  (** claude-opus-4 - Opus 4 model for complex tasks *)
  | `Haiku_4  (** claude-haiku-4 - Fast, cost-effective Haiku model *)
  | `Custom of string  (** Custom model string for future/unknown models *) ]
(** The type of Claude models. *)

val to_string : t -> string
(** [to_string t] converts a model to its CLI string representation.

    Examples:
    - [`Sonnet_4_5] becomes "claude-sonnet-4-5"
    - [`Opus_4_5] becomes "claude-opus-4-5"
    - [`Opus_4] becomes "claude-opus-4"
    - [`Custom "my-model"] becomes "my-model" *)

val of_string : string -> t
(** [of_string s] parses a model string into a typed model.

    Known model strings are converted to their typed variants. Unknown strings
    become [`Custom s].

    Examples:
    - "claude-sonnet-4-5" becomes [`Sonnet_4_5]
    - "future-model" becomes [`Custom "future-model"] *)
