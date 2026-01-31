(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  [ `Sonnet_4_5
  | `Sonnet_4
  | `Sonnet_3_5
  | `Opus_4_5
  | `Opus_4_1
  | `Opus_4
  | `Haiku_4
  | `Custom of string ]

let to_string = function
  | `Sonnet_4_5 -> "claude-sonnet-4-5"
  | `Sonnet_4 -> "claude-sonnet-4"
  | `Sonnet_3_5 -> "claude-sonnet-3-5"
  | `Opus_4_5 -> "claude-opus-4-5"
  | `Opus_4_1 -> "claude-opus-4-1"
  | `Opus_4 -> "claude-opus-4"
  | `Haiku_4 -> "claude-haiku-4"
  | `Custom s -> s

let of_string = function
  | "claude-sonnet-4-5" -> `Sonnet_4_5
  | "claude-sonnet-4" -> `Sonnet_4
  | "claude-sonnet-3-5" -> `Sonnet_3_5
  | "claude-opus-4-5" -> `Opus_4_5
  | "claude-opus-4-1" -> `Opus_4_1
  | "claude-opus-4" -> `Opus_4
  | "claude-haiku-4" -> `Haiku_4
  | s -> `Custom s
