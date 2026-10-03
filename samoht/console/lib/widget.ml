(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let border ?theme ?border ~default () =
  match border with
  | Some border -> border
  | None -> Option.fold ~none:default ~some:Theme.border theme

let animated = function
  | None -> false
  | Some theme -> Theme.animated_border theme
