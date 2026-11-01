(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let at ?(theme = Theme.default) ?(utf_8 = true) ?(live = true) ?width ~time v =
  let look = { Look.theme; utf_8; live } in
  View_core.to_string
    (View_core.paint look (View_core.resolve (Ctx.v ?width ~time look) v))

let laid ?(live = false) ?(tasks = false) () =
  let calls = ref 0 in
  let v =
    View_core.deferred
      ~kind:(fun _ -> { View_core.static with live; tasks })
      (fun c ->
        incr calls;
        View_core.text (String.make (Option.value (Ctx.width c) ~default:3) 'x'))
  in
  (v, calls)
