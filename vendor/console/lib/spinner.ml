(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let braille = Frames.braille
let ascii = Frames.ascii

(* A spinner is as wide as its widest frame, and no wider than its room. *)
let view ?frames () =
  let frames = Option.map (Frames.v "Spinner.view") frames in
  let frames (look : Look.t) =
    let frames = Option.value frames ~default:(Theme.spinner look.theme) in
    if look.utf_8 || List.for_all Width.is_ascii frames then frames
    else Frames.ascii
  in
  let moves look = match frames look with [ _ ] -> false | _ -> true in
  View_core.deferred
    ~kind:(fun look -> if moves look then View_core.timed else View_core.static)
    (fun c ->
      let frames = frames (Ctx.look c) in
      let widest =
        List.fold_left (fun n f -> max n (Width.string_width f)) 0 frames
      in
      let frame = View_core.text (Frames.at frames ~time:(Ctx.time c)) in
      let cells =
        match Ctx.width c with
        | Some room -> min widest (max 0 room)
        | None -> widest
      in
      if View_core.width frame > cells then
        Widget.truncate (Ctx.look c) cells frame
      else View_core.pad_to cells frame)
