(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string array

let pp ppf t = Fmt.pf ppf "spinner[%d frames]" (Array.length t)

let equal a b =
  Array.length a = Array.length b
  && Array.for_all2 (fun x y -> String.equal x y) a b

let v frames =
  if Array.length frames = 0 then invalid_arg "Console.Spinner.v: no frames";
  Array.copy frames

let frames t = Array.copy t

let braille = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let ascii = [| "|"; "/"; "-"; "\\" |]

let frame t i =
  let n = Array.length t in
  t.(((i mod n) + n) mod n)

let at ?(fps = 10.) t ~elapsed =
  if (not Float.(is_finite fps)) || fps <= 0. then
    invalid_arg "Console.Spinner.at: fps must be positive";
  if (not (Float.is_finite elapsed)) || elapsed <= 0. then t.(0)
  else
    let cycle = Float.of_int (Array.length t) /. fps in
    frame t (int_of_float (Float.rem elapsed cycle *. fps))

let anim ?fps t = Anim.v (fun ~elapsed -> at ?fps t ~elapsed)
