(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let braille = [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ]

let ascii = [ "|"; "/"; "-"; "\\" ]
let still = [ "*" ]

let v fn = function
  | [] -> Fmt.invalid_arg "Console.%s: no frames" fn
  | frames -> List.map (Console_text.sanitize ~keep_newlines:false) frames

let at frames ~time =
  let x = time *. 10. in
  if (not (Float.is_finite x)) || x <= 0. then List.hd frames
  else
    List.nth frames
      (Float.to_int (Float.rem x (Float.of_int (List.length frames))))
