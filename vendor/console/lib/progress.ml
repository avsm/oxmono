(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = [ `Auto | `Tty | `Plain | `Quiet | `Json ]

let to_string = function
  | `Auto -> "auto"
  | `Tty -> "tty"
  | `Plain -> "plain"
  | `Quiet -> "quiet"
  | `Json -> "json"

let all =
  List.map (fun p -> (to_string p, p)) [ `Auto; `Tty; `Plain; `Quiet; `Json ]

let of_string s =
  match List.assoc_opt s all with
  | Some p -> Ok p
  | None ->
      Fmt.error_msg "unknown progress %S, expected one of: %s" s
        (String.concat ", " (List.map fst all))

module Layout = Progress_layout

let pp_bytes ppf n = Fmt.string ppf (Task_text.bytes n)
let row = Progress_show.row
let tree = Progress_show.tree
let summary = Progress_show.summary
let view = Progress_show.view

let show ?progress ?layout ?view ?keep_ended ?commit ?plain ?delay ?size ppf
    root =
  Progress_show.show_with ?progress ?layout ?view ?keep_ended ?commit ?plain
    ?delay ?size ppf root
