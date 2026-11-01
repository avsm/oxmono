(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = { value : 'a; style : Style.t }

let v ?(style = Style.none) value = { value; style }
let get t = t.value
let style t = t.style
let with_style style t = { t with style }
