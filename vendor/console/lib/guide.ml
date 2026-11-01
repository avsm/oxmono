(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type glyphs = { branch : string; last : string; pipe : string; space : string }
type t = glyphs Inked.t

let v ?style ~branch ~last ~pipe ~space () =
  Inked.v ?style { branch; last; pipe; space }

let branch t = (Inked.get t).branch
let last t = (Inked.get t).last
let pipe t = (Inked.get t).pipe
let space t = (Inked.get t).space
let style = Inked.style
let with_style = Inked.with_style
let ascii = v ~branch:"+-- " ~last:"+-- " ~pipe:"|   " ~space:"    " ()

let single = v ~branch:"├── " ~last:"└── " ~pipe:"│   " ~space:"    " ()

let pp ppf t =
  let t = Inked.get t in
  Fmt.pf ppf "{ branch = %S; last = %S; pipe = %S; space = %S }" t.branch t.last
    t.pipe t.space
