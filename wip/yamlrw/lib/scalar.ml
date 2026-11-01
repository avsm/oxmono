(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML scalar values with metadata *)

type t = {
  anchor : string option;
  tag : string option;
  value : string;
  plain_implicit : bool;
  quoted_implicit : bool;
  style : Scalar_style.t;
}

let make ?(anchor : string option) ?(tag : string option)
    ?(plain_implicit = true) ?(quoted_implicit = false) ?(style = `Plain) value
    =
  { anchor; tag; value; plain_implicit; quoted_implicit; style }

let value t = t.value
let anchor t = t.anchor
let tag t = t.tag
let style t = t.style
let plain_implicit t = t.plain_implicit
let quoted_implicit t = t.quoted_implicit
let with_anchor anchor t = { t with anchor = Some anchor }
let with_tag tag t = { t with tag = Some tag }
let with_style style t = { t with style }

let pp fmt t =
  Format.fprintf fmt "scalar(%S" t.value;
  Option.iter (Format.fprintf fmt ", anchor=%s") t.anchor;
  Option.iter (Format.fprintf fmt ", tag=%s") t.tag;
  Format.fprintf fmt ", style=%a)" Scalar_style.pp t.style

let equal a b =
  Option.equal String.equal a.anchor b.anchor
  && Option.equal String.equal a.tag b.tag
  && String.equal a.value b.value
  && a.plain_implicit = b.plain_implicit
  && a.quoted_implicit = b.quoted_implicit
  && Scalar_style.equal a.style b.style

let compare a b =
  let c = Option.compare String.compare a.anchor b.anchor in
  if c <> 0 then c
  else
    let c = Option.compare String.compare a.tag b.tag in
    if c <> 0 then c
    else
      let c = String.compare a.value b.value in
      if c <> 0 then c
      else
        let c = Bool.compare a.plain_implicit b.plain_implicit in
        if c <> 0 then c
        else
          let c = Bool.compare a.quoted_implicit b.quoted_implicit in
          if c <> 0 then c else Scalar_style.compare a.style b.style
