(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML sequence (array) values with metadata *)

type 'a t = {
  anchor : string option;
  tag : string option;
  implicit : bool;
  style : Layout_style.t;
  members : 'a list;
}

let make ?(anchor : string option) ?(tag : string option) ?(implicit = true)
    ?(style = `Any) members =
  { anchor; tag; implicit; style; members }

let members t = t.members
let anchor t = t.anchor
let tag t = t.tag
let implicit t = t.implicit
let style t = t.style
let with_anchor anchor t = { t with anchor = Some anchor }
let with_tag tag t = { t with tag = Some tag }
let with_style style t = { t with style }
let map f t = { t with members = List.map f t.members }
let length t = List.length t.members
let is_empty t = t.members = []
let nth t n = List.nth t.members n
let nth_opt t n = List.nth_opt t.members n
let iter f t = List.iter f t.members
let fold f init t = List.fold_left f init t.members

let pp pp_elem fmt t =
  Format.fprintf fmt "@[<hv 2>sequence(@,";
  Option.iter (Format.fprintf fmt "anchor=%s,@ ") t.anchor;
  Option.iter (Format.fprintf fmt "tag=%s,@ ") t.tag;
  Format.fprintf fmt "style=%a,@ " Layout_style.pp t.style;
  Format.fprintf fmt "members=[@,%a@]@,)"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       pp_elem)
    t.members

let equal eq a b =
  Option.equal String.equal a.anchor b.anchor
  && Option.equal String.equal a.tag b.tag
  && a.implicit = b.implicit
  && Layout_style.equal a.style b.style
  && List.equal eq a.members b.members

let compare cmp a b =
  let c = Option.compare String.compare a.anchor b.anchor in
  if c <> 0 then c
  else
    let c = Option.compare String.compare a.tag b.tag in
    if c <> 0 then c
    else
      let c = Bool.compare a.implicit b.implicit in
      if c <> 0 then c
      else
        let c = Layout_style.compare a.style b.style in
        if c <> 0 then c else List.compare cmp a.members b.members
