(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML mapping (object) values with metadata *)

type ('k, 'v) t = {
  anchor : string option;
  tag : string option;
  implicit : bool;
  style : Layout_style.t;
  members : ('k * 'v) list;
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

let map_keys f t =
  { t with members = List.map (fun (k, v) -> (f k, v)) t.members }

let map_values f t =
  { t with members = List.map (fun (k, v) -> (k, f v)) t.members }

let map f t = { t with members = List.map (fun (k, v) -> f k v) t.members }
let length t = List.length t.members
let is_empty t = t.members = []

let find pred t =
  List.find_opt (fun (k, _) -> pred k) t.members |> Option.map snd

let find_key pred t = List.find_opt (fun (k, _) -> pred k) t.members
let mem pred t = List.exists (fun (k, _) -> pred k) t.members
let keys t = List.map fst t.members
let values t = List.map snd t.members
let iter f t = List.iter (fun (k, v) -> f k v) t.members
let fold f init t = List.fold_left (fun acc (k, v) -> f acc k v) init t.members

let pp pp_key pp_val fmt t =
  Format.fprintf fmt "@[<hv 2>mapping(@,";
  Option.iter (Format.fprintf fmt "anchor=%s,@ ") t.anchor;
  Option.iter (Format.fprintf fmt "tag=%s,@ ") t.tag;
  Format.fprintf fmt "style=%a,@ " Layout_style.pp t.style;
  Format.fprintf fmt "members={@,";
  List.iteri
    (fun i (k, v) ->
      if i > 0 then Format.fprintf fmt ",@ ";
      Format.fprintf fmt "@[<hv 2>%a:@ %a@]" pp_key k pp_val v)
    t.members;
  Format.fprintf fmt "@]@,})"

let equal eq_k eq_v a b =
  Option.equal String.equal a.anchor b.anchor
  && Option.equal String.equal a.tag b.tag
  && a.implicit = b.implicit
  && Layout_style.equal a.style b.style
  && List.equal
       (fun (k1, v1) (k2, v2) -> eq_k k1 k2 && eq_v v1 v2)
       a.members b.members

let compare cmp_k cmp_v a b =
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
        if c <> 0 then c
        else
          let cmp_pair (k1, v1) (k2, v2) =
            let c = cmp_k k1 k2 in
            if c <> 0 then c else cmp_v v1 v2
          in
          List.compare cmp_pair a.members b.members
