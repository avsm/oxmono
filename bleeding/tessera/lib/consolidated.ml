(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  order : string list;  (* Node paths in document order. *)
  nodes : (string, Jsont.json) Hashtbl.t;
  zones : int list;
}

(* A path is stored without its slashes so that ["utm30"],
   ["/utm30"] and ["utm30/"] all reach the same node. *)
let normalise path =
  let n = String.length path in
  let i = ref 0 and j = ref n in
  while !i < !j && path.[!i] = '/' do
    incr i
  done;
  while !j > !i && path.[!j - 1] = '/' do
    decr j
  done;
  String.sub path !i (!j - !i)

let inline_map mems =
  match Jsont.Json.find_mem "consolidated_metadata" mems with
  | Some (_, Jsont.Object (o, _)) -> (
      match Jsont.Json.find_mem "kind" o with
      | Some (_, Jsont.String ("inline", _)) -> (
          match Jsont.Json.find_mem "metadata" o with
          | Some (_, Jsont.Object (m, _)) -> Some m
          | _ -> None)
      | _ -> None)
  | _ -> None

let node_type j =
  match j with
  | Jsont.Object (o, _) -> (
      match Jsont.Json.find_mem "node_type" o with
      | Some (_, Jsont.String (s, _)) -> Some s
      | _ -> None)
  | _ -> None

(* ["utm"] and exactly two digits. The zone arrays sit one path
   component deeper, so nothing else in the store is this shape. *)
let zone_of_path p =
  if String.length p = 5 && String.sub p 0 3 = "utm" then
    let d0 = p.[3] and d1 = p.[4] in
    if d0 >= '0' && d0 <= '9' && d1 >= '0' && d1 <= '9' then
      let z = ((Char.code d0 - 48) * 10) + (Char.code d1 - 48) in
      if z >= 1 && z <= 60 then Some z else None
    else None
  else None

let of_group (m : Zarrz.Metadata.group_meta) =
  match inline_map m.group_unknown with
  | None -> None
  | Some mems ->
      let nodes = Hashtbl.create (List.length mems) in
      let order = ref [] and zones = ref [] in
      List.iter
        (fun ((name, _), j) ->
          let p = normalise name in
          if not (Hashtbl.mem nodes p) then begin
            Hashtbl.replace nodes p j;
            order := p :: !order;
            match zone_of_path p with
            | Some z when node_type j = Some "group" -> zones := z :: !zones
            | _ -> ()
          end)
        mems;
      Some
        {
          order = List.rev !order;
          nodes;
          zones = List.sort_uniq Int.compare !zones;
        }

let node t path = Hashtbl.find_opt t.nodes (normalise path)
let paths t = t.order
let zones t = t.zones
