(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  order : string list;  (* Node paths in document order. *)
  nodes : (string, Jsont.json) Hashtbl.t;
  kids : (string, string list) Hashtbl.t;
      (* Parent path to child names, in reverse document order. *)
}

(* A path is stored without its slashes so that ["utm30"], ["/utm30"]
   and ["utm30/"] all reach the same node, and so that the root is the
   empty string whichever way a caller spells it. *)
let normalise path =
  let n = String.length path in
  let i = ref 0 and j = ref n in
  while !i < !j && path.[!i] = '/' do
    incr i
  done;
  while !j > !i && path.[!j - 1] = '/' do
    decr j
  done;
  if !i = 0 && !j = n then path else String.sub path !i (!j - !i)

let split p =
  match String.rindex_opt p '/' with
  | None -> ("", p)
  | Some i -> (String.sub p 0 i, String.sub p (i + 1) (String.length p - i - 1))

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

let node_kind j =
  match j with
  | Jsont.Object (o, _) -> (
      match Jsont.Json.find_mem "node_type" o with
      | Some (_, Jsont.String ("array", _)) -> `Array
      | _ -> `Group)
  | _ -> `Group

let of_group (m : Metadata.group_meta) =
  match inline_map m.group_unknown with
  | None -> None
  | Some mems ->
      let n = List.length mems in
      let nodes = Hashtbl.create n and kids = Hashtbl.create n in
      let order = ref [] in
      List.iter
        (fun ((name, _), j) ->
          let p = normalise name in
          if (not (String.equal p "")) && not (Hashtbl.mem nodes p) then begin
            Hashtbl.replace nodes p j;
            order := p :: !order;
            let parent, base = split p in
            let sibs =
              match Hashtbl.find_opt kids parent with
              | Some l -> l
              | None -> []
            in
            Hashtbl.replace kids parent (base :: sibs)
          end)
        mems;
      Some { order = List.rev !order; nodes; kids }

let paths t = t.order
let node t path = Hashtbl.find_opt t.nodes (normalise path)

let children t path =
  let p = normalise path in
  match Hashtbl.find_opt t.kids p with
  | None -> []
  | Some rev ->
      (* [kids] holds each list in reverse, so this puts it back into
         document order at no extra cost. *)
      List.rev_map
        (fun name ->
          let full = if String.equal p "" then name else p ^ "/" ^ name in
          let kind =
            match Hashtbl.find_opt t.nodes full with
            | Some j -> node_kind j
            | None -> `Group
          in
          (name, kind))
        rev
