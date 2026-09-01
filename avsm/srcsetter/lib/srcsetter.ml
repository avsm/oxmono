(* Copyright (c) 2024, Anil Madhavapeddy <anil@recoil.org>

   Permission to use, copy, modify, and/or distribute this software for
   any purpose with or without fee is hereby granted, provided that the
   above copyright notice and this permission notice appear in all
   copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
   WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
   DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
   OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
   TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

module MS = struct
  (* A sorted association list, not [Map.Make (String)]. A variant map is built
     once and then only walked in key order, never searched, and an image has a
     handful of variants, so a tree buys nothing. What it costs is that
     [Map.S.t] declares no kind, which leaves {!t} unable to cross into a
     function marked [portable]. *)
  type 'a t = (string * 'a) list

  let empty = []

  (* Sort stably and keep the last binding of each key, which is what folding
     [Map.add] over the list would leave. *)
  let of_list l =
    let l = List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l in
    let rec last_of_each_run = function
      | (k1, _) :: ((k2, _) :: _ as rest) when String.equal k1 k2 ->
          last_of_each_run rest
      | b :: rest -> b :: last_of_each_run rest
      | [] -> []
    in
    last_of_each_run l

  let bindings l = l
  let cardinal = List.length
  let fold f l acc = List.fold_left (fun acc (k, v) -> f k v acc) acc l
end

type t = {
  name : string;
  slug : string;
  origin : string;
  dims : int * int;
  variants : (int * int) MS.t;
}

let v name slug origin variants dims = { name; slug; origin; variants; dims }
let name t = t.name
let slug t = t.slug
let origin t = t.origin
let dims t = t.dims
let variants t = t.variants

(** JSON codec for dimension pairs, encoded as a 2-element array. *)
let dims_json_t =
  let open Jsont in
  let dec w h = (w, h) in
  let enc (w, h) i = if i = 0 then w else h in
  t2 ~dec ~enc uint16

(* A variant map is a JSON object keyed by filename. [as_string_map] is the
   only object-as-map [Jsont] offers and it yields a stdlib map, so the
   bindings are handed on to {!MS}. Both sides walk in key order, so the bytes
   written are the bytes a stdlib map wrote. *)
let variants_json_t =
  let dec m =
    let bindings =
      Jsont.String_map.fold (fun k v acc -> (k, v) :: acc) m [] |> List.rev
    in
    MS.of_list bindings
  in
  let enc l =
    List.fold_left
      (fun m (k, v) -> Jsont.String_map.add k v m)
      (Jsont.String_map.create ()) (MS.bindings l)
  in
  Jsont.map ~dec ~enc (Jsont.Object.as_string_map dims_json_t)

let json_t =
  let open Jsont in
  let open Jsont.Object in
  map ~kind:"Entry" v
  |> mem "name" string ~enc:name
  |> mem "slug" string ~enc:slug
  |> mem "origin" string ~enc:origin
  |> mem "variants" variants_json_t ~enc:variants
  |> mem "dims" dims_json_t ~enc:dims
  |> finish

let list = Jsont.list json_t
let list_to_json entries = Jsont_bytesrw.encode_string list ~format:Jsont.Indent entries
let list_of_json = Jsont_bytesrw.decode_string list
