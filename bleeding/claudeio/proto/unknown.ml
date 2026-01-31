(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unknown fields for preserving extra JSON object members during
    round-tripping.

    This module provides an opaque type for storing unknown JSON fields as an
    association list. This is useful for preserving fields that are not part of
    the defined schema but should be maintained when reading and writing JSON.
*)

type t = (string * Jsont.json) list

let empty = []
let is_empty = function [] -> true | _ -> false
let of_assoc x = x
let to_assoc x = x

let jsont =
  let open Jsont in
  let dec obj =
    match obj with
    | Object (fields, _) ->
        (* Convert from Jsont.mem list (name * json) to (string * json) list *)
        List.map (fun ((name, _meta), json) -> (name, json)) fields
    | _ -> invalid_arg "Expected object"
  in
  let enc fields =
    (* Convert from (string * json) list to Jsont.mem list *)
    let mems =
      List.map (fun (name, json) -> ((name, Meta.none), json)) fields
    in
    Object (mems, Meta.none)
  in
  map ~dec ~enc json

(** Mems codec for use with Jsont.Object.keep_unknown.

    This provides a custom mems codec that converts between our (string *
    Jsont.json) list representation and the Jsont.mem list representation used
    by keep_unknown. *)
let mems : (t, Jsont.json, Jsont.mem list) Jsont.Object.Mems.map =
  let open Jsont in
  (* The decoder builds up a mem list (the third type parameter) and
     dec_finish converts it to our type t *)
  let dec_empty () = [] in
  let dec_add meta name json acc = ((name, meta), json) :: acc in
  let dec_finish _meta mems =
    (* Convert from mem list to (string * json) list *)
    List.rev_map (fun ((name, _meta), json) -> (name, json)) mems
  in
  let enc =
    {
      Object.Mems.enc =
        (fun k fields acc ->
          List.fold_left
            (fun acc (name, json) -> k Meta.none name json acc)
            acc fields);
    }
  in
  Object.Mems.map ~kind:"Unknown" ~dec_empty ~dec_add ~dec_finish ~enc
    Jsont.json
