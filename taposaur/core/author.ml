(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

type t = { given : string option; family : string }

(* A correction gives the authors as one value, so it is read back
   here: names are separated by ";", a "," separates the surname from
   the forenames, and a name without one takes its last word as the
   surname. *)
let of_correction v =
  String.split_on_char ';' v
  |> List.filter_map (fun part ->
      let some s = if s = "" then None else Some s in
      match String.trim part with
      | "" -> None
      | name ->
        let split i =
          ( String.trim (String.sub name 0 i),
            String.trim (String.sub name (i + 1) (String.length name - i - 1))
          )
        in
        match String.index_opt name ',' with
        | Some i ->
          let family, given = split i in
          Option.map (fun family -> { family; given = some given })
            (some family)
        | None ->
          match String.rindex_opt name ' ' with
          | None -> Some { family = name; given = None }
          | Some i ->
            let given, family = split i in
            Option.map (fun family -> { family; given = some given })
              (some family))

let jsont =
  Jsont.Object.map ~kind:"Author" (fun given family -> { given; family })
  |> Jsont.Object.opt_mem "given" Jsont.string ~enc:(fun a -> a.given)
  |> Jsont.Object.mem "family" Jsont.string ~enc:(fun a -> a.family)
  |> Jsont.Object.finish
