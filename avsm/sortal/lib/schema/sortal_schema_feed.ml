(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type feed_type =
  | Atom
  | Rss
  | Json
  | Manual

type t = {
  feed_type : feed_type;
  url : string;
  name : string option;
  hint : string option;
}

let make ~feed_type ~url ?name ?hint () =
  { feed_type; url; name; hint }

let feed_type t = t.feed_type
let url t = t.url
let name t = t.name
let hint t = t.hint

let set_name t name = { t with name = Some name }

let feed_type_to_string = function
  | Atom -> "atom"
  | Rss -> "rss"
  | Json -> "json"
  | Manual -> "manual"

let feed_type_of_string s =
  match String.lowercase_ascii s with
  | "atom" -> Some Atom
  | "rss" -> Some Rss
  | "json" -> Some Json
  | "manual" -> Some Manual
  | _ -> None

let json_t =
  let open Jsont in
  let open Jsont.Object in
  let make feed_type url name hint =
    match feed_type_of_string feed_type with
    | Some ft -> { feed_type = ft; url; name; hint }
    | None -> failwith ("Invalid feed type: " ^ feed_type)
  in
  map ~kind:"Feed" make
  |> mem "type" string ~enc:(fun f -> feed_type_to_string f.feed_type)
  |> mem "url" string ~enc:(fun f -> f.url)
  |> opt_mem "name" string ~enc:(fun f -> f.name)
  |> opt_mem "hint" string ~enc:(fun f -> f.hint)
  |> finish

let pp ppf t =
  let open Fmt in
  pf ppf "%a: %a%a%a"
    (styled (`Fg `Green) string) (feed_type_to_string t.feed_type)
    (styled (`Fg `Blue) string) t.url
    (option (fun ppf name -> pf ppf " (%a)" (styled `Faint string) name)) t.name
    (option (fun ppf hint -> pf ppf " [hint: %a]" (styled `Faint string) hint)) t.hint
