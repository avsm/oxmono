(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Unknown = struct
  type t = Jsont.json

  let empty = Jsont.Object ([], Jsont.Meta.none)
  let is_empty = function Jsont.Object ([], _) -> true | _ -> false
end

type t = {
  name : string option;
  url : string option;
  avatar : string option;
  unknown : Unknown.t;
}

let create ?name ?url ?avatar ?(unknown = Unknown.empty) () =
  if name = None && url = None && avatar = None then
    invalid_arg
      "Author.create: at least one field (name, url, or avatar) must be \
       provided";
  { name; url; avatar; unknown }

let name t = t.name
let url t = t.url
let avatar t = t.avatar
let unknown t = t.unknown
let is_valid t = t.name <> None || t.url <> None || t.avatar <> None
let equal a b = a.name = b.name && a.url = b.url && a.avatar = b.avatar

let pp ppf t =
  match (t.name, t.url) with
  | Some name, Some url -> Format.fprintf ppf "%s <%s>" name url
  | Some name, None -> Format.fprintf ppf "%s" name
  | None, Some url -> Format.fprintf ppf "<%s>" url
  | None, None -> (
      match t.avatar with
      | Some avatar -> Format.fprintf ppf "(avatar: %s)" avatar
      | None -> Format.fprintf ppf "(empty author)")

let jsont =
  let kind = "Author" in
  let doc = "An author object with at least one field set" in
  (* Constructor that matches the jsont object map pattern *)
  let create_obj name url avatar unknown =
    create ?name ?url ?avatar ~unknown ()
  in
  Jsont.Object.map ~kind ~doc create_obj
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:name
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:url
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:avatar
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
  |> Jsont.Object.finish
