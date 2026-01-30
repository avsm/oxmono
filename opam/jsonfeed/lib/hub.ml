(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Unknown = struct
  type t = Jsont.json

  let empty = Jsont.Object ([], Jsont.Meta.none)
  let is_empty = function Jsont.Object ([], _) -> true | _ -> false
end

type t = { type_ : string; url : string; unknown : Unknown.t }

let create ~type_ ~url ?(unknown = Unknown.empty) () = { type_; url; unknown }
let type_ t = t.type_
let url t = t.url
let unknown t = t.unknown
let equal a b = a.type_ = b.type_ && a.url = b.url
let pp ppf t = Format.fprintf ppf "%s: %s" t.type_ t.url

let jsont =
  let kind = "Hub" in
  let doc = "A hub endpoint" in
  let create_obj type_ url unknown = create ~type_ ~url ~unknown () in
  Jsont.Object.map ~kind ~doc create_obj
  |> Jsont.Object.mem "type" Jsont.string ~enc:type_
  |> Jsont.Object.mem "url" Jsont.string ~enc:url
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
  |> Jsont.Object.finish
