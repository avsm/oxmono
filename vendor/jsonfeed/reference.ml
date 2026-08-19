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
  url : string;
  doi : string option;
  cito : Cito.t list option;
  unknown : Unknown.t;
}

let create ~url ?doi ?cito ?(unknown = Unknown.empty) () =
  { url; doi; cito; unknown }

let url t = t.url
let doi t = t.doi
let cito t = t.cito
let unknown t = t.unknown
let equal a b = String.equal a.url b.url

let pp ppf t =
  let open Format in
  fprintf ppf "%s" t.url;
  Option.iter (fprintf ppf " [DOI: %s]") t.doi

let jsont =
  let kind = "Reference" in
  let doc = "A reference to a cited source" in
  let create_obj url doi cito unknown = create ~url ?doi ?cito ~unknown () in
  Jsont.Object.map ~kind ~doc create_obj
  |> Jsont.Object.mem "url" Jsont.string ~enc:url
  |> Jsont.Object.opt_mem "doi" Jsont.string ~enc:doi
  |> Jsont.Object.opt_mem "cito" (Jsont.list Cito.jsont) ~enc:cito
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
  |> Jsont.Object.finish
