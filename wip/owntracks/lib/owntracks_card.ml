(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { name : string option; face : string option; tid : string option }

let v ?name ?face ?tid () = { name; face; tid }
let name t = t.name
let face t = t.face
let tid t = t.tid

let jsont_bare : t Jsont.t =
  let make name face tid = { name; face; tid } in
  Jsont.Object.map ~kind:"card" make
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun c -> c.name)
  |> Jsont.Object.opt_mem "face" Jsont.string ~enc:(fun c -> c.face)
  |> Jsont.Object.opt_mem "tid" Jsont.string ~enc:(fun c -> c.tid)
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let jsont = Owntracks_codec.tagged "card" jsont_bare

let pp ppf card =
  Format.fprintf ppf "Card: %s" (Option.value ~default:"(no name)" card.name)
