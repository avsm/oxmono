(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { tst : int }

let v ~tst = { tst }
let tst t = t.tst

let jsont_bare : t Jsont.t =
  let make tst = { tst } in
  Jsont.Object.map ~kind:"lwt" make
  |> Jsont.Object.mem "tst" Owntracks_codec.integer ~enc:(fun l -> l.tst)
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let jsont = Owntracks_codec.tagged "lwt" jsont_bare

let pp ppf lwt =
  Format.fprintf ppf "LWT: connection established at %s"
    (Owntracks_location.format_timestamp lwt.tst)
