(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  tst : int;
  desc : string;
  lat : float option;
  lon : float option;
  rad : int option;
  uuid : string option;
  major : int option;
  minor : int option;
  rid : string option;
}

let v ~tst ~desc ?lat ?lon ?rad ?uuid ?major ?minor ?rid () =
  { tst; desc; lat; lon; rad; uuid; major; minor; rid }

let tst t = t.tst
let desc t = t.desc
let lat t = t.lat
let lon t = t.lon
let rad t = t.rad
let uuid t = t.uuid
let major t = t.major
let minor t = t.minor
let rid t = t.rid

let jsont_bare =
  let make tst desc lat lon rad uuid major minor rid =
    { tst; desc; lat; lon; rad; uuid; major; minor; rid }
  in
  Jsont.Object.map ~kind:"waypoint" make
  |> Jsont.Object.mem "tst" Owntracks_codec.integer ~enc:tst
  |> Jsont.Object.mem "desc" Jsont.string ~enc:desc
  |> Jsont.Object.opt_mem "lat" Owntracks_codec.number ~enc:lat
  |> Jsont.Object.opt_mem "lon" Owntracks_codec.number ~enc:lon
  |> Jsont.Object.opt_mem "rad" Owntracks_codec.integer ~enc:rad
  |> Jsont.Object.opt_mem "uuid" Jsont.string ~enc:uuid
  |> Jsont.Object.opt_mem "major" Owntracks_codec.integer ~enc:major
  |> Jsont.Object.opt_mem "minor" Owntracks_codec.integer ~enc:minor
  |> Jsont.Object.opt_mem "rid" Jsont.string ~enc:rid
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let jsont = Owntracks_codec.tagged "waypoint" jsont_bare
let pp ppf wp = Format.fprintf ppf "Waypoint: %s" wp.desc
