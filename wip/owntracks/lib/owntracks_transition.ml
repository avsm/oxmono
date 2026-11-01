(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  tid : string option;
  tst : int;
  lat : float option;
  lon : float option;
  acc : float option;
  event : string;
  desc : string option;
  wtst : int option;
}

let v ?tid ~tst ?lat ?lon ?acc ~event ?desc ?wtst () =
  { tid; tst; lat; lon; acc; event; desc; wtst }

let tid t = t.tid
let tst t = t.tst
let lat t = t.lat
let lon t = t.lon
let acc t = t.acc
let event t = t.event
let desc t = t.desc
let wtst t = t.wtst

let jsont_bare : t Jsont.t =
  let make tid tst lat lon acc event desc wtst =
    { tid; tst; lat; lon; acc; event; desc; wtst }
  in
  Jsont.Object.map ~kind:"transition" make
  |> Jsont.Object.opt_mem "tid" Jsont.string ~enc:(fun t -> t.tid)
  |> Jsont.Object.mem "tst" Owntracks_codec.integer ~enc:(fun t -> t.tst)
  |> Jsont.Object.opt_mem "lat" Owntracks_codec.number ~enc:(fun t -> t.lat)
  |> Jsont.Object.opt_mem "lon" Owntracks_codec.number ~enc:(fun t -> t.lon)
  |> Jsont.Object.opt_mem "acc" Owntracks_codec.number ~enc:(fun t -> t.acc)
  |> Jsont.Object.mem "event" Jsont.string ~enc:(fun t -> t.event)
  |> Jsont.Object.opt_mem "desc" Jsont.string ~enc:(fun t -> t.desc)
  |> Jsont.Object.opt_mem "wtst" Owntracks_codec.integer ~enc:(fun t -> t.wtst)
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let jsont = Owntracks_codec.tagged "transition" jsont_bare

let pp ppf tr =
  Format.fprintf ppf "@[<v 0>";
  Format.fprintf ppf "-------------------------------------------@,";
  Format.fprintf ppf "  Event:     %s@," (String.uppercase_ascii tr.event);
  Option.iter (fun desc -> Format.fprintf ppf "  Region:    %s@," desc) tr.desc;
  Option.iter (fun tid -> Format.fprintf ppf "  Tracker:   %s@," tid) tr.tid;
  Format.fprintf ppf "  Time:      %s@,"
    (Owntracks_location.format_timestamp tr.tst);
  (match (tr.lat, tr.lon) with
  | Some lat, Some lon -> Format.fprintf ppf "  Location:  %.6f, %.6f@," lat lon
  | _ -> ());
  Format.fprintf ppf "-------------------------------------------@]"
