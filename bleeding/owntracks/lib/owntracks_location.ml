(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  tid : string option;
  tst : int;
  lat : float;
  lon : float;
  alt : float option;
  acc : float option;
  vel : float option;
  cog : float option;
  batt : int option;
  bs : int option;
  conn : string option;
  t : string option;
  m : int option;
  poi : string option;
  inregions : string list;
  addr : string option;
  topic : string option;
}

let v ?tid ~tst ~lat ~lon ?alt ?acc ?vel ?cog ?batt ?bs ?conn ?t ?m ?poi
    ?(inregions = []) ?addr ?topic () =
  {
    tid;
    tst;
    lat;
    lon;
    alt;
    acc;
    vel;
    cog;
    batt;
    bs;
    conn;
    t;
    m;
    poi;
    inregions;
    addr;
    topic;
  }

let tid t = t.tid
let tst t = t.tst
let lat t = t.lat
let lon t = t.lon
let alt t = t.alt
let acc t = t.acc
let vel t = t.vel
let cog t = t.cog
let batt t = t.batt
let bs t = t.bs
let conn t = t.conn
let trigger t = t.t
let monitoring_mode t = t.m
let poi t = t.poi
let inregions t = t.inregions
let addr t = t.addr
let topic t = t.topic
let with_topic topic t = { t with topic = Some topic }

let jsont_bare : t Jsont.t =
  let make tid tst lat lon alt acc vel cog batt bs conn t m poi inregions addr
      topic =
    {
      tid;
      tst;
      lat;
      lon;
      alt;
      acc;
      vel;
      cog;
      batt;
      bs;
      conn;
      t;
      m;
      poi;
      inregions = Option.value ~default:[] inregions;
      addr;
      topic;
    }
  in
  Jsont.Object.map ~kind:"location" make
  |> Jsont.Object.opt_mem "tid" Jsont.string ~enc:(fun l -> l.tid)
  |> Jsont.Object.mem "tst" Owntracks_codec.integer ~enc:(fun l -> l.tst)
  |> Jsont.Object.mem "lat" Owntracks_codec.number ~enc:(fun l -> l.lat)
  |> Jsont.Object.mem "lon" Owntracks_codec.number ~enc:(fun l -> l.lon)
  |> Jsont.Object.opt_mem "alt" Owntracks_codec.number ~enc:(fun l -> l.alt)
  |> Jsont.Object.opt_mem "acc" Owntracks_codec.number ~enc:(fun l -> l.acc)
  |> Jsont.Object.opt_mem "vel" Owntracks_codec.number ~enc:(fun l -> l.vel)
  |> Jsont.Object.opt_mem "cog" Owntracks_codec.number ~enc:(fun l -> l.cog)
  |> Jsont.Object.opt_mem "batt" Owntracks_codec.integer ~enc:(fun l -> l.batt)
  |> Jsont.Object.opt_mem "bs" Owntracks_codec.integer ~enc:(fun l -> l.bs)
  |> Jsont.Object.opt_mem "conn" Jsont.string ~enc:(fun l -> l.conn)
  |> Jsont.Object.opt_mem "t" Jsont.string ~enc:(fun l -> l.t)
  |> Jsont.Object.opt_mem "m" Owntracks_codec.integer ~enc:(fun l -> l.m)
  |> Jsont.Object.opt_mem "poi" Jsont.string ~enc:(fun l -> l.poi)
  |> Jsont.Object.opt_mem "inregions" (Jsont.list Jsont.string) ~enc:(fun l ->
      match l.inregions with [] -> None | xs -> Some xs)
  |> Jsont.Object.opt_mem "addr" Jsont.string ~enc:(fun l -> l.addr)
  |> Jsont.Object.opt_mem "topic" Jsont.string ~enc:(fun l -> l.topic)
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let jsont = Owntracks_codec.tagged "location" jsont_bare

let format_timestamp tst =
  let t = Unix.gmtime (float_of_int tst) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC" (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min
    t.Unix.tm_sec

let pp_code_map ~unknown codes ppf = function
  | Some s ->
      let display = List.assoc_opt s codes |> Option.value ~default:s in
      Format.pp_print_string ppf display
  | None -> Format.pp_print_string ppf unknown

let pp_conn =
  pp_code_map ~unknown:"Unknown"
    [ ("w", "WiFi"); ("m", "Mobile"); ("o", "Offline") ]

let pp_trigger =
  pp_code_map ~unknown:"Unknown"
    [
      ("p", "Ping");
      ("c", "Circular region");
      ("b", "Beacon");
      ("r", "Response");
      ("u", "Manual");
      ("t", "Timer");
      ("v", "Monitoring");
    ]

let pp ppf loc =
  Format.fprintf ppf "@[<v 0>";
  Format.fprintf ppf "-------------------------------------------@,";
  Option.iter
    (fun topic -> Format.fprintf ppf "  Topic:     %s@," topic)
    loc.topic;
  Option.iter (fun tid -> Format.fprintf ppf "  Tracker:   %s@," tid) loc.tid;
  Format.fprintf ppf "  Time:      %s@," (format_timestamp loc.tst);
  Format.fprintf ppf "  Location:  %.6f, %.6f@," loc.lat loc.lon;
  Option.iter
    (fun alt -> Format.fprintf ppf "  Altitude:  %.1f m@," alt)
    loc.alt;
  Option.iter
    (fun acc -> Format.fprintf ppf "  Accuracy:  +/- %.0f m@," acc)
    loc.acc;
  Option.iter
    (fun vel -> Format.fprintf ppf "  Speed:     %.1f km/h@," vel)
    loc.vel;
  Option.iter
    (fun cog -> Format.fprintf ppf "  Heading:   %.0f deg@," cog)
    loc.cog;
  Option.iter
    (fun batt -> Format.fprintf ppf "  Battery:   %d%%@," batt)
    loc.batt;
  Format.fprintf ppf "  Conn:      %a@," pp_conn loc.conn;
  Option.iter
    (fun _ -> Format.fprintf ppf "  Trigger:   %a@," pp_trigger loc.t)
    loc.t;
  Option.iter (fun poi -> Format.fprintf ppf "  POI:       %s@," poi) loc.poi;
  if loc.inregions <> [] then
    Format.fprintf ppf "  Regions:   %s@," (String.concat ", " loc.inregions);
  Option.iter (fun addr -> Format.fprintf ppf "  Address:   %s@," addr) loc.addr;
  Format.fprintf ppf "-------------------------------------------@]"
