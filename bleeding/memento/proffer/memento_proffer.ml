(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
let accept_datetime req =
  match Proffer.Req.header_single req "accept-datetime" with
  | Error `Repeated -> Error "Repeated Accept-Datetime"
  | Ok None -> Ok None
  | Ok (Some s) -> Result.map Option.some (Memento.Datetime.of_http s)
let memento_headers ~original c =
  Proffer.Headers.of_list (Memento.Headers.memento ~original c)
let timegate respond ~original c =
  Proffer.Resp.empty respond ~status:Httpz.Res.Found
    ~headers:(Proffer.Headers.of_list (Memento.Headers.timegate ~original c)) ()
let json_timemap respond t =
  Proffer.Resp.encode respond (Proffer.Json.v Memento.Timemap.jsont) t
let link_timemap respond links =
  Proffer.Resp.encode respond Memento.Link.timemap_media links
