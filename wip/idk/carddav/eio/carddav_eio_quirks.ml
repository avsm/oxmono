(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { vcard3 : bool; lenient_hrefs : bool }

let standard = { vcard3 = false; lenient_hrefs = false }
let fastmail = { standard with vcard3 = true }

let host url =
  match Fetch.Middleware.Url.of_string url with
  | Ok u -> Fetch.Middleware.Url.host u
  | Error _ -> ""

let of_url url =
  let h = host url in
  let under d = h = d || String.ends_with ~suffix:("." ^ d) h in
  if under "fastmail.com" || under "messagingengine.com" then fastmail
  else standard

(* Cyrus stores KIND as X-ADDRESSBOOKSERVER-KIND and a preferred value as
   TYPE=PREF, the vCard 3.0 conventions, and reports VERSION:3.0. *)
let upgrade_vcard3 text =
  match Vcard.one_of_string text with
  | Error _ -> text
  | Ok card ->
      if Vcard.version card <> "3.0" then text
      else
        let module P = Vcard.Property in
        let props =
          List.map
            (fun p ->
              let name = P.name p in
              let params = P.params p in
              let types = P.types p in
              let has_pref = List.mem "pref" types in
              let params =
                if not has_pref then params
                else
                  List.concat_map
                    (fun prm ->
                      if Vcard.Param.name prm <> "TYPE" then [ prm ]
                      else
                        match
                          List.filter
                            (fun v -> String.lowercase_ascii v <> "pref")
                            (List.concat_map (String.split_on_char ',')
                               (Vcard.Param.values prm))
                        with
                        | [] -> []
                        | vs -> [ Vcard.Param.v "TYPE" vs ])
                    params
                  @
                  if P.find_first p "PREF" = None then
                    [ Vcard.Param.v "PREF" [ "1" ] ]
                  else []
              in
              let name =
                if name = "X-ADDRESSBOOKSERVER-KIND" then "KIND" else name
              in
              let value =
                if name = "KIND" then String.lowercase_ascii (P.value p)
                else P.value p
              in
              P.v ?group:(P.group p) ~params name value)
            (Vcard.properties card)
        in
        Vcard.to_string (Vcard.v ~version:"4.0" props)
