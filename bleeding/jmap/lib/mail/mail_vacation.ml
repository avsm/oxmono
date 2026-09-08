(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  id : Proto_id.t option;
  is_enabled : bool option;
  from_date : Ptime.t option;
  to_date : Ptime.t option;
  subject : string option;
  text_body : string option;
  html_body : string option;
}

let v ?is_enabled ?from_date ?to_date ?subject ?text_body ?html_body () =
  { id = None; is_enabled; from_date; to_date; subject; text_body; html_body }

let singleton_id = Proto_id.of_string_exn "singleton"

let jsont =
  let kind = "VacationResponse" in
  let make id is_enabled from_date to_date subject text_body html_body =
    { id; is_enabled; from_date; to_date; subject; text_body; html_body }
  in
  Jsont.Object.map ~kind make
  (* id and isEnabled are optional here so that a properties-restricted
     VacationResponse/get response still decodes (RFC 8621 Section 8.1). *)
  |> Jsont.Object.opt_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Jsont.Object.opt_mem "isEnabled" Jsont.bool ~enc:(fun t -> t.is_enabled)
  (* fromDate, toDate, subject, textBody and htmlBody are all T|null
     (RFC 8621 Section 8). *)
  |> Proto_json_map.nullable_mem "fromDate" Proto_date.utc_jsont ~enc:(fun t ->
      t.from_date)
  |> Proto_json_map.nullable_mem "toDate" Proto_date.utc_jsont ~enc:(fun t ->
      t.to_date)
  |> Proto_json_map.nullable_mem "subject" Jsont.string ~enc:(fun t ->
      t.subject)
  |> Proto_json_map.nullable_mem "textBody" Jsont.string ~enc:(fun t ->
      t.text_body)
  |> Proto_json_map.nullable_mem "htmlBody" Jsont.string ~enc:(fun t ->
      t.html_body)
  |> Jsont.Object.finish
