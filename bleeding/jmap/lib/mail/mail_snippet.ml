(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  email_id : Proto_id.t;
  subject : string option;
  preview : string option;
}

let jsont =
  let kind = "SearchSnippet" in
  let make email_id subject preview = { email_id; subject; preview } in
  (* RFC 8621 Section 5: subject and preview are String|null and are always
     present. *)
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "emailId" Proto_id.jsont ~enc:(fun t -> t.email_id)
  |> Proto_json_map.nullable_mem_null "subject" Jsont.string ~enc:(fun t ->
      t.subject)
  |> Proto_json_map.nullable_mem_null "preview" Jsont.string ~enc:(fun t ->
      t.preview)
  |> Jsont.Object.finish

type get_response = {
  account_id : Proto_id.t;
  list : t list;
  not_found : Proto_id.t list option;
}

let get_response_jsont =
  let kind = "SearchSnippet/get response" in
  let make account_id list not_found = { account_id; list; not_found } in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
  |> Jsont.Object.mem "list" (Jsont.list jsont) ~enc:(fun r -> r.list)
  |> Proto_json_map.nullable_mem_null "notFound" (Jsont.list Proto_id.jsont)
       ~enc:(fun r -> r.not_found)
  |> Jsont.Object.finish
