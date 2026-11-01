(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type property = [ `Id | `Email_ids ]

let property_to_string : [< property ] -> string = function
  | `Id -> "id"
  | `Email_ids -> "emailIds"

let property_of_string s : property option =
  match s with "id" -> Some `Id | "emailIds" -> Some `Email_ids | _ -> None

type t = { id : Proto_id.t option; email_ids : Proto_id.t list option }

let id t = t.id

let jsont =
  let kind = "Thread" in
  Jsont.Object.map ~kind (fun id email_ids -> { id; email_ids })
  |> Jsont.Object.opt_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Jsont.Object.opt_mem "emailIds" (Jsont.list Proto_id.jsont) ~enc:(fun t ->
      t.email_ids)
  |> Jsont.Object.finish
