(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { name : string option; email : string }

let create ?name email = { name; email }
let equal a b = String.equal a.email b.email

let pp ppf t =
  match t.name with
  | Some name ->
      Format.fprintf ppf "%a <%a>" Proto_error.pp_escaped name
        Proto_error.pp_escaped t.email
  | None -> Proto_error.pp_escaped ppf t.email

let jsont =
  let kind = "EmailAddress" in
  Jsont.Object.map ~kind (fun name email -> { name; email })
  |> Proto_json_map.nullable_mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun t -> t.email)
  |> Jsont.Object.finish

module Group = struct
  type address = t
  type t = { name : string option; addresses : address list }

  let create ?name addresses = { name; addresses }

  let jsont =
    let kind = "EmailAddressGroup" in
    Jsont.Object.map ~kind (fun name addresses -> { name; addresses })
    |> Proto_json_map.nullable_mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Jsont.Object.mem "addresses" (Jsont.list jsont) ~enc:(fun t ->
        t.addresses)
    |> Jsont.Object.finish
end
