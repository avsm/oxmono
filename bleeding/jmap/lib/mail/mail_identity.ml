(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type property =
  [ `Id
  | `Name
  | `Email
  | `Reply_to
  | `Bcc
  | `Text_signature
  | `Html_signature
  | `May_delete ]

let property_to_string : [< property ] -> string = function
  | `Id -> "id"
  | `Name -> "name"
  | `Email -> "email"
  | `Reply_to -> "replyTo"
  | `Bcc -> "bcc"
  | `Text_signature -> "textSignature"
  | `Html_signature -> "htmlSignature"
  | `May_delete -> "mayDelete"

let property_of_string s : property option =
  match s with
  | "id" -> Some `Id
  | "name" -> Some `Name
  | "email" -> Some `Email
  | "replyTo" -> Some `Reply_to
  | "bcc" -> Some `Bcc
  | "textSignature" -> Some `Text_signature
  | "htmlSignature" -> Some `Html_signature
  | "mayDelete" -> Some `May_delete
  | _ -> None

type t = {
  id : Proto_id.t option;
  name : string option;
  email : string option;
  reply_to : Mail_address.t list option;
  bcc : Mail_address.t list option;
  text_signature : string option;
  html_signature : string option;
  may_delete : bool option;
}

let v ?id ?name ?email ?reply_to ?bcc ?text_signature ?html_signature
    ?may_delete () =
  { id; name; email; reply_to; bcc; text_signature; html_signature; may_delete }

let id t = t.id

let jsont =
  let kind = "Identity" in
  let make id name email reply_to bcc text_signature html_signature may_delete =
    {
      id;
      name;
      email;
      reply_to;
      bcc;
      text_signature;
      html_signature;
      may_delete;
    }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.opt_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun t -> t.email)
  (* replyTo and bcc are EmailAddress[]|null (RFC 8621 Section 6). *)
  |> Proto_json_map.nullable_mem "replyTo" (Jsont.list Mail_address.jsont)
       ~enc:(fun t -> t.reply_to)
  |> Proto_json_map.nullable_mem "bcc" (Jsont.list Mail_address.jsont)
       ~enc:(fun t -> t.bcc)
  |> Jsont.Object.opt_mem "textSignature" Jsont.string ~enc:(fun t ->
      t.text_signature)
  |> Jsont.Object.opt_mem "htmlSignature" Jsont.string ~enc:(fun t ->
      t.html_signature)
  |> Jsont.Object.opt_mem "mayDelete" Jsont.bool ~enc:(fun t -> t.may_delete)
  |> Jsont.Object.finish

let creation = Proto_id.creation

let sending_address ~local_part t =
  match t.email with
  | None | Some "" | Some "*" -> None
  | Some e when String.starts_with ~prefix:"*@" e ->
      (* RFC 8620 Section 2 lets the session username be a full address, so a
         local part carrying an "@" would build a two-domain address. *)
      let domain = String.sub e 2 (String.length e - 2) in
      if
        String.equal domain "" || String.equal local_part ""
        || String.contains local_part '@'
      then None
      else Some (local_part ^ "@" ^ domain)
  | Some e -> Some e
