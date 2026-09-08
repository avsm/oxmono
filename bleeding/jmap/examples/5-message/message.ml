(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto

let doc = "Print the newest message with its headers, body and attachments"
let to_header = Proto.Email_header.addresses `To
let to_property = Proto.Email_header.header_property_to_string to_header
let bytes = Fmt.(option ~none:(any "?") int64)
let or_unknown = Option.value ~default:"?"
let date = function None -> "-" | Some t -> Proto.Date.to_utc_string t

let addresses ppf l =
  let email (a : Proto.Email_address.t) = a.email in
  Fmt.(option ~none:(any "-") (list ~sep:comma string))
    ppf
    (Option.map (List.map email) l)

let () =
  Jmap_eio.Cli.main "message" ~doc @@ fun ctx ->
  let account_id = ctx.account_id in
  let got =
    Client.call_exn ctx.client
      Chain.(
        let* q =
          email_query ~account_id
            ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
            ~limit:1L ()
        in
        email_get ~account_id ~ids:(from_query q)
          ~properties:
            [
              `Subject;
              `From;
              `Received_at;
              `Text_body;
              `Body_values;
              `Attachments;
              `Header to_header;
            ]
          ~body_properties:[ `Part_id; `Type; `Size; `Name ]
          ~fetch_text_body_values:true ~max_body_value_bytes:2000L ())
  in
  match got.list with
  | [] -> Fmt.failwith "the Inbox is empty; deliver a message first"
  | (email : Proto.Email.t) :: _ ->
      Fmt.pr "Received  %s@." (date email.received_at);
      Fmt.pr "From      %a@." addresses email.from;
      Fmt.pr "To        %a@." addresses
        (Proto.Email.find_header_addresses email to_property);
      Fmt.pr "Subject   %s@." (Option.value email.subject ~default:"(none)");
      List.iter
        (fun (p : Proto.Email_body.Part.t) ->
          Proto.Email.body_value email p
          |> Option.iter (fun (v : Proto.Email_body.Value.t) ->
              Fmt.pr "@.text part %s (%s, %a bytes%s)@." (or_unknown p.part_id)
                (or_unknown p.type_) bytes p.size
                (if v.is_truncated then ", truncated" else "");
              String.split_on_char '\n' v.value |> List.iter (Fmt.pr "  | %s@.")))
        (Option.value email.text_body ~default:[]);
      let attachments = Option.value email.attachments ~default:[] in
      Fmt.pr "@.attachments %d@." (List.length attachments);
      List.iter
        (fun (p : Proto.Email_body.Part.t) ->
          Fmt.pr "  %s (%s, %a bytes)@."
            (Option.value p.name ~default:"(unnamed)")
            (or_unknown p.type_) bytes p.size)
        attachments
