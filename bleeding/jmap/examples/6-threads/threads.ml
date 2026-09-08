(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Results = Jmap.Chain.Results
module Proto = Jmap.Proto

let doc = "Print the three newest conversations, fetched in one request"
let date = function None -> "?" | Some t -> Proto.Date.to_utc_string t

let () =
  Jmap_eio.Cli.main "threads" ~doc @@ fun ctx ->
  let account_id = ctx.account_id in
  let Results.[ threads; messages ] =
    Client.run_exn ctx.client
      Chain.(
        let* q =
          email_query ~account_id
            ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
            ~collapse_threads:true ~limit:3L ()
        in
        let* heads =
          email_get ~account_id ~ids:(from_query q) ~properties:[ `Thread_id ]
            ()
        in
        let* threads =
          thread_get ~account_id ~ids:(from_get_field heads Thread_id) ()
        in
        let+ messages =
          email_get ~account_id
            ~ids:(from_get_field threads Email_ids)
            ~properties:[ `Id; `Subject; `Received_at ]
            ()
        in
        Handles.[ threads; messages ])
  in
  if List.is_empty threads.list then
    Fmt.failwith "the Inbox is empty; deliver a message first";
  List.iter
    (fun (t : Proto.Thread.t) ->
      let email_ids = Option.value t.email_ids ~default:[] in
      Fmt.pr "Thread %a: %d message(s)@."
        Fmt.(option ~none:(any "?") Proto.Id.pp)
        t.id (List.length email_ids);
      Proto.Method.in_ids_order ~id:Proto.Email.id email_ids messages.list
      |> List.iter (fun (e : Proto.Email.t) ->
          Fmt.pr "  %s  %s@." (date e.received_at)
            (Option.value e.subject ~default:"(no subject)")))
    threads.list
