(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results

let opt pp = Fmt.(option ~none:(any "?") pp)
let date = Option.fold ~none:"?" ~some:Proto.Date.to_utc_string

let sender (e : Proto.Email.t) =
  match e.from with
  | Some ({ Proto.Email_address.name; email } :: _) ->
      Option.value name ~default:email
  | _ -> "-"

let print_message n (e : Proto.Email.t) =
  Fmt.pr "  %2d.%s %s  %-20s %s@." n
    (if Proto.Email.has_keyword `Seen e then " " else "*")
    (date e.received_at) (sender e)
    (Option.value e.subject ~default:"(no subject)");
  match e.preview with Some "" | None -> () | Some p -> Fmt.pr "       %s@." p

let run (ctx : Jmap_eio.Cli.context) limit =
  let account_id = ctx.account_id in
  let sort = [ Proto.Email.sort ~ascending:false `Received_at ] in
  let Results.[ threads; messages ] =
    Client.run_exn ctx.client
      Chain.(
        let* q =
          email_query ~account_id ~sort ~collapse_threads:true ~limit:1L ()
        in
        let* head =
          email_get ~account_id ~ids:(from_query q) ~properties:[ `Thread_id ]
            ()
        in
        let* threads =
          thread_get ~account_id ~ids:(from_get_field head Thread_id) ()
        in
        let+ messages =
          email_get ~account_id
            ~ids:(from_get_field threads Email_ids)
            ~properties:
              [ `Id; `Subject; `From; `Received_at; `Keywords; `Preview ]
            ()
        in
        Handles.[ threads; messages ])
  in
  (match threads.list with
  | [] -> Fmt.failwith "the Inbox is empty; deliver a message first"
  | (t : Proto.Thread.t) :: _ ->
      let email_ids = Option.value t.email_ids ~default:[] in
      Fmt.pr "Thread %a: %d message(s)@." (opt Proto.Id.pp) t.id
        (List.length email_ids);
      Proto.Method.in_ids_order ~id:Proto.Email.id email_ids messages.list
      |> List.iteri (fun i e -> print_message (i + 1) e));
  let limit = Int64.of_int limit in
  let Results.[ expanded; collapsed; heads ] =
    Client.run_exn ctx.client
      Chain.(
        let* expanded =
          email_query ~account_id ~sort ~limit ~calculate_total:true ()
        in
        let* collapsed =
          email_query ~account_id ~sort ~collapse_threads:true ~limit
            ~calculate_total:true ()
        in
        let+ heads =
          email_get ~account_id ~ids:(from_query expanded)
            ~properties:[ `Thread_id ] ()
        in
        Handles.[ expanded; collapsed; heads ])
  in
  let conversations =
    List.filter_map (fun (e : Proto.Email.t) -> e.thread_id) heads.list
    |> List.sort_uniq Proto.Id.compare
  in
  let count label (q : Proto.Method.query_response) =
    Fmt.pr "The newest %Ld with collapseThreads %s: %d id(s) of %a matching@."
      limit label (List.length q.ids) (opt Fmt.int64) q.total
  in
  Fmt.pr "@.";
  count "false" expanded;
  count "true " collapsed;
  Fmt.pr "Those %d message(s) belong to %d conversation(s)@."
    (List.length expanded.ids)
    (List.length conversations);
  let hidden =
    List.filter
      (fun id -> not (List.exists (Proto.Id.equal id) collapsed.ids))
      expanded.ids
  in
  if List.is_empty hidden then
    Fmt.pr "Collapsed away: nothing, so every message here is its own thread@."
  else Fmt.pr "Collapsed away: %a@." Fmt.(list ~sep:sp Proto.Id.pp) hidden

let limit_term =
  let open Cmdliner in
  let doc = "Ask each of the two queries for $(docv) results." in
  Arg.(value & opt int 20 & info [ "limit"; "n" ] ~docv:"N" ~doc)

let () =
  let doc = "Print a conversation, then collapse the message list into one" in
  Jmap_eio.Cli.main' "conversation" ~doc ~args:limit_term run
