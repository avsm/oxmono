(* SPDX-License-Identifier: ISC *)
(* Webhooks, which a repository's spindle sends. *)
open Cmdliner
module Api = Tangled.Api
module Defs = Api.Lex.Webhook.Defs

let webhook =
  Common.positional 0 "WEBHOOK" "The webhook's ID, as $(b,list) shows it."

let events =
  Common.strings [ "event"; "e" ] "EVENT"
    "Notify on EVENT. Repeat the option for more. The events are $(b,push), \
     $(b,repository:renamed), $(b,pull_request:created), \
     $(b,pull_request:resubmitted), $(b,pull_request:merged), \
     $(b,pull_request:closed) and $(b,pull_request:reopened)."

let secret =
  Common.optional [ "secret" ] "SECRET"
    "Sign each notification with SECRET, so that the receiver can tell it came \
     from here."

let with_target ?busy env repo spindle f =
  Common.with_api ?busy env (fun api ->
      let repo, spindle = Common.spindle_target api ~repo ~spindle in
      f api ~repo ~spindle)

let list_cmd =
  let action repo spindle audience json =
    Common.run (fun env ->
        let hooks =
          with_target ~busy:"Reading webhooks" env repo spindle
            (fun api ~repo ~spindle ->
              Api.webhooks api ~spindle ?audience ~repo ())
        in
        if json then Common.print (Jsont.list Defs.webhook_jsont) hooks
        else if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Webhooks" (List.length hooks))
            [
              Ui.column "ID";
              Ui.column "URL";
              Ui.column "Events";
              Ui.column "Active";
            ]
            (List.map
               (fun (w : Defs.webhook) ->
                 [
                   Ui.muted w.id;
                   Ui.link w.url;
                   Ui.text
                     (String.concat ", "
                        (List.map Tangled.Schema.text w.events));
                   (if w.active then Ui.state "success" else Ui.muted "no");
                 ])
               hooks)
        else
          List.iter
            (fun (w : Defs.webhook) ->
              Fmt.pr "%s\t%s\t%b@." w.id w.url w.active)
            hooks)
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List the repository's webhooks.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.spindle $ Common.audience
       $ Common.json_flag))

let add_cmd =
  let url = Common.positional 0 "URL" "The address to send notifications to." in
  let action repo spindle audience url events secret =
    Common.run (fun env ->
        let events = if events = [] then [ "push" ] else events in
        let id =
          with_target ~busy:"Adding webhook" env repo spindle
            (fun api ~repo ~spindle ->
              Api.create_webhook api ~spindle ?audience ~repo ~url ~events
                ?secret ())
        in
        if Ui.tty () then Ui.ok "Added webhook %s" id else print_endline id)
  in
  Common.cmd
    (Cmd.info "add" ~doc:"Send notifications to a URL."
       ~man:
         (Common.man
            [
              "Sends a notification to URL each time one of the events \
               happens, by default each push.";
            ]))
    Term.(
      term_result
        (const action $ Common.repo $ Common.spindle $ Common.audience $ url
       $ events $ secret))

let edit_cmd =
  let url =
    Common.optional [ "url" ] "URL" "Send notifications to URL instead."
  in
  let active =
    Arg.(
      value
      & vflag None
          [
            (Some true, info [ "enable" ] ~doc:"Resume notifications.");
            (Some false, info [ "disable" ] ~doc:"Pause notifications.");
          ])
  in
  let action repo spindle audience webhook url events secret active =
    Common.run (fun env ->
        with_target ~busy:"Updating webhook" env repo spindle
          (fun api ~repo ~spindle ->
            Api.update_webhook api ~spindle ?audience ~repo ~webhook ?url
              ?events:(Common.nonempty events) ?secret ?active ());
        Ui.ok "Updated webhook %s" webhook)
  in
  Common.cmd
    (Cmd.info "edit" ~doc:"Change a webhook."
       ~man:
         (Common.man
            [
              "Changes what the options name and leaves the rest as it is. \
               Events given replace the ones the webhook had.";
            ]))
    Term.(
      term_result
        (const action $ Common.repo $ Common.spindle $ Common.audience $ webhook
       $ url $ events $ secret $ active))

let remove_cmd =
  let action repo spindle audience webhook =
    Common.run (fun env ->
        with_target ~busy:"Removing webhook" env repo spindle
          (fun api ~repo ~spindle ->
            Api.delete_webhook api ~spindle ?audience ~repo webhook);
        Ui.ok "Removed webhook %s" webhook)
  in
  Common.cmd
    (Cmd.info "remove" ~doc:"Remove a webhook.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.spindle $ Common.audience
        $ webhook))

let deliveries_cmd =
  let action repo spindle audience webhook limit json =
    Common.run (fun env ->
        let deliveries =
          with_target ~busy:"Reading deliveries" env repo spindle
            (fun api ~repo ~spindle ->
              Api.webhook_deliveries api ~spindle ?audience ~repo ~webhook
                ~limit ())
        in
        if json then Common.print (Jsont.list Defs.delivery_jsont) deliveries
        else if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Deliveries" (List.length deliveries))
            [
              Ui.column "ID";
              Ui.column "Event";
              Ui.column "Status";
              Ui.column "When";
            ]
            (List.map
               (fun (d : Defs.delivery) ->
                 [
                   Ui.muted d.id;
                   Ui.text (Tangled.Schema.text d.event);
                   (let code =
                      Option.fold ~none:"---" ~some:string_of_int
                        d.response_code
                    in
                    if d.success then Ui.V.role `Succeeded (Ui.text code)
                    else Ui.V.role `Failed (Ui.text code));
                   Ui.muted d.created_at;
                 ])
               deliveries)
        else
          List.iter
            (fun (d : Defs.delivery) ->
              Fmt.pr "%s\t%s\t%b@." d.id
                (Tangled.Schema.text d.event)
                d.success)
            deliveries)
  in
  Common.cmd
    (Cmd.info "deliveries"
       ~doc:"List a webhook's recent notifications and whether they arrived.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.spindle $ Common.audience $ webhook
       $ Common.limit $ Common.json_flag))

let redeliver_cmd =
  let delivery =
    Common.positional 1 "DELIVERY"
      "The notification's ID, as $(b,deliveries) shows it."
  in
  let action repo spindle audience webhook delivery =
    Common.run (fun env ->
        with_target ~busy:"Redelivering" env repo spindle
          (fun api ~repo ~spindle ->
            Api.redeliver_webhook api ~spindle ?audience ~repo ~webhook
              delivery);
        Ui.ok "Redelivered %s" delivery)
  in
  Common.cmd
    (Cmd.info "redeliver" ~doc:"Send a notification again.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.spindle $ Common.audience $ webhook
       $ delivery))

let cmd =
  Cmd.group
    (Cmd.info "webhook" ~doc:"Notify other services of a repository's events."
       ~man:
         (Common.man
            [
              "A webhook sends an HTTP request to a URL each time something \
               happens in the repository, such as a push, so that another \
               service can act on it. The spindle that runs the repository's \
               CI sends them, so the repository needs one. Managing webhooks \
               needs push access.";
            ]))
    [ list_cmd; add_cmd; edit_cmd; remove_cmd; deliveries_cmd; redeliver_cmd ]
