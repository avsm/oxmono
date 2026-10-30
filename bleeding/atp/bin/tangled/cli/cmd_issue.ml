(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Ticket = Api.Lex.Track.Ticket

let repo =
  Common.positional 0 "REPO" "Repository DID, owner/name, or declaration URI."

let rkey = Common.positional 1 "ID" "Ticket record key."

let text_of field =
  match Tangled.Schema.member "text" field with
  | Some text -> Tangled.Schema.text text
  | None -> ""

let state_of (t : Ticket.main) = Tangled.Schema.text t.state

(* A ticket with a target branch is a pull request. *)
let is_pull (t : Ticket.main) = Option.is_some t.target_branch

let list ~pulls =
  let state =
    Common.optional [ "state" ] "STATE"
      "Show only tickets in this state: open, closed or completed."
  in
  let action repo knot state json =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            let tickets =
              Api.tickets api repository
              |> List.filter (fun (_, t) ->
                  is_pull t = pulls
                  && match state with None -> true | Some s -> state_of t = s)
            in
            if json then
              Common.json
                (Jsont.Json.list
                   (List.map
                      (fun (rkey, t) ->
                        Common.object_
                          [
                            ("id", Common.string rkey);
                            ("record", Api.encode Ticket.main_jsont t);
                          ])
                      tickets))
            else
              List.iter
                (fun (rkey, (t : Ticket.main)) ->
                  Fmt.pr "%-6s %-9s %s%a@." rkey (state_of t) (text_of t.title)
                    Fmt.(option (fmt " -> %s"))
                    t.target_branch)
                tickets))
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List tickets in the repository account.")
    Term.(
      term_result
        (const action $ repo $ Common.knot $ state $ Common.json_flag))

let show =
  let action repo rkey knot =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            match Api.ticket api repository rkey with
            | None -> failwith "Ticket not found"
            | Some t -> Common.print Ticket.main_jsont t))
  in
  Cmd.v
    (Cmd.info "show" ~doc:"Show one ticket.")
    Term.(term_result (const action $ repo $ rkey $ Common.knot))

let create ~pulls =
  let title =
    Arg.(required & opt (some string) None & info [ "title" ] ~docv:"TITLE")
  in
  let body =
    Common.optional [ "body-file" ] "FILE" "Body file, or - for stdin."
  in
  let target =
    if pulls then
      Term.(
        const Option.some
        $ Arg.(
            required
            & opt (some string) None
            & info [ "target" ] ~docv:"BRANCH"
                ~doc:"Branch the pull request targets."))
    else Term.const None
  in
  let action repo knot title body target_branch =
    Common.run (fun env ->
        let body = Option.map Common.read_file body in
        Common.with_api env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            Common.print Api.Atproto.Repo.CreateRecord.output_jsont
              (Api.open_ticket api repository ~title ?body ?target_branch ())))
  in
  Cmd.v
    (Cmd.info "create"
       ~doc:"Open a ticket. The knot writes it into the repository account.")
    Term.(
      term_result (const action $ repo $ Common.knot $ title $ body $ target))

let state name value =
  let action repo rkey knot =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            Api.set_ticket_state api
              (Api.resolve_repo api ?knot repo)
              ~rkey value))
  in
  Cmd.v
    (Cmd.info name
       ~doc:
         ("Set a ticket's state to " ^ value ^ ". The knot checks authority."))
    Term.(term_result (const action $ repo $ rkey $ Common.knot))

let cmd =
  Cmd.group
    (Cmd.info "issue" ~doc:"Query, open and update issues.")
    [
      list ~pulls:false;
      show;
      create ~pulls:false;
      state "close" "closed";
      state "complete" "completed";
      state "reopen" "open";
    ]

let pull_cmd =
  Cmd.group
    (Cmd.info "pull"
       ~doc:
         "Query and open pull requests. Submit revisions with \
          org.tangled.review.submitPatch.")
    [
      list ~pulls:true;
      show;
      create ~pulls:true;
      state "close" "closed";
      state "reopen" "open";
    ]
