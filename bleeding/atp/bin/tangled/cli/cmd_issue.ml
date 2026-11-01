(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Ticket = Api.Lex.Track.Ticket

let repo =
  Common.positional 0 "REPO"
    "Repository as handle/name, or a name of your own. Its DID or declaration \
     URI also work."

let rkey = Common.positional 1 "ID" "Ticket record key."

let text_of field =
  match Tangled.Schema.member "text" field with
  | Some text -> Tangled.Schema.text text
  | None -> ""

let state_of (t : Ticket.main) = Tangled.Schema.text t.state

(* A ticket with a target branch is a pull request. *)
let is_pull (t : Ticket.main) = Option.is_some t.target_branch
let kind ~pulls = if pulls then "pull request" else "issue"

let show_ticket rkey (t : Ticket.main) =
  let body = Option.fold ~none:"" ~some:text_of t.body in
  Ui.panel
    (Ui.V.concat [ Ui.muted ("#" ^ rkey ^ " "); Ui.title (text_of t.title) ])
    (Ui.V.vcat ~gap:1
       (Ui.fields
          [
            ("State", Some (Ui.state (state_of t)));
            ("Target", Option.map Ui.accent t.target_branch);
            ("Opened", Some (Ui.muted t.created_at));
            ( "Assignees",
              Option.map (fun a -> Ui.text (String.concat ", " a)) t.assignees
            );
          ]
       :: (if String.trim body = "" then [] else [ Ui.para body ])))

let list ~pulls =
  let state =
    Common.optional [ "state" ] "STATE"
      "Show only tickets in this state: open, closed or completed."
  in
  let action repo knot state json =
    Common.run (fun env ->
        let tickets =
          Common.with_api ~busy:"Reading tickets" env (fun api ->
              Api.tickets api (Api.resolve_repo api ?knot repo))
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
        else if Ui.tty () then
          Ui.table
            ~heading:
              (Ui.heading
                 (if pulls then "Pull requests" else "Issues")
                 (List.length tickets))
            ([
               Ui.column ~align:`Right "#";
               Ui.column "State";
               Ui.column "Title";
             ]
            @ (if pulls then [ Ui.column "Target" ] else [])
            @ [ Ui.column "Opened" ])
            (List.map
               (fun (rkey, (t : Ticket.main)) ->
                 [
                   Ui.muted rkey;
                   Ui.state (state_of t);
                   Ui.bold (text_of t.title);
                 ]
                 @ (if pulls then
                      [ Ui.accent (Option.value ~default:"" t.target_branch) ]
                    else [])
                 @ [ Ui.muted t.created_at ])
               tickets)
        else
          List.iter
            (fun (rkey, (t : Ticket.main)) ->
              Fmt.pr "%-6s %-9s %s%a@." rkey (state_of t) (text_of t.title)
                Fmt.(option (fmt " -> %s"))
                t.target_branch)
            tickets)
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List tickets in the repository account.")
    Term.(
      term_result
        (const action $ repo $ Common.knot $ state $ Common.json_flag))

let show =
  let action repo rkey knot json =
    Common.run (fun env ->
        match
          Common.with_api ~busy:("Reading ticket " ^ rkey) env (fun api ->
              Api.ticket api (Api.resolve_repo api ?knot repo) rkey)
        with
        | None -> failwith "Ticket not found"
        | Some t when json || not (Ui.tty ()) ->
            Common.print Ticket.main_jsont t
        | Some t -> show_ticket rkey t)
  in
  Cmd.v
    (Cmd.info "show" ~doc:"Show one ticket. Pipes get JSON.")
    Term.(
      term_result (const action $ repo $ rkey $ Common.knot $ Common.json_flag))

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
        let created =
          Common.with_api
            ~busy:("Opening " ^ kind ~pulls)
            env
            (fun api ->
              let repository = Api.resolve_repo api ?knot repo in
              Api.open_ticket api repository ~title ?body ?target_branch ())
        in
        if Ui.tty () then
          Ui.ok "Opened %s #%s  %s" (kind ~pulls)
            (Api.rkey_of_uri created.uri)
            created.uri
        else Common.print Api.Atproto.Repo.CreateRecord.output_jsont created)
  in
  Cmd.v
    (Cmd.info "create"
       ~doc:"Open a ticket. The knot writes it into the repository account.")
    Term.(
      term_result (const action $ repo $ Common.knot $ title $ body $ target))

let state name value =
  let action repo rkey knot =
    Common.run (fun env ->
        Common.with_api ~busy:("Updating ticket " ^ rkey) env (fun api ->
            Api.set_ticket_state api
              (Api.resolve_repo api ?knot repo)
              ~rkey value);
        Ui.ok "Ticket #%s is now %s" rkey value)
  in
  Cmd.v
    (Cmd.info name
       ~doc:
         ("Set a ticket's state to " ^ value ^ ". The knot checks authority."))
    Term.(term_result (const action $ repo $ rkey $ Common.knot))

let submit =
  let commit name doc =
    Arg.(required & opt (some string) None & info [ name ] ~docv:"SHA" ~doc)
  in
  let source =
    Common.optional [ "source" ] "REPO"
      "Repository holding the head commit (default: the target)."
  in
  let branch = Common.optional [ "branch" ] "BRANCH" "Branch of the head." in
  let action repo rkey knot base head source branch =
    Common.run (fun env ->
        let submitted =
          Common.with_api ~busy:("Submitting to #" ^ rkey) env (fun api ->
              let repository = Api.resolve_repo api ?knot repo in
              let source =
                Option.map (fun s -> (Api.resolve_repo api s).did) source
              in
              Api.submit_patch api repository ~ticket:rkey ~base ~head ?source
                ?branch ())
        in
        if Ui.tty () then
          Ui.ok "Submitted %s..%s to pull request #%s" (Ui.short_sha base)
            (Ui.short_sha head) rkey
        else Common.print Api.Lex.Review.SubmitPatch.output_jsont submitted)
  in
  Cmd.v
    (Cmd.info "submit" ~doc:"Submit a revision of a pull request.")
    Term.(
      term_result
        (const action $ repo $ rkey $ Common.knot
        $ commit "base" "Commit the revision is based on."
        $ commit "head" "Head commit of the revision."
        $ source $ branch))

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
       ~doc:"Query, open and revise pull requests.")
    [
      list ~pulls:true;
      show;
      create ~pulls:true;
      submit;
      state "close" "closed";
      state "reopen" "open";
    ]
