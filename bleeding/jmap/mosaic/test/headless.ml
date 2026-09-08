(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Cli = Jmap_eio.Cli
module Client = Jmap_eio.Client
module Io = Jmap_mosaic.Io
module Model = Jmap_mosaic.Model
module Proto = Jmap.Proto

let doc = "Perform every action of the client once against a live server"

let man =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "$(tname) drives $(b,Jmap_mosaic.Model) and $(b,Jmap_mosaic.Io) with a \
       scripted dispatch and no terminal. It logs in with a bad secret and \
       then with the right one, performs each action of the client once \
       against the account it is pointed at, prints the messages that came \
       back, and undoes everything it did before it returns.";
    `P
      "It writes to the account. Point it at a test server, not at a mailbox \
       that matters.";
  ]

let step io model action =
  Fmt.pr "@[<h>%a@]@." Model.pp_action action;
  let out = ref [] in
  Io.perform io action (fun m -> out := m :: !out);
  List.fold_left
    (fun (model, actions) msg ->
      Fmt.pr "@[<h>  %a@]@." Model.pp_msg msg;
      let model, more = Model.update msg model in
      Fmt.pr "@[<h>  status %S@]@." model.Model.status;
      (model, actions @ more))
    (model, []) (List.rev !out)

let rec drive io model = function
  | [] -> model
  | action :: rest ->
      let model, more = step io model action in
      drive io model (rest @ more)

let credentials (cfg : Cli.config) =
  let scheme =
    match cfg.auth with Cli.Bearer -> Model.Bearer | Basic -> Basic
  in
  match (scheme, String.index_opt cfg.api_key ':') with
  | Model.Basic, Some i ->
      Model.
        {
          url = cfg.session_url;
          scheme;
          user = String.sub cfg.api_key 0 i;
          secret =
            String.sub cfg.api_key (i + 1) (String.length cfg.api_key - i - 1);
        }
  | _ ->
      Model.{ url = cfg.session_url; scheme; user = ""; secret = cfg.api_key }

(* The Cyrus test server accepts any password for its users, so a credential
   it refuses has to name a user it does not have. *)
let wrong (c : Model.credentials) =
  match c.scheme with
  | Model.Bearer -> { c with secret = c.secret ^ "-wrong" }
  | Model.Basic -> { c with user = c.user ^ "-wrong" }

let listed client ~account_id box =
  Client.call_exn client ~capabilities:Io.capabilities
    Chain.(
      let* q =
        email_query ~account_id
          ~filter:(Proto.Email.filter ~in_mailbox:box ())
          ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
          ~limit:50L ()
      in
      email_get ~account_id ~ids:(from_query q) ~properties:[ `Id; `Subject ] ())

let with_subject client ~account_id box subject =
  List.filter_map
    (fun (e : Proto.Email.t) ->
      if Option.equal String.equal e.subject (Some subject) then e.id else None)
    (listed client ~account_id box).list

let destroy client ~account_id ids =
  if ids <> [] then
    let r =
      Client.call_exn client ~capabilities:Io.capabilities
        (Chain.email_set ~account_id ~destroy:(Chain.ids ids) ())
    in
    Fmt.pr "destroyed %d message(s)%a@."
      (List.length (Option.value r.destroyed ~default:[]))
      Fmt.(list ~sep:nop (any ", " ++ using fst Proto.Id.pp))
      (Proto.Method.set_failures r)

let mailbox_with_role (t : Model.t) role =
  List.find_opt (fun (m : Model.mailbox) -> m.role = Some role) t.mailboxes

let other_mailbox (t : Model.t) inbox =
  match mailbox_with_role t `Archive with
  | Some m -> m
  | None -> (
      match
        List.find_opt
          (fun (m : Model.mailbox) ->
            (not (Proto.Id.equal m.id inbox))
            && m.role <> Some `Drafts && m.role <> Some `Sent)
          t.mailboxes
      with
      | Some m -> m
      | None -> Fmt.failwith "the account has only an Inbox to move between")

let reply ctx io (t : Model.t) =
  let clock = Eio.Stdenv.clock ctx.Cli.env in
  let inbox =
    match t.listing with
    | Some (Model.Mailbox id) -> id
    | Some (Model.Smart_search _) -> Fmt.failwith "not listing a mailbox"
    | None -> Fmt.failwith "no mailbox listed"
  in
  let sent =
    match mailbox_with_role t `Sent with
    | Some m -> m.id
    | None -> Fmt.failwith "the account has no Sent mailbox"
  in
  let t, _ = Model.update (Model.Key (Model.Char 'r')) t in
  match (t.reply, t.identity) with
  | None, _ | _, None ->
      Fmt.failwith "the model refused to compose: %s" t.status
  | Some d, Some identity ->
      let subject =
        Printf.sprintf "%s (headless %.0f)" d.subject (Eio.Time.now clock)
      in
      let d =
        {
          d with
          subject;
          recipients = [ identity.address ];
          text = "headless\n";
        }
      in
      let answered_before =
        Option.fold ~none:false
          ~some:(fun (m : Model.message) -> m.head.answered)
          t.reading
      in
      let before =
        with_subject ctx.client ~account_id:ctx.account_id inbox subject
      in
      let clean () =
        let copies =
          with_subject ctx.client ~account_id:ctx.account_id inbox subject
          @ with_subject ctx.client ~account_id:ctx.account_id sent subject
        in
        destroy ctx.client ~account_id:ctx.account_id
          (List.filter (fun id -> not (List.mem id before)) copies);
        if not answered_before then
          let r =
            Client.call_exn ctx.client ~capabilities:Io.capabilities
              (Chain.email_set ~account_id:ctx.account_id
                 ~update:
                   [
                     ( d.answering,
                       Proto.Patch.v
                         [ Proto.Email.Patch.remove_keyword `Answered ] );
                   ]
                 ())
          in
          Fmt.pr "cleared $answered, state %s@." r.new_state
      in
      Fun.protect ~finally:clean (fun () ->
          let t = drive io t [ Model.Send d ] in
          let rec delivered n =
            let copies =
              List.filter
                (fun id -> not (List.mem id before))
                (with_subject ctx.client ~account_id:ctx.account_id inbox
                   subject)
            in
            if copies = [] && n > 0 then (
              Eio.Time.sleep clock 1.;
              delivered (n - 1))
            else copies
          in
          let copies = delivered 20 in
          Fmt.pr "%d copy delivered to the Inbox@." (List.length copies);
          Fmt.pr "%d copy filed into Sent@."
            (List.length
               (with_subject ctx.client ~account_id:ctx.account_id sent subject));
          t)

let open_it io t =
  let t, actions = Model.update (Model.Key Model.Enter) t in
  drive io t actions

let pick io t target =
  let t, _ = Model.update (Model.Key (Model.Char 'm')) t in
  let want =
    Option.get
      (List.find_index
         (fun (m : Model.mailbox) -> Proto.Id.equal m.id target)
         t.mailboxes)
  in
  let rec go (t : Model.t) =
    if t.picker = want then t
    else
      go
        (fst
           (Model.update
              (Model.Key (if t.picker < want then Model.Down else Model.Up))
              t))
  in
  let t, actions = Model.update (Model.Key Model.Enter) (go t) in
  drive io t actions

let () =
  Cli.main "headless" ~doc ~man @@ fun ctx ->
  let io =
    Io.create ~sw:ctx.sw ?account:ctx.config.account_id
      ~allow_insecure:ctx.config.allow_insecure ctx.env
  in
  let good = credentials ctx.config in
  let model, _ = Model.init Model.blank in
  Fmt.pr "logging in with a credential the server refuses@.";
  let model = drive io model [ Model.Connect (wrong good) ] in
  (match model.Model.screen with
  | Model.Login l when l.error <> "" -> Fmt.pr "refused: %s@." l.error
  | _ -> Fmt.failwith "the wrong secret was accepted");
  Fmt.pr "@.logging in@.";
  let t = drive io model [ Model.Connect good ] in
  if t.session = None then Fmt.failwith "the right secret was refused";
  Fmt.pr "@.%d mailbox(es), %d message(s) listed@." (List.length t.mailboxes)
    (List.length t.messages);
  let inbox =
    match t.listing with
    | Some (Model.Mailbox id) -> id
    | Some (Model.Smart_search _) -> Fmt.failwith "not listing the Inbox"
    | None -> Fmt.failwith "no Inbox listed"
  in
  let first =
    match t.messages with
    | s :: _ -> s
    | [] -> Fmt.failwith "the Inbox is empty; deliver a message first"
  in
  let restore () =
    Fmt.pr "@.restoring the keywords of %a@." Proto.Id.pp first.eid;
    ignore
      (drive io t
         [
           Model.Set_keyword (first.eid, `Seen, first.seen);
           Model.Set_keyword (first.eid, `Flagged, first.flagged);
         ])
  in
  Fun.protect ~finally:restore @@ fun () ->
  Fmt.pr "@.opening %a@." Proto.Id.pp first.eid;
  let t = open_it io t in

  Fmt.pr "@.keywords@.";
  let t = drive io t [ Model.Set_keyword (first.eid, `Flagged, true) ] in
  let t = drive io t [ Model.Set_keyword (first.eid, `Flagged, false) ] in

  let target = other_mailbox t inbox in
  Fmt.pr "@.filing into %s with the picker, then back@." target.name;
  let t = pick io t target.id in
  let t =
    drive io t
      [ Model.Move { email = first.eid; into = inbox; out_of = target.id } ]
  in
  let t = drive io t [ Model.Load_messages (Model.Mailbox inbox) ] in
  let t = open_it io t in

  Fmt.pr "@.replying@.";
  let t = reply ctx io t in
  Fmt.pr "@.done, status %S@." t.status
