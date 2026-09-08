(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Auth = Jmap_eio.Auth
module Chain = Jmap.Chain
module Cli = Jmap_eio.Cli
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results
module Sync = Jmap_eio.Sync
module Transport = Jmap_eio.Transport

(* A login must be answered while the user is still watching. The default
   policy honours a Retry-After of any length, and the Cyrus test server
   refuses a credential with a 503 asking for thirty seconds. *)
let retry = Fetch.Retry.v ~backoff_max:(Duration.of_sec 2) ()
let default_timeout = Cli.default_timeout

(* Server text reaches the terminal through the model, so every string a
   response contributes to it is escaped here, at the one boundary where JMAP
   data enters it. The model builds status lines out of these strings and the
   view draws both unaltered, so escaping where they are drawn would have to be
   repeated for each of those compositions. The error printers of
   {!Jmap.Proto.Error} and {!Jmap_eio} escape their own text already. *)
let terminal_text = Cli.terminal_text

type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  account : string option;
  allow_insecure : bool;
  timeout : float;
  transport : Transport.t;
  mutable connection : (Client.t * Proto.Id.t) option;
}

let create ~sw ?account ?(timeout = default_timeout) ?transport
    ?(allow_insecure = false) env =
  if not (Float.is_finite timeout && timeout >= 0.) then
    invalid_arg
      "Jmap_mosaic.Io.create: ?timeout must be finite and non-negative";
  let transport =
    match transport with
    | Some transport -> transport
    | None -> Transport.v ~retry env
  in
  if Option.is_none (Transport.clock transport) then
    invalid_arg "Jmap_mosaic.Io.create: ?transport must carry a clock";
  { env; sw; account; allow_insecure; timeout; transport; connection = None }

let connection t = t.connection

let credential (c : Model.credentials) =
  try
    Ok
      (match c.scheme with
      | Model.Bearer -> Auth.bearer c.secret
      | Model.Basic -> Auth.basic ~user:c.user ~password:c.secret)
  with Invalid_argument message -> Error message

let account t client =
  let session = Client.session client in
  let selected =
    match t.account with
    | Some id -> (
        match Proto.Id.of_string id with
        | Ok id -> Ok id
        | Error m -> Error (Printf.sprintf "invalid account id %S: %s" id m))
    | None -> (
        match
          Proto.Session.primary_account_for Proto.Capability.mail session
        with
        | Some id -> Ok id
        | None -> Error "the session has no primary mail account")
  in
  Result.bind selected (fun id ->
      if not (Proto.Session.has_capability Proto.Capability.mail session) then
        Error "the server does not advertise the JMAP Mail capability"
      else
        match Proto.Session.find_account id session with
        | None ->
            Error
              (Format.asprintf "account %a is not present in the session"
                 Proto.Id.pp id)
        | Some account -> (
            match Proto.Session.mail_capability account with
            | Some _ -> Ok id
            | None ->
                Error
                  (Format.asprintf
                     "account %a does not support the JMAP Mail capability"
                     Proto.Id.pp id)))

let connect t c dispatch =
  match credential c with
  | Error message -> dispatch (Model.Login_failed message)
  | Ok auth -> (
      match
        Client.connect ~sw:t.sw ~auth ~timeout:t.timeout
          ~allow_insecure:t.allow_insecure t.transport c.url
      with
      | Error e -> dispatch (Model.Login_failed (Client.error_to_string e))
      | Ok client -> (
          match account t client with
          | Error m -> dispatch (Model.Login_failed m)
          | Ok id ->
              t.connection <- Some (client, id);
              dispatch
                (Model.Connected
                   {
                     account = id;
                     user = terminal_text (Client.session client).username;
                   })))

let mail_capabilities = Proto.[ Capability.core; Capability.mail ]
let submission_capabilities = Proto.[ Capability.core; Capability.submission ]

let capabilities =
  Proto.[ Capability.core; Capability.mail; Capability.submission ]

let supports_submission client account_id =
  let session = Client.session client in
  Proto.Session.has_capability Proto.Capability.submission session
  &&
  match Proto.Session.find_account account_id session with
  | Some account -> Option.is_some (Proto.Session.submission_capability account)
  | None -> false

let mailbox_properties =
  [ `Id; `Name; `Role; `Parent_id; `Total_emails; `Unread_emails ]

let list_properties =
  [ `Id; `Received_at; `From; `Subject; `Keywords; `Has_attachment; `Preview ]

(* A listing is the newest hundred messages of its source. RFC 8620 Section 5.1
   answers a [/get] of more ids than [maxObjectsInGet] with [requestTooLarge],
   so the query asks for no more than the [Email/get] beside it may name. *)
let listing_limit client =
  let wanted = 100L in
  match Sync.max_objects_in_get client with
  | Some max when max < wanted -> Int64.max 1L max
  | _ -> wanted

let message_properties =
  [
    `Id;
    `Received_at;
    `From;
    `To;
    `Reply_to;
    `Subject;
    `Keywords;
    `Has_attachment;
    `Preview;
    `Message_id;
    `References;
    `Mailbox_ids;
    `Text_body;
    `Body_values;
  ]

(* A body is the one server string drawn over several rows, so its line
   endings are what structures it and each line is escaped on its own. A CRLF
   body arrives with the carriage return attached, which would otherwise mark
   the end of every line. *)
let terminal_body text =
  String.split_on_char '\n' text
  |> List.map (fun line ->
      let length = String.length line in
      if length > 0 && line.[length - 1] = '\r' then
        terminal_text (String.sub line 0 (length - 1))
      else terminal_text line)
  |> String.concat "\n"

let addresses =
  Option.fold ~none:[]
    ~some:(List.map (fun (a : Proto.Email_address.t) -> terminal_text a.email))

let sender (e : Proto.Email.t) =
  match e.from with
  | Some (a :: _) -> terminal_text (Option.value a.name ~default:a.email)
  | _ -> "-"

let body (e : Proto.Email.t) =
  Option.value e.text_body ~default:[]
  |> List.filter_map (fun p ->
      Option.map
        (fun (v : Proto.Email_body.Value.t) -> v.value)
        (Proto.Email.body_value e p))
  |> String.concat "\n" |> terminal_body

let summary (e : Proto.Email.t) =
  Option.map
    (fun eid ->
      Model.
        {
          eid;
          received_at = e.received_at;
          sender = sender e;
          subject =
            terminal_text (Option.value e.subject ~default:"(no subject)");
          preview = terminal_text (Option.value e.preview ~default:"");
          seen = Proto.Email.has_keyword `Seen e;
          flagged = Proto.Email.has_keyword `Flagged e;
          answered = Proto.Email.has_keyword `Answered e;
          attachment = Option.value e.has_attachment ~default:false;
        })
    e.id

let message (e : Proto.Email.t) =
  Option.map
    (fun head ->
      Model.
        {
          head;
          addresses = addresses e.from;
          recipients = addresses e.to_;
          reply_to = addresses e.reply_to;
          message_id = Option.value e.message_id ~default:[];
          references = Option.value e.references ~default:[];
          mailboxes =
            List.filter_map
              (fun (id, v) -> if v then Some id else None)
              (Option.value e.mailbox_ids ~default:[]);
          keywords = Proto.Email.keyword_list e;
          body = body e;
        })
    (summary e)

let mailbox (m : Proto.Mailbox.t) =
  Option.map
    (fun id ->
      Model.
        {
          id;
          name = terminal_text (Option.value m.name ~default:"(unnamed)");
          parent = m.parent_id;
          role = m.role;
          total = Option.value m.total_emails ~default:0L;
          unread = Option.value m.unread_emails ~default:0L;
          depth = 0;
        })
    m.id

let search_filter t = function
  | Model.Mailbox box -> Proto.Email.filter ~in_mailbox:box ()
  | Model.Smart_search Model.Unread -> Proto.Email.filter ~not_keyword:`Seen ()
  | Model.Smart_search Model.Needs_follow_up ->
      let now = Eio.Time.now (Eio.Stdenv.clock t.env) in
      let cutoff =
        match Ptime.of_float_s (now -. (30. *. 86_400.)) with
        | Some cutoff -> cutoff
        | None -> invalid_arg "the current time is outside Ptime's range"
      in
      Proto.Filter.and_
        [
          Proto.Email.filter ~before:cutoff ~not_keyword:`Answered ();
          Proto.Email.filter ~not_keyword:`Draft ();
        ]

let sending_address client (i : Proto.Identity.t) =
  let local_part = (Client.session client).username in
  Option.map terminal_text (Proto.Identity.sending_address ~local_part i)

let refused failures =
  Format.asprintf "the server refused the change: %a"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
       Proto.Method.pp_set_failure)
    failures

let was_updated id response =
  Option.value response.Proto.Method.updated ~default:[]
  |> List.exists (fun (updated, _) -> Proto.Id.equal id updated)

(* The three ways a [/set] answers for one record. RFC 8620 Section 5.3 puts a
   refusal in [notUpdated] as a SetError, and a server that reports the record
   in neither list has said nothing about it. *)
type change = Applied | Refused of string | Unreported

let change id response =
  match Proto.Method.set_failures response with
  | [] when was_updated id response -> Applied
  | [] -> Unreported
  | failures -> Refused (refused failures)

(* The same three ways for a creation. A send tells them apart because only a
   refusal proves that nothing was submitted. *)
type 'a creation =
  | Created of 'a
  | Not_created of string
  | No_outcome of string

let created kind id creation response =
  match Proto.Method.created response creation with
  | Some value -> (
      match id value with
      | Some id -> Created id
      | None -> No_outcome ("the server created the " ^ kind ^ " without an id")
      )
  | None -> (
      match Proto.Method.set_failures response with
      | _ :: _ as failures -> Not_created (refused failures)
      | [] ->
          No_outcome
            ("the server reported neither success nor failure for the " ^ kind))

let filing_warning ~email_id call response =
  let responses =
    Proto.Response.find_responses (Chain.call_id call) response
    |> List.filter (fun (r : Proto.Invocation.t) ->
        not (String.equal r.name "EmailSubmission/set"))
  in
  match responses with
  | [] -> Some "reply sent, but the server reported no filing outcome"
  | response :: _ -> (
      match Proto.Response.error response with
      | Some (Ok error) ->
          Some
            ("reply sent, but filing it failed: "
            ^ Format.asprintf "%a" Proto.Error.Method_error.pp error)
      | Some (Error error) ->
          Some
            ("reply sent, but its filing error was malformed: "
            ^ terminal_text (Jsont.Error.to_string error))
      | None when String.equal response.name "Email/set" -> (
          match
            Jsont.Json.decode'
              (Proto.Method.set_response_jsont Proto.Email.jsont)
              response.arguments
          with
          | Error error ->
              Some
                ("reply sent, but its filing response was malformed: "
                ^ terminal_text (Jsont.Error.to_string error))
          | Ok filed -> (
              match change email_id filed with
              | Applied -> None
              | Unreported ->
                  Some
                    "reply sent, but the server reported no outcome while \
                     filing it"
              | Refused message ->
                  Some ("reply sent, but filing it failed: " ^ message)))
      | None ->
          Some
            (Printf.sprintf
               "reply sent, but the server returned %s instead of its filing \
                outcome"
               (terminal_text response.name)))

let patch client ~account_id id entries answer dispatch =
  let r =
    Client.call_exn client ~capabilities:mail_capabilities
      (Chain.email_set ~account_id ~update:[ (id, Proto.Patch.v entries) ] ())
  in
  match change id r with
  | Applied -> dispatch answer
  | Unreported ->
      dispatch (Model.Failed "the server reported no outcome for the change")
  | Refused message -> dispatch (Model.Failed message)

let send client ~account_id (d : Model.draft) dispatch =
  let draft_id = Proto.Id.creation "draft"
  and send_id = Proto.Id.creation "send" in
  let email =
    Proto.Email.create ~mailbox_ids:[ d.drafts ] ~keywords:[ `Draft ]
      ~from:[ Proto.Email_address.create d.from ]
      ~to_:(List.map Proto.Email_address.create d.recipients)
      ~subject:d.subject ~in_reply_to:d.in_reply_to ~references:d.references
      ~text_body:d.text ()
  in
  let submission email_id =
    Proto.Submission.create ~identity_id:d.identity ~email_id ()
  in
  let file_it =
    Proto.Patch.v
      Proto.Email.Patch.
        [
          remove_from_mailbox d.drafts;
          add_to_mailbox d.sent;
          remove_keyword `Draft;
        ]
  in
  let save () = Chain.email_set ~account_id ~create:[ (draft_id, email) ] () in
  let submit email_id =
    Chain.email_submission_set ~account_id
      ~create:[ (send_id, submission email_id) ]
      ~on_success_update_email:[ (Proto.Id.creation_ref send_id, file_it) ]
      ()
  in
  let combined =
    Chain.(
      let* saved = save () in
      let+ submitted = submit (Proto.Id.creation_ref draft_id) in
      Handles.[ saved; submitted ])
  in
  (* Each call of the request is read on its own, since what a failed read was
     tells the sender what to do. A method error replaces the response of a
     call the server did not run (RFC 8620 Section 3.6.2), so nothing that call
     was to create exists, while a response that does not decode says nothing
     either way. *)
  let read call response =
    match Chain.parse call response with
    | Ok value -> `Read value
    | Error (Chain.Method_error e) ->
        `Failed (Format.asprintf "%a" Proto.Error.Method_error.pp e)
    | Error (Chain.Json_error e) ->
        `Unreadable (terminal_text (Jsont.Error.to_string e))
  in
  let judge ~email_id submitted_call response = function
    | `Failed message -> `Not_submitted (Some email_id, message)
    | `Unreadable message -> `Unknown message
    | `Read submitted -> (
        match created "submission" Proto.Submission.id send_id submitted with
        | Created _ -> `Sent (filing_warning ~email_id submitted_call response)
        | Not_created message -> `Not_submitted (Some email_id, message)
        | No_outcome message -> `Unknown message)
  in
  let outcome =
    if Sync.chain_fits client ~capabilities combined then
      let Chain.Handles.[ saved_call; submitted_call ], response =
        Client.chain_exn client ~capabilities combined
      in
      match read saved_call response with
      | `Failed message -> `Not_submitted (None, message)
      (* The submission took its email id from the creation by back reference,
         so a draft the server did not name may still have been submitted. *)
      | `Unreadable message -> `Unknown message
      | `Read saved -> (
          match created "draft" Proto.Email.id draft_id saved with
          | Not_created message -> `Not_submitted (None, message)
          | No_outcome message -> `Unknown message
          | Created email_id ->
              judge ~email_id submitted_call response
                (read submitted_call response))
    else
      let saved_call, response =
        Client.chain_exn client ~capabilities:mail_capabilities (save ())
      in
      match read saved_call response with
      (* The submission is a request of its own here, so nothing was submitted
         whatever the creation answered. *)
      | `Failed message | `Unreadable message -> `Not_submitted (None, message)
      | `Read saved -> (
          match created "draft" Proto.Email.id draft_id saved with
          | Not_created message | No_outcome message ->
              `Not_submitted (None, message)
          | Created email_id ->
              let request =
                Chain.(
                  let+ submitted = submit email_id in
                  Handles.[ submitted ])
              in
              let Chain.Handles.[ submitted_call ], response =
                Client.chain_exn client ~capabilities request
              in
              judge ~email_id submitted_call response
                (read submitted_call response))
  in
  match outcome with
  | `Not_submitted (draft, message) ->
      (* Nothing was submitted, so the draft the send created is destroyed and
         the model may invite a retry. *)
      Option.iter
        (fun email_id ->
          ignore
            (Client.call client ~capabilities:mail_capabilities
               (Chain.email_set ~account_id ~destroy:(Chain.ids [ email_id ]) ())))
        draft;
      dispatch (Model.Send_failed message)
  | `Unknown message ->
      dispatch
        (Model.Sent_with_warning
           ("the reply may have been submitted: " ^ message
          ^ "; look in Sent before sending it again"))
  | `Sent filing_warning ->
      (* Marking the source as answered is deliberately a second request. If it
         fails after submission, reporting an ordinary send failure would invite
         a retry and duplicate the delivered message. *)
      let answered =
        Proto.Patch.v [ Proto.Email.Patch.set_keyword `Answered ]
      in
      let answering_warning =
        match
          Client.call client ~capabilities:mail_capabilities
            (Chain.email_set ~account_id ~update:[ (d.answering, answered) ] ())
        with
        | Error error ->
            Some
              ("reply sent, but the original could not be marked answered: "
              ^ Client.error_to_string error)
        | Ok marked -> (
            match change d.answering marked with
            | Applied -> None
            | Unreported ->
                Some
                  "reply sent, but the server reported no outcome while \
                   marking the original answered"
            | Refused message ->
                Some
                  ("reply sent, but the original could not be marked answered: "
                 ^ message))
      in
      dispatch
        (match List.filter_map Fun.id [ filing_warning; answering_warning ] with
        | [] -> Model.Sent
        | warnings -> Model.Sent_with_warning (String.concat "; " warnings))

let run t client ~account_id action dispatch =
  match (action : Model.action) with
  (* [perform] answers [Connect] before it calls this. *)
  | Connect _ -> ()
  | Load_mailboxes ->
      let got =
        Client.call_exn client ~capabilities:mail_capabilities
          (Chain.mailbox_get ~account_id ~properties:mailbox_properties ())
      in
      dispatch (Model.Mailboxes (List.filter_map mailbox got.list))
  | Load_identity when not (supports_submission client account_id) ->
      dispatch
        (Model.Failed "this account does not support JMAP email submission")
  | Load_identity -> (
      let got =
        Client.call_exn client ~capabilities:submission_capabilities
          (Chain.identity_get ~account_id ())
      in
      match
        List.find_map
          (fun (i : Proto.Identity.t) ->
            Option.bind i.id (fun identity_id ->
                Option.map
                  (fun address -> (identity_id, address))
                  (sending_address client i)))
          got.list
      with
      | Some (identity_id, address) ->
          dispatch (Model.Identity { identity_id; address })
      | None ->
          dispatch
            (Model.Failed
               "this account has no Identity with a usable email address"))
  | Load_messages source ->
      let query () =
        Chain.email_query ~account_id ~filter:(search_filter t source)
          ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
          ~limit:(listing_limit client) ()
      in
      let get ids =
        Chain.email_get ~account_id ~ids ~properties:list_properties ()
      in
      let listing =
        Chain.(
          let* q = query () in
          let+ g = get (from_query q) in
          Handles.[ q; g ])
      in
      let query, got =
        if Sync.chain_fits client ~capabilities:mail_capabilities listing then
          let Results.[ query; got ] =
            Client.run_exn client ~capabilities:mail_capabilities listing
          in
          (query, got)
        else
          let query =
            Client.call_exn client ~capabilities:mail_capabilities (query ())
          in
          ( query,
            Client.call_exn client ~capabilities:mail_capabilities
              (get (Chain.ids query.ids)) )
      in
      let ordered =
        Proto.Method.in_ids_order ~id:Proto.Email.id query.ids got.list
      in
      dispatch (Model.Messages (source, List.filter_map summary ordered))
  | Load_message id -> (
      let got =
        Client.call_exn client ~capabilities:mail_capabilities
          (Chain.email_get ~account_id ~ids:(Chain.id id)
             ~properties:message_properties ~fetch_text_body_values:true
             ~max_body_value_bytes:65536L ())
      in
      match List.filter_map message got.list with
      | m :: _ -> dispatch (Model.Opened m)
      | [] -> dispatch (Model.Failed "the message is no longer there"))
  | Set_keyword (id, k, on) ->
      let entry =
        if on then Proto.Email.Patch.set_keyword k
        else Proto.Email.Patch.remove_keyword k
      in
      patch client ~account_id id [ entry ] (Model.Keyword (id, k, on)) dispatch
  | Move { email; into; out_of } ->
      patch client ~account_id email
        Proto.Email.Patch.[ add_to_mailbox into; remove_from_mailbox out_of ]
        (Model.Moved (email, into))
        dispatch
  | Send _ when not (supports_submission client account_id) ->
      dispatch
        (Model.Send_failed "this account does not support JMAP email submission")
  | Send d -> send client ~account_id d dispatch

let perform t action user_dispatch =
  let exception Dispatch_raised of exn * Printexc.raw_backtrace in
  let dispatch message =
    try user_dispatch message
    with exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      raise_notrace (Dispatch_raised (exn, backtrace))
  in
  let failed message =
    dispatch
      (match action with
      | Model.Send _ -> Model.Send_failed message
      | _ -> Model.Failed message)
  in
  let go () =
    match (action, t.connection) with
    | Model.Connect c, _ -> connect t c dispatch
    | _, Some (client, account_id) -> run t client ~account_id action dispatch
    | _, None ->
        failed
          (Format.asprintf "not connected, cannot %a" Model.pp_action action)
  in
  (* [failed] dispatches, and OCaml does not apply a [try ... with] to an
     exception its own handler raises, so the report of a failure is the inner
     try and the unwrapping is the outer one. *)
  let report () =
    try go () with
    | Client.Jmap_client_error e -> failed (Client.error_to_string e)
    | Chain.Parse_error e -> failed (Chain.parse_error_to_string e)
    | Invalid_argument m -> failed ("invalid argument: " ^ terminal_text m)
  in
  try report ()
  with Dispatch_raised (exn, backtrace) ->
    Printexc.raise_with_backtrace exn backtrace
