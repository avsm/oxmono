(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Model = Jmap_mosaic.Model
module Proto = Jmap.Proto

let id = Proto.Id.of_string_exn

let mailbox ?parent ?role name i =
  Model.
    {
      id = id i;
      name;
      parent = Option.map id parent;
      role;
      total = 0L;
      unread = 0L;
      depth = 0;
    }

let summary ?(seen = false) ?(flagged = false) ?(answered = false) i subject =
  Model.
    {
      eid = id i;
      received_at = None;
      sender = "Ada";
      subject;
      preview = "";
      seen;
      flagged;
      answered;
      attachment = false;
    }

let message ?(reply_to = []) ?(references = []) head =
  Model.
    {
      head;
      addresses = [ "ada@example.com" ];
      recipients = [ "me@example.com" ];
      reply_to;
      message_id = [ "m1@example.com" ];
      references;
      mailboxes = [ id "I" ];
      keywords = [ `Seen ];
      body = "hello\nthere";
    }

let mailboxes =
  [
    mailbox ~role:`Inbox "Inbox" "I";
    mailbox ~role:`Archive "Archive" "A";
    mailbox ~parent:"A" "Work" "W";
    mailbox ~role:`Drafts "Drafts" "D";
    mailbox ~role:`Sent "Sent" "S";
  ]

let identity = Model.{ identity_id = id "id1"; address = "me@example.com" }

let feed t msgs =
  List.fold_left
    (fun (t, acc) m ->
      let t, actions = Model.update m t in
      (t, acc @ actions))
    (t, []) msgs

let key k = Model.Key k

let typed s t =
  fst
    (feed t
       (List.map (fun c -> key (Model.Char c)) (List.of_seq (String.to_seq s))))

let filled =
  Model.
    {
      blank with
      url = "http://localhost:18080/.well-known/jmap";
      scheme = Basic;
      user = "user1";
      secret = "x";
    }

let session = Model.{ account = id "acc"; user = "user1" }

let form (t : Model.t) =
  match t.screen with
  | Model.Login l | Model.Connecting l -> l
  | _ -> Alcotest.fail "not on the login screen"

let actions = Alcotest.(list string)
let names l = List.map (Format.asprintf "%a" Model.pp_action) l
let check name expected got = Alcotest.check actions name expected (names got)

(* The state after the mailboxes, the Identity and the Inbox listing have all
   come back, which is where most of the tests start. *)
let ready ?(messages = []) () =
  let t, _ = Model.init filled in
  let t, _ =
    feed t
      [
        Model.Connected session;
        Model.Mailboxes mailboxes;
        Model.Identity identity;
        Model.Messages (Model.Mailbox (id "I"), messages);
      ]
  in
  t

let test_init () =
  let t, actions = Model.init filled in
  check "connects at once"
    [ "connect to http://localhost:18080/.well-known/jmap" ]
    actions;
  Alcotest.(check bool) "connecting" true (t.screen = Model.Connecting filled);
  Alcotest.(check int) "one in flight" 1 t.pending;
  let t, actions = Model.init Model.blank in
  check "nothing to do" [] actions;
  Alcotest.(check bool) "the form" true (t.screen = Model.Login Model.blank)

let test_login_editing () =
  let t, _ = Model.init Model.blank in
  let t = typed "http://h/jmap" t in
  Alcotest.(check string) "url" "http://h/jmap" (form t).url;
  let t = fst (Model.update (key Model.Backspace) t) in
  Alcotest.(check string) "rubbed out" "http://h/jma" (form t).url;
  let t = fst (feed t [ key Model.Tab; key Model.Tab ]) in
  Alcotest.(check bool)
    "the secret is next for a bearer" true
    ((form t).field = Model.Secret_field);
  let t = typed "tok" t in
  Alcotest.(check string) "secret" "tok" (form t).secret;
  let t = fst (Model.update (key Model.Back_tab) t) in
  Alcotest.(check bool)
    "back to the scheme" true
    ((form t).field = Model.Scheme_field);
  let t = fst (feed t [ key (Model.Char ' '); key Model.Tab ]) in
  Alcotest.(check bool)
    "a basic user is asked for" true
    ((form t).field = Model.User_field);
  let t = typed "ada" t in
  Alcotest.(check string) "user" "ada" (form t).user;
  Alcotest.(check string) "the url is untouched" "http://h/jma" (form t).url

let test_login_paste () =
  let t, _ = Model.init Model.blank in
  let t = fst (feed t [ key Model.Tab; key Model.Tab ]) in
  let t = fst (Model.update (key (Model.Paste "fmu1-abc-def\r\n")) t) in
  Alcotest.(check string)
    "a pasted token loses its line ending" "fmu1-abc-def" (form t).secret;
  let t = fst (Model.update (key (Model.Paste "  more")) t) in
  Alcotest.(check string) "a paste appends" "fmu1-abc-defmore" (form t).secret;
  let t = fst (Model.update (key (Model.Paste "\027\127\u{0085}")) t) in
  Alcotest.(check string)
    "pasted controls are discarded" "fmu1-abc-defmore" (form t).secret;
  let t = fst (Model.update (key (Model.Char '\127')) t) in
  Alcotest.(check string)
    "typed DEL is discarded" "fmu1-abc-defmore" (form t).secret

let test_login_scheme () =
  let t, _ = Model.init Model.blank in
  Alcotest.(check bool) "bearer to start" true ((form t).scheme = Model.Bearer);
  let t = fst (Model.update (key (Model.Ctrl 'a')) t) in
  Alcotest.(check bool) "basic" true ((form t).scheme = Model.Basic);
  let t = fst (feed t [ key Model.Tab; key Model.Tab ]) in
  Alcotest.(check bool)
    "the user field is there" true
    ((form t).field = Model.User_field);
  let t = fst (Model.update (key (Model.Ctrl 'a')) t) in
  Alcotest.(check bool) "bearer again" true ((form t).scheme = Model.Bearer);
  Alcotest.(check bool)
    "and the focus is off the hidden field" true
    ((form t).field = Model.Secret_field)

let test_login_connects () =
  let t, _ = Model.init Model.blank in
  let t, actions = Model.update (key Model.Enter) t in
  check "nothing without a url" [] actions;
  Alcotest.(check string)
    "said what is missing" "a session URL is needed" (form t).error;
  let controlled = { filled with url = "http://localhost/\027[2J" } in
  let t, actions = Model.init controlled in
  check "does not connect with a control in the URL" [] actions;
  let t, actions = Model.update (key Model.Enter) t in
  check "still no connection" [] actions;
  Alcotest.(check string)
    "the control is explained" "the session URL contains a control character"
    (form t).error;
  let t, _ = Model.init { filled with secret = "" } in
  let t = typed "x" t in
  let t, actions = Model.update (key Model.Enter) t in
  check "connects"
    [ "connect to http://localhost:18080/.well-known/jmap" ]
    actions;
  Alcotest.(check bool)
    "connecting" true
    (match t.screen with Model.Connecting _ -> true | _ -> false);
  let ignored, actions = Model.update (key (Model.Char 'q')) t in
  check "keys are dropped while it connects" [] actions;
  Alcotest.(check bool) "and change nothing" true (ignored = t)

let test_login_fails () =
  let t, _ = Model.init filled in
  let t, actions = Model.update (Model.Login_failed "401 unauthorized") t in
  check "nothing follows" [] actions;
  let l = form t in
  Alcotest.(check bool)
    "back on the form" true
    (match t.screen with Model.Login _ -> true | _ -> false);
  Alcotest.(check string) "the error is shown" "401 unauthorized" l.error;
  Alcotest.(check string) "the secret is gone" "" l.secret;
  Alcotest.(check string) "the user is kept" "user1" l.user;
  Alcotest.(check bool) "asking for it again" true (l.field = Model.Secret_field);
  Alcotest.(check int) "not in flight" 0 t.pending

let test_login_succeeds () =
  let t, _ = Model.init filled in
  let t, actions = Model.update (Model.Connected session) t in
  check "the first loads" [ "load mailboxes"; "load identity" ] actions;
  Alcotest.(check bool) "on the folders" true (t.screen = Model.Folders);
  Alcotest.(check bool) "the session is kept" true (t.session = Some session);
  Alcotest.(check int) "two in flight" 2 t.pending

let test_login_escape_quits () =
  let t, _ = Model.init Model.blank in
  let t, _ = Model.update (key Model.Escape) t in
  Alcotest.(check bool) "quitting" true t.quit

let test_profiles () =
  let work = { filled with profile = "work"; user = "user2" } in
  let t, actions = Model.init ~profiles:[ filled; work ] Model.blank in
  check "picker makes no request" [] actions;
  Alcotest.(check bool) "on the picker" true (t.screen = Model.Profiles);
  let t, _ = Model.update (key Model.Down) t in
  let t, actions = Model.update (key Model.Enter) t in
  check "selected profile connects"
    [ "connect to http://localhost:18080/.well-known/jmap" ]
    actions;
  Alcotest.(check string) "the selected profile is used" "work" (form t).profile;
  let t, _ = Model.update (Model.Connected session) t in
  Alcotest.(check (option string))
    "active profile" (Some "work") t.active_profile;
  let picker, _ = Model.init ~profiles:[ filled ] Model.blank in
  let creating, _ = Model.update (key (Model.Char 'n')) picker in
  Alcotest.(check string)
    "a new profile starts unnamed" "" (form creating).profile;
  Alcotest.(check bool)
    "the name has focus" true
    ((form creating).field = Model.Profile_field);
  let cancelled, _ = Model.update (key Model.Escape) creating in
  Alcotest.(check bool)
    "cancel returns to profiles" true
    (cancelled.screen = Model.Profiles)

let test_profile_names () =
  Alcotest.(check bool) "simple" true (Model.valid_profile_name "fastmail-work");
  Alcotest.(check bool) "dot" false (Model.valid_profile_name ".");
  Alcotest.(check bool) "slash" false (Model.valid_profile_name "../work");
  Alcotest.(check bool)
    "temporary suffix" false
    (Model.valid_profile_name "work.tmp~123-0")

let test_inbox_is_selected () =
  let t, _ = Model.init filled in
  let t, _ = Model.update (Model.Connected session) t in
  let t, actions = Model.update (Model.Mailboxes mailboxes) t in
  check "listing" [ "load messages of I" ] actions;
  Alcotest.(check bool)
    "listing the Inbox" true
    (t.listing = Some (Model.Mailbox (id "I")));
  Alcotest.(check int) "highlighted after the searches" 2 t.selection

let test_order () =
  let ordered = Model.order mailboxes in
  Alcotest.(check (list string))
    "roles then children"
    [ "Inbox"; "Drafts"; "Sent"; "Archive"; "Work" ]
    (List.map (fun (m : Model.mailbox) -> m.name) ordered);
  Alcotest.(check (list int))
    "depths" [ 0; 0; 0; 0; 1 ]
    (List.map (fun (m : Model.mailbox) -> m.depth) ordered)

let test_order_keeps_a_cycle () =
  let cycled =
    [
      mailbox ~role:`Inbox "Inbox" "I";
      mailbox ~parent:"B" "A" "A";
      mailbox ~parent:"A" "B" "B";
    ]
  in
  let ordered = Model.order cycled in
  Alcotest.(check (list string))
    "a mailbox in a parent cycle is still listed" [ "Inbox"; "A"; "B" ]
    (List.map (fun (m : Model.mailbox) -> m.name) ordered);
  Alcotest.(check (list int))
    "and is placed as a root" [ 0; 0; 1 ]
    (List.map (fun (m : Model.mailbox) -> m.depth) ordered)

let test_form_fields_agree () =
  let named = { filled with profile = "work space" } in
  Alcotest.(check bool) "an invalid name is not ready" false (Model.ready named);
  let t, actions = Model.init named in
  check "and connects to nothing" [] actions;
  Alcotest.(check bool)
    "the name is in hand" true
    ((form t).field = Model.Profile_field);
  let t, _ = Model.update (key Model.Enter) t in
  Alcotest.(check string)
    "the name is explained"
    "profile names use letters, digits, '.', '-' and '_'" (form t).error;
  Alcotest.(check bool)
    "a control in the user is not ready" false
    (Model.ready { filled with scheme = Model.Bearer; user = "a\tb" })

let test_other_mailbox_ignored () =
  let t = ready () in
  let t, _ =
    Model.update
      (Model.Messages (Model.Mailbox (id "A"), [ summary "e1" "x" ]))
      t
  in
  Alcotest.(check int) "not listed" 0 (List.length t.messages)

let test_navigation () =
  let t = ready ~messages:[ summary "e1" "one"; summary "e2" "two" ] () in
  Alcotest.(check bool) "focus follows the listing" true (t.focus = Message_pane);
  let t, _ = feed t [ key Down; key Down ] in
  Alcotest.(check int) "clamped at the last" 1 t.message;
  let t, _ = feed t [ key Tab; key (Char 'j') ] in
  Alcotest.(check int) "moves the navigation now" 3 t.selection;
  Alcotest.(check int) "and not the message" 1 t.message

let test_enter_opens_and_marks_seen () =
  let t =
    ready ~messages:[ summary "e1" "one"; summary ~seen:true "e2" "two" ] ()
  in
  let t', actions = Model.update (key Enter) t in
  check "read before changing search membership" [ "load message e1" ] actions;
  Alcotest.(check bool) "reading" true (t'.screen = Reading);
  let _, actions =
    Model.update (Model.Opened (message (summary "e1" "one"))) t'
  in
  check "mark seen after loading" [ "set $seen on e1" ] actions;
  let t, _ = Model.update (key Down) t in
  let _, actions = Model.update (key Enter) t in
  check "already seen" [ "load message e2" ] actions

let test_open_from_unread () =
  let head = summary "e1" "one" in
  let t = ready ~messages:[ head ] () in
  let t, _ = Model.update (key (Char '1')) t in
  let source = Model.Smart_search Model.Unread in
  let t, _ = Model.update (Model.Messages (source, [ head ])) t in
  let t, actions = Model.update (key Enter) t in
  check "only load is in flight" [ "load message e1" ] actions;
  let t, actions = Model.update (Model.Opened (message head)) t in
  check "loaded message can be marked" [ "set $seen on e1" ] actions;
  let t, _ = Model.update (Model.Keyword (head.eid, `Seen, true)) t in
  let t, _ = Model.update (Model.Messages (source, [])) t in
  Alcotest.(check bool)
    "removing the search row keeps the open message" true
    (Option.is_some t.reading);
  Alcotest.(check int) "search row removed" 0 (List.length t.messages)

let test_enter_on_a_mailbox () =
  let t = ready () in
  let t, _ = Model.update (key Tab) t in
  let t, _ = feed t [ key Down ] in
  let t, actions = Model.update (key Enter) t in
  check "listing the next mailbox" [ "load messages of D" ] actions;
  Alcotest.(check bool)
    "listing" true
    (t.listing = Some (Model.Mailbox (id "D")))

let test_smart_searches () =
  let t = ready ~messages:[ summary "e1" "one" ] () in
  let unread, actions = Model.update (key (Char '1')) t in
  check "unread shortcut" [ "load smart search \"Unread\"" ] actions;
  Alcotest.(check bool)
    "unread is selected" true
    (unread.listing = Some (Model.Smart_search Model.Unread));
  let unread, _ =
    Model.update
      (Model.Messages (Model.Smart_search Model.Unread, [ summary "e1" "one" ]))
      unread
  in
  let unread, actions =
    Model.update (Model.Keyword (id "e1", `Seen, true)) unread
  in
  check "keyword refreshes the search"
    [ "load smart search \"Unread\"" ]
    actions;
  let unread, _ =
    Model.update (Model.Messages (Model.Smart_search Model.Unread, [])) unread
  in
  Alcotest.(check int)
    "a message marked seen leaves Unread" 0
    (List.length unread.messages);
  let follow_up, actions = Model.update (key (Char '2')) t in
  check "follow-up shortcut" [ "load smart search \"Unanswered >30d\"" ] actions;
  Alcotest.(check bool)
    "follow-up is selected" true
    (follow_up.listing = Some (Model.Smart_search Model.Needs_follow_up));
  let blocked, actions = Model.update (key (Char 'm')) follow_up in
  check "filing from a search is blocked" [] actions;
  Alcotest.(check string)
    "explains how to file"
    "switch to a mailbox before filing a smart-search result" blocked.status

let test_toggles () =
  let t = ready ~messages:[ summary "e1" "one" ] () in
  let _, actions = Model.update (key (Char 'u')) t in
  check "setting" [ "set $seen on e1" ] actions;
  let t, _ = Model.update (Model.Keyword (id "e1", `Seen, true)) t in
  Alcotest.(check bool) "the row knows" true (List.hd t.messages).seen;
  let _, actions = Model.update (key (Char 'u')) t in
  check "clearing" [ "clear $seen on e1" ] actions;
  let _, actions = Model.update (key (Char 'f')) t in
  check "flagging" [ "set $flagged on e1" ] actions

let test_move () =
  let t = ready ~messages:[ summary "e1" "one" ] () in
  let t, _ = Model.update (key (Char 'm')) t in
  Alcotest.(check bool) "picking" true (t.screen = Picking Folders);
  let cancelled, actions = Model.update (key Escape) t in
  check "cancelled" [] actions;
  Alcotest.(check bool) "back" true (cancelled.screen = Folders);
  let t, _ = feed t [ key Down ] in
  let t, actions = Model.update (key Enter) t in
  check "moved" [ "move e1 from I to D" ] actions;
  Alcotest.(check bool) "back" true (t.screen = Folders);
  let t, actions = Model.update (Model.Moved (id "e1", id "D")) t in
  check "counts are stale" [ "load mailboxes" ] actions;
  Alcotest.(check int) "the row is gone" 0 (List.length t.messages)

let test_move_into_the_same_mailbox () =
  let t = ready ~messages:[ summary "e1" "one" ] () in
  let t, _ = Model.update (key (Char 'm')) t in
  let _, actions = Model.update (key Enter) t in
  check "nothing to do" [] actions

let test_reply_subject () =
  Alcotest.(check string) "prefixed" "Re: hi" (Model.reply_subject "hi");
  Alcotest.(check string) "kept" "Re: hi" (Model.reply_subject "Re: hi");
  Alcotest.(check string) "case folded" "RE: hi" (Model.reply_subject "RE: hi")

let test_quote () =
  Alcotest.(check string)
    "quoted" "On -, ada@example.com wrote:\n> hello\n> there\n"
    (Model.quote (message (summary "e1" "one")))

let opened ?reply_to t =
  let t, _ = feed t [ key Enter ] in
  let t, _ =
    Model.update (Model.Opened (message ?reply_to (summary "e1" "one"))) t
  in
  t

let test_reply_needs_an_identity () =
  let t, _ = Model.init filled in
  let t, _ =
    feed t
      [
        Model.Connected session;
        Model.Mailboxes mailboxes;
        Model.Messages (Model.Mailbox (id "I"), [ summary "e1" "one" ]);
      ]
  in
  let t = opened t in
  let t, actions = Model.update (key (Char 'r')) t in
  check "nothing sent" [] actions;
  Alcotest.(check bool) "still reading" true (t.screen = Reading);
  Alcotest.(check string)
    "said so" "this account has no Identity to send from" t.status

let test_reply () =
  let t = ready ~messages:[ summary "e1" "one" ] () in
  let t = opened t in
  let t, _ = Model.update (key (Char 'r')) t in
  Alcotest.(check bool) "composing" true (t.screen = Composing);
  let d = Option.get t.reply in
  Alcotest.(check string) "subject" "Re: one" d.subject;
  Alcotest.(check (list string))
    "to the sender" [ "ada@example.com" ] d.recipients;
  Alcotest.(check (list string))
    "in reply to" [ "m1@example.com" ] d.in_reply_to;
  Alcotest.(check (list string)) "references" [ "m1@example.com" ] d.references;
  Alcotest.(check bool) "drafts" true (Proto.Id.equal d.drafts (id "D"));
  Alcotest.(check bool) "sent" true (Proto.Id.equal d.sent (id "S"));
  Alcotest.(check bool) "quoted" true (String.length d.text > 0);
  let t, _ = Model.update (Model.Draft_text "a reply") t in
  Alcotest.(check string) "edited" "a reply" (Option.get t.reply).text;
  let sending, actions = Model.update (key (Ctrl 's')) t in
  check "sent" [ "send \"Re: one\"" ] actions;
  Alcotest.(check bool) "sending" true sending.sending;
  let still_sending, actions = Model.update (key (Ctrl 's')) sending in
  check "a second send is suppressed" [] actions;
  Alcotest.(check int)
    "no second request was counted" sending.pending still_sending.pending;
  let still_sending, actions = Model.update (key Escape) still_sending in
  check "cannot abandon an in-flight send" [] actions;
  Alcotest.(check bool) "still composing" true (still_sending.screen = Composing);
  let sent, _ = Model.update Model.Sent sending in
  Alcotest.(check bool) "back to the message" true (sent.screen = Reading);
  Alcotest.(check bool) "answered" true (List.hd sent.messages).answered;
  Alcotest.(check bool) "send settled" false sent.sending;
  let failed, _ = Model.update (Model.Send_failed "refused") sending in
  Alcotest.(check bool) "failure permits retry" false failed.sending;
  Alcotest.(check bool)
    "failure keeps the draft" true
    (Option.is_some failed.reply);
  let warned, _ =
    Model.update (Model.Sent_with_warning "sent, marking failed") sending
  in
  Alcotest.(check string)
    "post-send warning" "sent, marking failed" warned.status;
  Alcotest.(check bool)
    "a failed mark is not reported as applied" false
    (List.hd warned.messages).answered;
  Alcotest.(check bool)
    "warning still completes send" true
    (warned.screen = Reading && Option.is_none warned.reply);
  let abandoned, _ = Model.update (key Escape) t in
  Alcotest.(check bool) "no draft left" true (abandoned.reply = None)

let test_stale_open_is_ignored () =
  let t = ready ~messages:[ summary "e1" "one" ] () in
  let loading, _ = Model.update (key Enter) t in
  let left, _ = Model.update (key Escape) loading in
  let after, actions =
    Model.update (Model.Opened (message (summary "e1" "one"))) left
  in
  check "a stale response starts no work" [] actions;
  Alcotest.(check bool) "still at the list" true (after.screen = Folders);
  Alcotest.(check bool)
    "stale message not installed" true
    (Option.is_none after.reading)

let test_reply_to_header () =
  let t = ready ~messages:[ summary "e1" "one" ] () in
  let t = opened ~reply_to:[ "list@example.com" ] t in
  let t, _ = Model.update (key (Char 'r')) t in
  Alcotest.(check (list string))
    "Reply-To wins" [ "list@example.com" ] (Option.get t.reply).recipients

let test_escape_and_quit () =
  let t = ready ~messages:[ summary "e1" "one" ] () in
  let t = opened t in
  let t, _ = Model.update (key Escape) t in
  Alcotest.(check bool) "back" true (t.screen = Folders);
  let quitting, _ = Model.update (key (Char 'q')) t in
  Alcotest.(check bool) "quitting" true quitting.quit;
  let reading = opened t in
  let kept, _ = Model.update (key (Char 'q')) reading in
  Alcotest.(check bool) "only from the top" false kept.quit

let test_failure_is_reported () =
  let t = ready () in
  let t, actions = Model.update (Model.Failed "the server said no") t in
  check "nothing follows" [] actions;
  Alcotest.(check string) "on the status line" "the server said no" t.status;
  Alcotest.(check int) "not in flight" 0 t.pending

let test_reload () =
  let t = ready () in
  let _, actions = Model.update (key (Char 'R')) t in
  check "both" [ "load mailboxes"; "load messages of I" ] actions

let () =
  Alcotest.run "jmap-mosaic"
    [
      ( "model",
        List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [
            ("init", test_init);
            ("login editing", test_login_editing);
            ("login paste", test_login_paste);
            ("login scheme", test_login_scheme);
            ("login connects", test_login_connects);
            ("login fails", test_login_fails);
            ("login succeeds", test_login_succeeds);
            ("login escape quits", test_login_escape_quits);
            ("profiles", test_profiles);
            ("profile names", test_profile_names);
            ("inbox is selected", test_inbox_is_selected);
            ("order", test_order);
            ("order keeps a cycle", test_order_keeps_a_cycle);
            ("form fields agree", test_form_fields_agree);
            ("other mailbox ignored", test_other_mailbox_ignored);
            ("navigation", test_navigation);
            ("enter opens and marks seen", test_enter_opens_and_marks_seen);
            ("opening from Unread", test_open_from_unread);
            ("enter on a mailbox", test_enter_on_a_mailbox);
            ("smart searches", test_smart_searches);
            ("toggles", test_toggles);
            ("move", test_move);
            ("move into the same mailbox", test_move_into_the_same_mailbox);
            ("reply subject", test_reply_subject);
            ("quote", test_quote);
            ("reply needs an identity", test_reply_needs_an_identity);
            ("reply", test_reply);
            ("stale open", test_stale_open_is_ignored);
            ("reply-to header", test_reply_to_header);
            ("escape and quit", test_escape_and_quit);
            ("failure is reported", test_failure_is_reported);
            ("reload", test_reload);
          ] );
    ]
