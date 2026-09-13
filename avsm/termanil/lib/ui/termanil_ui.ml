(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
open Bonsai_term
open Bonsai.Let_syntax
module M = Termanil_model
module Buffer = Stdlib.Buffer
module Reply_editor = Reply_editor
module Metadata = Metadata

let wrap = Text_layout.wrap
let text ?(attrs = []) s = Text_layout.text ~attrs:(Theme.normal @ attrs) s
let fit = Text_layout.fit

let take_window xs ~offset ~height =
  List.take (List.drop xs offset) (max 0 height)

let label t =
  match t.M.tab with
  | Mail -> "Mail"
  | People -> "Contacts"
  | Todos -> "Dooit"
  | Outbox -> "Outbox"

let query t =
  match t.M.tab with
  | Mail -> t.page.query
  | People -> t.people_query
  | Todos -> t.task_query
  | Outbox -> ""

let header t =
  let tab key name selected =
    text
      ~attrs:
        (if selected then Theme.selected @ [ Attr.fg Theme.sand ]
         else [ Attr.fg Theme.muted ])
      (" " ^ key ^ " " ^ name ^ " ")
  in
  View.hcat
    [
      text ~attrs:Theme.heading
        (if t.M.demo then " termanil DEMO " else " termanil ");
      tab "1" "Mail" (Poly.equal t.M.tab M.Mail);
      tab "2" "Contacts" (Poly.equal t.tab M.People);
      tab "3" "Dooit" (Poly.equal t.tab M.Todos);
      tab "4"
        (Printf.sprintf "Outbox %d"
           (List.count t.saved_drafts ~f:(fun d ->
                not (String.equal d.M.state "sent"))))
        (Poly.equal t.tab M.Outbox);
    ]

let badges t (m : M.message) =
  let tasks = M.linked_tasks t m in
  let task =
    if List.is_empty tasks then ""
    else if List.for_all tasks ~f:(fun n -> String.equal n.M.status "done") then
      "[done] "
    else "[task] "
  in
  let drafts =
    List.filter t.M.saved_drafts ~f:(fun d ->
        M.same_email d.M.source m.source
        || M.same_email
             { d.source with id = d.thread_id }
             { m.source with id = m.thread_id })
  in
  task
  ^ String.concat ~sep:"" (List.map drafts ~f:(fun d -> "[" ^ d.M.state ^ "] "))

let rows t =
  match t.M.tab with
  | Mail when t.choosing_mailbox ->
      List.map t.mailboxes ~f:(fun (m : M.mailbox) ->
          Printf.sprintf "%s (%d unread)" m.name m.unread)
  | Mail ->
      List.map t.page.messages ~f:(fun (m : M.message) ->
          Printf.sprintf "%s%s %s | %s"
            (if m.seen then " " else "N")
            (if m.flagged then "*" else " ")
            (badges t m ^ m.subject)
            m.sender)
  | Outbox ->
      List.map t.saved_drafts ~f:(fun (d : M.draft) ->
          "[" ^ d.state ^ "] " ^ d.subject ^ " | "
          ^ String.concat ~sep:", " d.recipients)
  | People ->
      List.map (M.visible_people t) ~f:(fun (c : M.contact) ->
          c.name ^ " | " ^ String.concat ~sep:", " c.emails)
  | Todos ->
      List.map (M.visible_tasks t) ~f:(fun (n : M.task) ->
          Printf.sprintf "[%s] %s%s" n.status n.title
            (if List.is_empty n.tags then ""
             else " #" ^ String.concat ~sep:" #" n.tags))

let draft_body ?(review = false) (d : M.draft) =
  String.concat ~sep:"\n"
    ([ d.subject; "To: " ^ String.concat ~sep:", " d.recipients ]
    @ (if review then [] else [ "State: " ^ d.state; "File: " ^ d.path ])
    @ [ ""; d.body ])

let body t =
  match t.M.send_review with
  | Some ds ->
      "SEND REVIEW\nEnter sends these exact replies. Esc cancels.\n\n"
      ^ String.concat ~sep:"\n\n--------------------------------\n\n"
          (List.map ds ~f:(draft_body ~review:true))
  | None -> (
      match t.M.report with
      | Some lines -> String.concat ~sep:"\n" lines
      | None -> (
          match t.tab with
          | Mail -> (
              match M.selected_message t with
              | None -> "No message selected."
              | Some m ->
                  let body =
                    match t.detail with
                    | Some (source, body) when M.same_email source m.source ->
                        body
                    | _ -> m.preview ^ "\n\nEnter reads this message."
                  in
                  let tasks = M.linked_tasks t m in
                  let links =
                    if List.is_empty tasks then [ "Task: none | t create" ]
                    else
                      List.map tasks ~f:(fun n ->
                          "Task [" ^ n.M.status ^ "]: " ^ n.title ^ " | g open")
                  in
                  let history =
                    List.filter t.conversation ~f:(fun ((old : M.message), _) ->
                        (not (M.same_email old.M.source m.source))
                        && M.same_email
                             { old.source with id = old.thread_id }
                             { m.source with id = m.thread_id })
                  in
                  let previous =
                    List.concat_map history ~f:(fun ((old : M.message), body) ->
                        [
                          (if t.show_history then "v " else "> ")
                          ^ old.M.received ^ " " ^ old.sender;
                        ]
                        @ if t.show_history then [ body; "" ] else [])
                  in
                  String.concat ~sep:"\n"
                    (links
                    @ (if List.is_empty history then []
                       else
                         [
                           Printf.sprintf
                             "Conversation: %d messages | h %s history"
                             (List.length history + 1)
                             (if t.show_history then "collapse" else "expand");
                         ]
                         @ previous)
                    @ [ m.subject; "From: " ^ m.sender ]
                    @ List.map
                        (List.filter m.metadata ~f:(fun (label, _) ->
                             t.show_headers
                             || not
                                  (List.mem
                                     [ "Received"; "Message-ID"; "Reply-To" ]
                                     label ~equal:String.equal)))
                        ~f:(fun (label, value) -> label ^ ": " ^ value)
                    @ [ ""; body ]))
          | Outbox ->
              Option.value_map (M.selected_draft t)
                ~default:
                  "No saved replies. Open mail, e to reply, Ctrl-S to save."
                ~f:(draft_body ~review:false)
          | People -> (
              match M.selected_contact t with
              | None -> "No contact selected."
              | Some c ->
                  String.concat ~sep:"\n"
                    (List.map c.metadata ~f:(fun (k, v) -> k ^ ": " ^ v)))
          | Todos -> (
              match M.selected_task t with
              | None -> "No task selected."
              | Some n ->
                  String.concat ~sep:"\n"
                    [
                      n.title;
                      "Status: " ^ n.status;
                      "Updated: " ^ n.updated;
                      (if List.is_empty n.sources then "No email linked"
                       else "o OPEN LINKED EMAIL | d complete task");
                      "Tags: " ^ String.concat ~sep:", " n.tags;
                      String.concat ~sep:"\n"
                        (List.map n.sources ~f:(fun s ->
                             "Email: "
                             ^ Option.value_map
                                 (List.find t.page.messages
                                    ~f:(fun (m : M.message) ->
                                      M.same_email m.source s))
                                 ~default:s.id
                                 ~f:(fun m -> m.subject)));
                      "";
                      n.body;
                    ])))

let listing ~width ~height t =
  let rows = rows t in
  let offset = max 0 (t.M.selected - height + 1) in
  let view =
    if List.is_empty rows then text "  No results. r refresh | / search"
    else
      take_window rows ~offset ~height
      |> List.mapi ~f:(fun i row ->
          let selected = i + offset = t.selected in
          text
            ~attrs:
              ((if selected then Theme.selected else [])
              @ [
                  Attr.fg
                    (match t.tab with
                    | People -> Theme.sky
                    | Todos -> Theme.green
                    | Outbox ->
                        if String.is_substring row ~substring:"[uncertain]" then
                          Theme.red
                        else Theme.sand
                    | Mail ->
                        if String.is_substring row ~substring:"[task]" then
                          Theme.green
                        else if
                          String.is_substring row ~substring:"[ready]"
                          || String.is_substring row ~substring:"[draft]"
                        then Theme.sand
                        else Theme.foreground);
                ])
            ((if selected then "> " else "  ") ^ row))
      |> View.vcat
  in
  fit ~width ~height view

let detail ~width ~height t =
  let lines = wrap ~width:(max 1 (width - 2)) (body t) in
  let offset = min t.M.scroll (max 0 (List.length lines - height)) in
  let view =
    take_window lines ~offset ~height
    |> List.map ~f:(fun s ->
        text
          ~attrs:
            [
              Attr.fg
                (if
                   String.is_prefix s ~prefix:"Task"
                   || String.is_prefix s ~prefix:"Status:"
                 then Theme.green
                 else if String.is_prefix s ~prefix:"> " then Theme.muted
                 else if
                   String.is_prefix s ~prefix:"From:"
                   || String.is_prefix s ~prefix:"To:"
                 then Theme.sky
                 else if
                   String.is_prefix s ~prefix:"Conversation:"
                   || String.is_prefix s ~prefix:"SEND REVIEW"
                 then Theme.sand
                 else Theme.foreground);
            ]
          (" " ^ s))
    |> View.vcat
  in
  fit ~width ~height view

let pane_width { Dimensions.width; _ } =
  if width >= 100 then width - min 52 (width / 2) - 1 else width

let reply_height { Dimensions.height; _ } = max 1 (min 8 ((height - 5) / 3))

let editor_height dimensions t =
  if Poly.equal t.M.focus M.Reply then reply_height dimensions
  else min 2 (reply_height dimensions)

let reading t =
  Option.is_none t.M.send_review
  && Option.is_none t.report && (not t.help) && Poly.equal t.M.tab M.Mail
  && Option.is_some t.opened && (not t.choosing_mailbox)
  && not (Poly.equal t.focus M.Listing)

let reader ~reply ~width ~height t =
  if not (reading t) then detail ~width ~height t
  else
    let editor_height =
      editor_height { Dimensions.width; height = height + 5 } t
    in
    let active = Poly.equal t.focus M.Reply in
    let title =
      (if active then "> Reply" else "  Reply (e to edit)")
      ^ " | "
      ^
      match Option.bind t.opened ~f:(M.saved_reply t) with
      | None -> "unsaved"
      | Some d ->
          if
            M.draft_dirty t d.source
              (Option.value
                 (List.Assoc.find t.drafts d.source ~equal:M.same_email)
                 ~default:d.body)
          then "modified"
          else d.state
    in
    View.vcat
      [
        detail ~width ~height:(max 0 (height - editor_height - 1)) t;
        fit ~width ~height:1
          (text
             ~attrs:(Theme.heading @ if active then Theme.selected else [])
             title);
        fit ~width ~height:editor_height reply;
      ]

let help =
  [
    "Navigation";
    "1 Mail   2 Contacts   3 Dooit   4 Outbox";
    "Tab: next view. Shift-Tab: previous view, including while editing.";
    "j/k or arrows move   Enter open   Esc back   q/Ctrl-C quit";
    "/ search   Enter apply search   r refresh   ? help";
    "";
    "Mail";
    "b mailboxes   ] next page   [ previous page   H full headers";
    "a archive email   u toggle read   f toggle star   t capture as Dooit task";
    "p find sender in contacts. Reading alone does not mark seen.";
    "Opened mail: arrows select messages, PgUp/PgDn scroll, e reply.";
    "Reply: Ctrl-S saves to Markdown, Esc returns to the reader.";
    "Reader: g linked task, h conversation history, Q queue, S send now.";
    "Outbox: Enter reads source, Q queue/unqueue, S review batch send.";
    "Outbox: v checks uncertain submissions. It never retries a send.";
    "Search matches conversations including collapsed older messages.";
    "from:ada to:you cc:lin subject:\"garden plan\" body:thyme";
    "is:unread is:read is:starred has:attachment before:2026-09-01 \
     after:2026-09-01";
    "Filters combine with AND within the selected mailbox.";
    "";
    "Dooit";
    "d complete   o read linked email. Newest modified notes first.";
    "s dry-run sync   S sync (Enter confirms, Esc cancels)";
    "";
    "Contact and task details";
    "j/k or arrows select items. PageUp/PageDown scroll the open detail.";
    "Contacts show vCard metadata. Use sortal carddav migrate for YAML stores.";
  ]

let render_with_reply ~metadata ~reply
    ({ Dimensions.width; height } as dimensions) t =
  if width < 30 || height < 7 then
    fit ~width ~height
      (View.vcat
         [
           text "termanil";
           text "Resize to at least 30 x 7";
           text "q quit | ? help";
         ])
  else
    let content_height = height - 5 in
    let context =
      if t.M.choosing_mailbox then "Choose a mailbox"
      else if Option.is_some t.send_review then "Review before sending"
      else if Option.is_some t.report then "Operation report"
      else
        Printf.sprintf "%s | %d results%s%s" (label t) (M.count t)
          (if String.is_empty (query t) then "" else " | /" ^ query t)
          (if Poly.equal t.tab M.Mail then
             " | "
             ^ Option.value
                 (Option.bind t.page.mailbox ~f:(fun id ->
                      List.find_map t.mailboxes ~f:(fun b ->
                          if String.equal b.M.id id then Some b.name else None)))
                 ~default:"All mail"
           else if Poly.equal t.tab M.Todos then " | newest updated first"
           else "")
        ^ " | Tab/Shift-Tab views"
    in
    let content =
      if t.help then
        help
        |> List.concat_map ~f:(wrap ~width)
        |> (fun xs ->
        take_window xs
          ~offset:(min t.scroll (max 0 (List.length xs - content_height)))
          ~height:content_height)
        |> List.map ~f:text |> View.vcat
        |> fit ~width ~height:content_height
      else if t.choosing_mailbox then listing ~width ~height:content_height t
      else if Option.is_some t.report || Option.is_some t.send_review then
        detail ~width ~height:content_height t
      else if width >= 100 then
        let left = min 52 (width / 2) in
        View.hcat
          [
            listing ~width:left ~height:content_height t;
            View.rectangle
              ~attrs:[ Attr.fg Theme.muted; Attr.bg Theme.background ]
              ~fill:'|' ~width:1 ~height:content_height ();
            (if Poly.equal t.tab M.People then metadata
             else
               reader ~reply ~width:(pane_width dimensions)
                 ~height:content_height t);
          ]
      else
        match t.focus with
        | Listing -> listing ~width ~height:content_height t
        | Detail | Reply ->
            if Poly.equal t.tab M.People then metadata
            else reader ~reply ~width ~height:content_height t
    in
    let footer =
      match t.input with
      | Some s -> "/" ^ s ^ "_"
      | None when t.confirm_quit ->
          "q: discard unsaved edits and quit   Esc: keep working"
      | None when Option.is_some t.send_review ->
          "Enter: SEND reviewed replies   PgUp/Dn: review   Esc: cancel"
      | None when Option.is_some t.report -> "REPORT | PgUp/Dn scroll  Esc back"
      | None when t.confirm_sync -> "Enter: sync Dooit to WebDAV   Esc: cancel"
      | None when reading t && Poly.equal t.focus M.Reply ->
          "REPLY | Ctrl-S save  Enter newline  Esc reader | Tab next view"
      | None when reading t ->
          "e reply | t/g task | a archive | h history | Q queue | S send | Esc"
      | None when Poly.equal t.tab M.Outbox ->
          "OUTBOX | Enter source  Q queue/unqueue  S send queued  r reload \
           files"
      | None when Poly.equal t.tab M.Todos ->
          "Up/Down tasks | PgUp/Dn scroll | o email | d done | s sync | Esc"
      | None when Poly.equal t.focus M.Detail ->
          "Up/Down items | PgUp/Dn scroll | Esc list | ? help | q quit"
      | None -> "j/k move  Enter open  / search  r refresh  ? help  q quit"
    in
    View.vcat
      [
        fit ~width ~height:1 (header t);
        fit ~width ~height:1 (text context);
        fit ~width ~height:1 (text (String.make width '-'));
        content;
        fit ~width ~height:1 (text ~attrs:[ Attr.bold ] footer);
        fit ~width ~height:1
          (text
             (if reading t && Poly.equal t.focus M.Reply then t.status
              else (if Option.is_some t.pending then "... " else "") ^ t.status));
      ]
    |> fit ~width ~height
    |> fun view ->
    View.with_colors ~fill_backdrop:true view ~fg:Theme.foreground
      ~bg:Theme.background

let contact_fields t =
  match M.selected_contact t with
  | None -> [ ("Contacts", "No contact selected.") ]
  | Some c -> c.metadata @ List.map c.sources ~f:(fun s -> ("Source", s))

let render dimensions t =
  let metadata =
    Metadata.render ~fields:(contact_fields t) ~width:(pane_width dimensions)
      ~height:(dimensions.height - 5) ~scroll:t.scroll
  in
  render_with_reply ~metadata ~reply:(View.text "") dimensions t

let bound_scroll dimensions t action =
  let delta =
    match action with
    | M.Scroll d -> Some d
    | M.Move d when M.arrows_scroll t -> Some d
    | _ -> None
  in
  match delta with
  | None -> action
  | Some delta ->
      let width =
        if t.help || Option.is_some t.report || Option.is_some t.send_review
        then dimensions.Dimensions.width
        else pane_width dimensions
      in
      let height =
        max 0
          (dimensions.height - 5
          -
          if reading t && not t.help then editor_height dimensions t + 1 else 0
          )
      in
      let length =
        if t.help then List.length (List.concat_map help ~f:(wrap ~width))
        else if Poly.equal t.tab M.People then
          Metadata.content_height ~fields:(contact_fields t) ~width
        else List.length (wrap ~width:(max 1 (width - 2)) (body t))
      in
      let limit = max 0 (length - height) in
      let offset = min t.scroll limit in
      M.Scroll (max 0 (min limit (offset + delta)) - t.scroll)

let tab_action t (event : Event.t) =
  if t.M.pasting then None
  else
    match event with
    | Key_press { key = Tab; mods = [] } ->
        Some
          (M.Switch
             (match t.tab with
             | Mail -> People
             | People -> Todos
             | Todos -> Outbox
             | Outbox -> Mail))
    | Key_press { key = Tab; mods = [ Shift ] } ->
        Some
          (M.Switch
             (match t.tab with
             | Mail -> Outbox
             | People -> Mail
             | Todos -> People
             | Outbox -> Todos))
    | _ -> None

let key_action t (event : Event.t) =
  let open Event in
  match event with
  | _ when Option.is_some (tab_action t event) -> tab_action t event
  | Paste `Start -> Some M.Paste_start
  | Paste `End -> Some M.Paste_end
  | _ when t.M.pasting && Option.is_none t.input -> None
  | Key_press { key = Enter; _ } when t.pasting -> Some (M.Type " ")
  | Key_press { key = Escape; _ } -> Some M.Cancel
  | Key_press { key = Enter; mods = [] } -> Some M.Submit
  | Key_press { key = Backspace; mods = [] } when Option.is_some t.M.input ->
      Some M.Erase
  | Key_press { key = ASCII c; mods = [] } when Option.is_some t.input ->
      Some (M.Type (String.of_char c))
  | Key_press { key = Uchar u; mods = [] } when Option.is_some t.input ->
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b u;
      Some (M.Type (Buffer.contents b))
  | Key_press { key = ASCII '?'; mods = [] } -> Some M.Help
  | Key_press { key = ASCII 'j' | Arrow `Down; mods = [] } when t.help ->
      Some (M.Move 1)
  | Key_press { key = ASCII 'k' | Arrow `Up; mods = [] } when t.help ->
      Some (M.Move (-1))
  | Key_press { key = Arrow `Down; mods = [] } when Option.is_some t.send_review
    ->
      Some (M.Scroll 1)
  | Key_press { key = Arrow `Up; mods = [] } when Option.is_some t.send_review
    ->
      Some (M.Scroll (-1))
  | Key_press { key = Page `Down; _ } when Option.is_some t.send_review ->
      Some (M.Scroll 10)
  | Key_press { key = Page `Up; _ } when Option.is_some t.send_review ->
      Some (M.Scroll (-10))
  | _ when Option.is_some t.send_review -> None
  | _ when t.help || t.confirm_sync || Option.is_some t.input -> None
  | Key_press { key = ASCII '1'; mods = [] } -> Some (M.Switch Mail)
  | Key_press { key = ASCII '2'; mods = [] } -> Some (M.Switch People)
  | Key_press { key = ASCII '3'; mods = [] } -> Some (M.Switch Todos)
  | Key_press { key = ASCII '4'; mods = [] } -> Some (M.Switch Outbox)
  | Key_press { key = ASCII 'v'; mods = [] } -> Some M.Verify_send
  | Key_press { key = ASCII 'g'; mods = [] } -> Some M.Task_selected
  | Key_press { key = ASCII 'h'; mods = [] } -> Some M.Toggle_history
  | Key_press { key = ASCII 'H'; mods = [] } -> Some M.Toggle_headers
  | Key_press { key = ASCII 'Q'; mods = [] } -> Some M.Queue_draft
  | Key_press { key = ASCII ('s' | 'S'); mods = [ Ctrl ] } -> Some M.Save_draft
  | Key_press { key = ASCII 'e'; mods = [] } -> Some M.Focus_reply
  | Key_press { key = ASCII 'a'; mods = [] } -> Some M.Archive_selected
  | Key_press { key = ASCII 'j' | Arrow `Down; mods = [] } -> Some (M.Move 1)
  | Key_press { key = ASCII 'k' | Arrow `Up; mods = [] } -> Some (M.Move (-1))
  | Key_press { key = Page `Down; mods = [] } ->
      Some (if Poly.equal t.focus M.Listing then M.Move 10 else M.Scroll 10)
  | Key_press { key = Page `Up; mods = [] } ->
      Some
        (if Poly.equal t.focus M.Listing then M.Move (-10) else M.Scroll (-10))
  | Key_press { key = ASCII '/'; mods = [] } -> Some M.Search
  | Key_press { key = ASCII 'r'; mods = [] } -> Some M.Refresh
  | Key_press { key = ASCII 'b'; mods = [] } -> Some M.Choose_mailbox
  | Key_press { key = ASCII ']'; mods = [] } -> Some M.Next_page
  | Key_press { key = ASCII '['; mods = [] } -> Some M.Previous_page
  | Key_press { key = ASCII 't'; mods = [] } -> Some M.Capture_selected
  | Key_press { key = ASCII 'p'; mods = [] } -> Some M.Sender_contacts
  | Key_press { key = ASCII 'u'; mods = [] } -> Some M.Toggle_seen
  | Key_press { key = ASCII 'f'; mods = [] } -> Some M.Toggle_flagged
  | Key_press { key = ASCII 'd'; mods = [] } -> Some M.Complete_selected
  | Key_press { key = ASCII 'o'; mods = [] } -> Some M.Source_selected
  | Key_press { key = ASCII 's'; mods = [] } -> Some M.Preview_sync
  | Key_press { key = ASCII 'S'; mods = [] } ->
      Some (if Poly.equal t.tab M.Todos then M.Confirm_sync else M.Review_send)
  | _ -> None

let app ?(initial = M.initial) ?(autoload = true) ~execute ~exit ~dimensions
    (local_ graph) =
  let apply_action context model action =
    let model, request = M.update model action in
    Option.iter request ~f:(fun (id, request) ->
        let command_effect =
          let open Effect.Let_syntax in
          let%bind result = execute request in
          Bonsai.Apply_action_context.inject context (M.Finished (id, result))
        in
        Bonsai.Apply_action_context.schedule_event context command_effect);
    model
  in
  let model, inject =
    Bonsai.state_machine ~default_model:initial ~apply_action graph
  in
  if autoload then
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%arr inject in
         inject M.Start)
      graph;
  let draft_key model source =
    Sexplib.Sexp.to_string (M.sexp_of_email_ref source)
    ^ ":"
    ^ Int.to_string (M.editor_version model source)
  in
  let drafts =
    let%arr model in
    Map.of_alist_exn
      (module String)
      (List.map model.drafts ~f:(fun (s, text) ->
           (draft_key model s, (s, text))))
  in
  let width =
    let%arr dimensions in
    max 1 (pane_width dimensions)
  in
  let height =
    let%arr dimensions and model in
    editor_height dimensions model
  in
  let editors =
    Bonsai.assoc
      (module String)
      drafts
      ~f:(fun _ draft graph ->
        let source =
          let%arr source, _ = draft in
          source
        in
        let initial_text =
          let%arr _, text = draft in
          text
        in
        let on_change =
          let%arr source and inject in
          fun text -> inject (M.Draft_changed (source, text))
        in
        Reply_editor.component ~initial_text ~width ~height ~on_change graph)
      graph
  in
  let editor =
    let%arr editors and model in
    Option.bind model.opened ~f:(fun s -> Map.find editors (draft_key model s))
  in
  let metadata =
    let fields =
      let%arr model in
      contact_fields model
    in
    let height =
      let%arr dimensions in
      max 0 (dimensions.height - 5)
    in
    let scroll =
      let%arr model in
      model.scroll
    in
    Metadata.component ~fields ~width ~height ~scroll graph
  in
  let view =
    let%arr model and dimensions and editor and metadata in
    let reply =
      Option.value_map editor ~default:(View.text "") ~f:(fun (view, _, _) ->
          view)
    in
    render_with_reply ~metadata ~reply dimensions model
  in
  let set_cursor = Effect.set_cursor graph in
  let cursor =
    let%arr view and model and editor in
    let position =
      if reading model && Poly.equal model.focus M.Reply then
        Option.bind editor ~f:(fun (_, _, position) -> position view)
      else None
    in
    Option.map position ~f:(fun position ->
        { Cursor.position; kind = Cursor.Kind.Bar_blinking })
  in
  Bonsai.Edge.on_change' ~trigger:`After_display
    ~equal:(Option.equal Cursor.equal)
    cursor
    ~callback:
      (let%arr set_cursor in
       fun previous cursor ->
         if Option.is_none previous && Option.is_none cursor then Effect.Ignore
         else set_cursor cursor)
    graph;
  let handler =
    let%arr model and inject and editor and dimensions in
    fun event ->
      let quit () =
        if Option.is_some model.pending then inject M.Quit_busy
        else if
          (not model.confirm_quit)
          && List.exists model.drafts ~f:(fun (source, s) ->
              M.draft_dirty model source s)
        then inject M.Quit_drafts
        else exit ()
      in
      match event with
      | Event.Key_press { key = ASCII ('c' | 'C'); mods = [ Ctrl ] }
        when not model.pasting ->
          quit ()
      | _ when Option.is_some (tab_action model event) ->
          Option.value_map (tab_action model event) ~default:Effect.Ignore
            ~f:inject
      | _ when reading model && Poly.equal model.focus M.Reply -> (
          match event with
          | Event.Key_press { key = ASCII ('s' | 'S'); mods = [ Ctrl ] }
            when not model.pasting ->
              inject M.Save_draft
          | Event.Key_press { key = Escape; mods = [] } when not model.pasting
            ->
              inject M.Focus_reply
          | _ -> (
              let open Effect.Let_syntax in
              let%bind () =
                match event with
                | Event.Paste `Start -> inject M.Paste_start
                | Event.Paste `End -> inject M.Paste_end
                | _ -> Effect.Ignore
              in
              match editor with
              | None -> Effect.Ignore
              | Some (_, handler, _) ->
                  let%bind (_ : Captured_or_ignored.t) = handler event in
                  Effect.return ()))
      | _ when model.pasting ->
          Option.value_map (key_action model event) ~default:Effect.Ignore
            ~f:inject
      | Event.Key_press { key = ASCII 'q'; mods = [] }
        when Option.is_none model.input ->
          quit ()
      | Event.Key_press { key = Escape; _ } when model.confirm_quit ->
          inject M.Cancel
      | _ when model.confirm_quit -> Effect.Ignore
      | _ ->
          Option.value_map (key_action model event) ~default:Effect.Ignore
            ~f:(fun action -> inject (bound_scroll dimensions model action))
  in
  let can_batch =
    let%arr model in
    fun event ->
      match event with
      | Event.Paste _ -> false
      | _ when model.pasting -> true
      | Event.Key_press { key = Escape | Tab; _ } -> false
      | Event.Key_press { key = ASCII ('c' | 'C' | 's' | 'S'); mods = [ Ctrl ] }
        ->
          false
      | _ when reading model && Poly.equal model.focus M.Reply -> true
      | Event.Key_press { key = ASCII _ | Uchar _ | Backspace; mods = [] }
        when Option.is_some model.input ->
          true
      | _ -> false
  in
  let ready =
    let%arr model in
    fun event ->
      match (model.pending, event) with
      | ( Some (_, M.Conversation _),
          Event.Key_press { key = ASCII 'e'; mods = [] } )
        when reading model && not (Poly.equal model.focus M.Reply) ->
          false
      | _ -> true
  in
  let handler = Input_queue.component ~handler ~can_batch ~ready graph in
  (~view, ~handler)
