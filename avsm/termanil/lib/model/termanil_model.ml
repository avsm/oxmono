(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Sexplib.Std

type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b
[@@deriving sexp]

type email_ref = { service : string; account : string; id : string }
[@@deriving sexp]

type message = {
  source : email_ref;
  thread_id : string;
  subject : string;
  sender : string;
  addresses : string list;
  received : string;
  preview : string;
  seen : bool;
  flagged : bool;
  metadata : (string * string) list;
}
[@@deriving sexp]

type mailbox = { id : string; name : string; unread : int; inbox : bool }
[@@deriving sexp]

type page = {
  mailbox : string option;
  query : string;
  position : int;
  next : int option;
  messages : message list;
}
[@@deriving sexp]

type contact = {
  key : string;
  name : string;
  emails : string list;
  sources : string list;
  metadata : (string * string) list;
}
[@@deriving sexp]

type task = {
  id : string;
  revision : string;
  title : string;
  updated : string;
  status : string;
  tags : string list;
  body : string;
  sources : email_ref list;
  threads : email_ref list;
}
[@@deriving sexp]

type draft = {
  source : email_ref;
  thread_id : string;
  subject : string;
  recipients : string list;
  body : string;
  revision : string;
  state : string;
  path : string;
}
[@@deriving sexp]

type request =
  | Verify_replies
  | Workspace
  | Conversation of email_ref
  | Save_reply of {
      source : email_ref;
      body : string;
      expected : string option;
    }
  | Queue_reply of draft * bool
  | Send_replies of draft list
  | Mailboxes
  | Messages of { mailbox : string option; query : string; position : int }
  | Read of email_ref
  | Archive of email_ref
  | Set_seen of email_ref * bool
  | Set_flagged of email_ref * bool
  | Contacts
  | Tasks
  | Capture of message
  | Complete of task
  | Sync_tasks of { dry_run : bool }
[@@deriving sexp]

type response =
  | Workspace_loaded of task list * draft list * string list
  | Conversation_loaded of
      email_ref * (message * string) list * (string, string) result
  | Reply_saved of draft
  | Replies_sent of draft list * string list
  | Mailboxes_loaded of mailbox list
  | Messages_loaded of page
  | Message_loaded of message * string
  | Archived of email_ref
  | Message_changed of email_ref * [ `Seen | `Flagged ] * bool
  | Contacts_loaded of contact list * string list
  | Tasks_loaded of task list * string list
  | Captured of task list
  | Completed of task
  | Synced of string list
[@@deriving sexp]

type tab = Mail | People | Todos | Outbox
type focus = Listing | Detail | Reply

type action =
  | Verify_send
  | Task_selected
  | Toggle_headers
  | Toggle_history
  | Save_draft
  | Queue_draft
  | Review_send
  | Confirm_send
  | Switch of tab
  | Move of int
  | Scroll of int
  | Focus_reply
  | Draft_changed of email_ref * string
  | Open
  | Back
  | Refresh
  | Next_page
  | Previous_page
  | Choose_mailbox
  | Capture_selected
  | Source_selected
  | Sender_contacts
  | Archive_selected
  | Toggle_seen
  | Toggle_flagged
  | Complete_selected
  | Preview_sync
  | Confirm_sync
  | Search
  | Type of string
  | Erase
  | Submit
  | Cancel
  | Help
  | Quit_busy
  | Quit_drafts
  | Paste_start
  | Paste_end
  | Start
  | Finished of int * (response, string) result

type t = {
  demo : bool;
  tab : tab;
  focus : focus;
  selected : int;
  scroll : int;
  mailboxes : mailbox list;
  choosing_mailbox : bool;
  page : page;
  previous : int list;
  people : contact list;
  tasks : task list;
  people_query : string;
  task_query : string;
  detail : (email_ref * string) option;
  opened : email_ref option;
  opened_message : message option;
  drafts : (email_ref * string) list;
  draft_templates : (email_ref * string) list;
  signature_error : string option;
  views : (tab * (focus * int * int * email_ref option)) list;
  saved_drafts : draft list;
  draft_bases : (email_ref * string option) list;
  editor_versions : (email_ref * int) list;
  conversation : (message * string) list;
  show_history : bool;
  show_headers : bool;
  send_review : draft list option;
  input : string option;
  status : string;
  pending : (int * request) option;
  sequence : int;
  confirm_sync : bool;
  confirm_quit : bool;
  help : bool;
  report : string list option;
  reload_after : bool;
  pasting : bool;
}

let same_email (a : email_ref) b = a = b

let safe_text s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let n = Uchar.to_int u in
      if not (Uchar.utf_decode_is_valid d) then Buffer.add_string b "�"
      else if n = 10 || n = 9 then Buffer.add_utf_8_uchar b u
      else if
        n < 32
        || (n >= 127 && n <= 159)
        || (n >= 0x202a && n <= 0x202e)
        || (n >= 0x2066 && n <= 0x2069)
        || n = 0x200e || n = 0x200f || n = 0x61c
      then Buffer.add_char b '?'
      else Buffer.add_utf_8_uchar b u;
      loop (i + Uchar.utf_decode_length d))
  in
  loop 0;
  Buffer.contents b

let normalize s =
  let s = Uunf_string.normalize_utf_8 `NFC (safe_text s) in
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      (match Uucp.Case.Fold.fold u with
      | `Self -> Buffer.add_utf_8_uchar b u
      | `Uchars us -> List.iter (Buffer.add_utf_8_uchar b) us);
      loop (i + Uchar.utf_decode_length d))
  in
  loop 0;
  Buffer.contents b

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let matches ~query text =
  let text = normalize text in
  String.split_on_char ' ' (normalize query) |> List.for_all (contains text)

let initial =
  {
    demo = false;
    tab = Mail;
    focus = Listing;
    selected = 0;
    scroll = 0;
    mailboxes = [];
    choosing_mailbox = false;
    page =
      { mailbox = None; query = ""; position = 0; next = None; messages = [] };
    previous = [];
    people = [];
    tasks = [];
    people_query = "";
    task_query = "";
    detail = None;
    opened = None;
    opened_message = None;
    drafts = [];
    draft_templates = [];
    signature_error = None;
    views = [];
    saved_drafts = [];
    draft_bases = [];
    editor_versions = [];
    conversation = [];
    show_history = false;
    show_headers = false;
    send_review = None;
    input = None;
    status = "r refresh | ? help";
    pending = None;
    sequence = 0;
    confirm_sync = false;
    confirm_quit = false;
    help = false;
    report = None;
    reload_after = false;
    pasting = false;
  }

let visible_people t =
  List.filter
    (fun (c : contact) ->
      matches ~query:t.people_query
        (String.concat " "
           ((c.name :: c.emails)
           @ List.concat_map (fun (label, value) -> [ label; value ]) c.metadata
           )))
    t.people

let visible_tasks t =
  List.filter
    (fun (n : task) ->
      matches ~query:t.task_query
        (String.concat " " (n.title :: n.status :: n.body :: n.tags)))
    t.tasks
  |> List.stable_sort (fun (a : task) b -> String.compare b.updated a.updated)

let nth xs n = List.nth_opt xs (max 0 n)

let selected_message t =
  match (t.focus, t.opened) with
  | (Detail | Reply), Some id
    when Option.fold ~none:false
           ~some:(fun (m : message) -> same_email m.source id)
           t.opened_message ->
      t.opened_message
  | (Detail | Reply), Some id ->
      List.find_opt
        (fun (m : message) -> same_email m.source id)
        t.page.messages
  | _ -> nth t.page.messages t.selected

let linked_tasks t (m : message) =
  List.filter
    (fun (n : task) ->
      List.exists (same_email m.source) n.sources
      || List.exists (same_email { m.source with id = m.thread_id }) n.threads)
    t.tasks

let saved_reply t source =
  List.find_opt (fun (d : draft) -> same_email d.source source) t.saved_drafts

let selected_draft t = nth t.saved_drafts t.selected

let draft_dirty t source text =
  match saved_reply t source with
  | None ->
      text <> Option.value (List.assoc_opt source t.draft_templates) ~default:""
  | Some d -> text <> d.body

let editor_version t source =
  Option.value (List.assoc_opt source t.editor_versions) ~default:0

let merge_drafts t saved_drafts =
  let drafts, draft_bases, editor_versions =
    List.fold_left
      (fun (buffers, bases, versions) (d : draft) ->
        match List.assoc_opt d.source buffers with
        | Some text when draft_dirty t d.source text ->
            (buffers, bases, versions)
        | old ->
            let replace value xs =
              (d.source, value) :: List.remove_assoc d.source xs
            in
            ( replace d.body buffers,
              replace (Some d.revision) bases,
              if old = Some d.body then versions
              else replace (editor_version t d.source + 1) versions ))
      (t.drafts, t.draft_bases, t.editor_versions)
      saved_drafts
  in
  { t with saved_drafts; drafts; draft_bases; editor_versions }

let selected_contact t = nth (visible_people t) t.selected
let selected_task t = nth (visible_tasks t) t.selected

let count t =
  match t.tab with
  | Mail when t.choosing_mailbox -> List.length t.mailboxes
  | Mail -> List.length t.page.messages
  | People -> List.length (visible_people t)
  | Todos -> List.length (visible_tasks t)
  | Outbox -> List.length t.saved_drafts

let arrows_scroll t = t.help || t.report <> None || t.send_review <> None
let clamp t = { t with selected = min t.selected (max 0 (count t - 1)) }

let request_name = function
  | Verify_replies -> "Checking uncertain submissions"
  | Workspace -> "Loading linked tasks and saved replies"
  | Conversation _ -> "Loading conversation"
  | Save_reply _ -> "Saving reply"
  | Queue_reply _ -> "Updating reply queue"
  | Send_replies _ -> "Sending reviewed replies"
  | Mailboxes -> "Loading mailboxes"
  | Messages _ -> "Loading messages"
  | Read _ -> "Reading message"
  | Archive _ -> "Archiving email"
  | Set_seen _ -> "Updating read flag"
  | Set_flagged _ -> "Updating star"
  | Contacts -> "Loading contacts"
  | Tasks -> "Loading tasks"
  | Capture _ -> "Capturing task"
  | Complete _ -> "Completing task"
  | Sync_tasks { dry_run = true } -> "Previewing task sync"
  | Sync_tasks _ -> "Syncing tasks"

let start t req =
  match t.pending with
  | Some _ -> ({ t with status = "Request in progress; please wait" }, None)
  | None ->
      let pending = Some (t.sequence + 1, req) in
      ( {
          t with
          pending;
          sequence = t.sequence + 1;
          status = request_name req;
          confirm_sync = false;
        },
        pending )

let keep t = (t, None)
let notice t status = keep { t with status }

let messages t position =
  start t
    (Messages { mailbox = t.page.mailbox; query = t.page.query; position })

let refresh t =
  match t.tab with
  | Mail when t.mailboxes = [] || t.choosing_mailbox -> start t Mailboxes
  | Mail -> messages t t.page.position
  | People -> start t Contacts
  | Todos -> start t Tasks
  | Outbox -> start t Workspace

let upsert_task tasks (n : task) =
  if List.exists (fun (x : task) -> x.id = n.id) tasks then
    List.map (fun (x : task) -> if x.id = n.id then n else x) tasks
  else n :: tasks

let upsert_draft drafts (d : draft) =
  if List.exists (fun (x : draft) -> same_email x.source d.source) drafts then
    List.map
      (fun (x : draft) -> if same_email x.source d.source then d else x)
      drafts
  else d :: drafts

let preserve_selection key before after selected =
  match nth before selected with
  | None -> selected
  | Some old ->
      let rec find i = function
        | [] -> selected
        | x :: _ when key x = key old -> i
        | _ :: xs -> find (i + 1) xs
      in
      find 0 after

let update_message t (m : message) =
  {
    t with
    page =
      {
        t.page with
        messages =
          List.map
            (fun (x : message) -> if same_email m.source x.source then m else x)
            t.page.messages;
      };
  }

let loaded_status noun n warnings =
  Printf.sprintf "%d %s%s" n noun
    (if warnings = [] then "" else " | " ^ String.concat " | " warnings)

let accepts req response =
  match (req, response) with
  | Workspace, Workspace_loaded _ -> true
  | Conversation id, Conversation_loaded (id', ms, _) ->
      same_email id id'
      && List.exists (fun ((m : message), _) -> same_email id m.source) ms
  | Save_reply r, Reply_saved d ->
      same_email r.source d.source && r.body = d.body
  | Queue_reply (old, _), Reply_saved d ->
      same_email old.source d.source && old.revision = d.revision
  | (Send_replies _ | Verify_replies), Replies_sent _ -> true
  | Mailboxes, Mailboxes_loaded _
  | Contacts, Contacts_loaded _
  | Tasks, Tasks_loaded _
  | Capture _, Captured _
  | Sync_tasks _, Synced _ ->
      true
  | Messages q, Messages_loaded p ->
      q.mailbox = p.mailbox && q.query = p.query && q.position = p.position
  | Archive id, Archived id' -> same_email id id'
  | Read id, Message_loaded (m, _) -> same_email id m.source
  | Set_seen (id, v), Message_changed (id', `Seen, v')
  | Set_flagged (id, v), Message_changed (id', `Flagged, v') ->
      same_email id id' && v = v'
  | Complete n, Completed n' -> n.id = n'.id
  | _ -> false

let finish t req response =
  if not (accepts req response) then
    notice t "Backend response identity mismatch"
  else
    match response with
    | Workspace_loaded (tasks, drafts, warnings) ->
        let selected =
          if t.tab = Outbox then
            preserve_selection
              (fun (d : draft) -> d.source)
              t.saved_drafts drafts t.selected
          else if t.tab = Todos then
            preserve_selection
              (fun (n : task) -> n.id)
              (visible_tasks t)
              (visible_tasks { t with tasks })
              t.selected
          else t.selected
        in
        keep
          (clamp
             {
               (merge_drafts t drafts) with
               tasks;
               selected;
               status =
                 loaded_status "tasks" (List.length tasks) warnings
                 ^ Printf.sprintf " | %d saved replies" (List.length drafts);
             })
    | Conversation_loaded (source, conversation, signature) ->
        (* Seed each new buffer once. Reading a message does not make its
           untouched signature template an unsaved user edit. *)
        let t =
          match signature with
          | Error why -> { t with signature_error = Some why }
          | Ok signature ->
              let t = { t with signature_error = None } in
              if
                saved_reply t source <> None
                || List.mem_assoc source t.draft_templates
                || Option.value (List.assoc_opt source t.drafts) ~default:""
                   <> ""
              then t
              else
                let template =
                  if signature = "" then "" else "\n\n" ^ signature ^ "\n"
                in
                {
                  t with
                  drafts =
                    (source, template) :: List.remove_assoc source t.drafts;
                  draft_templates = (source, template) :: t.draft_templates;
                  editor_versions =
                    (source, editor_version t source + 1)
                    :: List.remove_assoc source t.editor_versions;
                }
        in

        let m, body =
          List.find
            (fun ((m : message), _) -> same_email source m.source)
            conversation
        in
        keep
          {
            (update_message t m) with
            conversation;
            detail = Some (source, body);
            opened_message = Some m;
            status =
              "e reply | t task | g linked task | h history | a archive | u \
               read | f star"
              ^ Option.fold ~none:""
                  ~some:(fun why -> " | Signature: " ^ why)
                  t.signature_error;
          }
    | Reply_saved d ->
        let saved_drafts = upsert_draft t.saved_drafts d in
        keep
          {
            t with
            saved_drafts;
            draft_bases =
              (d.source, Some d.revision)
              :: List.remove_assoc d.source t.draft_bases;
            status = "Reply " ^ d.state ^ " | " ^ d.path;
          }
    | Replies_sent (drafts, lines) ->
        let saved = List.fold_left upsert_draft t.saved_drafts drafts in
        keep
          {
            (merge_drafts t saved) with
            send_review = None;
            report = Some lines;
            focus = Detail;
            status = "Send results | Esc back";
          }
    | Mailboxes_loaded mailboxes ->
        let mailbox =
          if
            List.exists
              (fun (b : mailbox) -> Some b.id = t.page.mailbox)
              mailboxes
          then t.page.mailbox
          else
            Option.map
              (fun (b : mailbox) -> b.id)
              (List.find_opt (fun (b : mailbox) -> b.inbox) mailboxes)
        in
        let t = { t with mailboxes; page = { t.page with mailbox } } in
        if t.choosing_mailbox || t.tab <> Mail then keep (clamp t)
        else messages t 0
    | Messages_loaded page ->
        let previous =
          if page.mailbox <> t.page.mailbox || page.query <> t.page.query then
            []
          else if page.position > t.page.position then
            t.page.position :: t.previous
          else if page.position < t.page.position then
            List.filter (( > ) page.position) t.previous
          else t.previous
        in
        let selected =
          if t.tab <> Mail then t.selected
          else if
            page.mailbox = t.page.mailbox
            && page.query = t.page.query
            && page.position = t.page.position
          then
            preserve_selection
              (fun (m : message) -> m.source)
              t.page.messages page.messages t.selected
          else 0
        in
        start
          (clamp
             {
               t with
               page;
               previous;
               selected;
               scroll = (if t.focus = Listing then 0 else t.scroll);
               status =
                 loaded_status "conversations" (List.length page.messages) [];
             })
          Workspace
    | Message_loaded (m, body) ->
        keep
          {
            (update_message t m) with
            detail = Some (m.source, body);
            opened_message = Some m;
            status = "t task | u unread/read | f star | Esc back";
          }
    | Archived _ ->
        if t.tab = Mail then
          messages { t with focus = Listing; opened = None } t.page.position
        else notice t "Email archived"
    | Message_changed (id, kind, value) ->
        let messages =
          List.map
            (fun (m : message) ->
              if not (same_email m.source id) then m
              else
                match kind with
                | `Seen -> { m with seen = value }
                | `Flagged -> { m with flagged = value })
            t.page.messages
        in
        let opened_message =
          Option.map
            (fun (m : message) ->
              if not (same_email m.source id) then m
              else
                match kind with
                | `Seen -> { m with seen = value }
                | `Flagged -> { m with flagged = value })
            t.opened_message
        in
        keep
          {
            t with
            page = { t.page with messages };
            opened_message;
            status = "Message updated";
          }
    | Contacts_loaded (people, warnings) ->
        let selected =
          if t.tab <> People then t.selected
          else
            preserve_selection
              (fun c -> c.key)
              (visible_people t)
              (visible_people { t with people })
              t.selected
        in
        keep
          (clamp
             {
               t with
               people;
               selected;
               status = loaded_status "contacts" (List.length people) warnings;
             })
    | Tasks_loaded (tasks, warnings) ->
        let selected =
          if t.tab <> Todos then t.selected
          else
            preserve_selection
              (fun (n : task) -> n.id)
              (visible_tasks t)
              (visible_tasks { t with tasks })
              t.selected
        in
        keep
          (clamp
             {
               t with
               tasks;
               selected;
               status = loaded_status "tasks" (List.length tasks) warnings;
             })
    | Captured ns ->
        let tasks = List.fold_left upsert_task t.tasks ns in
        let selected =
          if t.tab = Todos then
            preserve_selection
              (fun (n : task) -> n.id)
              (visible_tasks t)
              (visible_tasks { t with tasks })
              t.selected
          else t.selected
        in
        keep
          {
            t with
            tasks;
            selected;
            status =
              "Task: "
              ^ String.concat ", "
                  (List.map
                     (fun (n : task) -> n.title ^ " [" ^ n.status ^ "]")
                     ns);
          }
    | Completed n ->
        let next =
          { t with tasks = upsert_task t.tasks n; status = "Task completed" }
        in
        let selected =
          if t.tab = Todos then
            preserve_selection
              (fun (n : task) -> n.id)
              (visible_tasks t) (visible_tasks next) t.selected
          else t.selected
        in
        keep (clamp { next with selected })
    | Synced lines ->
        if t.tab = Todos then
          keep
            {
              t with
              report = Some lines;
              focus = Detail;
              scroll = 0;
              status = "Sync result | Esc back | r refresh tasks";
            }
        else notice t "Task sync finished. Return to Dooit and refresh."

let remember_view t =
  {
    t with
    views =
      (t.tab, (t.focus, t.selected, t.scroll, t.opened))
      :: List.remove_assoc t.tab t.views;
  }

let retarget_views before after =
  let keys t tab =
    match tab with
    | Mail ->
        List.map
          (fun (m : message) ->
            Sexplib.Sexp.to_string (sexp_of_email_ref m.source))
          t.page.messages
    | People -> List.map (fun c -> c.key) (visible_people t)
    | Todos -> List.map (fun (n : task) -> n.id) (visible_tasks t)
    | Outbox ->
        List.map
          (fun (d : draft) ->
            Sexplib.Sexp.to_string (sexp_of_email_ref d.source))
          t.saved_drafts
  in
  {
    after with
    views =
      List.map
        (fun (tab, (focus, selected, scroll, opened)) ->
          let selected =
            preserve_selection Fun.id (keys before tab) (keys after tab)
              selected
          in
          (tab, (focus, selected, scroll, opened)))
        after.views;
  }

let rec update t = function
  | Finished (id, result) -> (
      match t.pending with
      | Some (expected, req) when id = expected ->
          let reload = t.reload_after in
          let t = { t with pending = None; reload_after = false } in
          let before = t in
          let t, next =
            match result with
            | Error s -> notice t ("Error: " ^ safe_text s)
            | Ok r -> finish t req r
          in
          let t = retarget_views before t in
          if reload then
            if next = None then refresh t
            else ({ t with reload_after = true }, next)
          else (t, next)
      | _ -> keep t)
  | Start -> refresh t
  | Verify_send when t.tab = Outbox -> start t Verify_replies
  | Toggle_headers ->
      keep { t with show_headers = not t.show_headers; scroll = 0 }
  | Toggle_history ->
      keep { t with show_history = not t.show_history; scroll = 0 }
  | Task_selected when t.tab = Mail -> (
      match
        Option.bind (selected_message t) (fun (m : message) ->
            nth (linked_tasks t m) 0)
      with
      | None -> notice t "No linked task. t captures this email."
      | Some n ->
          let rec index i = function
            | [] -> 0
            | (x : task) :: xs -> if x.id = n.id then i else index (i + 1) xs
          in
          keep
            {
              (remember_view t) with
              tab = Todos;
              task_query = "";
              selected = index 0 (visible_tasks { t with task_query = "" });
              focus = Detail;
              scroll = 0;
              opened = None;
              status = "o jumps back to email | d completes task";
            })
  | Save_draft when t.tab = Mail -> (
      match t.opened with
      | None -> notice t "Open an email to write a reply"
      | Some source ->
          let body =
            Option.value (List.assoc_opt source t.drafts) ~default:""
          in
          start t
            (Save_reply
               {
                 source;
                 body;
                 expected =
                   Option.value
                     (List.assoc_opt source t.draft_bases)
                     ~default:None;
               }))
  | Queue_draft -> (
      let draft =
        if t.tab = Outbox then selected_draft t
        else Option.bind t.opened (saved_reply t)
      in
      match draft with
      | None -> notice t "Save the reply first with Ctrl-S"
      | Some d
        when draft_dirty t d.source
               (Option.value (List.assoc_opt d.source t.drafts) ~default:d.body)
        ->
          notice t "Save your changes before queueing"
      | Some d -> start t (Queue_reply (d, d.state <> "ready")))
  | Review_send ->
      let ds =
        if t.tab = Outbox then
          List.filter (fun (d : draft) -> d.state = "ready") t.saved_drafts
        else Option.to_list (Option.bind t.opened (saved_reply t))
      in
      let ds =
        List.filter
          (fun (d : draft) -> d.state = "draft" || d.state = "ready")
          ds
      in
      if ds = [] then
        notice t "No replies to send. Save and queue a reply first."
      else if
        List.exists
          (fun (d : draft) ->
            draft_dirty t d.source
              (Option.value (List.assoc_opt d.source t.drafts) ~default:d.body))
          ds
      then notice t "Save all changes before reviewing a send"
      else
        keep
          {
            t with
            send_review = Some ds;
            focus = Detail;
            scroll = 0;
            status =
              "Review recipients and reply bodies. Enter sends these exact \
               revisions.";
          }
  | Confirm_send -> (
      match t.send_review with
      | None -> keep t
      | Some ds -> start { t with send_review = None } (Send_replies ds))
  | Help -> keep { t with help = not t.help; scroll = 0 }
  | Quit_busy ->
      notice t "Waiting for the current request; q quits after it finishes"
  | Quit_drafts ->
      keep
        {
          t with
          confirm_quit = true;
          focus = (if t.focus = Reply then Detail else t.focus);
        }
  | Paste_start -> keep { t with pasting = true }
  | Paste_end -> keep { t with pasting = false }
  | Draft_changed (source, text) ->
      keep
        {
          t with
          drafts =
            (source, text)
            :: List.filter (fun (s, _) -> not (same_email source s)) t.drafts;
        }
  | Focus_reply
    when t.tab = Mail && t.opened <> None && t.signature_error <> None
         && not
              (Option.fold ~none:false
                 ~some:(fun source ->
                   saved_reply t source <> None
                   || List.mem_assoc source t.draft_templates)
                 t.opened) ->
      notice t ("Cannot prepare signature: " ^ Option.get t.signature_error)
  | Focus_reply when t.tab = Mail && t.opened <> None ->
      keep { t with focus = (if t.focus = Reply then Detail else Reply) }
  | (Cancel | Back) when t.focus = Reply -> keep { t with focus = Detail }
  | Cancel | Back ->
      keep
        {
          t with
          input = None;
          confirm_sync = false;
          confirm_quit = false;
          send_review = None;
          help = false;
          report = None;
          focus = Listing;
          opened = None;
          scroll = 0;
          choosing_mailbox = false;
        }
  | Type s -> (
      match t.input with
      | None -> keep t
      | Some current ->
          let s = String.concat " " (String.split_on_char '\n' (safe_text s)) in
          if String.length current + String.length s > 1024 then keep t
          else keep { t with input = Some (current ^ s) })
  | Erase -> (
      match t.input with
      | Some s when s <> "" ->
          let rec before i =
            if i = 0 || Char.code s.[i] land 0xc0 <> 0x80 then i
            else before (i - 1)
          in
          keep
            {
              t with
              input = Some (String.sub s 0 (before (String.length s - 1)));
            }
      | _ -> keep t)
  | Search -> keep { t with input = Some ""; focus = Listing; report = None }
  | Submit -> (
      match t.input with
      | None ->
          if t.send_review <> None then update t Confirm_send
          else if t.confirm_sync then update t Confirm_sync
          else update t Open
      | Some query -> (
          if t.pending <> None then
            notice t "Wait for the current request before searching"
          else
            let t = { t with input = None; selected = 0; scroll = 0 } in
            match t.tab with
            | Mail ->
                start { t with previous = [] }
                  (Messages { mailbox = t.page.mailbox; query; position = 0 })
            | People -> keep { t with people_query = query }
            | Todos -> keep { t with task_query = query }
            | Outbox -> notice t "Outbox shows all saved replies"))
  | Switch tab when tab = t.tab -> keep t
  | Switch tab ->
      let t = remember_view t in
      let focus, selected, scroll, opened =
        Option.value (List.assoc_opt tab t.views) ~default:(Listing, 0, 0, None)
      in
      let t =
        {
          t with
          tab;
          send_review = None;
          selected;
          focus;
          scroll;
          input = None;
          choosing_mailbox = false;
          confirm_sync = false;
          confirm_quit = false;
          report = None;
          help = false;
          opened;
        }
      in
      if t.pending <> None then keep { t with reload_after = true }
      else refresh t
  | Move delta ->
      if arrows_scroll t then update t (Scroll delta)
      else if t.focus = Reply then keep t
      else if t.focus = Detail && t.tab = Mail then
        if t.pending <> None then notice t "Wait for the current request"
        else
          let rec index matches i = function
            | [] -> None
            | (m : message) :: ms ->
                if matches m then Some i else index matches (i + 1) ms
          in
          let anchor =
            match
              index
                (fun (m : message) -> Some m.source = t.opened)
                0 t.page.messages
            with
            | Some _ as exact -> exact
            | None ->
                Option.bind (selected_message t) (fun opened ->
                    index
                      (fun (m : message) ->
                        same_email
                          { m.source with id = m.thread_id }
                          { opened.source with id = opened.thread_id })
                      0 t.page.messages)
          in
          match anchor with
          | None -> notice t "Linked message is outside this list. Esc returns."
          | Some i ->
              let selected = max 0 (min (count t - 1) (i + delta)) in
              if selected = i then
                notice t "End of this page. Esc returns to the list."
              else update { t with selected; focus = Listing } Open
      else
        let selected = max 0 (min (count t - 1) (t.selected + delta)) in
        keep
          {
            t with
            selected;
            scroll = (if selected = t.selected then t.scroll else 0);
          }
  | Scroll delta ->
      keep { t with scroll = max 0 (min 1_000_000 (t.scroll + delta)) }
  | Refresh -> refresh { t with report = None; focus = Listing }
  | Choose_mailbox when t.tab = Mail ->
      let t =
        { t with choosing_mailbox = true; focus = Listing; selected = 0 }
      in
      if t.mailboxes = [] then start t Mailboxes else keep t
  | Open when t.choosing_mailbox -> (
      match nth t.mailboxes t.selected with
      | None -> keep t
      | Some b ->
          if t.pending <> None then notice t "Wait for the current request"
          else
            start
              { t with choosing_mailbox = false; selected = 0; previous = [] }
              (Messages { mailbox = Some b.id; query = ""; position = 0 }))
  | Open -> (
      match t.tab with
      | Mail -> (
          match selected_message t with
          | None -> keep t
          | Some m ->
              if t.pending <> None then notice t "Wait for the current request"
              else
                start
                  {
                    t with
                    focus = Detail;
                    scroll = 0;
                    opened = Some m.source;
                    opened_message = Some m;
                    drafts =
                      (if List.mem_assoc m.source t.drafts then t.drafts
                       else (m.source, "") :: t.drafts);
                    detail = None;
                    conversation = [];
                    show_history = false;
                  }
                  (Conversation m.source))
      | Outbox -> (
          match selected_draft t with
          | None -> keep t
          | Some d ->
              start
                {
                  (remember_view t) with
                  tab = Mail;
                  focus = Detail;
                  opened = Some d.source;
                  opened_message = None;
                  detail = None;
                  conversation = [];
                  show_history = false;
                  drafts =
                    (if List.mem_assoc d.source t.drafts then t.drafts
                     else (d.source, d.body) :: t.drafts);
                }
                (Conversation d.source))
      | People | Todos ->
          if count t = 0 then keep t
          else keep { t with focus = Detail; scroll = 0 })
  | Next_page when t.tab = Mail -> (
      match t.page.next with
      | None -> notice t "End of messages"
      | Some p -> messages { t with focus = Listing } p)
  | Previous_page when t.tab = Mail -> (
      match t.previous with
      | [] -> notice t "First page"
      | p :: _ -> messages { t with focus = Listing } p)
  | Capture_selected when t.tab = Mail && not t.choosing_mailbox -> (
      match selected_message t with
      | None -> keep t
      | Some m ->
          if linked_tasks t m = [] then start t (Capture m)
          else update t Task_selected)
  | Source_selected when t.tab = Todos -> (
      match selected_task t with
      | Some { sources = source :: _; _ } when t.pending = None ->
          start
            {
              (remember_view t) with
              tab = Mail;
              focus = Detail;
              scroll = 0;
              opened = Some source;
              opened_message = None;
              drafts =
                (if List.mem_assoc source t.drafts then t.drafts
                 else (source, "") :: t.drafts);
              detail = None;
              conversation = [];
              show_history = false;
              report = None;
            }
            (Conversation source)
      | _ -> notice t "No email source, or a request is still in progress")
  | Sender_contacts when t.tab = Mail -> (
      match selected_message t with
      | Some { addresses = address :: _; _ } when t.pending = None ->
          start
            {
              (remember_view t) with
              tab = People;
              focus = Listing;
              selected = 0;
              people_query = address;
              choosing_mailbox = false;
            }
            Contacts
      | _ -> notice t "No sender address, or a request is still in progress")
  | Archive_selected when t.tab = Mail && not t.choosing_mailbox -> (
      match selected_message t with
      | None -> keep t
      | Some m -> start t (Archive m.source))
  | Toggle_seen when t.tab = Mail && not t.choosing_mailbox -> (
      match selected_message t with
      | None -> keep t
      | Some m -> start t (Set_seen (m.source, not m.seen)))
  | Toggle_flagged when t.tab = Mail && not t.choosing_mailbox -> (
      match selected_message t with
      | None -> keep t
      | Some m -> start t (Set_flagged (m.source, not m.flagged)))
  | Complete_selected when t.tab = Todos -> (
      match selected_task t with
      | None -> keep t
      | Some n when n.status = "done" -> notice t "Task is already done"
      | Some n -> start t (Complete n))
  | Preview_sync when t.tab = Todos -> start t (Sync_tasks { dry_run = true })
  | Confirm_sync when t.tab = Todos ->
      if t.confirm_sync then start t (Sync_tasks { dry_run = false })
      else
        keep
          {
            t with
            confirm_sync = true;
            status =
              "Sync Dooit with configured WebDAV? Enter confirms; Esc cancels";
          }
  | _ -> keep t
