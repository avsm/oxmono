(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
type email_ref = { service : string; account : string; id : string }
[@@deriving sexp]
(** Domain values and deterministic terminal state transitions. *)

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

val initial : t

val update : t -> action -> t * (int * request) option
(** [update t action] is the next state and at most one backend request. Only a
    matching pending request can complete. Mutations carry stable object
    identities and task revisions, never a list index. *)

val visible_people : t -> contact list
val visible_tasks : t -> task list
val selected_message : t -> message option
val selected_contact : t -> contact option
val selected_task : t -> task option
val count : t -> int
val matches : query:string -> string -> bool

val safe_text : string -> string
(** [safe_text s] is valid UTF-8 with terminal controls and bidi formatting
    removed. Newlines and tabs are retained for layout. *)

val same_email : email_ref -> email_ref -> bool
val request_name : request -> string
val linked_tasks : t -> message -> task list
val saved_reply : t -> email_ref -> draft option
val selected_draft : t -> draft option
val draft_dirty : t -> email_ref -> string -> bool
val editor_version : t -> email_ref -> int

val arrows_scroll : t -> bool
(** [arrows_scroll t] is true in help, reports and send review. In other
    browsing views arrows select items, including opened contacts and linked
    tasks. The reply editor handles its own arrow keys. *)
