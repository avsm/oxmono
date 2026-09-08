(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The state of the mail client, its messages and its actions.

    The model is the whole of the client apart from what it draws and what it
    asks the server. {!update} is a pure function of a message and a state, so
    it runs without a terminal and without a server, and the JMAP calls it wants
    made come back as a list of {!type-action} for {!Jmap_mosaic.Io} to perform.

    Nothing here mentions Mosaic or Eio. {!Jmap_mosaic.View} draws a {!type-t}
    and turns terminal events into a {!type-msg}, and {!Jmap_mosaic.Io} turns an
    {!type-action} into a request and its answer back into a {!type-msg}.

    A client starts on the saved profile picker or the login form of {!init} and
    has no connection until {!Connect} has answered with {!Connected}. A secret
    is not drawn or printed by {!pp_action}; the executable persists it only in
    the selected XDG profile. *)

(** {1 Logging in} *)

(** The type for how a secret is presented to the server. *)
type scheme =
  | Bearer  (** A token, as RFC 6750 sends it. *)
  | Basic  (** A user and a password, as RFC 7617 joins them. *)

val scheme_to_string : scheme -> string
(** [scheme_to_string s] is ["bearer"] or ["basic"]. *)

val scheme_of_string : string -> scheme option
(** [scheme_of_string s] is the scheme [scheme_to_string] writes as [s], and
    [None] for anything else. Case is not significant. *)

val secret_name : scheme -> string
(** [secret_name s] is what the secret of [s] is called, which is ["token"] for
    {!Bearer} and ["password"] for {!Basic}. *)

type credentials = {
  url : string;  (** The URL of the session resource. *)
  scheme : scheme;
  user : string;  (** The user of a basic credential, empty for a bearer one. *)
  secret : string;  (** The password or the token. *)
}
(** The type for what a connection is made with. *)

(** The type for the field of the login form the keys edit. *)
type field =
  | Profile_field
  | Url_field
  | Scheme_field
  | User_field
  | Secret_field

type login = {
  profile : string;  (** The name used to save and select these credentials. *)
  url : string;
  scheme : scheme;
  user : string;
  secret : string;  (** The secret as typed or loaded from a saved profile. *)
  field : field;  (** The field the keys edit. *)
  error : string;  (** Why the last attempt failed, empty until one has. *)
}
(** The type for the login form. *)

type session = {
  account : Jmap.Proto.Id.t;  (** The account the client acts on. *)
  user : string;  (** The user the session says the credential belongs to. *)
}
(** The type for what a connection settled. *)

val blank : login
(** [blank] is the empty form. Its scheme is {!Bearer}, which is the default of
    the library, and its field is {!Url_field}. *)

val credentials_of : login -> credentials
(** [credentials_of l] is the credential the fields of [l] describe. *)

val valid_profile_name : string -> bool
(** [valid_profile_name name] accepts non-empty names made from ASCII letters,
    digits, dots, hyphens and underscores, except ["."] and [".."]. *)

val ready : login -> bool
(** [ready l] is [true] when [l] holds enough to connect with, which is a
    profile name, URL, secret, and a user when the scheme is {!Basic}. URL,
    user, and secret fields containing C0, DEL, or C1 controls are not ready. *)

(** {1 Records} *)

type mailbox = {
  id : Jmap.Proto.Id.t;
  name : string;
  parent : Jmap.Proto.Id.t option;
  role : Jmap.Proto.Mailbox.role option;
  total : int64;  (** The number of messages in the mailbox. *)
  unread : int64;  (** The number of messages in it without [$seen]. *)
  depth : int;  (** The number of ancestors, filled in by {!order}. *)
}
(** The type for a mailbox as the client shows it. *)

type summary = {
  eid : Jmap.Proto.Id.t;
  received_at : Ptime.t option;
  sender : string;  (** The display name of the first From address. *)
  subject : string;
  preview : string;  (** The extract of the body the server sends. *)
  seen : bool;
  flagged : bool;
  answered : bool;
  attachment : bool;
}
(** The type for one line of a message list. *)

type message = {
  head : summary;
  addresses : string list;  (** The addresses of the From field. *)
  recipients : string list;  (** The addresses of the To field. *)
  reply_to : string list;  (** The addresses of the Reply-To field. *)
  message_id : string list;
  references : string list;
  mailboxes : Jmap.Proto.Id.t list;
  keywords : Jmap.Proto.Keyword.t list;
  body : string;  (** The plain text body, its parts joined by newlines. *)
}
(** The type for an open message. *)

type identity = { identity_id : Jmap.Proto.Id.t; address : string }
(** The type for the Identity a reply is sent from. *)

type smart_search =
  | Unread
  | Needs_follow_up
      (** The built-in searches shown above the mailboxes. [Needs_follow_up] is
          mail received more than thirty days ago without the [$answered]
          keyword. *)

val smart_searches : smart_search list
(** [smart_searches] is the order of the searches in the navigation pane. *)

val smart_search_name : smart_search -> string
(** [smart_search_name s] is the label shown for [s]. *)

type source =
  | Mailbox of Jmap.Proto.Id.t
  | Smart_search of smart_search  (** The source of a message listing. *)

val source_equal : source -> source -> bool
(** [source_equal a b] compares mailbox ids with {!Jmap.Proto.Id.equal}. *)

type draft = {
  answering : Jmap.Proto.Id.t;  (** The message being replied to. *)
  identity : Jmap.Proto.Id.t;
  from : string;
  recipients : string list;
  subject : string;
  in_reply_to : string list;
  references : string list;
  text : string;
  drafts : Jmap.Proto.Id.t;  (** The mailbox the draft is created in. *)
  sent : Jmap.Proto.Id.t;  (** The mailbox it is filed into once sent. *)
}
(** The type for a reply being composed. *)

(** {1 Actions and messages} *)

(** The type for the work the client wants done against the server. An action
    may require several JMAP requests. *)
type action =
  | Connect of credentials  (** Fetch the session and resolve the account. *)
  | Load_mailboxes
  | Load_identity
  | Load_messages of source  (** List the messages of a mailbox or search. *)
  | Load_message of Jmap.Proto.Id.t  (** Fetch one message with its body. *)
  | Set_keyword of Jmap.Proto.Id.t * Jmap.Proto.Keyword.t * bool
  | Move of {
      email : Jmap.Proto.Id.t;
      into : Jmap.Proto.Id.t;
      out_of : Jmap.Proto.Id.t;
    }
  | Send of draft

(** The type for a key the client acts on. Anything else is dropped before it
    reaches {!update}. *)
type key =
  | Char of char
  | Ctrl of char
  | Enter
  | Escape
  | Tab
  | Back_tab  (** Tab with shift held. *)
  | Backspace
  | Up
  | Down
  | Paste of string  (** Text pasted into the terminal. *)

(** The type for the messages that drive {!update}. Every constructor but {!Key}
    and {!Draft_text} reports the outcome of an {!type-action}; those two report
    what the terminal did. *)
type msg =
  | Key of key
  | Connected of session
  | Login_failed of string  (** Why the connection was refused. *)
  | Mailboxes of mailbox list
  | Messages of source * summary list
      (** The messages of the named mailbox or smart search. *)
  | Opened of message
  | Keyword of Jmap.Proto.Id.t * Jmap.Proto.Keyword.t * bool
      (** The keyword now stands as given on the named message. *)
  | Moved of Jmap.Proto.Id.t * Jmap.Proto.Id.t
      (** The named message is now in the named mailbox alone. *)
  | Identity of identity
  | Sent
  | Sent_with_warning of string
      (** The reply was submitted, or may have been, and something after the
          submission did not succeed. It leaves the editor, since sending again
          could deliver the reply twice. *)
  | Send_failed of string
      (** Why the reply was not submitted. Nothing was delivered and no draft
          was left behind, so the reply may be sent again. *)
  | Draft_text of string  (** The reply body as the editor now holds it. *)
  | Failed of string

val pp_action : Format.formatter -> action -> unit
(** [pp_action ppf a] prints a one line description of [a]. *)

val pp_msg : Format.formatter -> msg -> unit
(** [pp_msg ppf m] prints a one line description of [m]. *)

(** {1 The model} *)

(** The type for the screen the client is showing. *)
type screen =
  | Profiles  (** The saved profile picker. *)
  | Login of login  (** The login form. *)
  | Connecting of login  (** The form of the attempt being made. *)
  | Folders  (** The mailboxes beside the messages of one of them. *)
  | Reading  (** One message. *)
  | Composing  (** The reply editor. *)
  | Picking of screen  (** The mailbox picker over the screen it returns to. *)

(** The type for the pane the keys move in. *)
type pane = Mailbox_pane | Message_pane

type navigation =
  | Search of smart_search
  | Folder of mailbox
      (** One selectable row of the left-hand navigation pane. *)

type t = {
  profiles : login list;  (** The saved profiles offered at startup. *)
  profile : int;  (** The highlighted saved profile. *)
  active_profile : string option;  (** The profile of the live connection. *)
  mailboxes : mailbox list;  (** In the order {!order} puts them. *)
  selection : int;  (** The highlighted item of {!val-navigation}. *)
  listing : source option;  (** The source {!messages} came from. *)
  messages : summary list;
  message : int;  (** The index of the highlighted message. *)
  reading : message option;  (** The message {!Reading} shows. *)
  identity : identity option;
  session : session option;
      (** What the connection settled, once it is made. *)
  screen : screen;
  focus : pane;
  picker : int;  (** The index of the highlighted mailbox in the picker. *)
  reply : draft option;
  sending : bool;  (** Whether the current reply has an in-flight send. *)
  status : string;  (** The line at the foot of the screen. *)
  pending : int;  (** The number of actions in flight. *)
  quit : bool;  (** [true] once the client has been asked to stop. *)
}
(** The type for the state of the client. *)

val init : ?profiles:login list -> login -> t * action list
(** [init ~profiles l] starts on the profile picker when [profiles] is not
    empty. Otherwise it connects at once when {!ready} holds of [l], and shows
    the form with the first empty field in hand when it does not. *)

val update : msg -> t -> t * action list
(** [update m t] is the state [t] becomes on [m], with the actions to perform.
    It performs none itself and never fails. *)

(** {1 Reading the model} *)

val current : t -> summary option
(** [current t] is the message the keys act on, which is the open message on
    {!Reading} and the highlighted line of the list otherwise. It is [None] when
    the list is empty. *)

val mailbox_named : t -> Jmap.Proto.Id.t -> mailbox option
(** [mailbox_named t id] is the mailbox of [t] whose id is [id]. *)

val navigation : t -> navigation list
(** [navigation t] is the built-in searches followed by the ordered mailboxes of
    [t]. *)

val order : mailbox list -> mailbox list
(** [order l] is [l] with each mailbox followed by its children, siblings
    ordered by role and then by name, and every {!field-depth} filled in. A
    mailbox whose parent is absent from [l] is treated as a root, as is one no
    root reaches, which is a mailbox in a parent cycle. Every mailbox of [l]
    appears once. *)

val stamp : Ptime.t option -> string
(** [stamp t] is [t] as ["YYYY-MM-DD HH:MM"] in UTC, or ["-"] when [t] is
    [None]. *)

val quote : message -> string
(** [quote m] is the body of [m] with an attribution line above it and every
    line prefixed with ["> "], which is the text a reply starts from. *)

val reply_subject : string -> string
(** [reply_subject s] is [s] prefixed with ["Re: "], or [s] itself when it
    already starts with a case insensitive ["re:"]. *)
