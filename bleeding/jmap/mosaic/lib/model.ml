(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Proto = Jmap.Proto

(* Keep the constructors in the same order as [Jmap_eio.Auth.scheme]. This
   separate type keeps the model independent of Eio; [Login] and [Io] adapt
   it to the shared profile and authentication types. *)
type scheme = Bearer | Basic

let scheme_to_string = function Bearer -> "bearer" | Basic -> "basic"

let scheme_of_string s =
  match String.lowercase_ascii s with
  | "bearer" -> Some Bearer
  | "basic" -> Some Basic
  | _ -> None

let secret_name = function Bearer -> "token" | Basic -> "password"

type credentials = {
  url : string;
  scheme : scheme;
  user : string;
  secret : string;
}

type field =
  | Profile_field
  | Url_field
  | Scheme_field
  | User_field
  | Secret_field

type login = {
  profile : string;
  url : string;
  scheme : scheme;
  user : string;
  secret : string;
  field : field;
  error : string;
}

type session = { account : Proto.Id.t; user : string }

let blank =
  {
    profile = "default";
    url = "";
    scheme = Bearer;
    user = "";
    secret = "";
    field = Url_field;
    error = "";
  }

let credentials_of l =
  { url = l.url; scheme = l.scheme; user = l.user; secret = l.secret }

(* This must stay in step with [Jmap_eio.Profile.valid_name], which decides
   whether [Login.write_profile] can store a profile under a name this
   accepts. The model may not mention Eio, so the rule is written twice. *)
let valid_profile_name name =
  let valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  name <> "" && name <> "." && name <> ".." && String.for_all valid_char name

let contains_control value =
  not (String.equal value (Format.asprintf "%a" Proto.Error.pp_escaped value))

let fields l =
  match l.scheme with
  | Bearer -> [ Profile_field; Url_field; Scheme_field; Secret_field ]
  | Basic ->
      [ Profile_field; Url_field; Scheme_field; User_field; Secret_field ]

(* What a connection needs, in the order the form asks for it, each entry
   naming the field at fault and what to tell the user. [ready], [first_gap]
   and [wanting] all read this, so none of them can disagree about a form. *)
let requirements (l : login) =
  let secret = secret_name l.scheme in
  let control what =
    Printf.sprintf "the %s contains a control character" what
  in
  List.concat
    [
      [
        (Profile_field, l.profile <> "", "a profile name is needed");
        ( Profile_field,
          valid_profile_name l.profile,
          "profile names use letters, digits, '.', '-' and '_'" );
        (Url_field, l.url <> "", "a session URL is needed");
        (Url_field, not (contains_control l.url), control "session URL");
      ];
      (if l.scheme = Basic then
         [ (User_field, l.user <> "", "a username is needed") ]
       else []);
      [
        (User_field, not (contains_control l.user), control "username");
        (Secret_field, l.secret <> "", Printf.sprintf "a %s is needed" secret);
        (Secret_field, not (contains_control l.secret), control secret);
      ];
    ]

let unmet l = List.find_opt (fun (_, holds, _) -> not holds) (requirements l)
let ready l = Option.is_none (unmet l)

(* A bearer form shows no user field, so a complaint about one leaves the keys
   where they were rather than editing a field the user cannot see. *)
let first_gap l =
  match unmet l with
  | Some (field, _, _) when List.mem field (fields l) -> field
  | _ -> Url_field

let wanting l =
  match unmet l with Some (_, _, complaint) -> complaint | None -> ""

type mailbox = {
  id : Proto.Id.t;
  name : string;
  parent : Proto.Id.t option;
  role : Proto.Mailbox.role option;
  total : int64;
  unread : int64;
  depth : int;
}

type summary = {
  eid : Proto.Id.t;
  received_at : Ptime.t option;
  sender : string;
  subject : string;
  preview : string;
  seen : bool;
  flagged : bool;
  answered : bool;
  attachment : bool;
}

type message = {
  head : summary;
  addresses : string list;
  recipients : string list;
  reply_to : string list;
  message_id : string list;
  references : string list;
  mailboxes : Proto.Id.t list;
  keywords : Proto.Keyword.t list;
  body : string;
}

type identity = { identity_id : Proto.Id.t; address : string }
type smart_search = Unread | Needs_follow_up

let smart_searches = [ Unread; Needs_follow_up ]

let smart_search_name = function
  | Unread -> "Unread"
  | Needs_follow_up -> "Unanswered >30d"

type source = Mailbox of Proto.Id.t | Smart_search of smart_search

let source_equal a b =
  match (a, b) with
  | Mailbox a, Mailbox b -> Proto.Id.equal a b
  | Smart_search a, Smart_search b -> a = b
  | _ -> false

type draft = {
  answering : Proto.Id.t;
  identity : Proto.Id.t;
  from : string;
  recipients : string list;
  subject : string;
  in_reply_to : string list;
  references : string list;
  text : string;
  drafts : Proto.Id.t;
  sent : Proto.Id.t;
}

type action =
  | Connect of credentials
  | Load_mailboxes
  | Load_identity
  | Load_messages of source
  | Load_message of Proto.Id.t
  | Set_keyword of Proto.Id.t * Proto.Keyword.t * bool
  | Move of { email : Proto.Id.t; into : Proto.Id.t; out_of : Proto.Id.t }
  | Send of draft

type key =
  | Char of char
  | Ctrl of char
  | Enter
  | Escape
  | Tab
  | Back_tab
  | Backspace
  | Up
  | Down
  | Paste of string

type msg =
  | Key of key
  | Connected of session
  | Login_failed of string
  | Mailboxes of mailbox list
  | Messages of source * summary list
  | Opened of message
  | Keyword of Proto.Id.t * Proto.Keyword.t * bool
  | Moved of Proto.Id.t * Proto.Id.t
  | Identity of identity
  | Sent
  | Sent_with_warning of string
  | Send_failed of string
  | Draft_text of string
  | Failed of string

let pp_action ppf = function
  | Connect c -> Format.fprintf ppf "connect to %s" c.url
  | Load_mailboxes -> Format.pp_print_string ppf "load mailboxes"
  | Load_identity -> Format.pp_print_string ppf "load identity"
  | Load_messages (Mailbox id) ->
      Format.fprintf ppf "load messages of %a" Proto.Id.pp id
  | Load_messages (Smart_search search) ->
      Format.fprintf ppf "load smart search %S" (smart_search_name search)
  | Load_message id -> Format.fprintf ppf "load message %a" Proto.Id.pp id
  | Set_keyword (id, k, on) ->
      Format.fprintf ppf "%s %a on %a"
        (if on then "set" else "clear")
        Proto.Keyword.pp k Proto.Id.pp id
  | Move { email; into; out_of } ->
      Format.fprintf ppf "move %a from %a to %a" Proto.Id.pp email Proto.Id.pp
        out_of Proto.Id.pp into
  | Send d -> Format.fprintf ppf "send %S" d.subject

let pp_msg ppf = function
  | Key _ -> Format.pp_print_string ppf "key"
  | Connected s -> Format.fprintf ppf "connected as %s" s.user
  | Login_failed e -> Format.fprintf ppf "login failed: %s" e
  | Mailboxes l -> Format.fprintf ppf "%d mailbox(es)" (List.length l)
  | Messages (Mailbox id, l) ->
      Format.fprintf ppf "%d message(s) of %a" (List.length l) Proto.Id.pp id
  | Messages (Smart_search search, l) ->
      Format.fprintf ppf "%d message(s) of smart search %S" (List.length l)
        (smart_search_name search)
  | Opened m ->
      Format.fprintf ppf "opened %a, %d byte(s) of body" Proto.Id.pp m.head.eid
        (String.length m.body)
  | Keyword (id, k, on) ->
      Format.fprintf ppf "%a on %a is %b" Proto.Keyword.pp k Proto.Id.pp id on
  | Moved (id, into) ->
      Format.fprintf ppf "moved %a to %a" Proto.Id.pp id Proto.Id.pp into
  | Identity i -> Format.fprintf ppf "identity %s" i.address
  | Sent -> Format.pp_print_string ppf "sent"
  | Sent_with_warning warning -> Format.fprintf ppf "sent: %s" warning
  | Send_failed error -> Format.fprintf ppf "send failed: %s" error
  | Draft_text s -> Format.fprintf ppf "draft of %d byte(s)" (String.length s)
  | Failed e -> Format.fprintf ppf "failed: %s" e

type screen =
  | Profiles
  | Login of login
  | Connecting of login
  | Folders
  | Reading
  | Composing
  | Picking of screen

type pane = Mailbox_pane | Message_pane

type t = {
  profiles : login list;
  profile : int;
  active_profile : string option;
  mailboxes : mailbox list;
  selection : int;
  listing : source option;
  messages : summary list;
  message : int;
  reading : message option;
  identity : identity option;
  session : session option;
  screen : screen;
  focus : pane;
  picker : int;
  reply : draft option;
  sending : bool;
  status : string;
  pending : int;
  quit : bool;
}

let stamp = function
  | None -> "-"
  | Some t ->
      let (y, m, d), ((hh, mm, _), _) = Ptime.to_date_time t in
      Printf.sprintf "%04d-%02d-%02d %02d:%02d" y m d hh mm

let role_rank = function
  | Some `Inbox -> 0
  | Some `Drafts -> 1
  | Some `Sent -> 2
  | Some `Archive -> 3
  | Some `Junk -> 4
  | Some `Trash -> 5
  | Some _ -> 6
  | None -> 7

let order l =
  let compare a b =
    match Int.compare (role_rank a.role) (role_rank b.role) with
    | 0 ->
        String.compare
          (String.lowercase_ascii a.name)
          (String.lowercase_ascii b.name)
    | n -> n
  in
  let sort ms = List.sort compare ms in
  let size = List.length l in
  let present = Hashtbl.create size in
  List.iter (fun m -> Hashtbl.replace present (Proto.Id.to_string m.id) ()) l;
  let children = Hashtbl.create size in
  let add parent m =
    Hashtbl.replace children parent
      (m :: Option.value (Hashtbl.find_opt children parent) ~default:[])
  in
  let roots =
    List.filter
      (fun m ->
        match m.parent with
        | Some parent when Hashtbl.mem present (Proto.Id.to_string parent) ->
            add (Proto.Id.to_string parent) m;
            false
        | _ -> true)
      l
  in
  let placed = Hashtbl.create size in
  let rec walk depth ms =
    List.concat_map
      (fun m ->
        let key = Proto.Id.to_string m.id in
        if Hashtbl.mem placed key then []
        else begin
          Hashtbl.replace placed key ();
          { m with depth }
          :: walk (depth + 1)
               (sort
                  (List.rev
                     (Option.value (Hashtbl.find_opt children key) ~default:[])))
        end)
      ms
  in
  let ordered = walk 0 (sort roots) in
  (* [parentId] is server data, so the mailboxes can hold a cycle, whose
     members are reachable from no root. Each is walked as a root of its own
     rather than dropped from the navigation pane. *)
  let stranded =
    List.filter (fun m -> not (Hashtbl.mem placed (Proto.Id.to_string m.id))) l
  in
  ordered @ walk 0 (sort stranded)

let mailbox_named t id =
  List.find_opt (fun m -> Proto.Id.equal m.id id) t.mailboxes

let mailbox_with_role t role =
  List.find_opt (fun m -> m.role = Some role) t.mailboxes

let rec base = function Picking s -> base s | s -> s

let current t =
  match (base t.screen, t.reading) with
  | Reading, Some m -> Some m.head
  | _ -> List.nth_opt t.messages t.message

let quote m =
  let who = match m.addresses with a :: _ -> a | [] -> m.head.sender in
  let lines = String.split_on_char '\n' (String.trim m.body) in
  String.concat "\n"
    (Printf.sprintf "On %s, %s wrote:" (stamp m.head.received_at) who
    :: List.map (fun l -> "> " ^ l) lines)
  ^ "\n"

let reply_subject s =
  let t = String.trim s in
  if String.length t >= 3 && String.lowercase_ascii (String.sub t 0 3) = "re:"
  then s
  else "Re: " ^ s

let empty =
  {
    profiles = [];
    profile = 0;
    active_profile = None;
    mailboxes = [];
    selection = 0;
    listing = None;
    messages = [];
    message = 0;
    reading = None;
    identity = None;
    session = None;
    screen = Login blank;
    focus = Mailbox_pane;
    picker = 0;
    reply = None;
    sending = false;
    status = "";
    pending = 0;
    quit = false;
  }

let init ?(profiles = []) l =
  let t, actions =
    if profiles <> [] then
      ( { empty with profiles; screen = Profiles; status = "choose a profile" },
        [] )
    else if ready l then
      ( { empty with screen = Connecting l; status = "connecting to " ^ l.url },
        [ Connect (credentials_of l) ] )
    else ({ empty with screen = Login { l with field = first_gap l } }, [])
  in
  ({ t with pending = List.length actions }, actions)

let clamp n len = if len = 0 then 0 else Int.max 0 (Int.min (len - 1) n)
let say status t = ({ t with status }, [])

let move_field d l =
  let fs = fields l in
  let n = List.length fs in
  let at = Option.value ~default:0 (List.find_index (( = ) l.field) fs) in
  { l with field = List.nth fs ((((at + d) mod n) + n) mod n) }

let edit f l =
  match l.field with
  | Profile_field -> { l with profile = f l.profile }
  | Url_field -> { l with url = f l.url }
  | User_field -> { l with user = f l.user }
  | Secret_field -> { l with secret = f l.secret }
  | Scheme_field -> l

let drop s =
  let rec start i =
    if i > 0 && Char.code s.[i] land 0xc0 = 0x80 then start (i - 1) else i
  in
  if s = "" then s else String.sub s 0 (start (String.length s - 1))

let toggle_scheme l =
  let l =
    {
      l with
      scheme = (match l.scheme with Bearer -> Basic | Basic -> Bearer);
    }
  in
  if List.mem l.field (fields l) then l else { l with field = Secret_field }

let control_length ~valid_utf8 value index =
  let byte = Char.code (String.unsafe_get value index) in
  if byte <= 0x1f || byte = 0x7f then Some 1
  else if
    byte = 0xc2
    && index + 1 < String.length value
    &&
    let next = Char.code (String.unsafe_get value (index + 1)) in
    next >= 0x80 && next <= 0x9f
  then Some 2
  else if (not valid_utf8) && byte >= 0x80 && byte <= 0x9f then Some 1
  else None

(* A token pasted from a browser often carries a trailing newline, and a
   field holds one line. Other control characters are not meaningful in any
   profile field and must not survive a terminal paste either. A paste is
   stripped rather than rendered, so it classifies the bytes itself instead of
   comparing against [Proto.Error.pp_escaped] as [contains_control] does. *)
let pasted text =
  let valid_utf8 = String.is_valid_utf_8 text in
  let clean = Buffer.create (String.length text) in
  let rec copy index =
    if index < String.length text then
      match control_length ~valid_utf8 text index with
      | Some length -> copy (index + length)
      | None ->
          Buffer.add_char clean (String.unsafe_get text index);
          copy (index + 1)
  in
  copy 0;
  String.trim (Buffer.contents clean)

let printable_input c =
  let code = Char.code c in
  code >= 0x20 && (code < 0x7f || code > 0x9f)

let login l k t =
  let form l = ({ t with screen = Login l }, []) in
  match k with
  | Tab | Down -> form (move_field 1 l)
  | Back_tab | Up -> form (move_field (-1) l)
  | Ctrl 'a' -> form (toggle_scheme l)
  | Char ' ' when l.field = Scheme_field -> form (toggle_scheme l)
  | Backspace -> form (edit drop l)
  | Char c when printable_input c ->
      form (edit (fun s -> s ^ String.make 1 c) l)
  | Paste text -> form (edit (fun s -> s ^ pasted text) l)
  | Enter ->
      if not (ready l) then form { l with error = wanting l }
      else
        ( { t with screen = Connecting l; status = "connecting to " ^ l.url },
          [ Connect (credentials_of l) ] )
  | Escape ->
      if t.profiles = [] then ({ t with quit = true }, [])
      else ({ t with screen = Profiles; status = "choose a profile" }, [])
  | _ -> (t, [])

let profiles k t =
  let selected () = List.nth_opt t.profiles t.profile in
  match k with
  | Up | Char 'k' ->
      ({ t with profile = clamp (t.profile - 1) (List.length t.profiles) }, [])
  | Down | Char 'j' ->
      ({ t with profile = clamp (t.profile + 1) (List.length t.profiles) }, [])
  | Enter -> (
      match selected () with
      | Some l when ready l ->
          ( { t with screen = Connecting l; status = "connecting to " ^ l.url },
            [ Connect (credentials_of l) ] )
      | Some l ->
          ( { t with screen = Login { l with field = first_gap l }; status = "" },
            [] )
      | None -> say "no profile" t)
  | Char 'n' ->
      ( {
          t with
          screen = Login { blank with profile = ""; field = Profile_field };
          status = "";
        },
        [] )
  | Char 'e' -> (
      match selected () with
      | Some l ->
          ({ t with screen = Login l; status = "editing " ^ l.profile }, [])
      | None -> say "no profile" t)
  | Escape | Char 'q' -> ({ t with quit = true }, [])
  | _ -> (t, [])

let with_keyword k on s =
  match k with
  | `Seen -> { s with seen = on }
  | `Flagged -> { s with flagged = on }
  | `Answered -> { s with answered = on }
  | _ -> s

let over_message id f t =
  let one s = if Proto.Id.equal s.eid id then f s else s in
  {
    t with
    messages = List.map one t.messages;
    reading =
      Option.map
        (fun m ->
          if Proto.Id.equal m.head.eid id then { m with head = f m.head } else m)
        t.reading;
  }

let toggle k held t =
  match current t with
  | None -> say "no message to change" t
  | Some s ->
      let on = not (held s) in
      ( {
          t with
          status =
            Printf.sprintf "%s %s"
              (if on then "setting" else "clearing")
              (Proto.Keyword.to_string k);
        },
        [ Set_keyword (s.eid, k, on) ] )

let open_message s t =
  ( { t with screen = Reading; reading = None; status = "loading message" },
    [ Load_message s.eid ] )

let select_source source name t =
  ( {
      t with
      listing = Some source;
      messages = [];
      message = 0;
      focus = Message_pane;
      status = "loading " ^ name;
    },
    [ Load_messages source ] )

let select_mailbox m t = select_source (Mailbox m.id) m.name t

let select_search search t =
  select_source (Smart_search search) (smart_search_name search) t

type navigation = Search of smart_search | Folder of mailbox

let navigation t =
  List.map (fun search -> Search search) smart_searches
  @ List.map (fun mailbox -> Folder mailbox) t.mailboxes

let select_navigation n t =
  match List.nth_opt (navigation t) n with
  | Some (Search search) -> select_search search t
  | Some (Folder mailbox) -> select_mailbox mailbox t
  | None -> say "nothing selected" t

let compose t =
  match (t.reading, t.identity) with
  | None, _ -> say "no message to reply to" t
  | _, None -> say "this account has no Identity to send from" t
  | Some m, Some i -> (
      match (mailbox_with_role t `Drafts, mailbox_with_role t `Sent) with
      | Some drafts, Some sent ->
          let recipients = match m.reply_to with [] -> m.addresses | l -> l in
          let reply =
            {
              answering = m.head.eid;
              identity = i.identity_id;
              from = i.address;
              recipients;
              subject = reply_subject m.head.subject;
              in_reply_to = m.message_id;
              references = m.references @ m.message_id;
              text = quote m;
              drafts = drafts.id;
              sent = sent.id;
            }
          in
          ( {
              t with
              screen = Composing;
              reply = Some reply;
              status = "Ctrl-S sends, Esc abandons";
            },
            [] )
      | _ -> say "the account has no Drafts or Sent mailbox" t)

let reload t =
  ( { t with status = "reloading" },
    Load_mailboxes
    :: (match t.listing with Some id -> [ Load_messages id ] | None -> []) )

let move_index d t =
  match t.focus with
  | Mailbox_pane ->
      {
        t with
        selection =
          clamp (t.selection + d)
            (List.length smart_searches + List.length t.mailboxes);
      }
  | Message_pane ->
      { t with message = clamp (t.message + d) (List.length t.messages) }

let pick_mailbox t =
  match t.listing with
  | Some (Mailbox id) ->
      let picker =
        Option.value ~default:0
          (List.find_index (fun m -> Proto.Id.equal m.id id) t.mailboxes)
      in
      ({ t with screen = Picking t.screen; picker }, [])
  | Some (Smart_search _) ->
      say "switch to a mailbox before filing a smart-search result" t
  | None -> say "no mailbox" t

let folders k t =
  match k with
  | Tab ->
      ( {
          t with
          focus =
            (match t.focus with
            | Mailbox_pane -> Message_pane
            | Message_pane -> Mailbox_pane);
        },
        [] )
  | Up | Char 'k' -> (move_index (-1) t, [])
  | Down | Char 'j' -> (move_index 1 t, [])
  | Enter -> (
      match t.focus with
      | Mailbox_pane -> select_navigation t.selection t
      | Message_pane -> (
          match current t with
          | Some s -> open_message s t
          | None -> say "no message" t))
  | Char 'q' -> ({ t with quit = true }, [])
  | Char '1' -> select_search Unread { t with selection = 0 }
  | Char '2' -> select_search Needs_follow_up { t with selection = 1 }
  | Char 'R' -> reload t
  | Char 'u' -> toggle `Seen (fun s -> s.seen) t
  | Char 'f' -> toggle `Flagged (fun s -> s.flagged) t
  | Char 'm' -> pick_mailbox t
  | _ -> (t, [])

let reading k t =
  match k with
  | Escape -> ({ t with screen = Folders; reading = None; status = "" }, [])
  | Char 'u' -> toggle `Seen (fun s -> s.seen) t
  | Char 'f' -> toggle `Flagged (fun s -> s.flagged) t
  | Char 'm' -> pick_mailbox t
  | Char 'r' -> compose t
  | Char 'R' -> (
      match current t with
      | Some s -> ({ t with status = "reloading" }, [ Load_message s.eid ])
      | None -> (t, []))
  | _ -> (t, [])

let composing k t =
  match (k, t.reply) with
  | Escape, _ when t.sending -> say "waiting for the send to finish" t
  | Escape, _ ->
      ({ t with screen = Reading; reply = None; status = "reply abandoned" }, [])
  | Ctrl 's', Some _ when t.sending -> say "already sending" t
  | Ctrl 's', Some d ->
      ({ t with sending = true; status = "sending" }, [ Send d ])
  | _ -> (t, [])

let picking back k t =
  match k with
  | Up | Char 'k' ->
      ({ t with picker = clamp (t.picker - 1) (List.length t.mailboxes) }, [])
  | Down | Char 'j' ->
      ({ t with picker = clamp (t.picker + 1) (List.length t.mailboxes) }, [])
  | Escape -> ({ t with screen = back; status = "" }, [])
  | Enter -> (
      match (current t, t.listing, List.nth_opt t.mailboxes t.picker) with
      | Some s, Some (Mailbox out_of), Some m
        when not (Proto.Id.equal m.id out_of) ->
          ( { t with screen = back; status = "moving to " ^ m.name },
            [ Move { email = s.eid; into = m.id; out_of } ] )
      | _ -> ({ t with screen = back; status = "nothing to move" }, []))
  | _ -> (t, [])

let mailboxes_loaded l t =
  let mailboxes = order l in
  let t =
    {
      t with
      mailboxes;
      selection =
        clamp t.selection (List.length smart_searches + List.length mailboxes);
    }
  in
  match t.listing with
  | Some _ -> ({ t with status = "" }, [])
  | None -> (
      match mailbox_with_role t `Inbox with
      | None -> say "this account has no Inbox" t
      | Some inbox ->
          let at =
            Option.value ~default:0
              (List.find_index
                 (fun m -> Proto.Id.equal m.id inbox.id)
                 mailboxes)
          in
          select_mailbox inbox
            { t with selection = List.length smart_searches + at })

let step msg t =
  match msg with
  | Key k -> (
      match t.screen with
      | Profiles -> profiles k t
      | Login l -> login l k t
      | Connecting _ -> (t, [])
      | Folders -> folders k t
      | Reading -> reading k t
      | Composing -> composing k t
      | Picking back -> picking back k t)
  | Connected s ->
      let active_profile =
        match t.screen with Connecting l -> Some l.profile | _ -> None
      in
      ( {
          t with
          active_profile;
          session = Some s;
          screen = Folders;
          focus = Mailbox_pane;
          status = "connected as " ^ s.user;
        },
        [ Load_mailboxes; Load_identity ] )
  | Login_failed e -> (
      match t.screen with
      | Login l | Connecting l ->
          ( {
              t with
              screen =
                Login { l with secret = ""; field = Secret_field; error = e };
              status = "";
            },
            [] )
      | _ -> say e t)
  | Mailboxes l -> mailboxes_loaded l t
  | Messages (source, l) ->
      if not (Option.equal source_equal t.listing (Some source)) then (t, [])
      else
        ( {
            t with
            messages = l;
            message = clamp t.message (List.length l);
            status = Printf.sprintf "%d message(s)" (List.length l);
          },
          [] )
  | Opened m -> (
      match (base t.screen, current t) with
      | Reading, Some expected when Proto.Id.equal expected.eid m.head.eid ->
          let t = over_message m.head.eid (fun _ -> m.head) t in
          ( { t with reading = Some m; status = m.head.subject },
            if m.head.seen then []
            else [ Set_keyword (m.head.eid, `Seen, true) ] )
      | _ -> (t, []))
  | Keyword (id, k, on) ->
      let t = over_message id (with_keyword k on) t in
      let t =
        {
          t with
          status =
            Printf.sprintf "%s %s"
              (Proto.Keyword.to_string k)
              (if on then "set" else "cleared");
        }
      in
      let actions =
        match (t.listing, k) with
        | Some (Smart_search Unread as source), `Seen ->
            [ Load_messages source ]
        | Some (Smart_search Needs_follow_up as source), `Answered ->
            [ Load_messages source ]
        | _ -> []
      in
      (t, actions)
  | Moved (id, into) ->
      let name =
        match mailbox_named t into with
        | Some m -> m.name
        | None -> "another mailbox"
      in
      let messages =
        List.filter (fun s -> not (Proto.Id.equal s.eid id)) t.messages
      in
      let t =
        {
          t with
          messages;
          message = clamp t.message (List.length messages);
          screen = Folders;
          reading = None;
          status = "moved to " ^ name;
        }
      in
      (t, [ Load_mailboxes ])
  | Identity i -> ({ t with identity = Some i }, [])
  | Sent ->
      let t =
        match t.reply with
        | Some d -> over_message d.answering (with_keyword `Answered true) t
        | None -> t
      in
      let actions =
        match t.listing with
        | Some (Smart_search Needs_follow_up as source) ->
            [ Load_messages source ]
        | _ -> []
      in
      ( {
          t with
          screen = Reading;
          reply = None;
          sending = false;
          status = "reply sent";
        },
        actions )
  | Sent_with_warning warning ->
      let actions =
        match t.reply with
        | Some draft -> [ Load_message draft.answering; Load_mailboxes ]
        | None -> []
      in
      ( {
          t with
          screen = Reading;
          reply = None;
          sending = false;
          status = warning;
        },
        actions )
  | Send_failed error ->
      ({ t with sending = false; status = "send failed: " ^ error }, [])
  | Draft_text text ->
      ({ t with reply = Option.map (fun d -> { d with text }) t.reply }, [])
  | Failed e -> say e t

let update msg t =
  let t =
    match msg with
    | Key _ | Draft_text _ -> t
    | _ -> { t with pending = Int.max 0 (t.pending - 1) }
  in
  let t, actions = step msg t in
  ({ t with pending = t.pending + List.length actions }, actions)
