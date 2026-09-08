(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Mosaic
module Proto = Jmap.Proto

let title_bg = Ansi.Color.of_rgb 30 80 100
let status_bg = Ansi.Color.grayscale ~level:3
let quiet = Ansi.Color.grayscale ~level:8
let accent = Ansi.Color.cyan

(* A list widget fills its rows with its background before drawing, and a
   transparent background leaves the previous frame's cells in place, so the
   lists take an opaque one. *)
let panel = Ansi.Color.grayscale ~level:0
let bold = Ansi.Style.make ~bold:true ()
let dim = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let label = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let full = { width = pct 100; height = pct 100 }
let loose = { width = px 0; height = px 0 }

let mailbox_label (m : Model.mailbox) =
  Printf.sprintf "%s%s (%Ld/%Ld)"
    (String.make (2 * m.depth) ' ')
    m.name m.unread m.total

let mailbox_items t =
  List.map
    (fun m -> { Select.label = mailbox_label m; description = None })
    t.Model.mailboxes

let navigation_label = function
  | Model.Search Model.Unread -> "1  Unread"
  | Model.Search Model.Needs_follow_up -> "2  Unanswered >30d"
  | Model.Folder mailbox -> "   " ^ mailbox_label mailbox

let navigation_items t =
  List.map
    (fun item -> { Select.label = navigation_label item; description = None })
    (Model.navigation t)

let flags (s : Model.summary) =
  Printf.sprintf "%s%s%s%s"
    (if s.seen then " " else "\u{25cf}")
    (if s.flagged then "\u{2605}" else " ")
    (if s.answered then "\u{21a9}" else " ")
    (if s.attachment then "@" else " ")

let columns =
  Table.
    [
      column ~width:(`Fixed 16) "Date";
      column ~width:(`Fixed 22) "From";
      column ~width:(`Flex 1.) "Subject";
      column ~width:(`Fixed 4) ~alignment:`Center "";
    ]

let rows t =
  List.map
    (fun (s : Model.summary) ->
      [|
        Table.cell (Model.stamp s.received_at);
        Table.cell s.sender;
        Table.cell
          ~style:(if s.seen then Ansi.Style.default else bold)
          s.subject;
        Table.cell (flags s);
      |])
    t.Model.messages

let pane_border t p = if t.Model.focus = p then accent else quiet

let mailbox_pane t =
  box ~border:true ~title:"Searches & mailboxes"
    ~border_color:(pane_border t Model.Mailbox_pane)
    ~size:{ width = px 34; height = pct 100 }
    ~min_size:loose
    [
      select ~focusable:false ~selected_index:t.Model.selection
        ~show_description:false ~background:panel ~selected_background:accent
        ~selected_text_color:Ansi.Color.black ~size:full (navigation_items t);
    ]

let listing_name t =
  match t.Model.listing with
  | None -> "Messages"
  | Some (Model.Mailbox id) -> (
      match Model.mailbox_named t id with
      | Some m -> m.name
      | None -> "Messages")
  | Some (Model.Smart_search search) -> Model.smart_search_name search

let message_pane t =
  let preview = match Model.current t with Some s -> s.preview | None -> "" in
  box ~border:true ~title:(listing_name t)
    ~border_color:(pane_border t Model.Message_pane)
    ~flex_grow:1. ~flex_direction:Column ~min_size:loose
    [
      table ~focusable:false ~columns ~rows:(rows t)
        ~selected_row:t.Model.message ~border:false ~show_header:true
        ~cell_padding:1 ~background:panel ~selected_background:accent
        ~selected_text_color:Ansi.Color.black ~size:full ();
      text ~style:dim ~truncate:true preview;
    ]

let folders t =
  box ~flex_direction:Row ~gap:(gap 1) ~flex_grow:1. ~min_size:loose
    [ mailbox_pane t; message_pane t ]

let field name value =
  box ~flex_direction:Row ~gap:(gap 1)
    [ text ~style:label (Printf.sprintf "%-9s" name); text value ]

let mailbox_names t ids =
  String.concat ", "
    (List.map
       (fun id ->
         match Model.mailbox_named t id with
         | Some m -> m.name
         | None -> Format.asprintf "%a" Proto.Id.pp id)
       ids)

let reading t (m : Model.message) =
  box ~flex_direction:Column ~flex_grow:1. ~gap:(gap 1) ~min_size:loose
    [
      box ~border:true ~border_color:quiet ~flex_direction:Column
        ~padding:(padding_xy 1 0)
        [
          field "From" (String.concat ", " m.addresses);
          field "To" (String.concat ", " m.recipients);
          field "Date" (Model.stamp m.head.received_at);
          field "Subject" m.head.subject;
          field "In" (mailbox_names t m.mailboxes);
          (* A keyword is a value rather than a string in the model, so the
             escaping [Jmap_mosaic.Io] applies to server text is the printer of
             the keyword itself. *)
          field "Keywords"
            (String.concat " "
               (List.map (Format.asprintf "%a" Proto.Keyword.pp) m.keywords));
        ];
      box ~border:true ~border_color:quiet ~title:"Body" ~flex_grow:1.
        ~min_size:loose
        [
          scroll_box ~focusable:true ~autofocus:true ~scroll_y:true ~size:full
            [ text ~wrap:`Word m.body ];
        ];
    ]

let composing (d : Model.draft) =
  box ~flex_direction:Column ~flex_grow:1. ~gap:(gap 1) ~min_size:loose
    [
      box ~border:true ~border_color:quiet ~flex_direction:Column
        ~padding:(padding_xy 1 0)
        [
          field "From" d.from;
          field "To" (String.concat ", " d.recipients);
          field "Subject" d.subject;
        ];
      box ~border:true ~border_color:accent ~title:"Reply" ~flex_grow:1.
        ~min_size:loose
        [
          textarea ~key:"reply" ~value:d.text ~autofocus:true ~wrap:`Word
            ~size:full
            ~on_input:(fun s -> Some (Model.Draft_text s))
            ();
        ];
    ]

let chars s =
  String.fold_left
    (fun n c -> if Char.code c land 0xc0 = 0x80 then n else n + 1)
    0 s

let bullets s = String.concat "" (List.init (chars s) (fun _ -> "\u{2022}"))

(* The box is 60 columns: two of border, two of padding, twelve of label, one
   of gap and one for the cursor leave 42 for the value. *)
let value_width = 42

let tail n s =
  let rec start i left =
    if i = 0 || left = 0 then i
    else
      let i = i - 1 in
      if Char.code s.[i] land 0xc0 = 0x80 then start i left
      else start i (left - 1)
  in
  let i = start (String.length s) n in
  if i = 0 then s else String.sub s i (String.length s - i)

let entry ~focused name value =
  let value = tail value_width value in
  box ~flex_direction:Row ~gap:(gap 1)
    [
      text
        ~style:(if focused then bold else label)
        (Printf.sprintf "%-12s" name);
      text (if focused then value ^ "\u{2588}" else value);
    ]

let form (l : Model.login) ~busy =
  let on f = (not busy) && l.field = f in
  let note =
    if busy then [ text ~style:dim "connecting" ]
    else if l.error = "" then []
    else
      [
        text
          ~style:(Ansi.Style.make ~fg:Ansi.Color.red ())
          ~truncate:true
          (List.hd (String.split_on_char '\n' l.error));
      ]
  in
  box ~flex_grow:1. ~align_items:Center ~justify_content:Center ~min_size:loose
    [
      box ~border:true ~border_color:accent ~title:"Sign in"
        ~padding:(padding_xy 1 0) ~flex_direction:Column
        ~size:{ width = px 60; height = auto }
        (List.concat
           [
             [
               entry ~focused:(on Model.Profile_field) "Profile" l.profile;
               entry ~focused:(on Model.Url_field) "Session URL" l.url;
               entry ~focused:(on Model.Scheme_field) "Auth"
                 (Model.scheme_to_string l.scheme);
             ];
             (if l.scheme = Model.Basic then
                [ entry ~focused:(on Model.User_field) "Username" l.user ]
              else []);
             [
               entry ~focused:(on Model.Secret_field)
                 (String.capitalize_ascii (Model.secret_name l.scheme))
                 (bullets l.secret);
             ];
             note;
           ]);
    ]

let profile_items t =
  List.map
    (fun (login : Model.login) ->
      {
        Select.label = login.profile;
        description =
          Some (Model.scheme_to_string login.scheme ^ "  " ^ login.url);
      })
    t.Model.profiles

let profiles t =
  box ~flex_grow:1. ~align_items:Center ~justify_content:Center ~min_size:loose
    [
      box ~border:true ~border_color:accent ~title:"Profiles"
        ~size:{ width = px 64; height = px 16 }
        [
          select ~focusable:false ~selected_index:t.Model.profile
            ~show_description:true ~background:panel ~selected_background:accent
            ~selected_text_color:Ansi.Color.black ~size:full (profile_items t);
        ];
    ]

let picking t =
  box ~flex_grow:1. ~align_items:Center ~justify_content:Center ~min_size:loose
    [
      box ~border:true ~border_color:accent ~title:"Move to"
        ~size:{ width = px 40; height = px 16 }
        [
          select ~focusable:false ~selected_index:t.Model.picker
            ~show_description:false ~background:panel
            ~selected_background:accent ~selected_text_color:Ansi.Color.black
            ~size:full (mailbox_items t);
        ];
    ]

let keys t =
  match t.Model.screen with
  | Profiles -> "j/k choose  Enter connect  n new  e edit  q quit"
  | Login _ ->
      if t.Model.profiles = [] then
        "Tab/Shift-Tab field  Ctrl-A auth  Enter connect  Esc quit"
      else "Tab/Shift-Tab field  Ctrl-A auth  Enter connect  Esc profiles"
  | Connecting _ -> "connecting"
  | Folders ->
      "1 unread  2 unanswered  Tab pane  j/k move  Enter open  u seen  f flag  \
       m move  R reload  q quit"
  | Reading ->
      "Esc back  r reply  u seen  f flag  m move  R reload  arrows scroll"
  | Composing -> "Ctrl-S send  Esc abandon"
  | Picking _ -> "j/k move  Enter move here  Esc cancel"

let content t =
  match t.Model.screen with
  | Profiles -> profiles t
  | Login l -> form l ~busy:false
  | Connecting l -> form l ~busy:true
  | Folders -> folders t
  | Reading -> (
      match t.reading with
      | Some m -> reading t m
      | None -> box ~flex_grow:1. [ text ~style:dim "loading" ])
  | Composing -> (
      match (t.reply, t.reading) with
      | Some d, Some _ -> composing d
      | _ -> box ~flex_grow:1. [ text ~style:dim "nothing to reply to" ])
  | Picking _ -> picking t

let render t =
  box ~flex_direction:Column ~size:full
    [
      box ~padding:(padding_xy 1 0) ~background:title_bg ~flex_direction:Row
        ~justify_content:Space_between
        ~size:{ width = pct 100; height = auto }
        [
          text ~style:bold
            (match (t.Model.active_profile, t.Model.session) with
            | Some profile, Some s ->
                "jmap-mosaic \u{2014} " ^ profile ^ " \u{2014} " ^ s.user
            | None, Some s -> "jmap-mosaic \u{2014} " ^ s.user
            | _, None -> "jmap-mosaic");
          text ~style:dim
            (if t.Model.pending > 0 then
               Printf.sprintf "%d request(s) in flight" t.pending
             else "");
        ];
      box ~padding:(padding 1) ~flex_grow:1. ~flex_direction:Column
        ~min_size:loose
        [ content t ];
      box ~padding:(padding_xy 1 0) ~background:status_bg ~flex_direction:Column
        ~size:{ width = pct 100; height = auto }
        [ text ~truncate:true t.status; text ~style:dim (keys t) ];
    ]

let key_of_event ev =
  let e = Event.Key.data ev in
  let ascii c = if Uchar.is_char c then Some (Uchar.to_char c) else None in
  match e.key with
  | Escape -> Some Model.Escape
  | Enter -> Some Model.Enter
  | Backspace -> Some Model.Backspace
  | Tab -> Some (if e.modifier.shift then Model.Back_tab else Model.Tab)
  | Up -> Some Model.Up
  | Down -> Some Model.Down
  | Char c -> (
      match ascii c with
      | None -> None
      | Some c when Char.code c < 32 ->
          Some (Model.Ctrl (Char.chr (Char.code c + 96)))
      | Some c when e.modifier.ctrl ->
          Some (Model.Ctrl (Char.lowercase_ascii c))
      | Some c -> Some (Model.Char c))
  | _ -> None

let subscriptions _ =
  Sub.batch
    [
      Sub.on_key_all (fun ev ->
          Option.map (fun k -> Model.Key k) (key_of_event ev));
      Sub.on_paste_all (fun p ->
          Some (Model.Key (Model.Paste (Event.Paste.text p))));
    ]
