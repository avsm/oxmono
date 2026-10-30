open! Core
open Bonsai_term
open Bonsai.Let_syntax
module M = Siftbot_model
module Settings = Settings

type state = {
  ui : M.ui;
  area : int;
  reasons : bool;
  threaded : bool;
  correction : (M.dimension * string) option;
  pane : int;
  preview : M.edit option;
  explanation : string;
  filter : string;
  editing_filter : bool;
  discussing : bool;
  pasting : bool;
  requested_detail : M.source option;
}

let initial =
  {
    ui = M.initial;
    area = 0;
    reasons = false;
    threaded = false;
    correction = None;
    pane = 1;
    preview = None;
    explanation = "";
    filter = "";
    editing_filter = false;
    discussing = false;
    pasting = false;
    requested_detail = None;
  }

type action =
  | Refresh
  | Finished of int * M.response
  | Move of int
  | Tab
  | Topics
  | Reasons
  | Thread_order
  | Correct
  | Correction_value of M.dimension * string
  | Correction_scope of M.rule_scope option
  | Preview of M.edit
  | Cancel
  | Commit
  | Type of string
  | Backspace
  | Filter
  | Discuss
  | Paste_state of bool
  | Scroll of int

let selected t =
  Option.bind t.ui.selected ~f:(fun source ->
      List.find t.ui.snapshot.messages ~f:(fun m ->
          Poly.equal m.M.source source))

let selected_area t = List.nth t.ui.snapshot.areas t.area

let thread_key (m : M.message) =
  ( m.source.service,
    m.source.account,
    if String.is_empty m.thread then `Message m.source.id else `Thread m.thread
  )

let messages_in_order t =
  let messages = t.ui.snapshot.messages in
  if not t.threaded then messages
  else
    List.sort_and_group messages ~compare:(fun a b ->
        Poly.compare (thread_key a) (thread_key b))
    |> List.map
         ~f:
           (List.sort ~compare:(fun a b ->
                match String.compare b.M.date a.M.date with
                | 0 -> Poly.compare a.source b.source
                | n -> n))
    |> List.sort ~compare:(fun a b ->
           let a = List.hd_exn a and b = List.hd_exn b in
           match String.compare b.M.date a.M.date with
           | 0 -> Poly.compare (thread_key a) (thread_key b)
           | n -> n)
    |> List.concat

let update t action =
  let request t r =
    match M.begin_request t.ui with
    | None -> (t, None)
    | Some (ui, id) ->
        let requested_detail =
          match r with
          | M.Inspect_message (_, source) -> Some source
          | Discuss (_, source) -> source
          | _ -> None
        in
        ({ t with ui; requested_detail }, Some (id, r))
  in
  match action with
  | Refresh -> request t (M.Inspect t.filter)
  | Finished (id, r) -> (
      let selected = Option.map (selected_area t) ~f:(fun a -> a.M.id) in
      let ui = M.receive t.ui id r in
      let area =
        Option.value_map selected ~default:t.area ~f:(fun id ->
            Option.value_map
              (List.findi ui.snapshot.areas ~f:(fun _ a ->
                   String.equal a.M.id id))
              ~default:0 ~f:fst)
      in
      let t = { t with ui; area } in
      let body_source =
        match t.requested_detail with
        | Some source -> Some source
        | None ->
            Option.map (List.hd ui.snapshot.messages) ~f:(fun m -> m.M.source)
      in
      match (r, ui.selected) with
      | Ok _, Some source when not (Poly.equal body_source (Some source)) ->
          request t (M.Inspect_message (t.filter, source))
      | _ -> (t, None))
  | Correct when Option.is_none t.ui.pending ->
      ( {
          t with
          correction = Some (M.Attention, "fyi");
          preview = None;
          discussing = false;
          editing_filter = false;
        },
        None )
  | Correct -> (t, None)
  | Correction_value (dimension, value) ->
      ({ t with correction = Some (dimension, value) }, None)
  | Correction_scope scope -> (
      match (selected t, t.correction) with
      | Some m, Some (dimension, value) -> (
          try
            Option.iter scope ~f:(fun scope ->
                ignore (M.rule_key scope m : string));
            let a : M.assignment =
              {
                target = m.source;
                conversation = false;
                dimension;
                value;
                origin = Admin;
                version = "review/v1";
                source_revision = m.revision;
                explanation = "";
                evidence = [ m.source ];
                score = None;
              }
            in
            let edit =
              match scope with
              | None -> M.Assign a
              | Some scope -> M.Assign_rule (scope, a)
            in
            ( {
                t with
                correction = None;
                preview = Some edit;
                explanation = "";
                ui = { t.ui with error = None };
              },
              None )
          with Invalid_argument error ->
            ({ t with ui = { t.ui with error = Some error } }, None))
      | _ -> (t, None))
  | Paste_state pasting -> ({ t with pasting }, None)
  | Tab -> ({ t with pane = (if t.pane = 1 then 2 else 1) }, None)
  | Thread_order -> ({ t with threaded = not t.threaded }, None)
  | Topics -> ({ t with pane = (if t.pane = 0 then 1 else 0) }, None)
  | Reasons ->
      ({ t with reasons = not t.reasons; ui = { t.ui with scroll = 0 } }, None)
  | Move delta when t.pane = 0 ->
      ( {
          t with
          area =
            Int.clamp_exn (t.area + delta) ~min:0
              ~max:(max 0 (List.length t.ui.snapshot.areas - 1));
        },
        None )
  | Move delta -> (
      let ms = messages_in_order t in
      let index =
        Option.value_map t.ui.selected ~default:0 ~f:(fun source ->
            Option.value_map
              (List.findi ms ~f:(fun _ m -> Poly.equal m.M.source source))
              ~default:0 ~f:fst)
      in
      let m =
        List.nth ms
          (Int.clamp_exn (index + delta) ~min:0
             ~max:(max 0 (List.length ms - 1)))
      in
      match m with
      | None -> (t, None)
      | Some m ->
          request
            { t with ui = { t.ui with selected = Some m.source; scroll = 0 } }
            (M.Inspect_message (t.filter, m.source)))
  | Scroll n ->
      ({ t with ui = { t.ui with scroll = max 0 (t.ui.scroll + n) } }, None)
  | Preview edit when Option.is_none t.ui.pending ->
      ( { t with preview = Some edit; explanation = ""; editing_filter = false },
        None )
  | Preview _ -> (t, None)
  | Cancel ->
      ( {
          t with
          preview = None;
          correction = None;
          pane = (if t.pane = 0 then 1 else t.pane);
          editing_filter = false;
          discussing = false;
          explanation = "";
        },
        None )
  | Commit when Option.is_some t.ui.pending -> (t, None)
  | Commit
    when Poly.equal t.preview
           (Option.map (selected_area t) ~f:(fun a -> M.Rename (a.id, "")))
         && Option.is_some t.preview
         && not (String.is_empty t.explanation) ->
      let a = Option.value_exn (selected_area t) in
      ( {
          t with
          preview = Some (M.Rename (a.id, t.explanation));
          explanation = "";
        },
        None )
  | Commit when t.discussing ->
      request
        { t with discussing = false }
        (M.Discuss (t.explanation, t.ui.selected))
  | Commit when t.editing_filter ->
      request { t with editing_filter = false } (M.Inspect t.filter)
  | Commit -> (
      match t.preview with
      | Some edit when not (String.is_empty (String.strip t.explanation)) ->
          request { t with preview = None }
            (M.Edit (t.ui.snapshot.revision, edit, t.explanation, t.filter))
      | _ -> (t, None))
  | Discuss ->
      ( {
          t with
          discussing = true;
          editing_filter = false;
          preview = None;
          explanation = "";
        },
        None )
  | Filter ->
      ({ t with editing_filter = true; preview = None; filter = "" }, None)
  | Type s when t.editing_filter && String.length t.filter < 1024 ->
      ({ t with filter = t.filter ^ s }, None)
  | Type s
    when (Option.is_some t.preview || t.discussing)
         && String.length t.explanation < 4096 ->
      ({ t with explanation = t.explanation ^ s }, None)
  | Backspace ->
      let drop s =
        let rec start i =
          if i <= 0 then 0
          else if Char.to_int s.[i] land 0xc0 = 0x80 then start (i - 1)
          else i
        in
        if String.is_empty s then s
        else String.prefix s (start (String.length s - 1))
      in
      if t.editing_filter then ({ t with filter = drop t.filter }, None)
      else ({ t with explanation = drop t.explanation }, None)
  | Type _ -> (t, None)

let text ?(attrs = []) s =
  View.text ~attrs
    (M.sanitize s |> String.map ~f:(function '\n' -> ' ' | c -> c))

let fit ~width ~height v =
  let width = max 0 width and height = max 0 height in
  View.zcat
    [
      View.crop
        ~r:(max 0 (View.width v - width))
        ~b:(max 0 (View.height v - height))
        v;
      View.transparent_rectangle ~width ~height;
    ]

let blue = Attr.Color.rgb ~r:110 ~g:180 ~b:245
let green = Attr.Color.rgb ~r:120 ~g:205 ~b:145
let amber = Attr.Color.rgb ~r:235 ~g:190 ~b:100
let red = Attr.Color.rgb ~r:245 ~g:125 ~b:125

let framed ~width ~height title content =
  fit ~width ~height
    (Bonsai_term_border_box.view ~title:(M.sanitize title)
       ~title_attrs:[ Attr.bold; Attr.fg blue ]
       ~line_type:Round_corners
       (fit ~width:(width - 2) ~height:(height - 2) content))

let pane ~width ~height title lines =
  let line s =
    let attrs =
      if String.is_prefix s ~prefix:"> " then
        [
          Attr.bg (Attr.Color.rgb ~r:40 ~g:65 ~b:90);
          Attr.fg (Attr.Color.rgb ~r:245 ~g:245 ~b:245);
          Attr.bold;
        ]
      else if String.is_prefix s ~prefix:"Action:" then
        [ Attr.fg amber; Attr.bold ]
      else if String.is_prefix s ~prefix:"Priority: Spam" then [ Attr.fg red ]
      else if String.is_prefix s ~prefix:"Priority:" then [ Attr.fg green ]
      else if String.is_prefix s ~prefix:"Delivery:" then [ Attr.fg blue ]
      else []
    in
    fit ~width:(width - 2) ~height:1 (text ~attrs s)
  in
  framed ~width ~height title (View.vcat (List.map lines ~f:line))

let preview_name = function
  | M.Approve id -> "Approve area " ^ id
  | Rename (id, name) -> "Rename " ^ id ^ " to " ^ name
  | Archive id -> "Archive " ^ id
  | Merge (a, b) -> "Merge " ^ a ^ " into " ^ b
  | Define (id, _, _) -> "Edit definition " ^ id
  | Assign a -> M.dimension a.dimension ^ " = " ^ a.value
  | Assign_rule (scope, a) ->
      (match scope with
      | Sender -> "Sender rule: "
      | Mailing_list -> "Mailing-list rule: ")
      ^ M.dimension a.dimension ^ " = " ^ a.value
  | Undo -> "Undo latest decision"

let assignments t (m : M.message) =
  List.filter t.ui.snapshot.assignments ~f:(fun (a : M.assignment) ->
      ((not a.conversation) && Poly.equal a.target m.source)
      || (a.conversation && Poly.equal a.target { m.source with id = m.thread }))

let label (a : M.assignment) =
  let value =
    match a.value with
    | "needs-reply" -> "May need a reply"
    | "waiting" -> "Waiting for someone else"
    | "deadline" -> "Has a deadline"
    | "fyi" -> "No reply / for information"
    | "direct" -> "Direct correspondence"
    | "mailing-list" -> "Mailing list"
    | value -> String.capitalize value
  in
  let dimension =
    match a.dimension with
    | Attention -> "Action"
    | Triage -> "Priority"
    | Delivery -> "Delivery"
    | Area -> "Topic"
  in
  dimension ^ ": " ^ value
  ^ (if Poly.equal a.origin M.Admin then " (confirmed)"
     else if String.is_prefix a.version ~prefix:"user-rule/" then
       " (saved rule)"
     else " (suggested)")
  ^ if a.conversation then " [conversation]" else ""

let badge t m dimension =
  let labels =
    assignments t m
    |> List.filter ~f:(fun a -> Poly.equal a.M.dimension dimension)
  in
  let label =
    match List.find labels ~f:(fun a -> not a.M.conversation) with
    | Some a -> Some a
    | None -> List.hd labels
  in
  match Option.map label ~f:(fun a -> a.M.value) with
  | Some "relevant" -> ("+ Relevant", green)
  | Some "social" -> ("@ Social", blue)
  | Some "spam" -> ("! Spam", red)
  | Some "needs-reply" -> ("Reply?", amber)
  | Some "waiting" -> ("Waiting", blue)
  | Some "deadline" -> ("Due", amber)
  | Some "fyi" -> ("FYI", green)
  | Some _ -> ("? Unsure", amber)
  | None -> ("-", blue)

let render ~phase ~profile_name { Dimensions.width; height } t =
  let h = max 1 (height - 4) in
  let left = max 1 (width * 42 / 100) in
  let right = max 1 (width - left) in
  let wrap = Termanil_ui.wrap ~width:(max 1 (right - 2)) in
  let messages = messages_in_order t in
  let position =
    Option.value_map t.ui.selected ~default:0 ~f:(fun source ->
        Option.value_map
          (List.findi messages ~f:(fun _ m -> Poly.equal m.M.source source))
          ~default:0 ~f:fst)
  in
  let rows = max 1 ((h - 3) / 2) in
  let message_window =
    if List.is_empty messages then
      let lines =
        match t.ui.error with
        | Some error -> [ "Could not load messages:"; error ]
        | None when Option.is_some t.ui.pending -> [ "Loading messages..." ]
        | None -> [ "No matching messages."; "Use / to search." ]
      in
      View.vcat (List.map lines ~f:(fun s -> text s))
    else
      let first = max 0 (position - rows + 1) in
      let entries =
        List.take (List.drop messages first) rows
        |> List.mapi ~f:(fun i (m : M.message) ->
               let chosen = Poly.equal t.ui.selected (Some m.source) in
               let selected_attrs =
                 if chosen then
                   [ Attr.bg (Attr.Color.rgb ~r:40 ~g:65 ~b:90); Attr.bold ]
                 else []
               in
               let continuation =
                 t.threaded
                 && Option.value_map
                      (if first + i = 0 then None
                       else List.nth messages (first + i - 1))
                      ~default:false
                      ~f:(fun previous ->
                        Poly.equal (thread_key previous) (thread_key m))
               in
               let count =
                 List.count messages ~f:(fun other ->
                     Poly.equal (thread_key other) (thread_key m))
               in
               let subject =
                 (if continuation then "  └ "
                  else if t.threaded then Printf.sprintf "[%d] " count
                  else "")
                 ^
                 if String.is_empty m.subject then "(No subject)" else m.subject
               in
               let row dimension content =
                 let label, colour = badge t m dimension in
                 View.hcat
                   [
                     fit ~width:2 ~height:1
                       (text ~attrs:selected_attrs
                          (if chosen then "> " else "  "));
                     fit ~width:10 ~height:1
                       (text ~attrs:(Attr.fg colour :: selected_attrs) label);
                     text ~attrs:[ Attr.fg blue ] " │ ";
                     fit ~width:(left - 17) ~height:1
                       (text ~attrs:selected_attrs content);
                   ]
               in
               View.vcat [ row M.Triage subject; row M.Attention m.sender ])
      in
      View.vcat
        (text
           ~attrs:[ Attr.fg blue; Attr.bold ]
           "  Labels     │ Subject / sender"
        :: entries)
  in
  let reading =
    match selected t with
    | None -> [ "Select a message with j/k or the arrow keys." ]
    | Some m ->
        let labels = assignments t m in
        wrap m.subject
        @ wrap ("From: " ^ m.sender)
        @ [ "Date: " ^ m.date; "" ]
        @ List.concat_map labels ~f:(fun a ->
              wrap (label a)
              @
              if t.reasons then wrap ("Why: " ^ a.M.explanation) @ [ "" ]
              else [])
        @ (if List.is_empty labels then
             [ "No classification saved for this message." ]
           else [])
        @ (if t.reasons then
             wrap
               "Reply suggestions describe this email, not whether you later \
                replied."
           else [])
        @ (if m.outside_interval then
             wrap "Older or newer thread context, outside your import dates."
           else [])
        @ (if m.truncated then [ "Body shortened for display." ] else [])
        @ [ ""; "--- Message ---" ] @ wrap m.original
  in
  let details, title =
    if Option.is_some t.correction then
      let dimension, value = Option.value_exn t.correction in
      ( [
          "Choose a label:";
          "1 No reply / information   2 Needs reply";
          "3 Waiting   4 Deadline   5 Unsure";
          "Delivery: 6 Automated   7 Direct";
          "8 Mailing list   9 Broadcast   0 Unsure";
          "";
          "Selected: " ^ M.dimension dimension ^ " = " ^ value;
          "";
          "Apply to: m this message | s this sender | l this list";
          "Then review the scope and enter an explanation.";
          "Esc cancels. Rules affect existing and future mail.";
        ],
        "Correct classification" )
    else if t.discussing then
      ( [ "Ask about this message:"; t.explanation; "Enter sends | Esc cancels" ]
        @ wrap t.ui.snapshot.discussion,
        "Discussion" )
    else
      match t.preview with
      | Some edit ->
          ( [
              preview_name edit;
              (match (edit, selected t) with
              | M.Assign_rule (scope, _), Some m ->
                  "All existing and future mail matching: " ^ M.rule_key scope m
              | _ -> "This decision is local to Siftbot.");
              (match edit with
              | M.Rename (_, "") -> "Type the new name, then Enter."
              | _ -> "Type your explanation, then Enter to save.");
              t.explanation;
              "Esc cancels";
            ],
            "Confirm decision" )
      | None when t.pane = 0 ->
          let areas =
            List.mapi t.ui.snapshot.areas ~f:(fun i (a : M.area) ->
                (if i = t.area then "> " else "  ")
                ^ a.name ^ " [" ^ a.status ^ "]")
          in
          ( ([
               "Topics organise mail by project or responsibility.";
               "j/k choose | a approve | n rename | x archive";
               "v assign to current message | g close";
               "";
             ]
            @
            if List.is_empty areas then
              [ "No topics defined. You can review mail without them." ]
            else List.drop areas (max 0 (t.area - h + 8))),
            "Topics (optional)" )
      | None ->
          ( List.drop
              (reading
              @
              if String.is_empty t.ui.snapshot.discussion then []
              else [ ""; "--- Discussion ---" ] @ wrap t.ui.snapshot.discussion
              )
              t.ui.scroll,
            if t.reasons then "Message and classifier reasoning"
            else "Message | e explains labels" )
  in
  let content =
    View.hcat
      [
        framed ~width:left ~height:h
          (Printf.sprintf "%s%s %d/%d"
             (if t.pane = 1 then "* " else "")
             (if t.threaded then "By thread" else "Messages")
             (if List.is_empty messages then 0 else position + 1)
             (List.length messages))
          message_window;
        pane ~width:right ~height:h
          ((if t.pane <> 1 then "* " else "") ^ title)
          (List.concat_map details ~f:wrap);
      ]
  in
  fit ~width ~height
    (View.vcat
       [
         text
           ~attrs:[ Attr.bold; Attr.fg blue ]
           ("Siftbot [" ^ profile_name ^ "] | local review, read-only mail");
         content;
         text
           (if t.editing_filter then "/" ^ t.filter
            else
              "j/k next/previous | PgUp/PgDn read | 1 relevant 2 social 3 spam \
               4 unsure");
         text
           "l labels | t threads | e explain | g topics | / search | c discuss \
            | u undo | r refresh | q quit";
         (match t.ui.error with
         | Some error -> text ~attrs:[ Attr.fg red ] error
         | None when Option.is_some t.ui.pending ->
             let size = max 1 (min 24 (width - 24)) in
             let bar =
               String.init size ~f:(fun i ->
                   if (i + phase) % size < min 4 size then '=' else ' ')
             in
             text
               ~attrs:[ Attr.bold; Attr.fg amber ]
               ("[" ^ bar ^ "] Loading | Ctrl-C exits")
         | None ->
             text
               (Printf.sprintf "Ready | %d shown (latest matches) | Revision %d"
                  (List.length messages) t.ui.snapshot.revision));
       ])

let key_action t event =
  let open Event in
  match event with
  | Paste `Start -> Some (Paste_state true)
  | Paste `End -> Some (Paste_state false)
  | Key_press { key = Escape; _ } when not t.pasting -> Some Cancel
  | Key_press { key = ASCII c; mods = [] }
    when Option.is_some t.correction && not t.pasting -> (
      match c with
      | '1' -> Some (Correction_value (M.Attention, "fyi"))
      | '2' -> Some (Correction_value (M.Attention, "needs-reply"))
      | '3' -> Some (Correction_value (M.Attention, "waiting"))
      | '4' -> Some (Correction_value (M.Attention, "deadline"))
      | '5' -> Some (Correction_value (M.Attention, "uncertain"))
      | '6' -> Some (Correction_value (M.Delivery, "automated"))
      | '7' -> Some (Correction_value (M.Delivery, "direct"))
      | '8' -> Some (Correction_value (M.Delivery, "mailing-list"))
      | '9' -> Some (Correction_value (M.Delivery, "broadcast"))
      | '0' -> Some (Correction_value (M.Delivery, "uncertain"))
      | 'm' -> Some (Correction_scope None)
      | 's' -> Some (Correction_scope (Some M.Sender))
      | 'l' -> Some (Correction_scope (Some M.Mailing_list))
      | _ -> None)
  | _ when Option.is_some t.correction -> None
  | Key_press { key = Enter; mods = [] } when not t.pasting -> Some Commit
  | Key_press { key = ASCII c; mods = [] }
    when t.editing_filter || t.discussing || Option.is_some t.preview ->
      Some (Type (String.of_char c))
  | Key_press { key = Uchar c; mods = [] }
    when t.editing_filter || t.discussing || Option.is_some t.preview ->
      let b = Stdlib.Buffer.create 4 in
      Stdlib.Buffer.add_utf_8_uchar b c;
      Some (Type (Stdlib.Buffer.contents b))
  | Key_press { key = Backspace; mods = [] }
    when t.editing_filter || t.discussing || Option.is_some t.preview ->
      Some Backspace
  | _
    when t.pasting || Option.is_some t.preview || t.discussing
         || t.editing_filter ->
      None
  | Key_press { key = Tab; mods = [] } -> Some Tab
  | Key_press { key = ASCII 'g'; mods = [] } -> Some Topics
  | Key_press { key = ASCII 'e'; mods = [] } -> Some Reasons
  | Key_press { key = ASCII 't'; mods = [] } -> Some Thread_order
  | Key_press { key = ASCII 'l'; mods = [] } -> Some Correct
  | Key_press { key = Arrow `Down; mods = [] }
  | Key_press { key = ASCII 'j'; mods = [] } ->
      Some (Move 1)
  | Key_press { key = Arrow `Up; mods = [] }
  | Key_press { key = ASCII 'k'; mods = [] } ->
      Some (Move (-1))
  | Key_press { key = Page `Down; mods = [] } -> Some (Scroll 10)
  | Key_press { key = Page `Up; mods = [] } -> Some (Scroll (-10))
  | Key_press { key = ASCII 'c'; mods = [] } -> Some Discuss
  | Key_press { key = ASCII '/'; mods = [] } -> Some Filter
  | Key_press { key = ASCII 'r'; mods = [] } -> Some Refresh
  | Key_press { key = ASCII 'u'; mods = [] } -> Some (Preview M.Undo)
  | Key_press { key = ASCII 'n'; mods = [] } ->
      Option.map (selected_area t) ~f:(fun a -> Preview (M.Rename (a.id, "")))
  | Key_press { key = ASCII 'x'; mods = [] } ->
      Option.map (selected_area t) ~f:(fun a -> Preview (M.Archive a.id))
  | Key_press { key = ASCII 'v'; mods = [] } ->
      Option.bind (selected t) ~f:(fun m ->
          Option.map (selected_area t) ~f:(fun area ->
              Preview
                (M.Assign
                   {
                     target = m.source;
                     conversation = false;
                     dimension = Area;
                     value = area.id;
                     origin = Admin;
                     version = "review/v1";
                     source_revision = m.revision;
                     explanation = "";
                     evidence = [ m.source ];
                     score = None;
                   })))
  | Key_press { key = ASCII 'a'; mods = [] } ->
      Option.map (selected_area t) ~f:(fun a -> Preview (M.Approve a.id))
  | Key_press { key = ASCII ('1' .. '4' as c); mods = [] } ->
      Option.map (selected t) ~f:(fun m ->
          Preview
            (M.Assign
               {
                 target = m.source;
                 conversation = false;
                 dimension = Triage;
                 value =
                   (match c with
                   | '1' -> "relevant"
                   | '2' -> "social"
                   | '3' -> "spam"
                   | _ -> "uncertain");
                 origin = Admin;
                 version = "review/v1";
                 source_revision = m.revision;
                 explanation = "";
                 evidence = [ m.source ];
                 score = None;
               }))
  | _ -> None

let app ?(profile_name = "personal") ?configure ~execute ~exit ~dimensions
    (local_ graph) =
  let apply_action context t action =
    let t, request = update t action in
    Option.iter request ~f:(fun (id, request) ->
        let effect =
          let open Effect.Let_syntax in
          let%bind response = execute request in
          Bonsai.Apply_action_context.inject context (Finished (id, response))
        in
        Bonsai.Apply_action_context.schedule_event context effect);
    t
  in
  let model, inject =
    Bonsai.state_machine ~default_model:initial ~apply_action graph
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr inject = inject in
       inject Refresh)
    graph;
  let pending =
    let%arr model = model in
    Option.is_some model.ui.pending
  in
  let phase =
    match%sub pending with
    | false -> Bonsai.return 0
    | true ->
        let now =
          Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_ms 100.) graph
        in
        let%arr now = now in
        Int.of_float
          (Float.mod_float
             (Time_ns.Span.to_ms (Time_ns.to_span_since_epoch now) /. 100.)
             24.)
  in
  let view =
    let%arr model = model and dimensions = dimensions and phase = phase in
    render ~phase ~profile_name dimensions model
  in
  let handler =
    let%arr model = model and inject = inject in
    fun event ->
      match event with
      | Event.Key_press { key = ASCII 'p'; mods = [] }
        when Option.is_some configure
             && Option.is_none model.ui.pending
             && (not model.pasting) && (not model.editing_filter)
             && (not model.discussing)
             && Option.is_none model.preview
             && Option.is_none model.correction ->
          (Option.value_exn configure) ()
      | _ -> (
          match key_action model event with
          | Some action -> inject action
          | None -> (
              match event with
              | Event.Key_press { key = ASCII 'q'; mods = [] }
                when Option.is_none model.ui.pending
                     && (not model.pasting)
                     && Option.is_none model.correction ->
                  exit ()
              | _ -> Effect.Ignore))
  in
  let can_batch =
    let%arr model = model in
    fun event ->
      match event with
      | Event.Key_press { key = ASCII _ | Uchar _ | Backspace; mods = [] }
        when model.editing_filter || model.discussing
             || Option.is_some model.preview ->
          true
      | _ -> false
  in
  let ready =
    let%arr model = model in
    fun event ->
      match event with
      | Event.Key_press
          { key = ASCII ('j' | 'k') | Arrow (`Up | `Down); mods = [] }
        when model.pane <> 0 && (not model.editing_filter)
             && (not model.discussing)
             && Option.is_none model.preview
             && not model.pasting ->
          Option.is_none model.ui.pending
      | _ -> true
  in
  let handler =
    Termanil_ui.Input_queue.component ~handler ~can_batch ~ready graph
  in
  let handler =
    let%arr handler = handler in
    function
    | Event.Key_press { key = ASCII ('c' | 'C'); mods = [ Ctrl ] } -> exit ()
    | event -> handler event
  in
  (~view, ~handler)
