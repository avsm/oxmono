open! Core
open Bonsai_term
open Bonsai.Let_syntax
module M = Siftbot_model
module Settings = Settings

type state = {
  ui : M.ui;
  area : int;
  pane : int;
  preview : M.edit option;
  explanation : string;
  filter : string;
  editing_filter : bool;
  discussing : bool;
  pasting : bool;
}

let initial =
  {
    ui = M.initial;
    area = 0;
    pane = 0;
    preview = None;
    explanation = "";
    filter = "";
    editing_filter = false;
    discussing = false;
    pasting = false;
  }

type action =
  | Refresh
  | Finished of int * M.response
  | Move of int
  | Tab
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

let update t action =
  let request t r =
    match M.begin_request t.ui with
    | None -> (t, None)
    | Some (ui, id) -> ({ t with ui }, Some (id, r))
  in
  match action with
  | Refresh -> request t (M.Inspect t.filter)
  | Finished (id, r) ->
      let selected = Option.map (selected_area t) ~f:(fun a -> a.M.id) in
      let ui = M.receive t.ui id r in
      let area =
        Option.value_map selected ~default:t.area ~f:(fun id ->
            Option.value_map
              (List.findi ui.snapshot.areas ~f:(fun _ a ->
                   String.equal a.M.id id))
              ~default:0 ~f:fst)
      in
      ({ t with ui; area }, None)
  | Paste_state pasting -> ({ t with pasting }, None)
  | Tab -> ({ t with pane = (t.pane + 1) mod 3 }, None)
  | Move delta when t.pane = 0 ->
      ( {
          t with
          area =
            Int.clamp_exn (t.area + delta) ~min:0
              ~max:(max 0 (List.length t.ui.snapshot.areas - 1));
        },
        None )
  | Move delta ->
      let ms = t.ui.snapshot.messages in
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
      ( {
          t with
          ui =
            {
              t.ui with
              selected = Option.map m ~f:(fun m -> m.M.source);
              scroll = 0;
            };
        },
        None )
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

let text s =
  View.text (M.sanitize s |> String.map ~f:(function '\n' -> ' ' | c -> c))

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

let pane ~width ~height title lines =
  fit ~width ~height (View.vcat (text title :: List.map lines ~f:text))

let preview_name = function
  | M.Approve id -> "Approve area " ^ id
  | Rename (id, name) -> "Rename " ^ id ^ " to " ^ name
  | Archive id -> "Archive " ^ id
  | Merge (a, b) -> "Merge " ^ a ^ " into " ^ b
  | Define (id, _, _) -> "Edit definition " ^ id
  | Assign a -> M.dimension a.dimension ^ " = " ^ a.value
  | Undo -> "Undo latest decision"

let render ~profile_name { Dimensions.width; height } t =
  let w = max 1 (width / 3) and h = max 1 (height - 4) in
  let areas =
    List.mapi t.ui.snapshot.areas ~f:(fun i (a : M.area) ->
        (if i = t.area then "> " else "  ") ^ a.name ^ " [" ^ a.status ^ "]")
  in
  let messages =
    List.map t.ui.snapshot.messages ~f:(fun (m : M.message) ->
        (if Poly.equal t.ui.selected (Some m.source) then "> " else "  ")
        ^ m.subject)
  in
  let evidence =
    match selected t with
    | None -> [ "No message selected" ]
    | Some m ->
        let assignments =
          List.filter t.ui.snapshot.assignments ~f:(fun (a : M.assignment) ->
              Poly.equal a.target m.source
              || a.conversation
                 && Poly.equal a.target { m.source with id = m.thread })
        in
        [ m.subject; m.date; m.sender; M.key m.source ]
        @ (if m.truncated then [ "[Body truncated]" ] else [])
        @ (if m.outside_interval then [ "[Context outside import interval]" ]
           else [])
        @ List.map assignments ~f:(fun (a : M.assignment) ->
              M.dimension a.dimension ^ ": " ^ a.value
              ^ (if a.conversation then " [conversation default]"
                 else " [message]")
              ^
              if Poly.equal a.origin M.Admin then " [confirmed]"
              else " [suggested]")
        @ [ "" ]
        @ Termanil_ui.wrap ~width:w m.original
  in
  let review =
    if t.discussing then
      [
        "Ask about the selected evidence:";
        t.explanation;
        "Enter sends; Esc cancels.";
      ]
    else
      match t.preview with
      | Some edit ->
          [
            "REVIEW: " ^ preview_name edit;
            (match edit with
            | M.Rename (_, "") -> "Type the new name, then Enter."
            | _ -> "Type an explanation, Enter commits.");
            "Esc cancels.";
            t.explanation;
          ]
      | None ->
          [
            "Area: a approve, n rename, x archive.";
            "Select a message: 1 relevant, 2 social,";
            "3 spam, 4 uncertain.";
            "v assigns selected area; u undoes.";
            "Each edit needs an explanation.";
            "";
            "c opens an Agentkit discussion.";
          ]
          @ Termanil_ui.wrap ~width:w t.ui.snapshot.discussion
  in
  let area_window =
    List.take (List.drop areas (max 0 (t.area - h + 4))) (max 1 (h - 3))
  in
  let message_position =
    Option.value_map t.ui.selected ~default:0 ~f:(fun source ->
        Option.value_map
          (List.findi t.ui.snapshot.messages ~f:(fun _ m ->
               Poly.equal m.M.source source))
          ~default:0 ~f:fst)
  in
  let message_window =
    List.take (List.drop messages (max 0 (message_position - 1))) 3
  in
  let title i s = (if t.pane = i then "* " else "  ") ^ s in
  let content =
    View.hcat
      [
        pane ~width:w ~height:h
          (title 0 "Areas and queues")
          (area_window
          @ [ Printf.sprintf "%d matching messages" (List.length messages) ]);
        pane ~width:w ~height:h
          (title 1 "Conversations and evidence")
          (message_window @ [ "--- Source ---" ]
          @ List.drop evidence t.ui.scroll);
        pane ~width:(width - (2 * w)) ~height:h (title 2 "Review") review;
      ]
  in
  fit ~width ~height
    (View.vcat
       [
         text
           ("Siftbot [" ^ profile_name ^ "] | local decisions, read-only mail");
         content;
         text
           (if t.editing_filter then "/" ^ t.filter
            else
              "Tab pane | j/k move | PgUp/PgDn scroll | / search | r refresh | \
               p profiles | q quit");
         text
           (Option.value t.ui.error
              ~default:
                (if Option.is_some t.ui.pending then "Working..."
                 else Printf.sprintf "Revision %d" t.ui.snapshot.revision));
       ])

let key_action t event =
  let open Event in
  match event with
  | Paste `Start -> Some (Paste_state true)
  | Paste `End -> Some (Paste_state false)
  | Key_press { key = Escape; _ } when not t.pasting -> Some Cancel
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
  | _ when t.pasting -> None
  | Key_press { key = Tab; mods = [] } -> Some Tab
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
  let view =
    let%arr model = model and dimensions = dimensions in
    render ~profile_name dimensions model
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
             && Option.is_none model.preview ->
          (Option.value_exn configure) ()
      | _ -> (
          match key_action model event with
          | Some action -> inject action
          | None -> (
              match event with
              | Event.Key_press { key = ASCII 'q'; mods = [] }
                when Option.is_none model.ui.pending && not model.pasting ->
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
  let ready = Bonsai.return (fun _ -> true) in
  let handler =
    Termanil_ui.Input_queue.component ~handler ~can_batch ~ready graph
  in
  (~view, ~handler)
