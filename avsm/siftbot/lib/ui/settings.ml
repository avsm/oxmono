open! Core
open Bonsai_term
open Bonsai.Let_syntax
module S = Siftbot_setup

type form = {
  profile : S.profile;
  token : string;
  field : int;
  cursor : int;
  fresh : bool;
}

type state = {
  profiles : S.profile list;
  selected : int;
  form : form option;
  pending : bool;
  error : string option;
  pasting : bool;
}

type action =
  | Load
  | Finished of S.response
  | Move of int
  | New
  | Edit
  | Type of string
  | Backspace
  | Delete
  | Cursor of [ `Left | `Right | `Home | `End ]
  | Next
  | Previous
  | Clear
  | Save
  | Cancel
  | Paste of bool

let empty name : S.profile =
  {
    name;
    jmap_profile = name;
    url = "";
    account = "";
    chat_model = "";
    has_token = false;
  }

let new_form name =
  {
    profile = empty name;
    token = "";
    field = 0;
    cursor = String.length name;
    fresh = true;
  }

let initial =
  {
    profiles = [];
    selected = 0;
    form = None;
    pending = false;
    error = None;
    pasting = false;
  }

let chosen t = List.nth t.profiles t.selected

let field f =
  match f.field with
  | 0 -> f.profile.name
  | 1 -> f.profile.jmap_profile
  | 2 -> f.profile.url
  | 3 -> f.token
  | 4 -> f.profile.account
  | 5 -> f.profile.chat_model
  | _ -> ""

let set_field f value =
  let p = f.profile in
  match f.field with
  | 0 when f.fresh ->
      {
        f with
        profile =
          {
            p with
            name = value;
            jmap_profile =
              (if String.equal p.name p.jmap_profile then value
               else p.jmap_profile);
          };
      }
  | 1 -> { f with profile = { p with jmap_profile = value } }
  | 2 -> { f with profile = { p with url = value } }
  | 3 -> { f with token = value }
  | 4 -> { f with profile = { p with account = value } }
  | 5 -> { f with profile = { p with chat_model = value } }
  | _ -> f

let previous s cursor =
  let rec loop i =
    if i <= 0 then 0
    else if Char.to_int s.[i] land 0xc0 = 0x80 then loop (i - 1)
    else i
  in
  if cursor = 0 then 0 else loop (cursor - 1)

let following s cursor =
  let rec loop i =
    if i >= String.length s then String.length s
    else if Char.to_int s.[i] land 0xc0 = 0x80 then loop (i + 1)
    else i
  in
  if cursor >= String.length s then String.length s else loop (cursor + 1)

let focus f i =
  let f = { f with field = i } in
  { f with cursor = String.length (field f) }

let update ~default_name t action =
  let edit f = ({ t with form = Option.map t.form ~f }, None) in
  match action with
  | Paste pasting -> ({ t with pasting }, None)
  | Finished (S.Error message) ->
      ({ t with pending = false; error = Some message }, None)
  | Finished (Profiles profiles) ->
      ( {
          t with
          profiles;
          pending = false;
          error = None;
          selected =
            Option.value_map
              (List.findi profiles ~f:(fun _ p ->
                   String.equal p.S.name default_name))
              ~default:0 ~f:fst;
          form =
            (if
               List.exists profiles ~f:(fun p ->
                   String.equal p.S.name default_name)
             then None
             else Some (new_form default_name));
        },
        None )
  | Finished (Saved profile) ->
      let profiles =
        profile
        :: List.filter t.profiles ~f:(fun p ->
               not (String.equal p.S.name profile.name))
      in
      ( {
          t with
          profiles;
          selected = 0;
          pending = false;
          form = None;
          error = Some "Saved. Enter opens this profile.";
        },
        None )
  | _ when t.pending -> (t, None)
  | Load -> ({ t with pending = true; error = None }, Some S.List)
  | New -> ({ t with form = Some (new_form ""); error = None }, None)
  | Edit ->
      ( {
          t with
          form =
            Option.map (chosen t) ~f:(fun profile ->
                {
                  profile;
                  token = "";
                  field = 1;
                  cursor = String.length profile.jmap_profile;
                  fresh = false;
                });
          error = None;
        },
        None )
  | Cancel -> ({ t with form = None; error = None }, None)
  | Move delta ->
      ( {
          t with
          selected =
            Int.clamp_exn (t.selected + delta) ~min:0
              ~max:(max 0 (List.length t.profiles - 1));
        },
        None )
  | Next ->
      edit (fun f ->
          focus f
            (if f.field = 6 then if f.fresh then 0 else 1 else f.field + 1))
  | Previous ->
      edit (fun f ->
          let first = if f.fresh then 0 else 1 in
          focus f (if f.field <= first then 6 else f.field - 1))
  | Clear -> edit (fun f -> { (set_field f "") with cursor = 0 })
  | Cursor direction ->
      edit (fun f ->
          let s = field f in
          let cursor =
            match direction with
            | `Left -> previous s f.cursor
            | `Right -> following s f.cursor
            | `Home -> 0
            | `End -> String.length s
          in
          { f with cursor })
  | Backspace ->
      edit (fun f ->
          let s = field f in
          let cursor = previous s f.cursor in
          let value = String.prefix s cursor ^ String.drop_prefix s f.cursor in
          { (set_field f value) with cursor })
  | Delete ->
      edit (fun f ->
          let s = field f in
          set_field f
            (String.prefix s f.cursor
            ^ String.drop_prefix s (following s f.cursor)))
  | Type s ->
      edit (fun f ->
          let limit =
            match f.field with
            | 0 | 1 -> 128
            | 2 -> 4096
            | 3 -> 8192
            | 4 -> 1024
            | _ -> 512
          in
          if f.field >= 6 || String.length (field f) + String.length s > limit
          then f
          else
            let value = field f in
            let value =
              String.prefix value f.cursor
              ^ s
              ^ String.drop_prefix value f.cursor
            in
            { (set_field f value) with cursor = f.cursor + String.length s })
  | Save -> (
      match t.form with
      | None -> (t, None)
      | Some f ->
          ( { t with pending = true; error = None },
            Some (S.Save (f.profile, f.token, f.fresh)) ))

let text ?(attrs = []) s =
  View.text ~attrs
    (Siftbot_model.sanitize s |> String.map ~f:(function '\n' -> ' ' | c -> c))

let fit width v = View.crop ~r:(max 0 (View.width v - max 0 width)) v

let render { Dimensions.width; height } t =
  let title = text ~attrs:[ Attr.bold ] "Siftbot | connection profiles" in
  let available = max 1 (height - 3) in
  let rows, selected, help =
    match t.form with
    | None ->
        let rows =
          if List.is_empty t.profiles then
            [ text "No profiles. Press n to create one." ]
          else
            List.mapi t.profiles ~f:(fun i p ->
                text
                  ~attrs:(if i = t.selected then [ Attr.invert ] else [])
                  ((if i = t.selected then "> " else "  ")
                  ^ p.S.name ^ "  " ^ p.url))
        in
        ( rows,
          t.selected,
          "j/k select | Enter open | e edit | n new | r reload | q quit | \
           Ctrl-C quit" )
    | Some f ->
        let labels =
          if width < 60 then
            [
              "Profile";
              "JMAP profile";
              "Session URL";
              "Token";
              "Account";
              "Chat model";
              "[ Save profile ]";
            ]
          else
            [
              "Siftbot profile";
              "Shared JMAP profile";
              "HTTPS session URL";
              "Bearer access token";
              "Account ID (optional)";
              "Chat model (optional)";
              "[ Save profile ]";
            ]
        in
        let rows =
          List.mapi labels ~f:(fun i label ->
              let active = i = f.field in
              let prefix =
                text
                  ~attrs:(if active then [ Attr.bold ] else [])
                  ((if active then "> " else "  ")
                  ^ label
                  ^ if i = 6 then "" else ": ")
              in
              if i = 6 then prefix
              else
                let value = field { f with field = i } in
                let value, cursor =
                  if i = 3 then
                    let masked =
                      if not (String.is_empty f.token) then "********"
                      else if f.profile.has_token then
                        "[stored; blank keeps token]"
                      else ""
                    in
                    (masked, String.length masked)
                  else (value, if active then f.cursor else String.length value)
                in
                let before = text (String.prefix value cursor) in
                let after = text (String.drop_prefix value cursor) in
                let content =
                  if active then
                    View.hcat [ before; text ~attrs:[ Attr.invert ] " "; after ]
                  else text value
                in
                let room = max 1 (width - View.width prefix) in
                let left =
                  if active then max 0 (View.width before + 1 - room) else 0
                in
                View.hcat [ prefix; fit room (View.crop ~l:left content) ])
        in
        let hint =
          match f.field with
          | 0 -> "Use a fresh profile for a different import date range."
          | 1 -> "Shared JMAP login changes affect other apps using that login."
          | 2 ->
              "Enter the HTTPS JMAP session URL. Changing it needs a new token."
          | 3 ->
              "Use a read-only mail token. Blank retains an unchanged shared \
               login."
          | 4 -> "Leave blank to use the primary mail account."
          | 5 ->
              "Leave blank for local import and search without model requests."
          | _ -> "Enter saves settings locally. No mail is fetched."
        in
        ( rows @ [ text hint ],
          f.field,
          "Tab/Shift-Tab field | arrows edit | Ctrl-U clear | Esc cancel | \
           Ctrl-C quit" )
  in
  let offset = max 0 (selected - available + 1) in
  let rows = List.take (List.drop rows offset) available in
  let status =
    if t.pending then "Working... Esc or Ctrl-C quits."
    else Option.value t.error ~default:"Settings only. No mail is fetched."
  in
  let body = View.vcat rows in
  let body = View.pad ~b:(max 0 (available - View.height body)) body in
  let help = if width < 65 then "^C quit | Tab next | Esc back" else help in
  let v = View.vcat [ title; body; text status; text help ] in
  View.crop ~b:(max 0 (View.height v - max 0 height)) (fit width v)

let app ~default_name ~execute ~select ~exit ~dimensions (local_ graph) =
  let apply_action context t action =
    let t, request = update ~default_name t action in
    Option.iter request ~f:(fun request ->
        Bonsai.Apply_action_context.schedule_event context
          (let open Effect.Let_syntax in
           let%bind response = execute request in
           Bonsai.Apply_action_context.inject context (Finished response)));
    t
  in
  let model, inject =
    Bonsai.state_machine ~default_model:initial ~apply_action graph
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr inject = inject in
       inject Load)
    graph;
  let view =
    let%arr model = model and dimensions = dimensions in
    render dimensions model
  in
  let handler =
    let%arr t = model and inject = inject in
    fun event ->
      let action a = inject a in
      match event with
      | Event.Paste `Start -> action (Paste true)
      | Paste `End -> action (Paste false)
      | Key_press { key = Escape; _ } when t.pending && not t.pasting -> exit ()
      | _ when t.pending -> Effect.Ignore
      | Key_press { key = ASCII c; mods = [] } when Option.is_some t.form ->
          action (Type (String.of_char c))
      | Key_press { key = Uchar c; mods = [] } when Option.is_some t.form ->
          let b = Stdlib.Buffer.create 4 in
          Stdlib.Buffer.add_utf_8_uchar b c;
          action (Type (Stdlib.Buffer.contents b))
      | _ when t.pasting -> Effect.Ignore
      | Key_press { key = Escape; _ } ->
          if Option.is_some t.form then action Cancel else exit ()
      | Key_press { key = Tab; mods = [] } when Option.is_some t.form ->
          action Next
      | Key_press { key = Tab; mods = [ Shift ] }
      | Key_press { key = Arrow `Up; mods = [] }
        when Option.is_some t.form ->
          action Previous
      | Key_press { key = Arrow `Down; mods = [] } when Option.is_some t.form ->
          action Next
      | Key_press { key = Backspace; mods = [] } -> action Backspace
      | Key_press { key = Delete; mods = [] } -> action Delete
      | Key_press { key = Arrow `Left; mods = [] } -> action (Cursor `Left)
      | Key_press { key = Arrow `Right; mods = [] } -> action (Cursor `Right)
      | Key_press { key = Home; mods = [] } -> action (Cursor `Home)
      | Key_press { key = End; mods = [] } -> action (Cursor `End)
      | Key_press { key = ASCII ('u' | 'U'); mods = [ Ctrl ] } -> action Clear
      | Key_press { key = Enter; mods = [] } -> (
          match t.form with
          | Some f -> action (if f.field = 6 then Save else Next)
          | None ->
              Option.value_map (chosen t) ~default:Effect.Ignore ~f:(fun p ->
                  select p.name))
      | Key_press { key = ASCII 'q'; mods = [] } -> exit ()
      | Key_press { key = ASCII 'n'; mods = [] } -> action New
      | Key_press { key = ASCII 'e'; mods = [] } -> action Edit
      | Key_press { key = ASCII 'r'; mods = [] } -> action Load
      | Key_press { key = ASCII 'j' | Arrow `Down; mods = [] } ->
          action (Move 1)
      | Key_press { key = ASCII 'k' | Arrow `Up; mods = [] } ->
          action (Move (-1))
      | _ -> Effect.Ignore
  in
  let can_batch =
    let%arr model = model in
    function
    | Event.Key_press { key = ASCII _ | Uchar _ | Backspace; mods = [] }
      when Option.is_some model.form ->
        true
    | _ -> false
  in
  let ready = Bonsai.return (fun _ -> true) in
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
