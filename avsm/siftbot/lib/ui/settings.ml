open! Core
open Bonsai_term
open Bonsai.Let_syntax
module S = Siftbot_setup

type form = { profile : S.profile; token : string; field : int; fresh : bool }

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
  | Next
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
  { profile = empty name; token = ""; field = 0; fresh = true }

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

let drop s =
  let rec start i =
    if i <= 0 then 0
    else if Char.to_int s.[i] land 0xc0 = 0x80 then start (i - 1)
    else i
  in
  if String.is_empty s then s else String.prefix s (start (String.length s - 1))

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
            (if List.is_empty profiles then Some (new_form default_name)
             else None);
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
                { profile; token = ""; field = 1; fresh = false });
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
          {
            f with
            field =
              (if f.field = 6 then if f.fresh then 0 else 1 else f.field + 1);
          })
  | Clear -> edit (fun f -> set_field f "")
  | Backspace -> edit (fun f -> set_field f (drop (field f)))
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
          if String.length (field f) + String.length s > limit then f
          else set_field f (field f ^ s))
  | Save -> (
      match t.form with
      | None -> (t, None)
      | Some f ->
          ( { t with pending = true; error = None },
            Some (S.Save (f.profile, f.token, f.fresh)) ))

let text s =
  View.text
    (Siftbot_model.sanitize s |> String.map ~f:(function '\n' -> ' ' | c -> c))

let render { Dimensions.width; height } t =
  let lines =
    match t.form with
    | None ->
        [
          "PROFILES | j/k select | Enter open | e edit | n new | r reload | q \
           quit";
        ]
        @
        if List.is_empty t.profiles then
          [ "No profiles yet. Press n to create one." ]
        else
          List.mapi t.profiles ~f:(fun i p ->
              (if i = t.selected then "> " else "  ") ^ p.S.name ^ "  " ^ p.url)
          |> fun rows ->
          List.take
            (List.drop rows (max 0 (t.selected - max 0 (height - 6))))
            (max 1 (height - 4))
    | Some f ->
        let labels =
          [
            "Siftbot profile";
            "Shared JMAP profile";
            "HTTPS session URL";
            "Bearer access token";
            "Account ID (blank = primary)";
            "Chat model (optional)";
            "[ Save profile ]";
          ]
        in
        [
          "PROFILE SETUP | Tab/Enter next | Ctrl-U clear | Esc cancel";
          "Shared JMAP login changes also affect other apps using that profile.";
        ]
        @ List.mapi labels ~f:(fun i label ->
              (if i = f.field then "> " else "  ")
              ^ label
              ^
              if i = 6 then ""
              else
                ": "
                ^
                if i = 3 then
                  if String.is_empty f.token then
                    if f.profile.has_token then "[stored; blank keeps existing]"
                    else "[enter token or reuse unchanged shared login]"
                  else "********"
                else field { f with field = i })
        @ [
            "Enter on Save writes settings. No mail is fetched.";
            "A changed URL requires a new token. Tokens are never displayed.";
          ]
  in
  let v =
    View.vcat
      (List.map
         ([ "Siftbot | connection profiles" ]
         @ lines
         @ [
             (if t.pending then "Working..."
              else Option.value t.error ~default:"");
           ])
         ~f:text)
  in
  View.crop
    ~r:(max 0 (View.width v - width))
    ~b:(max 0 (View.height v - height))
    v

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
      | Key_press { key = Backspace; mods = [] } -> action Backspace
      | Key_press { key = ASCII 'u'; mods = [ Ctrl ] } -> action Clear
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
  let can_batch = Bonsai.return (fun _ -> false) in
  let ready = Bonsai.return (fun _ -> true) in
  let handler =
    Termanil_ui.Input_queue.component ~handler ~can_batch ~ready graph
  in
  (~view, ~handler)
