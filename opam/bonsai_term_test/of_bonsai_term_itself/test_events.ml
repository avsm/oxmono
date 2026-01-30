open! Core
open Bonsai_test
open Bonsai_term
open Bonsai.Let_syntax

let green x =
  View.text
    ~attrs:[ Attr.fg (Bonsai_tui_catppuccin.color ~flavor:Mocha Green); Attr.bold ]
    x
;;

let render_mod mods f name = if List.exists ~f mods then name ^ "+" else ""

let render_event : Event.t -> View.t =
  fun event ->
  match event with
  | Key_press { key; mods } ->
    (* TODO: It also seems like the underlying notty library is not detecting _all_ of the
       keybindings I would expect it to. (e.g. it does not detect Ctrl+Shift+Z as an
       example... ). It could also be my custom setup being weird or even my keyboard
       doing weird things... *)
    let meta =
      render_mod
        mods
        (function
          | Meta -> true
          | _ -> false)
        "Meta"
    and ctrl =
      render_mod
        mods
        (function
          | Ctrl -> true
          | _ -> false)
        "Ctrl"
    and shift =
      render_mod
        mods
        (function
          | Shift -> true
          | _ -> false)
        "Shift"
    in
    let prefix = String.concat [ meta; ctrl; shift ] in
    let key =
      match key with
      | ASCII ' ' -> "SPACE"
      | ASCII c -> Char.to_string c
      | Uchar uchar -> Uchar.Utf8.to_string uchar
      | ( Escape
        | Enter
        | Tab
        | Backspace
        | Insert
        | Delete
        | Home
        | End
        | Arrow _
        | Page _
        | Function _ ) as event -> Sexp.to_string_mach [%sexp (event : Event.Key.t)]
    in
    View.hcat
      [ green "Key "
      ; View.text
          ~attrs:[ Attr.bold; Attr.fg (Bonsai_tui_catppuccin.color ~flavor:Mocha Blue) ]
          (prefix ^ key)
      ]
  | Mouse { kind; position; mods } ->
    View.hcat
      [ green "Mouse "
      ; View.text
          ~attrs:[ Attr.bold; Attr.fg (Bonsai_tui_catppuccin.color ~flavor:Mocha Blue) ]
          (Sexp.to_string
             [%message
               (kind : Event.mouse_kind)
                 (position : Position.t)
                 (mods : Event.Modifier.t list)])
      ]
  | Paste `Start -> View.hcat [ green "Paste (start)" ]
  | Paste `End -> View.hcat [ green "Paste (end)" ]
;;

let events_app ~dimensions:_ (local_ graph) =
  let events, add_event =
    Bonsai.state_machine
      ~default_model:[]
      ~apply_action:(fun _ events event -> event :: List.take events 30)
      graph
  in
  let view =
    let%arr events in
    View.vcat (View.text "Press some keys!" :: List.map events ~f:render_event)
  in
  ~view, ~handler:add_event
;;

let%expect_test "Sending some keys" =
  let handle = Bonsai_term_test.create_handle events_app in
  Bonsai_term_test.set_dimensions handle { width = 78; height = 10 };
  Handle.show handle;
  Handle.recompute_view handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'c'; mods = [] });
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │Key c                                                                         │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'a'; mods = [] });
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'p'; mods = [] });
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'y'; mods = [] });
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │Key y                                                                         │
    │Key p                                                                         │
    │Key a                                                                         │
    │Key c                                                                         │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Mouse events" =
  let handle = Bonsai_term_test.create_handle events_app in
  Bonsai_term_test.set_dimensions handle { width = 78; height = 10 };
  Handle.show handle;
  Handle.recompute_view handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event
    handle
    (Mouse { kind = Left; position = { x = 0; y = 0 }; mods = [] });
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │Mouse ((kind Left)(position((x 0)(y 0)))(mods()))                             │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event
    handle
    (Mouse { kind = Release; position = { x = 2; y = 5 }; mods = [] });
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │Mouse ((kind Release)(position((x 2)(y 5)))(mods()))                          │
    │Mouse ((kind Left)(position((x 0)(y 0)))(mods()))                             │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Paste events" =
  let handle = Bonsai_term_test.create_handle events_app in
  Bonsai_term_test.set_dimensions handle { width = 78; height = 10 };
  Handle.show handle;
  Handle.recompute_view handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (Paste `Start);
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │Paste (start)                                                                 │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'y'; mods = [] });
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'p'; mods = [] });
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'a'; mods = [] });
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'c'; mods = [] });
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │Key c                                                                         │
    │Key a                                                                         │
    │Key p                                                                         │
    │Key y                                                                         │
    │Paste (start)                                                                 │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (Paste `End);
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │Press some keys!                                                              │
    │Paste (end)                                                                   │
    │Key c                                                                         │
    │Key a                                                                         │
    │Key p                                                                         │
    │Key y                                                                         │
    │Paste (start)                                                                 │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
