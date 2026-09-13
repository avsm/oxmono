(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
open Bonsai_term
open Bonsai.Let_syntax
module Editor = Bonsai_term_text_editor

let component ?(initial_text = Bonsai.return "") ~width ~height ~on_change
    (local_ graph) =
  let editor =
    Editor.component
      ~text_attrs:(Bonsai.return Theme.normal)
      ~width ~max_height:height graph
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr set_text = editor.set_text and initial_text in
       set_text initial_text)
    graph;
  Bonsai.Edge.on_change' ~equal:String.equal editor.text
    ~callback:
      (let%arr on_change in
       fun previous text ->
         if Option.is_none previous then Effect.Ignore else on_change text)
    graph;
  let handler =
    let%arr send_actions = editor.send_actions in
    fun event ->
      let event =
        match event with
        | Event.Key_press { key = ASCII c; mods = [ Ctrl ] } ->
            Event.Key_press { key = ASCII (Char.uppercase c); mods = [ Ctrl ] }
        | event -> event
      in
      Editor.default_keybindings_handler send_actions event
  in
  let handler =
    Editor.Buffer_and_apply_paste_events_in_bulk.f
      ~send_actions:editor.send_actions ~handler graph
  in
  let%arr view = editor.view
  and handler
  and get_cursor_position = editor.get_cursor_position in
  (view, handler, get_cursor_position)
