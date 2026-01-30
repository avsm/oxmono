open! Core
open! Bonsai
open Bonsai_test
open Bonsai_term

module Capability = struct
  type t =
    | Ansi
    | Not_ansi

  let to_notty_cap = function
    | Ansi -> Notty.Cap.ansi
    | Not_ansi -> Notty.Cap.dumb
  ;;

  let cursor_next_line_regex = lazy (Re.compile (Re.str "(CursorNextLine)"))

  let prettify_ansi s =
    let string = Ansi_text.visualize s in
    Re.replace (force cursor_next_line_regex) string ~f:(fun _ -> "\n")
  ;;
end

module Box = struct
  let hline n = String.concat (List.init n ~f:(fun _ -> "─"))

  let string_pad_right_utf8_aware s ~len:target_length =
    let string_length =
      (* notty has a good estimate for how wide a display-char is *)
      Notty.I.string Notty.A.empty s |> Notty.I.width
    in
    if string_length >= target_length
    then s
    else s ^ String.init (target_length - string_length) ~f:(function _ -> ' ')
  ;;

  let surround { Dimensions.width; height = _ } string =
    (* TODO: Hmm, I probably shouldn't ignore the height. It's for a test, so just keep it
       in mind if tests get funky... *)
    String.concat_lines
      [ String.concat [ "┌"; hline width; "┐" ]
      ; String.concat
          ~sep:"\n"
          (String.split_lines string
           |> List.map ~f:(fun line ->
             String.concat [ "│"; string_pad_right_utf8_aware line ~len:width; "│" ]))
      ; String.concat [ "└"; hline width; "┘" ]
      ]
  ;;
end

module Result_spec = struct
  type ('a, 'incoming) t =
    { a : 'a
    ; to_view_with_handler : 'a -> View.With_handler.t
    ; set_dimensions : Dimensions.t -> unit Effect.t
    ; dimensions : Dimensions.t
    ; handle_incoming : 'a -> 'incoming -> unit Effect.t
    ; capability : Capability.t
    }

  type 'a incoming =
    | Set_dimensions of { dimensions : Dimensions.t }
    | Broadcast_event of { event : Event.t }
    | External_event of 'a

  let view
    { a
    ; to_view_with_handler
    ; set_dimensions = _
    ; dimensions = { width; height }
    ; handle_incoming = _
    ; capability
    }
    =
    let buffer = Buffer.create 100 in
    let ~view, .. = to_view_with_handler a in
    let notty_capability =
      match capability with
      | Not_ansi -> Notty.Cap.dumb
      | Ansi -> Notty.Cap.ansi
    in
    Notty.Render.to_buffer
      buffer
      notty_capability
      (0, 0)
      (width, height)
      (View.Private.notty_image view);
    let string = Buffer.contents buffer in
    match capability with
    | Not_ansi -> Box.surround { width; height } string
    | Ansi -> Capability.prettify_ansi string
  ;;

  let incoming
    { a
    ; to_view_with_handler
    ; set_dimensions
    ; dimensions = _
    ; handle_incoming
    ; capability = _
    }
    = function
    | Set_dimensions { dimensions } -> set_dimensions dimensions
    | Broadcast_event { event } ->
      let ~handler, .. = to_view_with_handler a in
      handler event
    | External_event e -> handle_incoming a e
  ;;
end

let create_handle_generic
  (type a incoming)
  ?(initial_dimensions : Dimensions.t = { height = 40; width = 80 })
  ?(capability = Capability.Not_ansi)
  ~(to_view_with_handler : a -> View.With_handler.t)
  ~(handle_incoming : a -> incoming -> unit Effect.t)
  (app : dimensions:Dimensions.t Bonsai.t -> local_ Bonsai.graph -> a Bonsai.t)
  =
  let module Outer_result_spec = Result_spec in
  let module Result_spec = struct
    type t = (a, incoming) Result_spec.t
    type nonrec incoming = incoming Result_spec.incoming

    let view = Result_spec.view
    let incoming = Result_spec.incoming
  end
  in
  Bonsai_test.Handle.create
    (module Result_spec)
    (let open Bonsai.Let_syntax in
     fun (local_ graph) ->
       let dimensions, set_dimensions = Bonsai.state initial_dimensions graph in
       let%sub a =
         Cursor.For_mock_tests.register
           (fun (local_ graph) ->
             Title.For_mock_tests.register
               (fun (local_ graph) -> app ~dimensions graph)
               graph)
           graph
       in
       let%arr a and set_dimensions and dimensions in
       { Outer_result_spec.a
       ; to_view_with_handler
       ; set_dimensions
       ; dimensions
       ; handle_incoming
       ; capability
       })
;;

let create_handle ?initial_dimensions ?capability app =
  let app ~dimensions (local_ graph) =
    let ~view, ~handler = app ~dimensions graph in
    let%arr.Bonsai view and handler in
    ~view, ~handler
  in
  create_handle_generic
    ?initial_dimensions
    ?capability
    app
    ~to_view_with_handler:Fn.id
    ~handle_incoming:(fun _ (nothing : Nothing.t) ->
      match nothing with
      | _ -> .)
;;

let create_handle_without_handler ?initial_dimensions ?capability app =
  create_handle_generic
    ?initial_dimensions
    ?capability
    app
    ~to_view_with_handler:(fun view -> ~view, ~handler:(fun _ -> Effect.Ignore))
    ~handle_incoming:(fun _ (nothing : Nothing.t) ->
      match nothing with
      | _ -> .)
;;

let set_dimensions handle dimensions =
  Handle.do_actions handle [ Result_spec.Set_dimensions { dimensions } ]
;;

let send_event handle event =
  Handle.do_actions handle [ Result_spec.Broadcast_event { event } ]
;;

let do_actions handle actions =
  Handle.do_actions handle (List.map actions ~f:(fun x -> Result_spec.External_event x))
;;

module For_minimal_mocking_test_suite = struct
  let cap_to_notty_cap = Capability.to_notty_cap
  let prettify_ansi = Capability.prettify_ansi
end
