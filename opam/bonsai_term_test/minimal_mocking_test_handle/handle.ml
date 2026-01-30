open! Core
open Async
open Bonsai_term
open Bonsai_term.Private
open For_testing

module Frame_status = struct
  type t =
    | Frame_started
    | Frame_finished
end

type 'exit t =
  { driver : 'exit Driver.t
  ; stdin_writer : string Pipe.Writer.t
  ; stdout_reader : string Pipe.Reader.t
  ; time_source : Async.Time_source.t
  ; mutable frame_status : Frame_status.t
  ; current_dimensions : Dimensions.t ref
  ; window_changed_bvar : (unit, read_write) Bvar.t
  ; on_release : unit -> unit Deferred.t
  }

let release { driver; on_release; _ } =
  let%bind () = Driver.release driver in
  on_release ()
;;

let with_handle_with_exit
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?(time_source : Async.Time_source.t = Time_source.create ~now:Time_ns.epoch ())
  ?optimize
  ?target_frames_per_second
  ?(cap = Bonsai_term_test.Capability.Ansi)
  ?(file_descriptors = File_descriptors.create_default_for_testing)
  ~(dimensions : Dimensions.t)
  app
  f
  =
  let%bind { stdin_reader; stdin_writer; stdout_reader; stdout_writer } =
    file_descriptors ()
  in
  let on_release () =
    let%bind () = Writer.flushed stdout_writer in
    Writer.close stdout_writer
  in
  let window_changed_bvar = Bvar.create () in
  let current_dimensions = ref dimensions in
  Deferred.Or_error.ok_exn
    (let%with driver =
       let for_mocking =
         Notty_async.For_mocking.create
           ~capabilities:(fun _ ->
             Bonsai_term_test.For_minimal_mocking_test_suite.cap_to_notty_cap cap)
           ~dimensions:(fun _ ->
             let%tydi { width; height } = !current_dimensions in
             Some (width, height))
           ~wait_for_next_window_change:(fun _ -> Bvar.wait window_changed_bvar)
           ~is_a_tty:(fun _ -> Deferred.return true)
       in
       with_driver
         ~dispose
         ~nosig
         ~mouse
         ~bpaste
         ~reader:(Some stdin_reader)
         ~writer:(Some stdout_writer)
         ~time_source:(Some time_source)
         ~for_mocking:(Some for_mocking)
         ~optimize
         ~target_frames_per_second
         app
     in
     Driver.compute_first_frame driver;
     f
       { driver
       ; stdin_writer
       ; stdout_reader
       ; time_source
       ; frame_status = Frame_finished
       ; current_dimensions
       ; window_changed_bvar
       ; on_release
       })
;;

let with_handle
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?time_source
  ?optimize
  ?target_frames_per_second
  ?cap
  ?file_descriptors
  ~dimensions
  app
  f
  =
  with_handle_with_exit
    ?dispose
    ?nosig
    ?mouse
    ?bpaste
    ?time_source
    ?optimize
    ?target_frames_per_second
    ?cap
    ?file_descriptors
    ~dimensions
    (Bonsai_term.Private.For_testing.make_app_exit_on_ctrlc app)
    f
;;

let with_previous_image ~(here : [%call_pos]) t f =
  match Driver.prev_view t.driver with
  | None ->
    print_s [%message "Error! previous image is None" (here : Source_code_position.t)]
  | Some view -> f view
;;

let cursor_next_line_regex = lazy (Re.compile (Re.str "(CursorNextLine)"))

let show_view t ~cap view =
  let buffer = Buffer.create 10 in
  let%tydi { width; height } = Driver.dimensions t.driver in
  let cap = Bonsai_term_test.For_minimal_mocking_test_suite.cap_to_notty_cap cap in
  Notty.Render.to_buffer buffer cap (0, 0) (width, height) view;
  Buffer.contents buffer
  |> Ansi_text.visualize
  |> Re.replace (force cursor_next_line_regex) ~f:(fun _ -> "\n")
;;

let show ~(here : [%call_pos]) ?(cap = Bonsai_term_test.Capability.Ansi) t =
  let%with view = with_previous_image ~here t in
  print_endline (show_view t ~cap (View.Private.notty_image view))
;;

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

let show_mocked : here:[%call_pos] -> 'exit t -> unit =
  fun ~(here : [%call_pos]) t ->
  let%with view = with_previous_image ~here t in
  print_endline
    (Box.surround
       (Driver.dimensions t.driver)
       (show_view t ~cap:Not_ansi (View.Private.notty_image view)))
;;

let start_frame ~(here : [%call_pos]) t =
  match t.frame_status with
  | Frame_started ->
    raise_s
      [%message
        "[start_frame] was called before the previous frame finished. Please \
         Deferred.bind on the [`Frame_finished] event before calling [start_frame] \
         again."
          (here : Source_code_position.t)]
  | Frame_finished ->
    let frame_painted_var = Ivar.create () in
    let frame_finished_var = Ivar.create () in
    let out =
      let%map () = Ivar.read frame_painted_var in
      `Frame_painted, Ivar.read frame_finished_var
    in
    t.frame_status <- Frame_started;
    don't_wait_for
      (let%bind (`Frame_painted frame_finished) = Driver.compute_frame t.driver in
       Ivar.fill_exn frame_painted_var ();
       let%bind (`Frame_finished continue_or_shutdown) = frame_finished in
       t.frame_status <- Frame_finished;
       Ivar.fill_exn frame_finished_var (`Frame_finished continue_or_shutdown);
       return ());
    out
;;

let do_frame t =
  let%bind (`Frame_painted frame_finished) = Driver.compute_frame t.driver in
  let%bind (`Frame_finished continue_or_shutdown) = frame_finished in
  return continue_or_shutdown
;;

let do_frame_and_continue ~(here : [%call_pos]) t =
  match%map do_frame t with
  | Continue -> ()
  | Stdin_closed ->
    Expect_test_helpers_core.print_cr
      ~here
      [%message "Expected bonsai app to continue, but stdin was closed."]
  | Exit _ ->
    Expect_test_helpers_core.print_cr
      ~here
      [%message "Expected bonsai app to continue, but [exit] was called."]
;;

let do_frame_and_expect_shutdown ~(here : [%call_pos]) t =
  match%map do_frame t with
  | Exit _ | Stdin_closed -> ()
  | Continue ->
    Expect_test_helpers_core.print_cr
      ~here
      [%message "Expected bonsai app to exit, but app continued."]
;;

let finish_frame_and_continue ~(here : [%call_pos]) finish_frame =
  let%bind (`Frame_finished continue_or_shutdown) = finish_frame in
  match continue_or_shutdown with
  | Frame_outcome.Exit _ ->
    Expect_test_helpers_core.print_cr
      ~here
      [%message "Error expected app to continue after this frame, but [exit] was called!"];
    Deferred.return ()
  | Stdin_closed ->
    Expect_test_helpers_core.print_cr
      ~here
      [%message "Error expected app to continue after this frame, but stdin was closed!"];
    Deferred.return ()
  | Continue -> return ()
;;

let finish_frame_and_expect_shutdown ~(here : [%call_pos]) finish_frame =
  let%bind (`Frame_finished continue_or_shutdown) = finish_frame in
  match continue_or_shutdown with
  | Frame_outcome.Exit _ | Stdin_closed -> Deferred.return ()
  | Continue ->
    Expect_test_helpers_core.print_cr
      ~here
      [%message "Error expected app to shutdown after this frame, but the app continued!"];
    Deferred.return ()
;;

let stdin_writer t = t.stdin_writer
let stdout_reader t = t.stdout_reader
let time_source t = t.time_source

let change_dimensions t dimensions =
  t.current_dimensions := dimensions;
  Bvar.broadcast t.window_changed_bvar ()
;;
