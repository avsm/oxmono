open! Core
open! Bonsai
open! Bonsai_test

let seconds n = Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (Int.to_float n))

let tick_every_second (local_ graph) =
  let () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:false
      (Bonsai.return (Time_ns.Span.of_sec 1.0))
      (Bonsai.return (Ui_effect.print_s [%message "ticked!"]))
      graph
  in
  Bonsai.return ()
;;

let%expect_test "every - Advancing the clock like normal" =
  let handle = Handle.create (Result_spec.sexp (module Unit)) tick_every_second in
  Handle.recompute_view handle;
  let go n =
    Handle.advance_clock handle ~to_:(seconds n);
    Handle.recompute_view handle
  in
  go 1;
  [%expect {| |}];
  go 2;
  [%expect {| ticked! |}];
  go 3;
  [%expect {| |}];
  go 4;
  [%expect {| ticked! |}];
  go 5;
  [%expect {| |}];
  go 6;
  [%expect {| ticked! |}]
;;

let%expect_test "every - Advancing the clock like normal and then going back in time - \
                 and then continuing"
  =
  let handle = Handle.create (Result_spec.sexp (module Unit)) tick_every_second in
  let go n =
    Handle.advance_clock handle ~to_:(seconds n);
    Handle.recompute_view handle
  in
  Handle.recompute_view handle;
  go 1;
  [%expect {| |}];
  go 2;
  [%expect {| ticked! |}];
  go 3;
  [%expect {| |}];
  go 2;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:03Z") (to_ "1970-01-01 00:00:02Z"))
    ticked!
    |}];
  go 1;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:03Z") (to_ "1970-01-01 00:00:01Z"))
    |}];
  go 0;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:03Z") (to_ "1970-01-01 00:00:00Z"))
    |}];
  go 1;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:03Z") (to_ "1970-01-01 00:00:01Z"))
    |}];
  go 2;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:03Z") (to_ "1970-01-01 00:00:02Z"))
    |}];
  go 3;
  [%expect {| |}];
  go 4;
  [%expect {| |}];
  go 5;
  [%expect {| ticked! |}];
  go 6;
  [%expect {| |}];
  go 7;
  [%expect {| ticked! |}];
  go 8;
  [%expect {| |}];
  go 9;
  [%expect {| ticked! |}];
  go 10;
  [%expect {| |}]
;;

let before_or_after_a_second (local_ graph) =
  Bonsai.Clock.at (Bonsai.return (seconds 1)) graph
;;

let%expect_test "before or after is weird" =
  let handle =
    Handle.create
      (Result_spec.sexp (module Bonsai.Clock.Before_or_after))
      before_or_after_a_second
  in
  Handle.show handle;
  [%expect {| Before |}];
  Handle.advance_clock handle ~to_:(seconds 1);
  Handle.show handle;
  [%expect {| After |}];
  Handle.advance_clock handle ~to_:(seconds 0);
  Handle.show handle;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:01Z") (to_ "1970-01-01 00:00:00Z"))
    After
    |}]
;;

let%expect_test "Approx now - kind of weird" =
  let handle =
    Handle.create
      (Result_spec.sexp (module Time_ns.Alternate_sexp))
      (fun (local_ graph) ->
        Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.0) graph)
  in
  let go n =
    Handle.advance_clock handle ~to_:(seconds n);
    Handle.show handle
  in
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  go 1;
  [%expect {| "1970-01-01 00:00:01Z" |}];
  go 0;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:01Z") (to_ "1970-01-01 00:00:00Z"))
    "1970-01-01 00:00:01Z"
    |}];
  go 0;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:01Z") (to_ "1970-01-01 00:00:00Z"))
    "1970-01-01 00:00:01Z"
    |}];
  go 1;
  [%expect {| "1970-01-01 00:00:01Z" |}];
  go 2;
  [%expect {| "1970-01-01 00:00:02Z" |}];
  go 3;
  [%expect {| "1970-01-01 00:00:03Z" |}]
;;

let%expect_test "now - kind of weird" =
  let handle =
    Handle.create
      (Result_spec.sexp (module Time_ns.Alternate_sexp))
      (fun (local_ graph) -> Bonsai.Clock.Expert.now graph)
  in
  let go n =
    Handle.advance_clock handle ~to_:(seconds n);
    Handle.show handle
  in
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  go 1;
  [%expect {| "1970-01-01 00:00:01Z" |}];
  go 0;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:01Z") (to_ "1970-01-01 00:00:00Z"))
    "1970-01-01 00:00:01Z"
    |}];
  go 0;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:01Z") (to_ "1970-01-01 00:00:00Z"))
    "1970-01-01 00:00:01Z"
    |}];
  go 1;
  [%expect {| "1970-01-01 00:00:01Z" |}];
  go 2;
  [%expect {| "1970-01-01 00:00:02Z" |}];
  go 3;
  [%expect {| "1970-01-01 00:00:03Z" |}]
;;

let%expect_test "get_current_time - behaves correctly" =
  let module Spec = struct
    type t = unit Ui_effect.t
    type incoming = Print_current_time

    let view _ = ""
    let incoming effect Print_current_time = effect
  end
  in
  let handle =
    Handle.create (module Spec) (fun (local_ graph) ->
      let get_current_time = Bonsai.Clock.get_current_time graph in
      let open Bonsai.Let_syntax in
      let%arr get_current_time in
      let%bind.Ui_effect current_time = get_current_time in
      Ui_effect.print_s [%message (current_time : Time_ns.Alternate_sexp.t)])
  in
  let go n =
    Handle.advance_clock handle ~to_:(seconds n);
    Handle.do_actions handle [ Print_current_time ];
    Handle.recompute_view handle
  in
  Handle.show handle;
  [%expect {| |}];
  go 1;
  [%expect {| (current_time "1970-01-01 00:00:01Z") |}];
  go 0;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:01Z") (to_ "1970-01-01 00:00:00Z"))
    (current_time "1970-01-01 00:00:01Z")
    |}];
  go 0;
  [%expect
    {|
    (lib/ui_concrete/time_source/ui_time_source.ml:76:8 "time moving backwards"
     (now "1970-01-01 00:00:01Z") (to_ "1970-01-01 00:00:00Z"))
    (current_time "1970-01-01 00:00:01Z")
    |}];
  go 1;
  [%expect {| (current_time "1970-01-01 00:00:01Z") |}];
  go 2;
  [%expect {| (current_time "1970-01-01 00:00:02Z") |}];
  go 3;
  [%expect {| (current_time "1970-01-01 00:00:03Z") |}]
;;

module%test Overflow = struct
  let days n = Time_ns.of_span_since_epoch (Time_ns.Span.of_int_day n)

  let simmulate_all_when_to_start_next_effect ~f =
    f `Every_multiple_of_period_blocking;
    f `Every_multiple_of_period_non_blocking;
    f `Wait_period_after_previous_effect_finishes_blocking;
    f `Wait_period_after_previous_effect_starts_blocking
  ;;

  let tick_every_max_value ~when_to_start_next_effect (local_ graph) =
    let () =
      Bonsai.Clock.every
        ~when_to_start_next_effect
        ~trigger_on_activate:true
        (Bonsai.return Time_ns.Span.max_value_representable)
        (Bonsai.return (Ui_effect.print_s [%message "ticked!"]))
        graph
    in
    Bonsai.return ()
  ;;

  let%expect_test "every w/ max_value - showing that we don't overflow when doing the \
                   initial on_activate logic"
    =
    simmulate_all_when_to_start_next_effect ~f:(fun when_to_start_next_effect ->
      let handle =
        Handle.create
          (Result_spec.sexp (module Unit))
          (tick_every_max_value ~when_to_start_next_effect)
      in
      Handle.recompute_view handle;
      let go n =
        Handle.advance_clock handle ~to_:(days n);
        Handle.recompute_view handle
      in
      go 1;
      [%expect {| ticked! |}];
      go 2;
      [%expect {| |}];
      go 3;
      [%expect {| |}];
      go 4;
      [%expect {| |}];
      go 5;
      [%expect {| |}];
      go 1000000;
      [%expect {| |}];
      go 1000001;
      [%expect {| |}])
  ;;
end
