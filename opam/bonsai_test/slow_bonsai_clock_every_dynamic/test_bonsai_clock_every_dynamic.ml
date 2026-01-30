open! Core
open Bonsai
open Bonsai.Let_syntax
open Bonsai_test

module When_to_start_next_effect = struct
  type t =
    [ `Wait_period_after_previous_effect_starts_blocking
    | `Wait_period_after_previous_effect_finishes_blocking
    | `Every_multiple_of_period_non_blocking
    | `Every_multiple_of_period_blocking
    ]
  [@@deriving enumerate, sexp_of]
end

let bisimulate f = List.iter When_to_start_next_effect.all ~f

module Spec = struct
  type t =
    { count : int
    ; set_every : Time_ns.Span.t -> unit Effect.t
    ; set_effect_time : Time_ns.Span.t option -> unit Effect.t
    ; toggle_active_status : unit Effect.t
    ; set_active_status : bool -> unit Effect.t
    }

  type incoming =
    | Set_every of Time_ns.Span.t
    | Set_effect_time of Time_ns.Span.t option
    | Toggle_active_status
    | Set_active_status of bool

  let view
    { count
    ; set_every = _
    ; set_effect_time = _
    ; toggle_active_status = _
    ; set_active_status = _
    }
    =
    Sexp.to_string_hum [%sexp (count : int)]
  ;;

  let incoming
    { count = _; set_every; set_effect_time; toggle_active_status; set_active_status }
    incoming
    =
    match incoming with
    | Set_every every -> set_every every
    | Set_effect_time effect_time -> set_effect_time effect_time
    | Toggle_active_status -> toggle_active_status
    | Set_active_status active_status -> set_active_status active_status
  ;;
end

let component ?trigger_on_activate ~when_to_start_next_effect (local_ graph) =
  let count, update = Bonsai.state' 0 graph in
  let effect_time, set_effect_time = Bonsai.state_opt graph in
  let every, set_every = Bonsai.state (Time_ns.Span.of_sec 1.0) graph in
  let incr =
    let sleep = Bonsai.Clock.sleep graph in
    let%arr update and sleep and effect_time in
    let effect = update succ in
    match effect_time with
    | None -> effect
    | Some effect_time ->
      let%bind.Bonsai.Effect () = sleep effect_time in
      effect
  in
  let%tydi { state = is_active
           ; toggle = toggle_active_status
           ; set_state = set_active_status
           }
    =
    Bonsai.toggle' ~default_model:true graph
  in
  let%sub () =
    match%sub is_active with
    | true ->
      Bonsai.Clock.every ?trigger_on_activate ~when_to_start_next_effect every incr graph;
      Bonsai.return ()
    | false -> Bonsai.return ()
  in
  let%arr count
  and set_every
  and set_effect_time
  and toggle_active_status
  and set_active_status in
  { Spec.count; set_every; set_effect_time; toggle_active_status; set_active_status }
;;

module%test Instant_effect_tests = struct
  let%expect_test "Manually driving a constant clock" =
    bisimulate
    @@ fun when_to_start_next_effect ->
    let handle =
      Handle.create (module Spec) (fun (local_ graph) ->
        component ~when_to_start_next_effect graph)
    in
    (* NOTE: The "tick" takes two [Handle.show]s. *)
    Handle.show handle;
    [%expect {| 0 |}];
    Handle.show handle;
    [%expect {| 1 |}];
    let show () =
      Handle.recompute_view handle;
      [%expect {| |}];
      Handle.show handle
    in
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    [%expect {| 2 |}];
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    [%expect {| 3 |}];
    ()
  ;;

  let%expect_test "Manually driving a dynamic clock" =
    bisimulate
    @@ fun when_to_start_next_effect ->
    let handle =
      Handle.create (module Spec) (fun (local_ graph) ->
        component ~when_to_start_next_effect graph)
    in
    let show () =
      Handle.recompute_view handle;
      [%expect {| |}];
      Handle.show handle
    in
    show ();
    [%expect {| 1 |}];
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    [%expect {| 2 |}];
    (* NOTE: If we change [every] when the clock was previously waiting we end up using
       the previous time interval We wait for 1s instead of 2s. *)
    Handle.do_actions handle [ Set_every (Time_ns.Span.of_sec 2.0) ];
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    [%expect {| 3 |}];
    (* The counter doesn't increase! We need to wait for another second to notice the
       increase. *)
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    [%expect {| 3 |}];
    (* The counter increases after 2s. *)
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    [%expect {| 4 |}]
  ;;

  let%expect_test "Zeno's race" =
    (* https://en.wikipedia.org/wiki/Zeno%27s_paradoxes

       The first "tick" will take 1s, the second "tick" will wait 0.5s, the third will
       wait 0.25s and so on.

       How many times will the clock tick when it reaches 2s?

       This test will "clamp"/stop when it reaches 0s.
    *)
    bisimulate
    @@ fun when_to_start_next_effect ->
    let handle =
      Handle.create (module Spec) (fun (local_ graph) ->
        component ~trigger_on_activate:false ~when_to_start_next_effect graph)
    in
    Handle.recompute_view handle;
    let current = Time_ns.Span.of_sec 1.0 in
    let half span =
      let span = Time_ns.Span.to_int63_ns span in
      Int63.O.( lsr ) span 1 |> Time_ns.Span.of_int63_ns
    in
    let rec loop ~current =
      match Time_ns.Span.equal current Time_ns.Span.zero with
      | true -> ()
      | false ->
        Handle.advance_clock_by handle current;
        Handle.do_actions handle [ Set_every (half current) ];
        Handle.recompute_view handle;
        Handle.recompute_view handle;
        loop ~current:(half current)
    in
    loop ~current;
    Handle.show handle;
    (* log2(10^9) ~= 29.8975 so this roughly matches! *)
    [%expect {| 30 |}]
  ;;

  let with_values values f = List.iter values ~f

  let%expect_test "What happens when the clock interval hits 0? What about when the time \
                   is negative?"
    =
    with_values [ Time_ns.Span.zero; Time_ns.Span.of_sec (-1.0) ]
    @@ fun span ->
    bisimulate
    @@ fun when_to_start_next_effect ->
    let handle =
      Handle.create (module Spec) (fun (local_ graph) ->
        component ~trigger_on_activate:false ~when_to_start_next_effect graph)
    in
    Handle.do_actions handle [ Set_every span ];
    Handle.recompute_view handle;
    let try_to_advance () =
      Handle.show handle;
      [%expect {| 1 |}]
    in
    (* Clock does not advance! *)
    Fn.apply_n_times ~n:100 try_to_advance ();
    (*=When every = zero|-1s, it behaves as if every = 1ns. *)
    let show () =
      Handle.recompute_view handle;
      Handle.show handle
    in
    Handle.advance_clock_by handle (Time_ns.Span.of_int63_ns Int63.one);
    show ();
    [%expect {| 2 |}];
    Handle.advance_clock_by handle (Time_ns.Span.of_int63_ns Int63.one);
    show ();
    [%expect {| 3 |}];
    Handle.advance_clock_by handle (Time_ns.Span.of_int63_ns Int63.one);
    show ();
    [%expect {| 4 |}];
    (* Setting it back to a valid value (1s) results in the clock still working! *)
    Handle.do_actions handle [ Set_every (Time_ns.Span.of_sec 1.0) ];
    Handle.advance_clock_by handle (Time_ns.Span.of_int63_ns Int63.one);
    show ();
    (* NOTE: One last tick due to [every] changing not taking effect in the current tick,
       but only taking effect until the next tick. *)
    [%expect {| 5 |}];
    Fn.apply_n_times
      ~n:100
      (fun () ->
        Handle.advance_clock_by handle (Time_ns.Span.of_int63_ns Int63.one);
        show ();
        (* Nothing happens as we need to wait a full second. *)
        [%expect {| 5 |}])
      ();
    Handle.advance_clock_by
      handle
      Time_ns.Span.O.(Time_ns.Span.of_sec 1.0 - Time_ns.Span.of_ns 100.0);
    show ();
    [%expect {| 6 |}]
  ;;
end

module%test Effect_that_takes_time = struct
  (* NOTE: Unlike the above test suite, these tests show what happens when the scheduled
     effect takes time / isn't "instant". *)

  let%expect_test "Manually driving a constant clock" =
    bisimulate
    @@ fun when_to_start_next_effect ->
    let handle =
      Handle.create (module Spec) (fun (local_ graph) ->
        component ~when_to_start_next_effect graph)
    in
    Handle.do_actions handle [ Set_effect_time (Some (Time_ns.Span.of_sec 1.0)) ];
    Handle.show handle;
    [%expect {| 0 |}];
    let show () =
      Handle.recompute_view handle;
      [%expect {| |}];
      Handle.show handle
    in
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    [%expect {| 0 |}];
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    (* The "tick" does not happen until 2s. *)
    [%expect {| 1 |}]
  ;;

  let%expect_test "Manually driving a dynamic clock" =
    bisimulate
    @@ fun when_to_start_next_effect ->
    let handle =
      Handle.create (module Spec) (fun (local_ graph) ->
        component ~when_to_start_next_effect graph)
    in
    Handle.do_actions handle [ Set_effect_time (Some (Time_ns.Span.of_sec 1.0)) ];
    let show () =
      Handle.recompute_view handle;
      [%expect {| |}];
      Handle.show handle
    in
    show ();
    [%expect {| 0 |}];
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    [%expect {| 1 |}];
    Handle.do_actions handle [ Set_every (Time_ns.Span.of_sec 2.0) ];
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    let () =
      match when_to_start_next_effect with
      | `Every_multiple_of_period_non_blocking ->
        (* The sole [_non_blocking] mode will be ahead by 1 due to the extra tick! *)
        [%expect {| 2 |}]
      | `Every_multiple_of_period_blocking
      | `Wait_period_after_previous_effect_starts_blocking
      | `Wait_period_after_previous_effect_finishes_blocking -> [%expect {| 1 |}]
    in
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    show ();
    let () =
      match when_to_start_next_effect with
      | `Every_multiple_of_period_non_blocking -> [%expect {| 3 |}]
      | `Every_multiple_of_period_blocking
      | `Wait_period_after_previous_effect_starts_blocking
      | `Wait_period_after_previous_effect_finishes_blocking -> [%expect {| 2 |}]
    in
    ()
  ;;
end

module%test Quickcheck = struct
  (* NOTE: This quickcheck test show that the clock "continues" working after weird
     sequences of operation. The randomized operations are:

     1. Setting a random "every" + waiting "every"
     2. Activating/de-activating the clock
     3. Waiting every.
     4. Computing/re-computing.
  *)

  module Action = struct
    type t =
      | Advance_clock_by of Time_ns.Span.t
      | Toggle_active_status
      | Recompute_view
      | Set_every_interval of Time_ns.Span.t
      | Set_effect_time of Time_ns.Span.t option
    [@@deriving sexp, quickcheck ~shrinker]

    include struct
      open Quickcheck.Generator.Let_syntax

      let time_span =
        Quickcheck.Generator.of_list
        @@
        let%map.List time = [ 0.0; 0.25; 1.0; 5.0 ] in
        Time_ns.Span.of_sec time
      ;;

      let advance_clock_by =
        let%map time_span in
        Advance_clock_by time_span
      ;;

      let toggle_active_status = return Toggle_active_status
      let recompute_view = return Recompute_view

      let set_every_interval =
        let%map time_span in
        Set_every_interval time_span
      ;;

      let set_effect_time_generator =
        Quickcheck.Generator.weighted_union
          [ ( 0.9
            , let%map time_span in
              Set_effect_time (Some time_span) )
          ; 0.1, return (Set_effect_time None)
          ]
      ;;

      let quickcheck_generator =
        Quickcheck.Generator.weighted_union
          [ 0.3, recompute_view
          ; 0.3, advance_clock_by
          ; 0.2, set_every_interval
          ; 0.1, toggle_active_status
          ; 0.1, set_effect_time_generator
          ]
      ;;

      let perform ~(handle : (_, Spec.incoming) Handle.t) = function
        | Advance_clock_by by -> Handle.advance_clock_by handle by
        | Toggle_active_status -> Handle.do_actions handle [ Toggle_active_status ]
        | Set_every_interval every -> Handle.do_actions handle [ Set_every every ]
        | Set_effect_time effect_time ->
          Handle.do_actions handle [ Set_effect_time effect_time ]
        | Recompute_view -> Handle.recompute_view handle
      ;;
    end
  end

  let%expect_test _ =
    let%quick_test[@trials 1_000] prop (actions : Action.t list) =
      bisimulate
      @@ fun when_to_start_next_effect ->
      let handle =
        Handle.create (module Spec) (fun (local_ graph) ->
          component ~when_to_start_next_effect graph)
      in
      List.iter actions ~f:(fun action -> Action.perform ~handle action);
      Handle.do_actions
        handle
        [ Set_effect_time None
        ; Set_every (Time_ns.Span.of_sec 1.0)
        ; Set_active_status true
        ];
      let ( (* Flush any remaining ticks/effects. *) ) =
        Fn.apply_n_times
          ~n:10
          (fun () ->
            Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
            Handle.recompute_view handle;
            Handle.recompute_view handle)
          ()
      in
      let%tydi { count = initial_count; _ } = Handle.last_result handle in
      let show () =
        Handle.recompute_view handle;
        Handle.recompute_view handle;
        let%tydi { count; _ } = Handle.last_result handle in
        print_s [%sexp (count - initial_count : int)]
      in
      let advance_and_show () =
        Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
        show ()
      in
      show ();
      (* There have been 0 ticks since the last tick! *)
      [%expect {| 0 |}];
      (* Clock should continue to tick! - is not broken! *)
      advance_and_show ();
      [%expect {| 1 |}];
      advance_and_show ();
      [%expect {| 2 |}];
      advance_and_show ();
      [%expect {| 3 |}];
      advance_and_show ();
      [%expect {| 4 |}];
      advance_and_show ();
      [%expect {| 5 |}];
      advance_and_show ();
      [%expect {| 6 |}];
      ()
        [@@remember_failures]
    in
    ()
  ;;
end
