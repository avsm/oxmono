open! Core
module Bonsai = Bonsai.Cont
open Bonsai_test
open Bonsai.Let_syntax

let run_effect_on_change_and_remind_every_span_if_true
  :  span:Time_ns.Span.t -> condition:bool Bonsai.t -> unit Ui_effect.t Bonsai.t
  -> local_ Bonsai.graph -> unit Bonsai.t
  =
  fun ~span ~condition effect (local_ graph) ->
  let () =
    Bonsai.Edge.on_change'
      ~trigger:`After_display
      ~equal:[%equal: bool]
      condition
      graph
      ~callback:
        (let%arr effect in
         fun prev curr ->
           let should_run =
             match prev, curr with
             | None, true | Some false, true -> true
             | Some true, _ | None, false | Some false, false -> false
           in
           match should_run with
           | false -> Ui_effect.Ignore
           | true -> effect)
  in
  match%sub condition with
  | false -> Bonsai.return ()
  | true ->
    let (_ : unit Bonsai.t), reset =
      Bonsai.with_model_resetter
        ~f:(fun (local_ graph) ->
          let () =
            Bonsai.Clock.every
              ~trigger_on_activate:false
              ~when_to_start_next_effect:`Every_multiple_of_period_blocking
              (return span)
              effect
              graph
          in
          Bonsai.return ())
        graph
    in
    Bonsai.Edge.lifecycle ~on_deactivate:reset graph;
    Bonsai.return ()
;;

let create_handle ~default =
  let var = Bonsai.Expert.Var.create default in
  let handle =
    Handle.create
      (Result_spec.sexp (module Unit))
      (fun (local_ graph) ->
        run_effect_on_change_and_remind_every_span_if_true
          ~span:(Time_ns.Span.of_min 10.0)
          ~condition:(Bonsai.Expert.Var.value var)
          (Bonsai.return (Ui_effect.print_s [%message "ran effect!"]))
          graph)
  in
  handle, fun x -> Bonsai.Expert.Var.set var x
;;

let%expect_test "true from the very beginning" =
  let handle, _set = create_handle ~default:true in
  Handle.recompute_view handle;
  [%expect {| "ran effect!" |}]
;;

let%expect_test "false from the very beginning" =
  let handle, _set = create_handle ~default:false in
  Handle.recompute_view handle;
  [%expect {| |}]
;;

let%expect_test "false -> true -> false -> true" =
  let handle, set = create_handle ~default:false in
  Handle.recompute_view handle;
  [%expect {| |}];
  set true;
  Handle.recompute_view handle;
  [%expect {| "ran effect!" |}];
  set false;
  Handle.recompute_view handle;
  [%expect {| |}];
  set true;
  Handle.recompute_view handle;
  [%expect {| "ran effect!" |}]
;;

let%expect_test "false -> true -> wait 9 mins -> wait 1 min -> wait 9 mins -> wait 1 min" =
  let handle, set = create_handle ~default:false in
  Handle.recompute_view handle;
  [%expect {| |}];
  set true;
  Handle.recompute_view handle;
  [%expect {| "ran effect!" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_min 9.0);
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_min 1.0);
  (* Sadly 2 [recompute]'s are required here, due to an implementation detail in
     clock.every, but it should still tick at 10 minutes, with a single frame delay. *)
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  [%expect {| "ran effect!" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_min 9.0);
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_min 1.0);
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  [%expect {| "ran effect!" |}]
;;

let%expect_test "false -> true -> wait 9 mins -> false -> wait 1 min -> wait 10 mins" =
  let handle, set = create_handle ~default:false in
  Handle.recompute_view handle;
  [%expect {| |}];
  set true;
  Handle.recompute_view handle;
  [%expect {| "ran effect!" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_min 9.0);
  Handle.recompute_view handle;
  [%expect {| |}];
  set false;
  Handle.advance_clock_by handle (Time_ns.Span.of_min 1.0);
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_min 10.0);
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_min 10.0);
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_min 10.0);
  Handle.recompute_view handle;
  [%expect {| |}]
;;
