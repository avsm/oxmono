open! Core
open! Bonsai
open! Bonsai_test

module Result_spec = struct
  type t =
    { n : int
    ; start_incrementing : unit Effect.t
    }

  type incoming = Start_incrementing

  let view { n; start_incrementing = _ } = Int.to_string n
  let incoming { n = _; start_incrementing } Start_incrementing = start_incrementing
end

let component ~start ~limit (local_ graph) =
  let open Bonsai.Let_syntax in
  let n, inject =
    Bonsai.state_machine
      ~default_model:start
      ~apply_action:(fun ctx n () ->
        match n >= limit with
        | true -> n
        | false ->
          Apply_action_context.schedule_event ctx (Apply_action_context.inject ctx ());
          n + 1)
      graph
  in
  let%arr n and inject in
  { Result_spec.start_incrementing = inject (); n }
;;

let%expect_test "Does schedule_event run in the current frame or the next frame?" =
  (* NOTE: This test case solely demonstrates existing behavior. It shows that if a
     state_machine schedules an event with [schedule_event] that effect will run in the
     same frame. *)
  let handle = Handle.create (module Result_spec) (component ~start:0 ~limit:1_000) in
  Handle.show handle;
  [%expect {| 0 |}];
  let () = Handle.do_actions handle [ Start_incrementing ] in
  (* NOTE: In a single [Handle.show] we will keep running actions if we keep making calls
     to [schedule_event]. *)
  Handle.show handle;
  [%expect {| 1000 |}]
;;
