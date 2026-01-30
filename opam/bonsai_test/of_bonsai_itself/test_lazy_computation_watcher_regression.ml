open! Core
open! Import
open! Bonsai_test
open Bonsai.For_open

(* This test will only catch regressions of the bug if [run] is called multiple times. *)

module Result_spec = struct
  type t = unit
  type incoming = unit

  let view _ = ""
  let incoming () () = Effect.Ignore
end

let here =
  { Source_code_position.pos_fname = [%string "test-location"]
  ; pos_lnum = 0
  ; pos_bol = 0
  ; pos_cnum = 0
  }
;;

let enable_computation_watcher computation graph =
  Bonsai.Private.handle graph ~f:computation
  |> Bonsai.Private.Enable_computation_watcher.run ~watcher_queue:(Queue.create ())
  |> Bonsai.Private.perform graph
;;

let%expect_test _ =
  let component graph =
    let var = Bonsai.Expert.Var.create () in
    Bonsai.Debug.watch_computation
      ~here
      ~f:(fun graph -> Bonsai.delay ~f:(fun _graph -> Bonsai.Expert.Var.value var) graph)
      graph
  in
  let handle =
    Handle.create
      ~optimize:false
      (module Result_spec)
      (enable_computation_watcher component)
  in
  Handle.show handle;
  [%expect {| |}]
;;
