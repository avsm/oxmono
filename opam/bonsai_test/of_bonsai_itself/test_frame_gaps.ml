open! Core
open Bonsai_test
open Bonsai.Let_syntax

(* NOTE: This is a small demo demo'ing an awkward situation in which [on_change]
   introduced a "frame gap". This test cases demonstrates/documents this behavior. *)

let component input (local_ graph) =
  let state, set_state = Bonsai.state false graph in
  Bonsai.Edge.on_change
    ~trigger:`After_display
    ~equal:[%equal: bool]
    input
    ~callback:set_state
    graph;
  state
;;

let%expect_test _ =
  let var = Bonsai.Expert.Var.create false in
  let handle =
    Handle.create
      (Result_spec.sexp (module Bool))
      (fun (local_ graph) -> component (Bonsai.Expert.Var.value var) graph)
  in
  Handle.show handle;
  [%expect {| false |}];
  Bonsai.Expert.Var.set var true;
  Handle.show handle;
  (* NOTE: Did not update! *)
  [%expect {| false |}];
  (* NOTE: Updates in the next frame! *)
  Handle.show handle;
  [%expect {| true |}]
;;

(* This can be fixed by passing [~trigger:`Before_display]... *)

let component input (local_ graph) =
  let state, set_state = Bonsai.state false graph in
  Bonsai.Edge.on_change
    ~trigger:`Before_display
    ~equal:[%equal: bool]
    input
    ~callback:set_state
    graph;
  state
;;

let%expect_test _ =
  let var = Bonsai.Expert.Var.create false in
  let handle =
    Handle.create
      (Result_spec.sexp (module Bool))
      (fun (local_ graph) -> component (Bonsai.Expert.Var.value var) graph)
  in
  Handle.show handle;
  [%expect {| false |}];
  Bonsai.Expert.Var.set var true;
  Handle.show handle;
  (* This updates immediately now. *)
  [%expect {| true |}];
  Handle.show handle;
  [%expect {| true |}]
;;

(*=...but this stops working when multiple other [before_display]s set the state.

   This example is a bit convoluted, but only because this state is hard to
   get into. The idea is that [state] is set in two different before_display loops in
   a single frame, where any [on_change]s attached to it will
   only trigger on the first change.

   input ----> input_plus_one
         \                    \
          \--> state <--------/
                     \
                      \---> state_mirror

   [state_mirror] still updates in the next frame, but ideally, we'd want to avoid
   synchronizing state like this in the first place.
*)

let component input (local_ graph) =
  (* This is meant to be synced to [input] + 1. *)
  let input_plus_one, set_input_plus_one = Bonsai.state 0 graph in
  (* The first element is meant to be synced with [input], and the second is meant to be
     synced with [input_plus_one]. *)
  let state, set_state = Bonsai.state' (0, 0) graph in
  (* This is meant to be synced with [state]. *)
  let state_mirror, set_state_mirror = Bonsai.state (0, 0) graph in
  Bonsai.Edge.on_change
    ~trigger:`Before_display
    ~equal:[%equal: int]
    input
    ~callback:
      (let%arr set_input_plus_one in
       fun i -> set_input_plus_one (i + 1))
    graph;
  Bonsai.Edge.on_change
    ~trigger:`Before_display
    ~equal:[%equal: int]
    input
    ~callback:
      (let%arr set_state in
       fun i -> set_state (fun (_, old) -> i, old))
    graph;
  Bonsai.Edge.on_change
    ~trigger:`Before_display
    ~equal:[%equal: int]
    input_plus_one
    ~callback:
      (let%arr set_state in
       fun i -> set_state (fun (old, _) -> old, i))
    graph;
  Bonsai.Edge.on_change
    ~trigger:`Before_display
    ~equal:[%equal: int * int]
    state
    ~callback:set_state_mirror
    graph;
  let%arr input_plus_one and state and state_mirror in
  input_plus_one, state, state_mirror
;;

let%expect_test _ =
  let var = Bonsai.Expert.Var.create 0 in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int * (int * int) * (int * int) [@@deriving sexp]
         end))
      (fun (local_ graph) -> component (Bonsai.Expert.Var.value var) graph)
  in
  Handle.show handle;
  (* Here, the [on_change]s all run at the beginning, so they can't run again to sync with
     the updated [input_plus_one]. *)
  [%expect {| (1 (0 0) (0 0)) |}];
  Handle.show handle;
  [%expect {| (1 (0 1) (0 1)) |}];
  Bonsai.Expert.Var.set var 1;
  Handle.show handle;
  (* Here, the two on_changes attached to [input] run first, updating [state] and and
     [input_plus_one].

     Then, the on_changes attached to each of those run, updating [state_mirror] and
     [state], respectively.

     The on_change attached to [state] can't run again in this frame, so [state_mirror]
     contains a stale value for one frame. *)
  [%expect {| (2 (1 2) (1 1)) |}];
  Handle.show handle;
  [%expect {| (2 (1 2) (1 2)) |}]
;;
