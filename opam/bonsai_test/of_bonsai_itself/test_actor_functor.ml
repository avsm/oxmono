open! Core
open! Import
open! Bonsai_test
module Effect = Bonsai.Effect
open Bonsai.Let_syntax

module Action = struct
  type 'a t =
    | Increment : int t
    | Get_string : string -> string t
  [@@deriving sexp_of]
end

let%expect_test "Actor functor - basic functionality" =
  let module Actor = Bonsai.Actor (Action) in
  let recv_fn : type a. Actor.get_apply_action_context -> int -> a Action.t -> int * a =
    fun _ctx model action ->
    match action with
    | Action.Increment -> model + 1, model
    | Action.Get_string s -> model, sprintf "Got %s at count %d" s model
  in
  let component graph =
    let model, inject = Actor.create ~default_model:0 ~recv:{ Actor.f = recv_fn } graph in
    let%arr model
    and (inject : Actor.inject) = inject in
    ( model
    , let%bind.Effect count = inject.f Action.Increment in
      let%bind.Effect message = inject.f (Action.Get_string "hello") in
      Effect.print_s [%message (count : int) (message : string)] )
  in
  let handle =
    Handle.create
      (module struct
        type t = int * unit Effect.t
        type incoming = unit

        let view (model, _) = Int.to_string model
        let incoming (_, effect) () = effect
      end)
      component
  in
  Handle.show handle;
  [%expect {| 0 |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect
    {|
    ((count 0) (message "Got hello at count 1"))
    1
    |}]
;;

let%expect_test "Actor functor - using apply_action_context" =
  let module Actor = Bonsai.Actor (Action) in
  let recv_fn
    : type a.
      Actor.get_apply_action_context -> string list -> a Action.t -> string list * a
    =
    fun ctx model action ->
    let context = ctx.get () in
    match action with
    | Action.Increment ->
      let new_model = "increment" :: model in
      (* Schedule another action using the context *)
      Bonsai.Apply_action_context.schedule_event
        context
        (let%bind.Effect msg =
           Bonsai.Apply_action_context.inject context (Action.Get_string "recursive")
         in
         Effect.print_s [%message "from recursive call" (msg : string)]);
      new_model, List.length new_model
    | Action.Get_string s ->
      let new_model = s :: model in
      new_model, sprintf "processed %s" s
  in
  let component graph =
    let model, inject =
      Actor.create ~default_model:[] ~recv:{ Actor.f = recv_fn } graph
    in
    Bonsai.both model inject
  in
  let handle =
    Handle.create
      (module struct
        type t = string list * Actor.inject
        type incoming = [ `Increment ]

        let view (model, _) = String.concat ~sep:"; " model

        let incoming (_, (inject : Actor.inject)) `Increment =
          let%bind.Effect _ = inject.f Action.Increment in
          Effect.return ()
        ;;
      end)
      component
  in
  Handle.do_actions handle [ `Increment ];
  Handle.show handle;
  [%expect
    {|
    ("from recursive call" (msg "processed recursive"))
    recursive; increment
    |}]
;;

let%expect_test "Actor functor - basic functionality (create with input is callable / \
                 type checks)"
  =
  (* NOTE: This is a tiny check that guards against a bug where [create_with_input] was
     uncallable. *)
  let module Actor = Bonsai.Actor (Action) in
  let recv_fn
    : type a.
      Actor.get_apply_action_context
      -> unit Bonsai.Computation_status.t
      -> int
      -> a Action.t
      -> int * a
    =
    fun _ctx _ model action ->
    match action with
    | Action.Increment -> model + 1, model
    | Action.Get_string s -> model, sprintf "Got %s at count %d" s model
  in
  let component graph =
    let model, inject =
      Actor.create_with_input
        ~default_model:0
        ~recv:{ Actor.f = recv_fn }
        (Bonsai.return ())
        graph
    in
    let%arr model
    and (inject : Actor.inject) = inject in
    ( model
    , let%bind.Effect count = inject.f Action.Increment in
      let%bind.Effect message = inject.f (Action.Get_string "hello") in
      Effect.print_s [%message (count : int) (message : string)] )
  in
  let handle =
    Handle.create
      (module struct
        type t = int * unit Effect.t
        type incoming = unit

        let view (model, _) = Int.to_string model
        let incoming (_, effect) () = effect
      end)
      component
  in
  Handle.show handle;
  [%expect {| 0 |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect
    {|
    ((count 0) (message "Got hello at count 1"))
    1
    |}]
;;
