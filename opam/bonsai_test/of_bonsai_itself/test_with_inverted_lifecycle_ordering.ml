open! Core
open! Import
open Bonsai_test
open Bonsai
open Bonsai.Let_syntax

module Test (S : sig
    val subs_before : int
  end) =
struct
  let print_lifecycles ~inverted name (local_ graph) =
    let path_check =
      if inverted
      then (
        (* We register 2 paths to confirm that multiple components inside are assigned the
           same path, not just the first component within. *)
        let trim s = String.sub s ~pos:0 ~len:(String.length s - 1) in
        let%arr path1 = Bonsai.path_id graph
        and path2 = Bonsai.path_id graph in
        if String.equal (trim path1) (trim path2)
        then Or_error.return ()
        else
          Or_error.error_s
            [%message
              "Expected subsequent paths inside inverted lifecycles to only differ in \
               last char"
                (path1 : string)
                (path2 : string)])
      else
        (* [with_inverted_lifecycle_ordering] acts as a "boundary" on balancing a chain of
           subs into a tree, so we can be confident about the relative values of paths
           within a [with_inverted_lifecycle_ordering] call. This is not the same for the
           "normal" case. *)
        return (Or_error.return ())
    in
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%arr path_check in
         Or_error.ok_exn path_check;
         Effect.print_s [%message "on_activate" ~_:(name : string)])
      ~on_deactivate:
        (let%arr path_check in
         Or_error.ok_exn path_check;
         Effect.print_s [%message "on_deactivate" ~_:(name : string)])
      graph;
    Bonsai.Edge.on_change
      ~trigger:`After_display
      ~equal:[%equal: unit Or_error.t]
      path_check
      ~callback:
        (return (fun path_check ->
           Or_error.ok_exn path_check;
           Effect.print_s [%message "on_change" ~_:(name : string)]))
      graph
  ;;

  let insert_subs (local_ graph) =
    for _ = 1 to S.subs_before do
      Bonsai.path_id graph |> Fn.ignore
    done
  ;;

  let bisimulate_default_and_inverted ~f =
    f
      (fun ~compute_dep ~f (local_ graph) ->
        insert_subs graph;
        let dep = compute_dep graph in
        f dep graph)
      ~print_lifecycles:(print_lifecycles ~inverted:false)
      ~expect_diff:(fun ~normal ~inverted:_ -> normal ());
    f
      (fun ~compute_dep ~f (local_ graph) ->
        insert_subs graph;
        Bonsai.with_inverted_lifecycle_ordering ~compute_dep ~f graph)
      ~print_lifecycles:(print_lifecycles ~inverted:true)
      ~expect_diff:(fun ~normal:_ ~inverted -> inverted ())
  ;;

  let%expect_test "trivial" =
    bisimulate_default_and_inverted ~f:(fun build ~print_lifecycles ~expect_diff ->
      let handle =
        Handle.create
          (Result_spec.string (module Int))
          (build
             ~compute_dep:(fun graph ->
               print_lifecycles "a" graph;
               return 0)
             ~f:(fun r graph ->
               print_lifecycles "b" graph;
               r))
      in
      Handle.recompute_view handle;
      expect_diff
        ~normal:(fun () ->
          [%expect
            {|
            (on_activate a)
            (on_activate b)
            (on_change a)
            (on_change b)
            |}])
        ~inverted:(fun () ->
          [%expect
            {|
            (on_activate b)
            (on_activate a)
            (on_change b)
            (on_change a)
            |}]))
  ;;

  module Bool_action_no_view = struct
    type t = bool -> unit Effect.t
    type incoming = bool

    let incoming f incoming = f incoming
    let view _ = ""
  end

  let%expect_test "within [match%sub]" =
    bisimulate_default_and_inverted ~f:(fun build ~print_lifecycles ~expect_diff ->
      let handle =
        Handle.create (module Bool_action_no_view) (fun (local_ graph) ->
          let active, set_active = Bonsai.state false graph in
          let (_ : unit Bonsai.t) =
            match%sub active with
            | false -> return ()
            | true ->
              build
                ~compute_dep:(fun graph ->
                  print_lifecycles "a" graph;
                  return ())
                ~f:(fun r graph ->
                  print_lifecycles "b" graph;
                  r)
                graph
          in
          set_active)
      in
      Handle.recompute_view handle;
      expect_diff
        ~normal:(fun () -> [%expect {| |}])
        ~inverted:(fun () -> [%expect {| |}]);
      Handle.do_actions handle [ true ];
      Handle.recompute_view handle;
      expect_diff
        ~normal:(fun () ->
          [%expect
            {|
            (on_activate a)
            (on_activate b)
            (on_change a)
            (on_change b)
            |}])
        ~inverted:(fun () ->
          [%expect
            {|
            (on_activate b)
            (on_activate a)
            (on_change b)
            (on_change a)
            |}]);
      Handle.do_actions handle [ false ];
      Handle.recompute_view handle;
      (* Note that for inverted lifecycles, the "compute" on_deactivate runs first. For
         e.g. portalled modals, this is important because it allows the [content]
         computation's cleanup to run before the modal is destroyed. *)
      expect_diff
        ~normal:(fun () ->
          [%expect
            {|
            (on_deactivate a)
            (on_deactivate b)
            |}])
        ~inverted:(fun () ->
          [%expect
            {|
            (on_deactivate b)
            (on_deactivate a)
            |}]);
      ())
  ;;

  let%expect_test "[match%sub] within" =
    bisimulate_default_and_inverted ~f:(fun build ~print_lifecycles ~expect_diff ->
      let handle =
        Handle.create (module Bool_action_no_view) (fun (local_ graph) ->
          let active, set_active = Bonsai.state false graph in
          let (_ : unit Bonsai.t) =
            build
              ~compute_dep:(fun graph ->
                match%sub active with
                | false -> active
                | true ->
                  print_lifecycles "a" graph;
                  active)
              ~f:(fun active graph ->
                match%sub active with
                | false -> return ()
                | true ->
                  print_lifecycles "b" graph;
                  return ())
              graph
          in
          set_active)
      in
      Handle.recompute_view handle;
      expect_diff
        ~normal:(fun () -> [%expect {| |}])
        ~inverted:(fun () -> [%expect {| |}]);
      Handle.do_actions handle [ true ];
      Handle.recompute_view handle;
      expect_diff
        ~normal:(fun () ->
          [%expect
            {|
            (on_activate a)
            (on_activate b)
            (on_change a)
            (on_change b)
            |}])
        ~inverted:(fun () ->
          [%expect
            {|
            (on_activate b)
            (on_activate a)
            (on_change b)
            (on_change a)
            |}]);
      Handle.do_actions handle [ false ];
      Handle.recompute_view handle;
      (* Note that for inverted lifecycles, the "compute" on_deactivate runs first. For
         e.g. portalled modals, this is important because it allows the [content]
         computation's cleanup to run before the modal is destroyed. *)
      expect_diff
        ~normal:(fun () ->
          [%expect
            {|
            (on_deactivate a)
            (on_deactivate b)
            |}])
        ~inverted:(fun () ->
          [%expect
            {|
            (on_deactivate b)
            (on_deactivate a)
            |}]);
      ())
  ;;

  let%expect_test "nested" =
    bisimulate_default_and_inverted ~f:(fun build ~print_lifecycles ~expect_diff ->
      let handle =
        Handle.create
          (Result_spec.string (module Unit))
          (build
             ~compute_dep:(fun graph ->
               print_lifecycles "a" graph;
               let%sub () =
                 build
                   ~compute_dep:(fun graph ->
                     print_lifecycles "a.a" graph;
                     return ())
                   ~f:(fun r graph ->
                     print_lifecycles "a.b" graph;
                     r)
                   graph
               in
               return ())
             ~f:(fun r graph ->
               print_lifecycles "b" graph;
               let%sub () =
                 build
                   ~compute_dep:(fun graph ->
                     print_lifecycles "b.a" graph;
                     return ())
                   ~f:(fun r graph ->
                     print_lifecycles "b.b" graph;
                     r)
                   graph
               in
               r))
      in
      Handle.recompute_view handle;
      expect_diff
        ~normal:(fun () ->
          [%expect
            {|
            (on_activate a)
            (on_activate a.a)
            (on_activate a.b)
            (on_activate b)
            (on_activate b.a)
            (on_activate b.b)
            (on_change a)
            (on_change a.a)
            (on_change a.b)
            (on_change b)
            (on_change b.a)
            (on_change b.b)
            |}])
        ~inverted:(fun () ->
          [%expect
            {|
            (on_activate b)
            (on_activate b.b)
            (on_activate b.a)
            (on_activate a)
            (on_activate a.b)
            (on_activate a.a)
            (on_change b)
            (on_change b.b)
            (on_change b.a)
            (on_change a)
            (on_change a.b)
            (on_change a.a)
            |}]))
  ;;
end

module%test _ = Test (struct
    let subs_before = 0
  end)

module%test _ = Test (struct
    let subs_before = 1
  end)

module%test _ = Test (struct
    let subs_before = 5
  end)

module%test _ = Test (struct
    let subs_before = 7
  end)

module%test _ = Test (struct
    let subs_before = 25
  end)

module%test _ = Test (struct
    let subs_before = 42
  end)

module%test _ = Test (struct
    let subs_before = 49
  end)

module%test _ = Test (struct
    let subs_before = 100
  end)
