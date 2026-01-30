open! Core
open! Import

module Expect_test_config = struct
  module IO = Monad.Ident

  let run f =
    f ();
    Bonsai_test_handle_garbage_collector.garbage_collect ()
  ;;

  let sanitize = Fn.id
  let upon_unreleasable_issue = `CR
end

module Result_spec = struct
  module type S = sig
    type t
    type incoming

    val view : t -> string
    val incoming : t -> incoming -> unit Effect.t
  end

  type ('result, 'incoming) t =
    (module S with type t = 'result and type incoming = 'incoming)

  module No_incoming = struct
    type incoming = Nothing.t

    let incoming _t incoming = Nothing.unreachable_code incoming
  end

  module type Sexpable = sig
    type t [@@deriving sexp_of]
  end

  module type Stringable = sig
    type t

    val to_string : t -> string
  end

  let invisible (type a) : (a, Nothing.t) t =
    (module struct
      type t = a

      include No_incoming

      let view _ = ""
    end)
  ;;

  let sexp (type a) (module S : Sexpable with type t = a) =
    (module struct
      type t = a

      include No_incoming

      let view s = s |> S.sexp_of_t |> Sexp.to_string_hum
    end : S
      with type t = a
       and type incoming = Nothing.t)
  ;;

  let string (type a) (module S : Stringable with type t = a) =
    (module struct
      type t = a

      include No_incoming

      let view s = s |> S.to_string
    end : S
      with type t = a
       and type incoming = Nothing.t)
  ;;
end

module Handle = struct
  type ('result, 'incoming) t =
    (unit, 'result * string Lazy.t * ('incoming -> unit Effect.t)) Driver.t

  let create
    (type result incoming)
    ~(here : [%call_pos])
    ?(start_time = Time_ns.epoch)
    ~optimize
    (result_spec : (result, incoming) Result_spec.t)
    computation
    =
    let (module R) = result_spec in
    let component (_ : unit Bonsai.t) (local_ graph) =
      let open Bonsai.Let_syntax in
      let result = computation graph in
      let%arr result in
      result, lazy (R.view result), R.incoming result
    in
    let time_source = Bonsai.Time_source.create ~start:start_time in
    let handle = Driver.create ~here ~optimize ~initial_input:() ~time_source component in
    Bonsai_test_handle_garbage_collector.register_cleanup (fun () ->
      Driver.invalidate_observers handle);
    handle
  ;;

  let node_paths_from_skeleton
    : type a. a Bonsai.Private.Computation.t -> Bonsai.Private.Node_path.Set.t
    =
    fun t ->
    let find_node_paths =
      object
        inherit
          [Bonsai.Private.Node_path.Set.t] Bonsai.Private.Skeleton.Traverse.fold as super

        method! value value acc =
          let acc = Set.add acc (Lazy.force value.node_path) in
          super#value value acc

        method! computation computation acc =
          let acc = Set.add acc (Lazy.force computation.node_path) in
          super#computation computation acc
      end
    in
    find_node_paths#computation
      (Bonsai.Private.Skeleton.Computation.of_computation t)
      Bonsai.Private.Node_path.Set.empty
  ;;

  let node_paths_from_transform
    : type a. a Bonsai.Private.Computation.t -> Bonsai.Private.Node_path.Set.t
    =
    fun t ->
    let node_paths = ref Bonsai.Private.Node_path.Set.empty in
    let computation_map
      (type result)
      (context : _ Bonsai.Private.Transform.For_computation.context)
      state
      (computation : result Bonsai.Private.Computation.t)
      =
      node_paths := Set.add !node_paths (Lazy.force context.current_path);
      let out = context.recurse state computation in
      out
    in
    let value_map
      (type a)
      (context : _ Bonsai.Private.Transform.For_value.context)
      state
      (wrapped_value : a Bonsai.Private.Value.t)
      =
      node_paths := Set.add !node_paths (Lazy.force context.current_path);
      context.recurse state wrapped_value
    in
    let (_ : _ Bonsai.Private.Computation.t) =
      Bonsai.Private.Transform.map
        ~init:()
        ~computation_mapper:{ f = computation_map }
        ~value_mapper:{ f = value_map }
        t
    in
    !node_paths
  ;;

  let locations_from_computation : type a. a Bonsai.Private.Computation.t -> String.Set.t =
    fun t ->
    let find_locations =
      object
        inherit [String.Set.t] Bonsai.Private.Skeleton.Traverse.fold

        method! source_code_position location acc =
          Set.add acc [%string "%{location.pos_fname}:%{location.pos_lnum#Int}"]
      end
    in
    find_locations#computation
      (Bonsai.Private.Skeleton.Computation.of_computation t)
      String.Set.empty
  ;;

  let assert_no_unthreaded_locations_in_bonsai's_internals
    : type a. a Bonsai.Private.Computation.t -> unit
    =
    fun computation ->
    let locations = locations_from_computation computation in
    let internal_locations =
      Set.filter locations ~f:(fun location ->
        String.is_prefix ~prefix:"lib/bonsai/src/" location)
    in
    if (not (Set.is_empty internal_locations)) && Core.am_running_test
    then (
      Expect_test_helpers_core.print_cr
        ~cr:Comment
        (Sexp.Atom "SMALL BUG IN BONSAI! Un-threadded internal locations");
      print_s [%message (internal_locations : String.Set.t)])
  ;;

  let assert_node_paths_identical_between_transform_and_skeleton_nodepaths
    : type a. a Bonsai.Private.Computation.t -> unit
    =
    fun computation ->
    let from_transform = node_paths_from_transform computation in
    let from_skeleton = node_paths_from_skeleton computation in
    if not ([%equal: Set.M(Bonsai.Private.Node_path).t] from_transform from_skeleton)
    then (
      Expect_test_helpers_core.print_cr (Sexp.Atom "BUG IN BONSAI! Node Path Mismatch");
      Expect_test_patdiff.print_patdiff_s
        ([%sexp_of: Bonsai.Private.Node_path.Set.t] from_transform)
        ([%sexp_of: Bonsai.Private.Node_path.Set.t] from_skeleton))
  ;;

  let create
    (type result incoming)
    ~(here : [%call_pos])
    ?start_time
    ?(optimize = true)
    (result_spec : (result, incoming) Result_spec.t)
    computation
    =
    let handle = create ~here ?start_time ~optimize result_spec computation in
    (* [assert_node_paths_identical_between_transform_and_skeleton_nodepaths] is a useful
       function to verify that the skeleton code correctly generates node_path
       identifiers. It's nice to run on every test, but was taking up ~20% of the run time
       for most tests, so we disable it here, keeping the code around in case there's any
       major changes to the skeleton code and want to easily run it on every test in the
       tree *)
    if false
    then
      assert_node_paths_identical_between_transform_and_skeleton_nodepaths
        (Bonsai.Private.top_level_handle computation);
    if false
       || String.is_prefix here.pos_fname ~prefix:{|lib/bonsai/test/of_bonsai_itself|}
       || String.is_prefix here.pos_fname ~prefix:{|lib/bonsai/web_test/of_bonsai_itself|}
    then
      assert_no_unthreaded_locations_in_bonsai's_internals
        (Driver.Private.running_computation handle);
    handle
  ;;

  let last_result handle =
    let result, _, _ = Driver.result handle in
    result
  ;;

  let time_source = Driver.time_source
  let advance_clock_by t = Bonsai.Time_source.advance_clock_by (Driver.time_source t)
  let advance_clock ~to_ t = Bonsai.Time_source.advance_clock ~to_ (Driver.time_source t)

  let do_actions handle actions =
    let _, _, inject_action = Driver.result handle in
    let event = actions |> List.map ~f:inject_action |> Effect.sequence in
    Driver.schedule_event handle event
  ;;

  let recompute_view ?(simulate_diff_patch = fun _ -> ()) (handle : (_, 'r) Driver.t) =
    Driver.flush handle;
    let computed, _, _ = Driver.result handle in
    simulate_diff_patch computed;
    Driver.trigger_lifecycles handle
  ;;

  let recompute_view_until_stable ?(max_computes = 100) ?simulate_diff_patch handle =
    recompute_view ?simulate_diff_patch handle;
    let computes = ref 1 in
    while Driver.has_after_display_events handle do
      recompute_view ?simulate_diff_patch handle;
      computes := !computes + 1;
      if !computes >= max_computes
      then failwithf "view not stable after %d recomputations" max_computes ()
    done
  ;;

  let recompute_store_and_show ?simulate_diff_patch handle =
    recompute_view ?simulate_diff_patch handle;
    let _, view, _ = Driver.result handle in
    Driver.store_view handle view;
    view
  ;;

  let show_into_string ?simulate_diff_patch handle =
    recompute_store_and_show ?simulate_diff_patch handle |> Lazy.force
  ;;

  let show ?simulate_diff_patch handle =
    show_into_string ?simulate_diff_patch handle |> print_endline
  ;;

  let show_diff
    ?(location_style = Patdiff_kernel.Format.Location_style.None)
    ?(diff_context = 16)
    ?simulate_diff_patch
    handle
    =
    let before = Driver.last_view handle in
    let after = recompute_store_and_show ?simulate_diff_patch handle in
    Expect_test_patdiff.print_patdiff
      ~location_style
      ~context:diff_context
      (force before)
      (force after)
  ;;

  let store_view handle = ignore (recompute_store_and_show handle : string Lazy.t)

  let show_model handle =
    Driver.flush handle;
    Driver.sexp_of_model handle |> print_s
  ;;

  let result_incr handle =
    let%pattern_bind.Incr result, _view, _inject = Driver.result_incr handle in
    result
  ;;

  let action_input_incr = Driver.action_input_incr
  let lifecycle_incr = Driver.lifecycle_incr
  let print_actions = Driver.print_actions
  let print_stabilizations = Driver.print_stabilizations
  let print_stabilization_tracker_stats = Driver.print_stabilization_tracker_stats
  let has_after_display_events = Driver.has_after_display_events

  let print_computation_structure t =
    Driver.Private.running_computation t
    |> Bonsai.Private.Skeleton.Computation.of_computation
    |> Bonsai.Private.Skeleton.Computation.sanitize_for_testing
    |> Bonsai.Private.Skeleton.Computation.minimal_sexp_of_t
    |> print_s
  ;;
end
