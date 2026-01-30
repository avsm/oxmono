open! Core
open Bonsai_bench_scenario

module Mini_profile = struct
  let pending = Hashtbl.create (module String)
  let times = Hashtbl.create (module String)
  let start ~label = Hashtbl.set pending ~key:label ~data:(Time_ns.now ())

  let stop ~label =
    let stop = Time_ns.now () in
    match Hashtbl.find pending label with
    | None -> failwith ("No start found for: " ^ label)
    | Some start ->
      let diff = Time_ns.diff stop start in
      Hashtbl.update times label ~f:(function
        | None -> diff
        | Some x -> Time_ns.Span.(x + diff))
  ;;

  let clear () =
    Hashtbl.clear pending;
    Hashtbl.clear times
  ;;

  let _print () =
    Hashtbl.to_alist times |> [%sexp_of: (string * Time_ns.Span.t) list] |> print_s
  ;;
end

module Incr_report = struct
  type t =
    { node_count : int
    ; nodes_created : int
    ; nodes_recomputed : int
    ; nodes_invalidated : int
    ; max_height : int
    ; max_node_id : int
    ; annotated_counts_diff : Bonsai.Private.Annotate_incr.Counts.t
    }
  [@@deriving sexp_of]

  module Start_measurement = struct
    type t =
      { nodes_created_before : int
      ; nodes_recomputed_before : int
      ; nodes_invalidated_before : int
      ; annotated_before : Bonsai.Private.Annotate_incr.Counts.t
      }
    [@@deriving sexp_of]
  end

  let start_measure () =
    let nodes_created_before = Incremental.State.num_nodes_created Ui_incr.State.t in
    let nodes_recomputed_before =
      Incremental.State.num_nodes_recomputed Ui_incr.State.t
    in
    let nodes_invalidated_before =
      Incremental.State.num_nodes_invalidated Ui_incr.State.t
    in
    let annotated_before = Bonsai.Private.Annotate_incr.Counts.current () in
    Mini_profile.start ~label:"run f";
    { Start_measurement.nodes_created_before
    ; nodes_recomputed_before
    ; nodes_invalidated_before
    ; annotated_before
    }
  ;;

  let finish_measure
    { Start_measurement.nodes_created_before
    ; nodes_recomputed_before
    ; nodes_invalidated_before
    ; annotated_before
    }
    =
    Mini_profile.stop ~label:"run f";
    let nodes_created_after = Incremental.State.num_nodes_created Ui_incr.State.t in
    let nodes_recomputed_after = Incremental.State.num_nodes_recomputed Ui_incr.State.t in
    let nodes_invalidated_after =
      Incremental.State.num_nodes_invalidated Ui_incr.State.t
    in
    let annotated_after = Bonsai.Private.Annotate_incr.Counts.current () in
    let nodes_created = nodes_created_after - nodes_created_before in
    let nodes_recomputed = nodes_recomputed_after - nodes_recomputed_before in
    let nodes_invalidated = nodes_invalidated_after - nodes_invalidated_before in
    let annotated_counts =
      Bonsai.Private.Annotate_incr.Counts.diff
        ~after:annotated_after
        ~before:annotated_before
    in
    Mini_profile.start ~label:"skeleton";
    let skeleton = Incremental_skeleton.(snapshot ~normalize:true Ui_incr.State.t) in
    Mini_profile.stop ~label:"skeleton";
    Mini_profile.start ~label:"max height";
    let max_height =
      List.max_elt
        ~compare:Int.ascending
        (List.map skeleton.nodes ~f:(fun node -> node.height))
      |> Option.value_exn
    in
    Mini_profile.stop ~label:"max height";
    let node_count = List.length skeleton.nodes in
    Mini_profile.start ~label:"max node id";
    let max_node_id =
      List.max_elt
        ~compare:Int.ascending
        (List.map skeleton.nodes ~f:(fun node ->
           node.id |> Incremental.For_analyzer.Node_id.to_int))
      |> Option.value_exn
    in
    Mini_profile.stop ~label:"max node id";
    { node_count
    ; nodes_created
    ; nodes_recomputed
    ; nodes_invalidated
    ; max_height
    ; max_node_id
    ; annotated_counts_diff = annotated_counts
    }
  ;;

  let measure f =
    let start_measurement = start_measure () in
    let result = f () in
    let report = finish_measure start_measurement in
    result, report
  ;;
end

let format_diff a b =
  let diff = b - a in
  let pct_change = Float.of_int diff /. Float.of_int a *. 100. |> Float.abs in
  if Int.equal diff 0 || Float.(pct_change < 1.)
  then "."
  else (
    let sign = if diff > 0 then "+" else "" in
    [%string
      "%{sign}%{Int.to_string_hum diff} (%{Float.to_string_hum ~decimals:0 pct_change}%)"])
;;

module Startup = struct
  let run c =
    let bonsai_node_counts = Bonsai.Debug.bonsai_node_counts c in
    let driver, incr_report =
      Incr_report.measure (fun () ->
        Bonsai_driver.create
          ~instrumentation:(Bonsai_driver.Instrumentation.default_for_test_handles ())
          ~time_source:(Bonsai.Time_source.create ~start:Time_ns.epoch)
          c)
    in
    Bonsai_driver.Expert.invalidate_observers driver;
    bonsai_node_counts, incr_report
  ;;

  let print_many reports =
    print_endline "======= Startup Incr Node Stats =======";
    Expectable.print_alist
      (fun ( _
           , { Incr_report.max_height
             ; node_count
             ; max_node_id
             ; nodes_created
             ; nodes_recomputed
             ; nodes_invalidated
             ; _
             } ) ->
        [%sexp
          { max_height : int
          ; node_count : int
          ; max_node_id : int
          ; nodes_created : int
          ; nodes_recomputed : int
          ; nodes_invalidated : int
          }])
      reports;
    print_endline "======= Startup Incr Annotated Node Counts =======";
    List.Assoc.map reports ~f:(fun (_, { annotated_counts_diff; _ }) ->
      annotated_counts_diff)
    |> Expectable.print_alist [%sexp_of: Bonsai.Private.Annotate_incr.Counts.t];
    print_endline "======= Bonsai Computation Nodes =======";
    List.Assoc.map reports ~f:(fun (bonsai_node_counts, _) ->
      Bonsai.Private.Skeleton.Counts.computation bonsai_node_counts)
    |> Expectable.print_alist [%sexp_of: Bonsai.Private.Skeleton.Counts.Computation.t];
    print_endline "======= Bonsai Value Nodes =======";
    List.Assoc.map reports ~f:(fun (bonsai_node_counts, _) ->
      Bonsai.Private.Skeleton.Counts.value bonsai_node_counts)
    |> Expectable.print_alist [%sexp_of: Bonsai.Private.Skeleton.Counts.Value.t]
  ;;

  let const_value_not_constant_folded x = Bonsai.Expert.Var.(value (create x))

  let run_and_print_compare ~computations inputs =
    List.cartesian_product inputs computations
    |> List.map ~f:(fun ((input_name, input), (comp_name, computation)) ->
      let c = computation (const_value_not_constant_folded input) in
      String.uncapitalize comp_name ^ ": " ^ input_name, run c)
    |> print_many
  ;;

  let print_many_diff ?title reports =
    let title =
      match title with
      | None -> ""
      | Some title -> [%string " (%{title})"]
    in
    print_endline [%string "======= Startup Incr Node Stats%{title} ======="];
    Expectable.print_alist
      (fun ( ( _
             , { Incr_report.max_height = mh1
               ; node_count = ncount1
               ; max_node_id = mni1
               ; nodes_created = ncreated1
               ; _
               } )
           , ( _
             , { Incr_report.max_height = mh2
               ; node_count = ncount2
               ; max_node_id = mni2
               ; nodes_created = ncreated2
               ; _
               } ) ) ->
        let max_height = format_diff mh1 mh2 in
        let node_count = format_diff ncount1 ncount2 in
        let max_node_id = format_diff mni1 mni2 in
        let nodes_created = format_diff ncreated1 ncreated2 in
        [%sexp
          { max_height : string
          ; node_count : string
          ; max_node_id : string
          ; nodes_created : string
          }])
      reports
  ;;

  let diff_pairs_incr_summary_only ?title ~computation_pairs inputs =
    List.cartesian_product inputs computation_pairs
    |> List.map
         ~f:(fun ((input_name, input), (comparison_name, computation1, computation2)) ->
           let r1 = computation1 (const_value_not_constant_folded input) |> run in
           let r2 = computation2 (const_value_not_constant_folded input) |> run in
           String.uncapitalize comparison_name ^ ": " ^ input_name, (r1, r2))
    |> print_many_diff ?title
  ;;
end

module Interaction = struct
  let run ~get_inject c interactions =
    Mini_profile.start ~label:"prepare driver";
    let time_source = Bonsai.Time_source.create ~start:Time_ns.epoch in
    let driver =
      Bonsai_driver.create
        ~instrumentation:(Bonsai_driver.Instrumentation.default_for_test_handles ())
        ~time_source
        c
    in
    let inject_action = Bonsai_driver.result driver |> get_inject in
    Mini_profile.stop ~label:"prepare driver";
    let (), incr_report =
      Incr_report.measure (fun () ->
        List.iter
          interactions
          ~f:
            (Interaction.Finalized.handle
               ~driver
               ~time_source
               ~inject_action
               ~handle_profile:(fun _ -> ())))
    in
    Bonsai_driver.Expert.invalidate_observers driver;
    incr_report
  ;;

  let run' ~get_inject ~computation ~initial ~interaction =
    (* This has to happen in an inner loop, so that we get a fresh set of vars for each
       run. *)
    let input = Input.create initial in
    let interactions = Interaction.finalize ~filter_profiles:true (interaction input) in
    run ~get_inject (computation (Input.value input)) interactions
  ;;

  let run_and_print_many
    ?(print_max_height = false)
    ?(print_node_count = true)
    ?(print_max_node_id = false)
    ?(print_num_created = true)
    ?(print_num_recomputed = true)
    ?(print_num_invalidated = true)
    ?title
    scenarios
    ~to_compare
    ~run_report
    ~format_output
    =
    Mini_profile.clear ();
    let reports =
      List.map scenarios ~f:(fun { Scenario.initial; test_name; interaction } ->
        let cells =
          List.map to_compare ~f:(fun x -> run_report ~initial ~interaction x)
        in
        test_name, cells)
    in
    Expect_test_helpers_base.expect_test_output () |> (ignore : string -> unit);
    let print_report_for_field ~flag ~f name =
      if flag
      then (
        let title =
          match title with
          | None -> ""
          | Some title -> [%string " (%{title})"]
        in
        print_endline [%string "====== %{name}%{title} ======"];
        List.Assoc.map reports ~f:(List.Assoc.map ~f:(fun data -> format_output ~f data))
        |> Expectable.print_alist [%sexp_of: (string * string) list])
    in
    print_report_for_field
      ~flag:print_max_height
      ~f:(fun { Incr_report.max_height; _ } -> max_height)
      "Max Height";
    print_report_for_field
      ~flag:print_node_count
      ~f:(fun { Incr_report.node_count; _ } -> node_count)
      "Node Count";
    print_report_for_field
      ~flag:print_max_node_id
      ~f:(fun { Incr_report.max_node_id; _ } -> max_node_id)
      "Max Node ID";
    print_report_for_field
      ~flag:print_num_created
      ~f:(fun { Incr_report.nodes_created; _ } -> nodes_created)
      "Nodes Created";
    print_report_for_field
      ~flag:print_num_recomputed
      ~f:(fun { Incr_report.nodes_recomputed; _ } -> nodes_recomputed)
      "Nodes Recomputed";
    print_report_for_field
      ~flag:print_num_invalidated
      ~f:(fun { Incr_report.nodes_invalidated; _ } -> nodes_invalidated)
      "Nodes Invalidated"
  ;;

  let run_and_print_compare
    ?print_max_height
    ?print_node_count
    ?print_max_node_id
    ?print_num_created
    ?print_num_recomputed
    ?print_num_invalidated
    ?title
    ~get_inject
    ~computations
    scenarios
    =
    run_and_print_many
      ?print_max_height
      ?print_node_count
      ?print_max_node_id
      ?print_num_created
      ?print_num_recomputed
      ?print_num_invalidated
      ?title
      scenarios
      ~to_compare:computations
      ~run_report:(fun ~initial ~interaction (comp_name, computation) ->
        let report = run' ~get_inject ~computation ~initial ~interaction in
        String.uncapitalize comp_name, report)
      ~format_output:(fun ~f report -> f report |> Int.to_string)
  ;;

  let diff_pairs
    ?print_max_height
    ?print_node_count
    ?print_max_node_id
    ?print_num_created
    ?print_num_recomputed
    ?print_num_invalidated
    ?title
    ~get_inject
    ~computation_pairs
    scenarios
    =
    run_and_print_many
      ?print_max_height
      ?print_node_count
      ?print_max_node_id
      ?print_num_created
      ?print_num_recomputed
      ?print_num_invalidated
      ?title
      scenarios
      ~to_compare:computation_pairs
      ~run_report:(fun ~initial ~interaction (name, computation1, computation2) ->
        let r1 = run' ~get_inject ~computation:computation1 ~initial ~interaction in
        let r2 = run' ~get_inject ~computation:computation2 ~initial ~interaction in
        name, (r1, r2))
      ~format_output:(fun ~f (fst, snd) -> format_diff (f fst) (f snd))
  ;;
end
