open! Core
open Bonsai
open Bonsai.Let_syntax
open Bonsai_bench_scenario

module Dynamic_num = struct
  type t =
    | Let_arr
    | Map
    | Assoc_simple
    | Assoc
  [@@deriving compare, sexp_of, enumerate]

  let name = function
    | Let_arr -> "let%arr"
    | Map -> "Bonsai.Map.map"
    | Assoc_simple -> "assoc_simple"
    | Assoc -> "assoc"
  ;;

  let transform_float v =
    (* No nice lookup tables for you! *)
    let v' = sqrt v +. 1.353454354 in
    let remainder = Float.mod_float v' 1. in
    ((v' -. (1. /. v')) ** remainder) +. (v' *. sqrt v' /. log (Float.abs v'))
  ;;

  let fold_inputs vs = Map.fold vs ~init:0. ~f:(fun ~key:_ ~data acc -> acc +. data)

  let fold_inputs' vs =
    Bonsai.Map.unordered_fold
      vs
      ~init:0.
      ~add:(fun ~key:_ ~data acc -> acc +. data)
      ~remove:(fun ~key:_ ~data acc -> acc -. data)
  ;;

  let computation config inputs (local_ graph) =
    match config with
    | Let_arr ->
      let%arr inputs in
      Map.map inputs ~f:transform_float |> fold_inputs
    | Map ->
      let mapped = Bonsai.Map.map inputs ~f:transform_float graph in
      fold_inputs' mapped graph
    | Assoc_simple ->
      let mapped =
        Bonsai.assoc
          (module Opaque_map.Key)
          inputs
          ~f:(fun _ v (local_ _graph) ->
            let%arr v in
            transform_float v)
          graph
      in
      fold_inputs' mapped graph
    | Assoc ->
      let mapped =
        Bonsai.assoc
          (module Opaque_map.Key)
          inputs
          ~f:(fun _ v (local_ graph) ->
            let state, _ = Bonsai.state () graph in
            let%arr v
            and () = state in
            transform_float v)
          graph
      in
      fold_inputs' mapped graph
  ;;

  let all_computations =
    lazy (all |> List.map ~f:(fun config -> name config, computation config))
  ;;

  let fiddle_scenario ~num_fiddles ~size ~hit_ratio_denominator
    : (float Opaque_map.t, Nothing.t) Scenario.t
    =
    let initial = Opaque_map.of_list (List.init size ~f:Float.of_int) in
    let fiddle_with_data ~round (vals : float Opaque_map.t) =
      Map.fold vals ~init:([], 0) ~f:(fun ~key:_ ~data (acc, i) ->
        let new_ =
          match i mod hit_ratio_denominator with
          | x when Int.equal x round -> data +. Random.float_range (-5.) 5.
          | _ -> data
        in
        new_ :: acc, i + 1)
      |> Tuple2.get1
      |> List.rev
      |> Opaque_map.of_list
    in
    { Scenario.initial
    ; test_name =
        [%string
          "%{size#Int}, (1/%{hit_ratio_denominator#Int}) updated %{num_fiddles#Int} times"]
    ; interaction =
        (fun input ->
          List.init num_fiddles ~f:(fun i ->
            Interaction.update_input input ~f:(fun prev ->
              fiddle_with_data ~round:(i + 1) prev))
          |> Interaction.many_with_recomputes)
    }
  ;;

  let scenarios =
    lazy
      (List.cartesian_product [ 10; 1000; 100_000 ] [ 5; 10; 50 ]
       |> List.cartesian_product [ 5 ]
       |> List.map
            ~f:
              (fun
                (num_fiddles, (size, hit_ratio_denominator))
                : (float Opaque_map.t, 'a) Scenario.t
              -> fiddle_scenario ~size ~num_fiddles ~hit_ratio_denominator))
  ;;

  let startup_inputs =
    lazy
      (List.map [ 10; 1000; 100_000 ] ~f:(fun size ->
         Int.to_string size, Opaque_map.of_list (List.init size ~f:Float.of_int)))
  ;;
end

module Switch = struct
  type t =
    | Arr_then_match of
        { uses_state : bool
        ; two_inputs : bool
        }
    | Match_sub of
        { uses_state : bool
        ; two_inputs : bool
        }
  [@@deriving compare, sexp_of, enumerate]

  let get_qualifiers ~uses_state ~two_inputs =
    match uses_state, two_inputs with
    | true, true -> " (state, two inputs)"
    | false, true -> " (two inputs)"
    | true, false -> " (state)"
    | false, false -> ""
  ;;

  let name = function
    | Arr_then_match { uses_state; two_inputs } ->
      "arr+match" ^ get_qualifiers ~uses_state ~two_inputs
    | Match_sub { uses_state; two_inputs } ->
      "match%sub" ^ get_qualifiers ~uses_state ~two_inputs
  ;;

  let computation config enabled (local_ graph) =
    let const x _ = return x in
    let with_state x graph =
      let state, _ = Bonsai.state x graph in
      state
    in
    let uses_state =
      match config with
      | Arr_then_match { uses_state; _ } -> uses_state
      | Match_sub { uses_state; _ } -> uses_state
    in
    let f =
      match uses_state with
      | true -> const
      | false -> with_state
    in
    match config with
    | Arr_then_match { two_inputs = false; _ } ->
      let%arr enabled
      and branch_true = f "true" graph
      and branch_false = f "false" graph in
      (match enabled with
       | true -> branch_true
       | false -> branch_false)
    | Arr_then_match { two_inputs = true; _ } ->
      let unused = Bonsai.Expert.Var.value (Bonsai.Expert.Var.create ()) in
      let%arr enabled
      and unused
      and branch_true = f "true" graph
      and branch_false = f "false" graph in
      (match enabled, unused with
       | true, _ -> branch_true
       | false, _ -> branch_false)
    | Match_sub { two_inputs = false; _ } ->
      (match%sub enabled with
       | true -> f "true" graph
       | false -> f "false" graph)
    | Match_sub { two_inputs = true; _ } ->
      let unused = Bonsai.Expert.Var.value (Bonsai.Expert.Var.create ()) in
      (match%sub enabled, unused with
       | true, _ -> f "true" graph
       | false, _ -> f "false" graph)
  ;;

  let all_computations =
    lazy (all |> List.map ~f:(fun config -> name config, computation config))
  ;;

  let startup_inputs = [ "Start true", true; "Start false", false ]

  let scenarios =
    List.cartesian_product [ 1; 5; 10; 100 ] Bool.all
    |> List.map ~f:(fun (num_switches, initial) ->
      { Scenario.initial
      ; test_name = [%string "Start %{initial#Bool}, switch %{num_switches#Int} times"]
      ; interaction =
          (fun input ->
            List.init num_switches ~f:(fun _ ->
              Interaction.update_input input ~f:(fun prev -> not prev))
            |> Interaction.many_with_recomputes)
      })
  ;;
end
