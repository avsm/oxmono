open! Core

let%expect_test "Check [Runner.evaluate_axis_in_document_order] respects the order" =
  let input = Test_runner_small.input in
  let all_nodes =
    Xpath.Private.evaluate_axis_in_document_order Descendant_or_self [] (Root input)
    |> Sequence.concat_map ~f:(fun (trail, node) ->
      Sequence.of_list
        [ Sequence.return (trail, node)
        ; Xpath.Private.evaluate_axis_in_document_order Attribute trail node
        ; Xpath.Private.evaluate_axis_in_document_order Namespace trail node
        ]
      |> Sequence.concat)
  in
  Sequence.iter all_nodes ~f:(fun (trail, node) ->
    List.iter Xpath.Axis.all ~f:(fun axis ->
      let axis_evaluation =
        Xpath.Private.evaluate_axis_in_document_order axis trail node
        |> Sequence.map ~f:fst
        |> Sequence.to_list
      in
      let sorted = List.sort axis_evaluation ~compare:Xpath.Trail.compare in
      if not ([%equal: Xpath.Trail.t list] sorted axis_evaluation)
      then
        raise_s
          [%message
            "Bad sort"
              (axis_evaluation : (Xpath.Trail.Specifier.t * _) list list)
              (sorted : (Xpath.Trail.Specifier.t * _) list list)
              (axis : Xpath.Axis.t)]))
;;
