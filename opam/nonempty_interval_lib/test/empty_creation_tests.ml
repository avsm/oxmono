open! Core
open! Quickcheck.Generator.Let_syntax

let disjoint_nonempty_intervals_of_int_list l =
  let rec inner acc = function
    | hd :: hd' :: tl ->
      let acc = Interval.Nonempty.create_exn hd hd' :: acc in
      inner acc tl
    | [ _ ] | [] -> acc
  in
  inner [] l
;;

let gen_disjoint_intervals =
  let%bind length = Quickcheck.Generator.small_non_negative_int in
  let%map generated_list =
    List.gen_with_length length (Int.gen_incl Int.min_value Int.max_value)
  in
  generated_list
  (* Unique, ordered list elements to ensure disjoint intervals. *)
  |> List.dedup_and_sort ~compare:Int.ascending
  |> disjoint_nonempty_intervals_of_int_list
;;

let%test_unit "[list_intersect]'" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 gen_disjoint_intervals gen_disjoint_intervals)
    ~sexp_of:[%sexp_of: Interval.Nonempty.t list * Interval.Nonempty.t list]
    ~trials:100_000
    ~f:(fun (ts, ts') ->
      Interval.Nonempty.list_intersect ts ts'
      |> List.iter ~f:(fun nonempty_interval ->
        if nonempty_interval |> Interval.Nonempty.to_interval |> Interval.is_empty
        then
          raise_s
            [%message
              "[list_intersect] returning a list with an empty interval: "
                (nonempty_interval : Interval.Nonempty.t)]))
;;
