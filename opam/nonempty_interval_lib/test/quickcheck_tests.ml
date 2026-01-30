open! Core

let%test_unit "[create_exn]" =
  Quickcheck.test
    Common.interval_and_nonempty_interval
    ~sexp_of:[%sexp_of: Interval.t * Interval.Nonempty.t]
    ~f:(fun (interval, nonempty_interval) ->
      [%test_eq: Interval.t] interval (Interval.Nonempty.to_interval nonempty_interval))
;;

let%test_unit "[intersect]" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2
       Common.interval_and_nonempty_interval
       Common.interval_and_nonempty_interval)
    ~sexp_of:
      [%sexp_of: (Interval.t * Interval.Nonempty.t) * (Interval.t * Interval.Nonempty.t)]
    ~f:(fun ((interval, nonempty_interval), (interval', nonempty_interval')) ->
      [%test_eq: Interval.t]
        (Interval.intersect interval interval')
        (Interval.Nonempty.intersect nonempty_interval nonempty_interval'
         |> Option.value_map ~default:Interval.empty ~f:Interval.Nonempty.to_interval))
;;

let%test_unit "[bound]" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2
       Common.interval_and_nonempty_interval
       (Int.gen_incl Int.min_value Int.max_value))
    ~sexp_of:[%sexp_of: (Interval.t * Interval.Nonempty.t) * int]
    ~f:(fun ((interval, nonempty_interval), n) ->
      [%test_eq: int]
        (Interval.bound interval n |> Option.value_exn)
        (Interval.Nonempty.bound nonempty_interval n))
;;

let%test_unit "[convex_hull]" =
  Quickcheck.test
    (List.gen_non_empty Common.interval_and_nonempty_interval)
    ~sexp_of:[%sexp_of: (Interval.t * Interval.Nonempty.t) list]
    ~f:(fun interval_and_nonempty_interval_list ->
      let interval_list, nonempty_interval_list =
        List.unzip interval_and_nonempty_interval_list
      in
      let nonempty_interval_nonempty_list =
        Nonempty_list.of_list_exn nonempty_interval_list
      in
      [%test_eq: Interval.t]
        (Interval.convex_hull interval_list)
        (Interval.Nonempty.convex_hull nonempty_interval_nonempty_list
         |> Interval.Nonempty.to_interval))
;;

let%test_unit "[compare_value]" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2
       Common.interval_and_nonempty_interval
       (Int.gen_incl Int.min_value Int.max_value))
    ~sexp_of:[%sexp_of: (Interval.t * Interval.Nonempty.t) * Int.t]
    ~f:(fun ((interval, nonempty_interval), n) ->
      [%test_eq: Compared.t]
        (Interval.compare_value interval n)
        (Interval.Nonempty.compare_value nonempty_interval n :> Compared.t))
;;
