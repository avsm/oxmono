open! Core

let%test_unit "a tuple with arbitrary types" =
  let (Ppx_anonymous_record_internal__don't_match_on_this_manually (a, b, c)
        : ([ `a ] * [ `b ] * [ `c ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
    =
    [%anon { a = 1; b = 1.; c = "1" }]
  in
  [%test_result: int * float * string] (a, b, c) ~expect:(1, 1.0, "1")
;;

let%test_unit "a single field" =
  let (Ppx_anonymous_record_internal__don't_match_on_this_manually a
        : ([ `a ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
    =
    [%anon { a = 1 }]
  in
  [%test_result: int] a ~expect:1
;;

let%test_unit "a common use case" =
  let (Ppx_anonymous_record_internal__don't_match_on_this_manually (max, min)
        : ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
    =
    [%anon { max = 1; min = -1 }]
  in
  [%test_result: int * int] (max, min) ~expect:(1, -1)
;;

let%test_unit "variables in place of values" =
  let max_value = Int.max_value in
  let min_value = Int.min_value in
  let (Ppx_anonymous_record_internal__don't_match_on_this_manually (max, min)
        : ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
    =
    [%anon { max = max_value; min = min_value }]
  in
  [%test_result: int * int] (max, min) ~expect:(Int.max_value, Int.min_value)
;;

let%test_unit "variables with the same names as the fields" =
  let max = Int.max_value in
  let min = Int.min_value in
  let (Ppx_anonymous_record_internal__don't_match_on_this_manually (max, min)
        : ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
    =
    [%anon { max; min }]
  in
  [%test_result: int * int] (max, min) ~expect:(Int.max_value, Int.min_value)
;;

let%test_unit "nested anonymous records" =
  let (Ppx_anonymous_record_internal__don't_match_on_this_manually
         ( Ppx_anonymous_record_internal__don't_match_on_this_manually
             (max_float, min_float)
         , Ppx_anonymous_record_internal__don't_match_on_this_manually (max_int, min_int)
         )
        : ( [ `float ] * [ `int ]
            , ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t
              * ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t
            )
            Ppx_anonymous_record_runtime.Anonymous_record.t)
    =
    [%anon
      { float = [%anon { max = Float.infinity; min = Float.neg_infinity }]
      ; int = [%anon { max = Int.max_value; min = Int.min_value }]
      }]
  in
  [%test_result: (int * int) * (float * float)]
    ((max_int, min_int), (max_float, min_float))
    ~expect:((Int.max_value, Int.min_value), (Float.infinity, Float.neg_infinity))
;;
