open! Core

let%test_unit "a tuple with arbitrary types" =
  let [%anon? { a; b; c }] =
    (Ppx_anonymous_record_runtime.Anonymous_record.Private.create (1, 1., "1")
     : ([ `a ] * [ `b ] * [ `c ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
  in
  [%test_result: int * float * string] (a, b, c) ~expect:(1, 1.0, "1")
;;

let%test_unit "ignore a field" =
  let [%anon? { a; b; c = _ }] =
    (Ppx_anonymous_record_runtime.Anonymous_record.Private.create (1, 1., "1")
     : ([ `a ] * [ `b ] * [ `c ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
  in
  [%test_result: int * float] (a, b) ~expect:(1, 1.0)
;;

let%test_unit "a single field" =
  let [%anon? { a }] =
    (Ppx_anonymous_record_runtime.Anonymous_record.Private.create 1
     : ([ `a ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
  in
  [%test_result: int] a ~expect:1
;;

let%test_unit "a common use case" =
  let [%anon? { max; min }] =
    (Ppx_anonymous_record_runtime.Anonymous_record.Private.create (1, -1)
     : ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
  in
  [%test_result: int * int] (max, min) ~expect:(1, -1)
;;

let%test_unit "rename fields" =
  let [%anon? { max = max_value; min = min_value }] =
    (Ppx_anonymous_record_runtime.Anonymous_record.Private.create
       (Int.max_value, Int.min_value)
     : ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
  in
  [%test_result: int * int] (max_value, min_value) ~expect:(Int.max_value, Int.min_value)
;;

let%test_unit "a function argument pattern" =
  let f [%anon? { a; b; c }] = a + int_of_float b + int_of_string c in
  let result =
    f
      (Ppx_anonymous_record_runtime.Anonymous_record.Private.create (1, 1., "1")
       : ([ `a ] * [ `b ] * [ `c ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
  in
  [%test_result: int] result ~expect:3
;;

let%test_unit "multiple arguments" =
  let f [%anon? { a; b; c }] d = a + int_of_float b + int_of_string c + d in
  let result =
    f
      (Ppx_anonymous_record_runtime.Anonymous_record.Private.create (1, 1., "1")
       : ([ `a ] * [ `b ] * [ `c ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
      1
  in
  [%test_result: int] result ~expect:4
;;

let%test_unit "nested anonymous records" =
  let [%anon?
        { float = [%anon? { max = max_float; min = min_float }]
        ; int = [%anon? { max = max_int; min = min_int }]
        }]
    =
    (Ppx_anonymous_record_runtime.Anonymous_record.Private.create
       ( (Ppx_anonymous_record_runtime.Anonymous_record.Private.create
            (Float.infinity, Float.neg_infinity)
          : ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
       , (Ppx_anonymous_record_runtime.Anonymous_record.Private.create
            (Int.max_value, Int.min_value)
          : ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t) )
     : ( [ `float ] * [ `int ]
         , ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t
           * ([ `max ] * [ `min ], _) Ppx_anonymous_record_runtime.Anonymous_record.t )
         Ppx_anonymous_record_runtime.Anonymous_record.t)
  in
  [%test_result: (int * int) * (float * float)]
    ((max_int, min_int), (max_float, min_float))
    ~expect:((Int.max_value, Int.min_value), (Float.infinity, Float.neg_infinity))
;;
