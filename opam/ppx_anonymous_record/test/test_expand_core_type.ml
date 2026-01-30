open! Core

module type Anonymous_record = sig
  type t = [%anon { a : int; b : float; c : string }]

  val t : t
end

module Tuple : Anonymous_record = struct
  type t =
    ( [ `a ] * [ `b ] * [ `c ]
      , int * float * string )
      Ppx_anonymous_record_runtime.Anonymous_record.t

  let t =
    (Ppx_anonymous_record_runtime.Anonymous_record.Private.create (1, 1., "1")
     : ([ `a ] * [ `b ] * [ `c ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
  ;;
end

module Anonymous_record : Anonymous_record = struct
  type t = [%anon { a : int; b : float; c : string }]

  let t : t = ([%anon { a = 1; b = 1.; c = "1" }] :> Tuple.t)
end

let%test_unit "anonymous record" =
  let [%anon? { a; b; c }] = Anonymous_record.t in
  [%test_result: int * float * string] (a, b, c) ~expect:(1, 1.0, "1")
;;

module type Anonymous_record_with_one_field = sig
  type t = [%anon { a : int }]

  val t : t
end

module Tuple_with_one_field : Anonymous_record_with_one_field = struct
  type t = ([ `a ], int) Ppx_anonymous_record_runtime.Anonymous_record.t

  let t =
    (Ppx_anonymous_record_runtime.Anonymous_record.Private.create 1
     : ([ `a ], _) Ppx_anonymous_record_runtime.Anonymous_record.t)
  ;;
end

module Anonymous_record_with_one_field : Anonymous_record_with_one_field = struct
  type t = [%anon { a : int }]

  let t : t = ([%anon { a = 1 }] :> Tuple_with_one_field.t)
end

let%test_unit "anonymous record with one field" =
  let [%anon? { a }] = Anonymous_record_with_one_field.t in
  [%test_result: int] a ~expect:1
;;

module type Polymorphic_anonymous_record = sig
  type 'a t = [%anon { option : 'a option; list : 'a list }]

  val create : 'a -> 'a t
end

module Polymorphic_tuple : Polymorphic_anonymous_record = struct
  type 'a t =
    (* Tuples are sorted lexicographically *)
    ( [ `list ] * [ `option ]
      , 'a list * 'a option )
      Ppx_anonymous_record_runtime.Anonymous_record.t

  let create a
    : ([ `list ] * [ `option ], _) Ppx_anonymous_record_runtime.Anonymous_record.t
    =
    Ppx_anonymous_record_runtime.Anonymous_record.Private.create ([ a ], Some a)
  ;;
end

module Polymorphic_anonymous_record : Polymorphic_anonymous_record = struct
  type 'a t = [%anon { option : 'a option; list : 'a list }]

  let create : 'a -> 'a t =
    fun a -> ([%anon { list = [ a ]; option = Some a }] :> 'a Polymorphic_tuple.t)
  ;;
end

let%test_unit "int polymorphic anonymous record" =
  let [%anon? { list; option }] = Polymorphic_anonymous_record.create 1 in
  [%test_result: int list * int option] (list, option) ~expect:([ 1 ], Some 1)
;;

let%test_unit "float polymorphic anonymous record" =
  let [%anon? { list; option }] = Polymorphic_anonymous_record.create 1. in
  [%test_result: float list * float option] (list, option) ~expect:([ 1. ], Some 1.)
;;

let%test_unit "string polymorphic anonymous record" =
  let [%anon? { list; option }] = Polymorphic_anonymous_record.create "1" in
  [%test_result: string list * string option] (list, option) ~expect:([ "1" ], Some "1")
;;

module type Nested_anonymous_record = sig
  type t =
    [%anon
      { float : [%anon { max : float; min : float }]
      ; int : [%anon { max : int; min : int }]
      }]

  val t : t
end

module Nested_tuple : Nested_anonymous_record = struct
  type t =
    ( [ `float ] * [ `int ]
      , ( [ `max ] * [ `min ]
          , float * float )
          Ppx_anonymous_record_runtime.Anonymous_record.t
        * ([ `max ] * [ `min ], int * int) Ppx_anonymous_record_runtime.Anonymous_record.t
      )
      Ppx_anonymous_record_runtime.Anonymous_record.t

  let t =
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
  ;;
end

module Nested_anonymous_record : Nested_anonymous_record = struct
  type t =
    [%anon
      { float : [%anon { max : float; min : float }]
      ; int : [%anon { max : int; min : int }]
      }]

  let t : t =
    ([%anon
       { float = [%anon { max = Float.infinity; min = Float.neg_infinity }]
       ; int = [%anon { max = Int.max_value; min = Int.min_value }]
       }]
      :> Nested_tuple.t)
  ;;
end

let%test_unit "nested anonymous record" =
  let [%anon?
        { float = [%anon? { max = max_float; min = min_float }]
        ; int = [%anon? { max = max_int; min = min_int }]
        }]
    =
    Nested_anonymous_record.t
  in
  [%test_result: (int * int) * (float * float)]
    ((max_int, min_int), (max_float, min_float))
    ~expect:((Int.max_value, Int.min_value), (Float.infinity, Float.neg_infinity))
;;
