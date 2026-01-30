open! Base

external is_not_a_pointer : ('a : value_or_null). 'a -> bool @@ portable = "%obj_is_int"

module Array = struct
  include Array

  let[@inline] unsafe_clear_if_pointer (type a : value_or_null mod non_float) (t : a t) i =
    let e = unsafe_get t i in
    if is_not_a_pointer e
    then ()
    else (
      let e : a = Obj.magic 0 in
      unsafe_set t i e)
  ;;

  let unsafe_clear_if_pointer_range t ~from ~to_ =
    for i = from to to_ do
      unsafe_clear_if_pointer t i
    done
  ;;

  [%%template
  [@@@kind.default k' = (bits64, float64, immediate)]
  [@@@kind k = k' mod non_float]

  let[@inline] unsafe_clear_if_pointer (type a : k) (_ : a t) (_ : int) = ()
  let[@inline] unsafe_clear_if_pointer_range (type a : k) (_ : a t) ~from:_ ~to_:_ = ()]

  let%template[@kind k = immediate64] might_be_pointer =
    match Word_size.word_size with
    | W32 -> true
    | W64 -> false
  ;;

  let%template[@kind k = value_or_null] might_be_pointer = true

  [%%template
  [@@@kind.default k' = immediate64]
  [@@@kind k = k' mod non_float]

  let[@inline] unsafe_clear_if_pointer (type a : k) (t : a t) i =
    if might_be_pointer [@kind k'] then unsafe_clear_if_pointer t i
  ;;

  let[@inline] unsafe_clear_if_pointer_range (type a : k) (t : a t) ~from ~to_ =
    if might_be_pointer [@kind k'] then unsafe_clear_if_pointer_range t ~from ~to_
  ;;]

  [%%template
  [@@@kind k1 = (value_or_null, immediate64)]

  let[@kind k = (k1 & k1)] unsafe_clear_if_pointer (type a : k mod non_float) (t : a t) i =
    if might_be_pointer [@kind k1]
    then (
      let e = unsafe_get t i in
      let #(a1, a2) : #(Obj.t * Obj.t) = Obj.magic e in
      if is_not_a_pointer a1 && is_not_a_pointer a2
      then ()
      else (
        let e : a = Obj.magic #(Obj.magic 0, Obj.magic 0) in
        unsafe_set t i e))
  ;;

  let[@kind k = (k1 & k1)] unsafe_clear_if_pointer_range t ~from ~to_ =
    if might_be_pointer [@kind k1]
    then
      for i = from to to_ do
        (unsafe_clear_if_pointer [@kind k]) t i
      done
  ;;

  let[@kind k = (k1 & k1 & k1)] unsafe_clear_if_pointer
    (type a : k mod non_float)
    (t : a t)
    i
    =
    if might_be_pointer [@kind k1]
    then (
      let e = unsafe_get t i in
      let #(a1, a2, a3) : #(Obj.t * Obj.t * Obj.t) = Obj.magic e in
      if is_not_a_pointer a1 && is_not_a_pointer a2 && is_not_a_pointer a3
      then ()
      else (
        let e : a = Obj.magic #(Obj.magic 0, Obj.magic 0, Obj.magic 0) in
        unsafe_set t i e))
  ;;

  let[@kind k = (k1 & k1 & k1)] unsafe_clear_if_pointer_range t ~from ~to_ =
    if might_be_pointer [@kind k1]
    then
      for i = from to to_ do
        (unsafe_clear_if_pointer [@kind k]) t i
      done
  ;;

  let[@kind k = (k1 & k1 & k1 & k1)] unsafe_clear_if_pointer
    (type a : k mod non_float)
    (t : a t)
    i
    =
    if might_be_pointer [@kind k1]
    then (
      let e = unsafe_get t i in
      let #(a1, a2, a3, a4) : #(Obj.t * Obj.t * Obj.t * Obj.t) = Obj.magic e in
      if is_not_a_pointer a1
         && is_not_a_pointer a2
         && is_not_a_pointer a3
         && is_not_a_pointer a4
      then ()
      else (
        let e : a = Obj.magic #(Obj.magic 0, Obj.magic 0, Obj.magic 0, Obj.magic 0) in
        unsafe_set t i e))
  ;;

  let[@kind k = (k1 & k1 & k1 & k1)] unsafe_clear_if_pointer_range t ~from ~to_ =
    if might_be_pointer [@kind k1]
    then
      for i = from to to_ do
        (unsafe_clear_if_pointer [@kind k]) t i
      done
  ;;]
end
