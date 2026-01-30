open! Core
open! Import

external magic_32 : ('a : float32) -> ('b : float32) @@ portable = "%identity"
external magic_64 : ('a : bits64) -> ('b : bits64) @@ portable = "%identity"

module Arr_impl = struct
  open Uniform_array

  (* Safely exposes ['a t] as [mutable_data with 'a] even though [Obj.t Uniform_array.t]
     has kind [value]. *)
  type 'a t : mutable_data with 'a

  external wrap : Obj.t Uniform_array.t -> 'a t @@ portable = "%identity"
  external unwrap : 'a t -> Obj.t Uniform_array.t @@ portable = "%identity"

  let[@inline] length t = length (unwrap t)
  let[@inline] unsafe_clear_if_pointer t i = unsafe_clear_if_pointer (unwrap t) i

  let[@inline] unsafe_get (type a) (t : a t) i : a =
    unsafe_get (unwrap t) i |> Obj.Expert.obj
  ;;

  let[@inline] get_mono t i = get (unwrap t) i

  let[@inline always] unsafe_set (type a) (t : a t) i (element : a) =
    unsafe_set (unwrap t) i (Obj.repr element)
  ;;

  let[@inline] unsafe_set_int_assuming_currently_int t i int =
    unsafe_set_int_assuming_currently_int (unwrap t) i int
  ;;

  let[@inline] unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    unsafe_blit ~src:(unwrap src) ~src_pos ~dst:(unwrap dst) ~dst_pos ~len
  ;;

  let[@inline] sort ~pos ~len t ~compare = sort ~pos ~len (unwrap t) ~compare
  let[@inline] unsafe_create_uninitialized ~len = wrap (unsafe_create_uninitialized ~len)

  let[@inline] unsafe_to_array_inplace__promise_not_a_float t =
    unsafe_to_array_inplace__promise_not_a_float (unwrap t)
  ;;

  let[@inline] copy t = wrap (copy (unwrap t))

  let[@inline] init (type a) n ~(f : (int -> a) @ local) : a t =
    wrap (init n ~f:(fun i -> f i |> Obj.repr)) [@nontail]
  ;;
end

module Array = struct
  include Array

  let%template[@kind k = value] [@inline] unsafe_clear_if_pointer
    (type a : k)
    (t : a array)
    i
    =
    let e = unsafe_get t i in
    let e = Obj.repr e in
    if not (Obj.is_int e)
    then (
      let e : a = Obj.magic 0 in
      unsafe_set t i e)
  ;;

  let%template[@kind k = (value & value)] [@inline] unsafe_clear_if_pointer
    (type a : k)
    (t : a array)
    i
    =
    let e = unsafe_get t i in
    let #(a1, a2) : #(Obj.t * Obj.t) = Obj.magic e in
    if not (Obj.is_int a1 && Obj.is_int a2)
    then (
      let e : a = Obj.magic #(Obj.repr 0, Obj.repr 0) in
      unsafe_set t i e)
  ;;

  let%template[@kind k = (value & value & value)] [@inline] unsafe_clear_if_pointer
    (type a : k)
    (t : a array)
    i
    =
    let e = unsafe_get t i in
    let #(a1, a2, a3) : #(Obj.t * Obj.t * Obj.t) = Obj.magic e in
    if not (Obj.is_int a1 && Obj.is_int a2 && Obj.is_int a3)
    then (
      let e : a = Obj.magic #(Obj.repr 0, Obj.repr 0, Obj.repr 0) in
      unsafe_set t i e)
  ;;

  let%template[@kind k = (value & value & value & value)] [@inline] unsafe_clear_if_pointer
    (type a : k)
    (t : a array)
    i
    =
    let e = unsafe_get t i in
    let #(a1, a2, a3, a4) : #(Obj.t * Obj.t * Obj.t * Obj.t) = Obj.magic e in
    if not (Obj.is_int a1 && Obj.is_int a2 && Obj.is_int a3 && Obj.is_int a4)
    then (
      let e : a = Obj.magic #(Obj.repr 0, Obj.repr 0, Obj.repr 0, Obj.repr 0) in
      unsafe_set t i e)
  ;;

  let%template[@kind k = immediate64] [@inline] unsafe_clear_if_pointer
    (type a : k)
    (t : a array)
    i
    =
    match Base.Int63.Private.repr with
    | Int -> ()
    | Int64 -> (unsafe_clear_if_pointer [@kind value]) t i
  ;;

  let%template[@kind k = (immediate64 & immediate64)] [@inline] unsafe_clear_if_pointer
    (type a : k)
    (t : a array)
    i
    =
    match Base.Int63.Private.repr with
    | Int -> ()
    | Int64 -> (unsafe_clear_if_pointer [@kind value & value]) t i
  ;;

  let%template[@kind k = (immediate64 & immediate64 & immediate64)] [@inline] unsafe_clear_if_pointer
    (type a : k)
    (t : a array)
    i
    =
    match Base.Int63.Private.repr with
    | Int -> ()
    | Int64 -> (unsafe_clear_if_pointer [@kind value & value & value]) t i
  ;;

  let%template[@kind k = (immediate64 & immediate64 & immediate64 & immediate64)]
              [@inline] unsafe_clear_if_pointer
    (type a : k)
    (t : a array)
    i
    =
    match Base.Int63.Private.repr with
    | Int -> ()
    | Int64 -> (unsafe_clear_if_pointer [@kind value & value & value & value]) t i
  ;;

  let%template[@kind k = (value & value, immediate64 & immediate64)] unsafe_create_uninitialized
    (type a : k)
    ~len
    : a t
    =
    let v : a = Obj.magic #(0, 0) in
    create ~len v
  ;;

  let%template[@kind k = (value & value & value, immediate64 & immediate64 & immediate64)] unsafe_create_uninitialized
    (type a : k)
    ~len
    : a t
    =
    let v : a = Obj.magic #(0, 0, 0) in
    create ~len v
  ;;

  let%template[@kind
                k
                = ( value & value & value & value
                  , immediate64 & immediate64 & immediate64 & immediate64 )] unsafe_create_uninitialized
    (type a : k)
    ~len
    : a t
    =
    let v : a = Obj.magic #(0, 0, 0, 0) in
    create ~len v
  ;;

  let%template[@kind k = immediate64] unsafe_create_uninitialized (type a : k) ~len : a t =
    let v : a = Obj.magic 0 in
    create ~len v
  ;;
end

module%template
  [@kind
    k
    = ( immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )] Arr_impl =
struct
  open Array

  type ('a : k) t = 'a Array.t

  let[@inline] unsafe_create_uninitialized ~len =
    (unsafe_create_uninitialized [@kind k]) ~len
  ;;

  let init len ~f =
    let t = (unsafe_create_uninitialized [@kind k]) ~len in
    for i = 0 to len - 1 do
      unsafe_set t i (f i)
    done;
    t
  ;;

  let[@inline] unsafe_get (t : _ t) i = unsafe_get t i
  let[@inline] unsafe_set (t : _ t) i e = unsafe_set t i e

  let unsafe_blit ~(src : _ t) ~src_pos ~(dst : _ t) ~dst_pos ~len =
    let blit_one i = unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i)) in
    if phys_equal src dst && src_pos < dst_pos
    then
      for i = len - 1 downto 0 do
        (blit_one [@inlined]) i
      done
    else
      for i = 0 to len - 1 do
        (blit_one [@inlined]) i
      done
  ;;

  let copy t =
    let t' = (unsafe_create_uninitialized [@kind k]) ~len:(length t) in
    unsafe_blit ~src:t ~src_pos:0 ~dst:t' ~dst_pos:0 ~len:(length t);
    t'
  ;;

  let[@inline] length t = length t
  let get_mono = get

  let[@inline] unsafe_clear_if_pointer (t : _ t) i =
    (unsafe_clear_if_pointer [@kind k]) t i
  ;;
end

module%template [@kind bits64] Arr_impl = struct
  include I64.Array

  type ('a : bits64) t = 'a I64.Array.t

  let get_mono = get

  let[@inline] unsafe_create_uninitialized (type a : bits64) ~len : a t =
    unsafe_create_uninitialized ~len
  ;;
end

module%template [@kind float32] Arr_impl = struct
  include F32.Array

  type ('a : float32) t = 'a F32.Array.t

  let get_mono = get

  let[@inline] unsafe_create_uninitialized (type a : float32) ~len : a t =
    unsafe_create_uninitialized ~len
  ;;
end

[%%template
[@@@kind.default
  k
  = ( value
    , immediate64
    , value & value
    , immediate64 & immediate64
    , value & value & value
    , immediate64 & immediate64 & immediate64
    , value & value & value & value
    , immediate64 & immediate64 & immediate64 & immediate64
    , float32
    , bits64 )]

open Arr_impl [@kind k]

type 'a arr_impl = 'a t]

let sexp_of_out_of_range_element (type a) (a : a) =
  let imm : int = Obj.magic a in
  Sexp.Atom (sprintf "_%d" imm)
;;

let%template[@kind k = (value & value)] sexp_of_out_of_range_element (type a : k) (a : a) =
  let #(imm1, imm2) : #(int * int) = Obj.magic a in
  Sexp.Atom (sprintf "(_%d, _%d)" imm1 imm2)
;;

let%template[@kind k = (value & value & value)] sexp_of_out_of_range_element
  (type a : k)
  (a : a)
  =
  let #(imm1, imm2, imm3) : #(int * int * int) = Obj.magic a in
  Sexp.Atom (sprintf "(_%d, _%d, _%d)" imm1 imm2 imm3)
;;

let%template[@kind k = (value & value & value & value)] sexp_of_out_of_range_element
  (type a : k)
  (a : a)
  =
  let #(imm1, imm2, imm3, imm4) : #(int * int * int * int) = Obj.magic a in
  Sexp.Atom (sprintf "(_%d, _%d, _%d, _%d)" imm1 imm2 imm3 imm4)
;;

let%template[@kind k = (immediate64 & immediate64)] [@inline] sexp_of_out_of_range_element
  (type a : k)
  (a : a)
  =
  (sexp_of_out_of_range_element [@kind value & value]) a
;;

let%template[@kind k = immediate64] [@inline] sexp_of_out_of_range_element
  (type a : k)
  (a : a)
  =
  (sexp_of_out_of_range_element [@kind value]) a
;;

let%template[@kind k = (immediate64 & immediate64 & immediate64)] [@inline] sexp_of_out_of_range_element
  (type a : k)
  (a : a)
  =
  (sexp_of_out_of_range_element [@kind value & value & value]) a
;;

let%template[@kind k = (immediate64 & immediate64 & immediate64 & immediate64)] [@inline] sexp_of_out_of_range_element
  (type a : k)
  (a : a)
  =
  (sexp_of_out_of_range_element [@kind value & value & value & value]) a
;;

let%template[@kind float32] sexp_of_out_of_range_element (type a : float32) (a : a) =
  let imm : float32# = magic_32 a in
  Sexp.Atom [%string "_%{imm#F32}"]
;;

let%template[@kind bits64] sexp_of_out_of_range_element (type a : bits64) (a : a) =
  let imm : i64 = magic_64 a in
  Sexp.Atom [%string "_%{imm#I64}"]
;;

let out_of_range_invariant (type a : value) (element : a) =
  assert (Obj.repr element |> Obj.is_int)
;;

let%template[@kind k = (value & value)] out_of_range_invariant (type a : k) (e : a) =
  let #(a1, a2) : #(Obj.t * Obj.t) = Obj.magic e in
  out_of_range_invariant a1;
  out_of_range_invariant a2
;;

let%template[@kind k = (value & value & value)] out_of_range_invariant
  (type a : k)
  (e : a)
  =
  let #(a1, a2, a3) : #(Obj.t * Obj.t * Obj.t) = Obj.magic e in
  out_of_range_invariant a1;
  out_of_range_invariant a2;
  out_of_range_invariant a3
;;

let%template[@kind k = (value & value & value & value)] out_of_range_invariant
  (type a : k)
  (e : a)
  =
  let #(a1, a2, a3, a4) : #(Obj.t * Obj.t * Obj.t * Obj.t) = Obj.magic e in
  out_of_range_invariant a1;
  out_of_range_invariant a2;
  out_of_range_invariant a3;
  out_of_range_invariant a4
;;

let%template[@kind k = immediate64] [@inline] out_of_range_invariant (type a : k) (e : a) =
  (out_of_range_invariant [@kind value]) e
;;

let%template[@kind k = (immediate64 & immediate64)] [@inline] out_of_range_invariant
  (type a : k)
  (e : a)
  =
  (out_of_range_invariant [@kind value & value]) e
;;

let%template[@kind k = (immediate64 & immediate64 & immediate64)] [@inline] out_of_range_invariant
  (type a : k)
  (e : a)
  =
  (out_of_range_invariant [@kind value & value & value]) e
;;

let%template[@kind k = (immediate64 & immediate64 & immediate64 & immediate64)] [@inline] out_of_range_invariant
  (type a : k)
  (e : a)
  =
  (out_of_range_invariant [@kind value & value & value & value]) e
;;

let%template[@kind float32] out_of_range_invariant _element = ()
let%template[@kind bits64] out_of_range_invariant _element = ()

[%%template
[@@@kind.default
  k
  = ( float32
    , bits64
    , value
    , immediate64
    , value & value
    , immediate64 & immediate64
    , value & value & value
    , immediate64 & immediate64 & immediate64
    , value & value & value & value
    , immediate64 & immediate64 & immediate64 & immediate64 )]

module [@kind] Arr = Arr_impl [@kind k]

type ('a : k) t =
  { mutable arr : 'a Arr.t
  ; mutable length : int
  ; mutable capacity : int
  (** Invariant: [capacity = Arr.length arr]. We maintain it here to eliminate an
      indirection when accessing long arrays. *)
  }

let unsafe_set_length t len = t.length <- len
let capacity t = t.capacity
let length t = t.length

let[@kind] check_capacity capacity =
  if capacity < 0 then invalid_argf "Vec: negative capacity %d" capacity ()
;;

(* [initial_capacity] is mostly arbitrary, but it does make our array take one header
   word + 7 data words = 8 words * 8 bytes = 64 bytes = one cacheline by default. (Of
   course, there's no alignment guarantee.) *)
let create ?(initial_capacity = 7) () =
  check_capacity initial_capacity;
  { arr = Arr.unsafe_create_uninitialized ~len:initial_capacity
  ; length = 0
  ; capacity = initial_capacity
  }
;;

let unsafe_create_uninitialized ~len:n =
  check_capacity n;
  { arr = Arr.unsafe_create_uninitialized ~len:n; length = n; capacity = n }
;;

let init n ~f =
  check_capacity n;
  { arr = Arr.init n ~f; length = n; capacity = n }
;;

let copy t = { arr = Arr.copy t.arr; length = t.length; capacity = t.capacity }

let[@inline always] unsafe_get (type a : k) (t : (a t[@kind k])) i : a =
  Arr.unsafe_get t.arr i
[@@mode m = (local, global)]
;;

let[@inline always] unsafe_set (type a : k) (t : (a t[@kind k])) i (element : a) =
  Arr.unsafe_set t.arr i element
;;

let[@inline always] unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  Arr.unsafe_blit ~src:src.arr ~src_pos ~dst:dst.arr ~dst_pos ~len
;;

let[@inline always] max_index t = t.length - 1

let grow_capacity_to_exactly t ~capacity =
  let arr = Arr.unsafe_create_uninitialized ~len:capacity in
  for i = 0 to (max_index [@kind k]) t do
    Arr.unsafe_set arr i (Arr.unsafe_get t.arr i)
  done;
  t.arr <- arr;
  t.capacity <- capacity
;;

let[@kind] growth_factor = 2

let grow_capacity_once t =
  (grow_capacity_to_exactly [@kind k]) t ~capacity:(Int.max 1 t.capacity * growth_factor)
;;

let grow_capacity_to_at_least t ~capacity:target_capacity =
  assert (growth_factor = 2);
  if t.capacity < target_capacity
  then
    (grow_capacity_to_exactly [@kind k])
      t
      ~capacity:(Int.ceil_pow2 (Int.max 1 target_capacity))
;;]

module With_structure_details = struct
  [%%template
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  type nonrec 'a t = ('a t[@kind k])

  let sexp_of_t (type a : k) (sexp_of_a : a -> Sexp.t) (t : (a t[@kind k])) =
    let { arr; length; capacity } : (_ t[@kind k]) = t in
    let module Arr = Arr_impl [@kind k] in
    let elements =
      Uniform_array.init (Arr.length arr) ~f:(fun i ->
        let element = Arr.unsafe_get arr i in
        (* Only the first [length] elements can safely be given to [sexp_of_a]. *)
        if i < length
        then element |> sexp_of_a
        else element |> (sexp_of_out_of_range_element [@kind k]))
    in
    [%sexp { elements : Sexp.t Uniform_array.t; length : int; capacity : int }]
  ;;]
end

let%template[@kind
              k
              = ( float32
                , bits64
                , value
                , immediate64
                , value & value
                , immediate64 & immediate64
                , value & value & value
                , immediate64 & immediate64 & immediate64
                , value & value & value & value
                , immediate64 & immediate64 & immediate64 & immediate64 )] invariant
  (type a : k)
  (a_inv : a -> unit)
  (t : (a t[@kind k]))
  =
  Invariant.invariant t [%sexp_of: (_ With_structure_details.t[@kind k])] (fun () ->
    let module Arr = Arr_impl [@kind k] in
    let { capacity; length; arr } : (_ t[@kind k]) = t in
    if capacity <> Arr.length t.arr
    then
      raise_s
        [%message
          "capacity should equal Option_array length"
            (capacity : int)
            (Arr.length t.arr : int)];
    if capacity < 0 then raise_s [%message "negative capacity" (capacity : int)];
    if length > capacity
    then
      raise_s
        [%message
          "length shouldn't be more than capacity" (length : int) (capacity : int)];
    for pos = 0 to length - 1 do
      a_inv (Arr.unsafe_get arr pos)
    done;
    for pos = length to capacity - 1 do
      Arr.get_mono arr pos |> (out_of_range_invariant [@kind k])
    done)
;;

let[@inline always] unsafe_clear_pointer_at (t : _ t) pos =
  Arr_impl.unsafe_clear_if_pointer t.arr pos
;;

let%template[@inline always]
            [@kind
              k
              = ( immediate64
                , value & value
                , immediate64 & immediate64
                , value & value & value
                , immediate64 & immediate64 & immediate64
                , value & value & value & value
                , immediate64 & immediate64 & immediate64 & immediate64 )] unsafe_clear_pointer_at
  (t : (_ t[@kind k]))
  pos
  =
  let module Arr_impl = Arr_impl [@kind k] in
  Arr_impl.unsafe_clear_if_pointer t.arr pos
;;

let%template[@inline always] [@kind float32] unsafe_clear_pointer_at
  (_ : (_ t[@kind float32]))
  _
  : unit
  =
  ()
;;

let%template[@inline always] [@kind bits64] unsafe_clear_pointer_at
  (_ : (_ t[@kind bits64]))
  _
  : unit
  =
  ()
;;

module Arr = Arr_impl

let sort (type a) ?pos ?len t ~(compare : a -> a -> int) =
  let compare : Obj.t -> Obj.t -> int = Obj.magic compare in
  (* [Uniform_array] checks this but has an overestimate of our length. *)
  let pos, len =
    Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
  in
  Arr.sort ~pos ~len t.arr ~compare
;;

module Expert = struct
  [%%template
  [@@@kind.default k = (float32, bits64, value)]

  let[@inline always] unsafe_inner (t : (_ t[@kind k])) = t.arr]
end

let[@inline always] unsafe_set_imm (type a : immediate64) (t : a t) i (element : a) =
  Arr.unsafe_set_int_assuming_currently_int t.arr i (Obj.magic element)
;;
