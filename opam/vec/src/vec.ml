open! Core
open! Import

module With_integer_index = struct
  include Kernel

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

  let is_sorted t ~compare =
    (* This is a copy-paste from [Array.is_sorted]. *)
    let i = ref ((length [@kind k]) t - 1) in
    let result = ref true in
    while !i > 0 && !result do
      let elt_i = (unsafe_get [@kind k]) t !i in
      let elt_i_minus_1 = (unsafe_get [@kind k]) t (!i - 1) in
      if compare elt_i_minus_1 elt_i > 0 then result := false;
      decr i
    done;
    !result
  ;;

  let is_sorted_strictly t ~compare =
    (* This is a copy-paste from [Array.is_sorted_strictly]. *)
    let i = ref ((length [@kind k]) t - 1) in
    let result = ref true in
    while !i > 0 && !result do
      let elt_i = (unsafe_get [@kind k]) t !i in
      let elt_i_minus_1 = (unsafe_get [@kind k]) t (!i - 1) in
      if compare elt_i_minus_1 elt_i >= 0 then result := false;
      decr i
    done;
    !result
  ;;]

  include%template Binary_searchable.Make1 [@modality portable] (struct
      type nonrec 'a t = 'a t

      let length = length
      let get = unsafe_get
    end)

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

  let next_free_index = (length [@kind k])

  let[@cold] raise__bad_index (t : (_ t[@kind k])) i ~op : unit =
    match
      raise_s
        [%message
          "tried to access vec out of bounds"
            (t : (_ With_structure_details.t[@kind k]))
            (i : int)
            (op : string)]
    with
    | (_ : Nothing.t) -> .
  ;;

  (* Tailcalls to raising functions are to be avoided, as the stack traces are much worse.
     Instead, we try really hard to inline wrapper functions that just perform non-tail
     calls to the raising functions.
  *)
  let[@inline always] raise__bad_index t i ~op : unit =
    (raise__bad_index [@kind k]) t i ~op [@nontail]
  ;;

  let[@inline always] check_index t i ~op =
    if i < 0 || i >= (length [@kind k]) t then (raise__bad_index [@kind k]) t i ~op
  ;;

  let get (type a : k) (t : (a t[@kind k])) i : a =
    (check_index [@kind k]) t i ~op:"get";
    (unsafe_get [@kind k]) t i
  ;;

  let set t i element =
    (check_index [@kind k]) t i ~op:"set";
    (unsafe_set [@kind k]) t i element
  ;;]

  let maybe_get t i = if i < 0 || i >= length t then None else Some (unsafe_get t i)

  let maybe_get_local t i =
    if i < 0 || i >= length t then None else exclave_ Some { global = unsafe_get t i }
  ;;

  let maybe_get_or_null t i =
    if i < 0 || i >= length t then Null else This (unsafe_get t i)
  ;;

  let last t =
    let i = length t - 1 in
    if i >= 0 then This (unsafe_get t i) else Null
  ;;

  let set_imm (type a : immediate64) (t : a t) i (element : a) : unit =
    check_index t i ~op:"set_imm";
    unsafe_set_imm t i element
  ;;

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

  let[@inline always] push_back__we_know_we_have_space t element =
    let length = (length [@kind k]) t in
    (unsafe_set [@kind k]) t length element;
    (unsafe_set_length [@kind k]) t (length + 1)
  ;;

  let push_back_index t element =
    let length = (length [@kind k]) t in
    if length = (capacity [@kind k]) t then (grow_capacity_once [@kind k]) t;
    (push_back__we_know_we_have_space [@kind k]) t element;
    length
  ;;

  let[@inline always] push_back t element =
    let (_ : int) = (push_back_index [@kind k]) t element in
    ()
  ;;]

  let[@inline always] push_back__we_know_we_have_space_imm t element =
    let length = length t in
    unsafe_set_imm t length element;
    unsafe_set_length t (length + 1)
  ;;

  let push_back_index_imm t element =
    let length = length t in
    if length = capacity t then grow_capacity_once t;
    push_back__we_know_we_have_space_imm t element;
    length
  ;;

  let[@inline always] push_back_imm t element =
    let (_ : int) = push_back_index_imm t element in
    ()
  ;;

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

  let remove_exn t i =
    if i < 0 || i >= (length [@kind k]) t
    then (raise__bad_index [@kind k]) t i ~op:"remove_exn";
    let new_length = (length [@kind k]) t - 1 in
    (* As per the ocaml stdlib documentation, blitting with src and dst overlapping is
       safe.
       https://github.com/ocaml-flambda/flambda-backend/blob/main/ocaml/stdlib/array.mli#L143
    *)
    (unsafe_blit [@kind k])
      ~src:t
      ~src_pos:(i + 1)
      ~dst:t
      ~dst_pos:i
      ~len:((length [@kind k]) t - i - 1);
    (unsafe_set_length [@kind k]) t new_length
  ;;

  let[@inline always] unsafe_peek_back_exn t =
    (unsafe_get [@kind k]) t ((max_index [@kind k]) t)
  ;;

  let peek_back_exn t =
    let length = (length [@kind k]) t in
    if length <= 0 then (raise__bad_index [@kind k]) t length ~op:"peek_back";
    (unsafe_peek_back_exn [@kind k]) t
  ;;]

  let peek_back t = if length t <= 0 then Null else This (unsafe_peek_back_exn t)

  let[@inline always] pop_back_unit_imm_exn (type a : immediate64) (t : a t) =
    let pos = max_index t in
    if pos < 0 then raise__bad_index t (length t) ~op:"pop_back_unit_imm_exn";
    unsafe_set_length t pos
  ;;

  let pop_back_imm_exn t =
    let e = peek_back_exn t in
    pop_back_unit_imm_exn t;
    e
  ;;

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

  let[@inline always] pop_back_unit_exn t =
    let pos = (max_index [@kind k]) t in
    if pos < 0
    then (raise__bad_index [@kind k]) t ((length [@kind k]) t) ~op:"pop_back_unit_exn";
    (* Don't leak the value. *)
    (unsafe_clear_pointer_at [@kind k]) t pos;
    (unsafe_set_length [@kind k]) t pos
  ;;

  let pop_back_exn t =
    let e = (peek_back_exn [@kind k]) t in
    (pop_back_unit_exn [@kind k]) t;
    e
  ;;

  let[@inline never] grow_to_unchecked t ~len ~default =
    (grow_capacity_to_at_least [@kind k]) t ~capacity:len;
    for i = (length [@kind k]) t to len - 1 do
      (unsafe_set [@kind k]) t i default
    done;
    (unsafe_set_length [@kind k]) t len
  ;;

  let[@inline always] grow_to t ~len ~default =
    if len > (length [@kind k]) t then (grow_to_unchecked [@kind k]) t ~len ~default
  ;;

  let grow_to_include t idx ~default = (grow_to [@kind k]) t ~len:(idx + 1) ~default

  let grow_to' t ~len ~default =
    if len > (length [@kind k]) t
    then (
      (grow_capacity_to_at_least [@kind k]) t ~capacity:len;
      for i = (length [@kind k]) t to len - 1 do
        (unsafe_set [@kind k]) t i (default i)
      done;
      (unsafe_set_length [@kind k]) t len)
  ;;

  let grow_to_include' t idx ~default = (grow_to' [@kind k]) t ~len:(idx + 1) ~default

  let shrink_to t ~len =
    if len < 0
    then (raise__bad_index [@kind k]) t len ~op:"shrink_to"
    else if len < (length [@kind k]) t
    then (
      for i = len to (max_index [@kind k]) t do
        (unsafe_clear_pointer_at [@kind k]) t i
      done;
      (unsafe_set_length [@kind k]) t len)
  ;;]

  let pop_back t =
    let e = peek_back t in
    Or_null.iter e ~f:(fun _ -> pop_back_unit_exn t);
    e
  ;;

  let shrink_to_imm (type a : immediate64) (t : a t) ~len =
    if len < 0
    then raise__bad_index t len ~op:"shrink_to_imm"
    else if len < length t
    then unsafe_set_length t len
  ;;

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

  let iteri t ~f =
    for i = 0 to (max_index [@kind k]) t do
      f i ((unsafe_get [@kind k]) t i)
    done
  ;;

  let iter t ~f =
    for i = 0 to (max_index [@kind k]) t do
      f ((unsafe_get [@kind k]) t i)
    done
  ;;]

  let rec iter_until' t ~f ~finish ~max_index i =
    if i > max_index
    then finish ()
    else (
      match (f (unsafe_get t i) : _ Continue_or_stop.t) with
      | Stop s -> s
      | Continue () -> iter_until' t ~f ~finish ~max_index (i + 1))
  ;;

  let iter_until t ~f ~finish = iter_until' t ~f ~finish ~max_index:(max_index t) 0

  let to_list t =
    let result = ref [] in
    for i = max_index t downto 0 do
      result := unsafe_get t i :: !result
    done;
    !result
  ;;

  let to_local_list t = exclave_
    let rec aux t i acc = exclave_
      if i < 0 then acc else aux t (i - 1) (unsafe_get t i :: acc)
    in
    aux t (max_index t) []
  ;;

  external unsafe_array_sub
    :  local_ Obj.t array
    -> int
    -> int
    -> local_ 'a iarray
    @@ portable
    = "caml_array_sub_local"

  external unsafe_iarray_of_array__promise_no_mutation
    : 'a.
    ('a array[@local_opt]) -> ('a iarray[@local_opt])
    @@ portable
    = "%array_to_iarray"

  let float_arrays_are_flat = Obj.tag (Obj.repr [| 42 |]) <> Obj.tag (Obj.repr [| 42.0 |])

  let[@cold] nonempty_vec_to_local_float_iarray_slow t ~length ~first_elem = exclave_
    let r = Array.create_local ~len:length first_elem in
    for i = 0 to length - 1 do
      let e = unsafe_get t i in
      Array.unsafe_set r i e
    done;
    unsafe_iarray_of_array__promise_no_mutation r
  ;;

  (* Calls [unsafe_array_sub], which is basically a local allocation and a memcpy.

     ...unless [t] represents an array of floats. In that case, [caml_array_sub_local]
     will preserve the tag of the input array. [float Uniform_array.t] is tagged like an
     array, but [float Iarray.t] is tagged with [Double_array_tag] (unless compiling with
     -no-flat-floatarray).

     So... in that case, we can't use [unsafe_array_sub], and have to write out the for
     loop. *)
  let to_local_iarray t = exclave_
    let length = length t in
    if length = 0
    then unsafe_iarray_of_array__promise_no_mutation [||]
    else (
      let first_elem = unsafe_get t 0 in
      if (not float_arrays_are_flat) && Obj.tag (Obj.repr first_elem) = Obj.double_tag
      then nonempty_vec_to_local_float_iarray_slow t ~length ~first_elem
      else (
        let non_float_array =
          Expert.unsafe_inner t
          |> Kernel.Arr_impl.unsafe_to_array_inplace__promise_not_a_float
        in
        unsafe_array_sub non_float_array 0 length))
  ;;

  let to_alist t =
    let result = ref [] in
    for i = max_index t downto 0 do
      result := (i, unsafe_get t i) :: !result
    done;
    !result
  ;;

  (* Convert to a sequence but does not attempt to protect against modification in the
     vec. *)
  let to_sequence_mutable t =
    Sequence.unfold_step ~init:0 ~f:(fun i ->
      if i >= length t then Done else Yield { value = unsafe_get t i; state = i + 1 })
  ;;

  let to_sequence t = to_sequence_mutable (copy t)

  let of_list xs =
    let t = create ~initial_capacity:(List.length xs) () in
    List.iter xs ~f:(push_back t);
    t
  ;;

  let of_array arr = init (Array.length arr) ~f:(fun i -> Array.get arr i)

  let of_sequence seq =
    (* We don't know the length of seq, so we can't set an initial capacity *)
    let t = create () in
    Sequence.iter seq ~f:(push_back t);
    t
  ;;

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

  let foldi t ~init ~f =
    let r = ref init in
    for i = 0 to (max_index [@kind k]) t do
      r := f i !r ((unsafe_get [@kind k]) t i)
    done;
    !r
  ;;

  let fold t ~init ~f =
    let r = ref init in
    for i = 0 to (max_index [@kind k]) t do
      r := f !r ((unsafe_get [@kind k]) t i)
    done;
    !r
  ;;

  let foldi_local_accum t ~init:acc ~f = exclave_
    let rec aux t i ~acc ~f = exclave_
      if i >= (length [@kind k]) t
      then acc
      else (
        let acc = f i acc ((unsafe_get [@kind k]) t i) in
        aux t (i + 1) ~acc ~f)
    in
    aux t 0 ~acc ~f
  ;;

  let rec foldi_until' t ~f ~acc ~finish ~max_index i =
    if i > max_index
    then finish acc
    else (
      match (f i acc ((unsafe_get [@kind k]) t i) : _ Continue_or_stop.t) with
      | Stop s -> s
      | Continue acc -> (foldi_until' [@kind k]) t ~f ~max_index (i + 1) ~acc ~finish)
  ;;

  let foldi_until t ~init ~f ~finish =
    (foldi_until' [@kind k]) t ~f ~acc:init ~finish ~max_index:((max_index [@kind k]) t) 0
  ;;]

  include%template Blit.Make1 [@modality portable] (struct
      type nonrec 'a t = 'a t

      let create_like ~len _t =
        (* Note that even though we [unsafe_create_uninitialized], every time this
           function is called, the [Vec] is immediately blitted with valid values. *)
        Kernel.unsafe_create_uninitialized ~len
      ;;

      let length = length
      let unsafe_blit = unsafe_blit
    end)

  [%%template
  [@@@kind.default
    k
    = ( float32
      , bits64
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  (* hand write for lack of a functor *)
  let sub t ~pos ~len =
    (* we don't need this *)
    let (_ : _) = Kernel.unsafe_create_uninitialized [@kind k] in
    Ordered_collection_common.check_pos_len_exn
      ~pos
      ~len
      ~total_length:((length [@kind k]) t);
    (init [@kind k]) len ~f:(fun i -> (unsafe_get [@kind k]) t (pos + i))
  ;;

  let blit (type a : k) ~(src : (a t[@kind k])) ~src_pos ~dst ~dst_pos ~len =
    (* we don't need this *)
    Ordered_collection_common.check_pos_len_exn
      ~pos:src_pos
      ~len
      ~total_length:((length [@kind k]) src);
    Ordered_collection_common.check_pos_len_exn
      ~pos:dst_pos
      ~len
      ~total_length:((length [@kind k]) dst);
    if len > 0 then (unsafe_blit [@kind k]) ~src ~src_pos ~dst ~dst_pos ~len
  ;;]

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

  (** Returns the length of the longest prefix for which [f] is true. *)
  let take_while_len (type a : k) (t : (a t[@kind k])) ~(local_ f) : int =
    let rec loop i =
      if i >= (length [@kind k]) t || not (f ((get [@kind k]) t i))
      then i
      else (loop [@tailcall]) (i + 1)
    in
    loop 0 [@nontail]
  ;;

  let take_while t ~f =
    let len = (take_while_len [@kind k]) t ~f in
    (sub [@kind k]) t ~pos:0 ~len
  ;;]

  module Inplace = struct
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

    let sub t ~pos ~len =
      Ordered_collection_common.check_pos_len_exn
        ~pos
        ~len
        ~total_length:((length [@kind k]) t);
      if pos <> 0 then (blit [@kind k]) ~src:t ~src_pos:pos ~dst:t ~dst_pos:0 ~len;
      (shrink_to [@kind k]) t ~len
    ;;

    let take_while t ~f =
      let to_len = (take_while_len [@kind k]) t ~f in
      (shrink_to [@kind k]) t ~len:to_len
    ;;

    let filter t ~f =
      let dest = ref 0 in
      for i = 0 to (max_index [@kind k]) t do
        let x = (unsafe_get [@kind k]) t i in
        if f x
        then (
          if !dest < i then (unsafe_set [@kind k]) t !dest x;
          incr dest)
      done;
      let dest = !dest in
      (shrink_to [@kind k]) t ~len:dest
    ;;

    let map t ~f =
      for i = 0 to (max_index [@kind k]) t do
        (unsafe_set [@kind k]) t i (f ((unsafe_get [@kind k]) t i))
      done
    ;;

    let mapi t ~f =
      for i = 0 to (max_index [@kind k]) t do
        (unsafe_set [@kind k]) t i (f i ((unsafe_get [@kind k]) t i))
      done
    ;;

    let remove_consecutive_duplicates ?(which_to_keep = `First) t ~equal =
      let max_idx = (max_index [@kind k]) t in
      if max_idx < 1
      then ()
      else (
        let new_length =
          match which_to_keep with
          | `First ->
            (* src_idx iterates ahead and writes the new element to dest_idx when a new
               run starts. *)
            let rec loop ~src_idx ~dest_idx ~curr_elt =
              match src_idx > max_idx with
              | true ->
                (* we didn't write here, so dest_idx=7 -> vec is 0..6 -> length=7 *)
                dest_idx
              | false ->
                let src = (unsafe_get [@kind k]) t src_idx in
                (match equal src curr_elt with
                 | true -> loop ~src_idx:(src_idx + 1) ~dest_idx ~curr_elt
                 | false ->
                   if dest_idx < src_idx
                   then (* set to the new element *)
                     (unsafe_set [@kind k]) t dest_idx src;
                   loop ~src_idx:(src_idx + 1) ~dest_idx:(dest_idx + 1) ~curr_elt:src)
            in
            loop ~src_idx:1 ~dest_idx:1 ~curr_elt:((unsafe_get [@kind k]) t 0)
          | `Last ->
            (* src_idx iterates ahead and writes to dest_idx when a run ends *)
            let rec loop ~src_idx ~dest_idx ~elt_to_keep =
              match src_idx > max_idx with
              | true ->
                (* we need to make sure the last element is properly set *)
                (unsafe_set [@kind k]) t dest_idx elt_to_keep;
                (* we did write here, so dest_idx=7 -> vec is 0..7 -> length=7+1=8 *)
                dest_idx + 1
              | false ->
                let src = (unsafe_get [@kind k]) t src_idx in
                (match equal src elt_to_keep with
                 | true -> loop ~src_idx:(src_idx + 1) ~dest_idx ~elt_to_keep:src
                 | false ->
                   if dest_idx < src_idx
                   then
                     (* set to the previous element *)
                     (unsafe_set [@kind k]) t dest_idx elt_to_keep;
                   loop ~src_idx:(src_idx + 1) ~dest_idx:(dest_idx + 1) ~elt_to_keep:src)
            in
            loop ~src_idx:1 ~dest_idx:0 ~elt_to_keep:((unsafe_get [@kind k]) t 0)
        in
        (shrink_to [@kind k]) t ~len:new_length)
    ;;]
  end

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

  let rec forall2__same_length (t1 @ m) (t2 @ m) ~f i length =
    if i >= length
    then true
    else
      f ((unsafe_get [@mode m] [@kind k]) t1 i) ((unsafe_get [@mode m] [@kind k]) t2 i)
      && (forall2__same_length [@mode m] [@kind k]) t1 t2 ~f (i + 1) length
  [@@mode m = (local, global)]
  ;;

  let equal equal (t @ m) (t' @ m) =
    let length' = (length [@kind k]) t in
    if length' <> (length [@kind k]) t'
    then false
    else (forall2__same_length [@mode m] [@kind k]) t t' ~f:equal 0 length' [@nontail]
  [@@mode m = (local, global)]
  ;;

  let clear (type a : k) (t : (a t[@kind k])) =
    if (length [@kind k]) t > 0 then (shrink_to [@kind k]) t ~len:0
  ;;]

  let clear_imm (t : 'a t) = if length t > 0 then shrink_to_imm t ~len:0

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

  let sexp_of_t (type a : k) (sexp_of_a : a -> Sexp.t) t =
    Array.init ((length [@kind k]) t) ~f:(fun i -> sexp_of_a ((unsafe_get [@kind k]) t i))
    |> [%sexp_of: Sexp.t array]
  ;;

  let is_empty t = (length [@kind k]) t = 0

  let exists t ~f =
    let i = ref 0 in
    let n = (length [@kind k]) t in
    let result = ref false in
    while !i < n && not !result do
      if f ((unsafe_get [@kind k]) t !i) then result := true else incr i
    done;
    !result
  ;;

  let for_all t ~f =
    let i = ref 0 in
    let n = (length [@kind k]) t in
    let result = ref true in
    while !i < n && !result do
      if f ((unsafe_get [@kind k]) t !i) then incr i else result := false
    done;
    !result
  ;;

  let mem t a ~equal =
    (exists [@kind k] [@inlined hint]) t ~f:(fun b -> equal a b) [@nontail]
  ;;]

  let count t ~f = Container.count ~fold t ~f
  let sum module_ t ~f = Container.sum ~fold module_ t ~f

  (* Reimplemented for layouts *)

  [%%template
  [@@@kind.default
    k
    = ( float32
      , bits64
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  let count t ~f =
    (fold [@kind k]) t ~init:0 ~f:(fun n a -> if f a then n + 1 else n) [@nontail]
  ;;

  let sum (type a) (module M : Container.Summable with type t = a) t ~f =
    (fold [@kind k]) t ~init:M.zero ~f:(fun n a -> M.( + ) n (f a)) [@nontail]
  ;;]

  (* The code for [find] and [find_exn] would be simpler (wouldn't involve threading
     through [max_index]) if we iterated backward, but we iterate forward to be consistent
     with other containers. *)

  let rec find' t ~f ~max_index i =
    if i > max_index
    then None
    else (
      let x = unsafe_get t i in
      if f x then Some x else find' t ~f ~max_index (i + 1))
  ;;

  let[@cold] raise__not_found () =
    raise (Base.Not_found_s [%message "Vec.find_exn: not found"])
  ;;

  (* Tailcalls to raising functions are to be avoided, as the stack traces are much worse.
     Instead, we try really hard to inline wrapper functions that just perform non-tail
     calls to the raising functions.
  *)
  let[@inline always] raise__not_found () = raise__not_found () [@nontail]

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

  let rec find_exn' t ~f ~max_index i =
    if i > max_index
    then (
      match raise__not_found () with
      | (_ : Nothing.t) -> .)
    else (
      let x = (unsafe_get [@kind k]) t i in
      if f x then x else (find_exn' [@kind k]) t ~f ~max_index (i + 1))
  ;;

  let find_exn t ~f = (find_exn' [@kind k]) t ~f ~max_index:((max_index [@kind k]) t) 0]

  let find t ~f = find' t ~f ~max_index:(max_index t) 0

  let rec findi' t ~f ~max_index i =
    if i > max_index
    then None
    else (
      let x = unsafe_get t i in
      if f x then Some (i, x) else findi' t ~f ~max_index (i + 1))
  ;;

  let findi t ~f = findi' t ~f ~max_index:(max_index t) 0

  let find_and_remove t ~f =
    match findi t ~f with
    | None -> None
    | Some (i, found) ->
      remove_exn t i;
      Some found
  ;;

  let rec find_map' t ~f ~max_index i =
    if i > max_index
    then None
    else (
      match f (unsafe_get t i) with
      | None -> find_map' t ~f ~max_index (i + 1)
      | some -> some)
  ;;

  let find_map t ~f = find_map' t ~f ~max_index:(max_index t) 0

  let rec fold_result' t ~f ~acc ~max_index i =
    if i > max_index
    then Ok acc
    else (
      match f acc (unsafe_get t i) with
      | Ok acc -> fold_result' t ~f ~max_index (i + 1) ~acc
      | err -> err)
  ;;

  let fold_result t ~init ~f = fold_result' t ~f ~acc:init ~max_index:(max_index t) 0

  let rec fold_until' t ~f ~acc ~finish ~max_index i =
    if i > max_index
    then finish acc
    else (
      match (f acc (unsafe_get t i) : _ Continue_or_stop.t) with
      | Stop s -> s
      | Continue acc -> fold_until' t ~f ~max_index (i + 1) ~acc ~finish)
  ;;

  let fold_until t ~init ~f ~finish =
    fold_until' t ~f ~acc:init ~finish ~max_index:(max_index t) 0
  ;;

  let max_elt t ~compare =
    if is_empty t
    then None
    else (
      let max = ref (unsafe_get t 0) in
      for i = 1 to max_index t do
        let x = unsafe_get t i in
        let max' = !max in
        max := if compare max' x < 0 then x else max'
      done;
      Some !max)
  ;;

  let min_elt t ~compare =
    if is_empty t
    then None
    else (
      let min = ref (unsafe_get t 0) in
      for i = 1 to max_index t do
        let x = unsafe_get t i in
        let min' = !min in
        min := if compare min' x > 0 then x else min'
      done;
      Some !min)
  ;;

  let[@inline available] to_array t = Array.init (length t) ~f:(unsafe_get t)

  [%%template
  [@@@kind.default k = (float32, bits64)]

  let to_array t =
    (Array.init [@kind k]) ((length [@kind k]) t) ~f:((unsafe_get [@kind k]) t)
  ;;]

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

  let t_of_sexp a_of_sexp t =
    let arr = [%of_sexp: Sexp.t array] t in
    (init [@kind k]) (Array.length arr) ~f:(fun i -> Array.unsafe_get arr i |> a_of_sexp)
  ;;

  let compare cmp (t1 @ m) (t2 @ m) =
    let len1 = (length [@kind k]) t1 in
    let len2 = (length [@kind k]) t2 in
    let min_len = Int.min len1 len2 in
    let result = ref 0 in
    let i = ref 0 in
    while !i < min_len && !result = 0 do
      result
      := cmp
           ((unsafe_get [@mode m] [@kind k]) t1 !i)
           ((unsafe_get [@mode m] [@kind k]) t2 !i);
      i := !i + 1
    done;
    if !result = 0 then Int.compare len1 len2 else !result
  [@@mode m = (local, global)]
  ;;

  let unsafe_swap t i j =
    let e = (unsafe_get [@kind k]) t i in
    (unsafe_set [@kind k]) t i ((unsafe_get [@kind k]) t j);
    (unsafe_set [@kind k]) t j e
  ;;

  let swap t i j =
    (check_index [@kind k]) t i ~op:"swap";
    (check_index [@kind k]) t j ~op:"swap";
    (unsafe_swap [@kind k]) t i j
  ;;

  let swap_to_last_and_pop t i =
    (check_index [@kind k]) t i ~op:"swap_to_last_and_pop";
    (unsafe_swap [@kind k]) t i ((max_index [@kind k]) t);
    (pop_back_exn [@kind k]) t
  ;;]

  let to_iarray t =
    (to_array [@inlined hint]) t |> unsafe_iarray_of_array__promise_no_mutation
  ;;

  let swap_to_last_and_pop_imm t i =
    check_index t i ~op:"swap_to_last_and_pop";
    unsafe_swap t i (max_index t);
    pop_back_imm_exn t
  ;;

  module Stable = struct
    module V1 = struct
      type nonrec 'a t = 'a t [@@deriving compare ~localize, sexp]

      include%template Bin_prot.Utils.Make_iterable_binable1 [@modality portable] (struct
          type nonrec 'a t = 'a t
          type 'a el = 'a [@@deriving bin_io]

          let caller_identity =
            Bin_prot.Shape.Uuid.of_string "2ec1d047-7cf8-49bc-991b-0badd17d8359"
          ;;

          let module_name = Some "Vec"
          let init ~len ~next = init len ~f:(fun _ -> next ()) [@nontail]
          let iter = iter
          let length = length
        end)

      (* [Make_iterable_binable] doesn't provide a stable witness, but is stable. *)
      let stable_witness (_ : 'a Stable_witness.t) = Stable_witness.assert_stable
    end
  end

  module For_testing = struct
    let%template zero_unused_capacity t =
      let inner : _ I64.Array.t = (Expert.unsafe_inner [@kind bits64]) t in
      let inner : I64.t I64.Array.t = Obj.magic inner in
      let capacity = (capacity [@kind bits64]) t in
      let length = (length [@kind bits64]) t in
      for i = length to capacity - 1 do
        I64.Array.set inner i #0L
      done
    ;;
  end
end

include With_integer_index

module type S = Vec_intf.S

module%template.portable Make (M : Intable.S) = struct
  include With_integer_index

  let[@inline always] [@zero_alloc] to_int_exn index =
    (M.to_int_exn [@zero_alloc assume]) index
  ;;

  let[@inline always] [@zero_alloc] of_int_exn index =
    (M.of_int_exn [@zero_alloc assume]) index
  ;;

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

  let[@inline always] unsafe_get t index = (unsafe_get [@kind k]) t (to_int_exn index)
  let get t index = (get [@kind k]) t (to_int_exn index)]

  let maybe_get t index = maybe_get t (to_int_exn index)
  let maybe_get_local t index = exclave_ maybe_get_local t (to_int_exn index)
  let maybe_get_or_null t index = maybe_get_or_null t (to_int_exn index)

  let[@inline always] unsafe_set_imm t index x : unit =
    unsafe_set_imm t (to_int_exn index) x
  ;;

  let set_imm t index x : unit = set_imm t (to_int_exn index) x

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

  let[@inline always] unsafe_set t index x : unit =
    (unsafe_set [@kind k]) t (to_int_exn index) x
  ;;

  let set t index x : unit = (set [@kind k]) t (to_int_exn index) x
  let next_free_index t = (next_free_index [@kind k]) t |> of_int_exn

  let foldi t ~init ~f =
    (foldi [@kind k] [@inlined hint]) t ~init ~f:(fun [@inline] int accum x ->
      f (of_int_exn int) accum x)
    [@nontail]
  ;;

  let foldi_local_accum t ~init ~f = exclave_
    (foldi_local_accum [@kind k] [@inlined hint])
      t
      ~init
      ~f:(fun [@inline] int accum x -> exclave_ f (of_int_exn int) accum x)
    [@nontail]
  ;;

  let foldi_until t ~init ~f ~finish =
    (foldi_until [@kind k] [@inlined hint])
      t
      ~init
      ~f:(fun [@inline] int accum x -> f (of_int_exn int) accum x)
      ~finish [@nontail]
  ;;

  let iteri t ~f =
    (iteri [@kind k] [@inlined hint]) t ~f:(fun [@inline] int x -> f (of_int_exn int) x)
    [@nontail]
  ;;

  let push_back_index t element = (push_back_index [@kind k]) t element |> of_int_exn]

  let push_back_index_imm t e = push_back_index_imm t e |> of_int_exn

  let findi t ~f =
    match (findi [@inlined hint]) t ~f with
    | Some (i, a) -> Some (of_int_exn i, a)
    | None -> None
  ;;

  let to_alist t =
    (* We could do:
       {[
         to_alist t |> List.map ~f:(fun (i, x) -> of_int_exn i, x)
       ]}

       at the expense of an extra allocation. This is a bit more copy-pasty, but avoids
       that.
    *)
    let result = ref [] in
    for i = max_index t downto 0 do
      let m = of_int_exn i in
      result := (m, unsafe_get t m) :: !result
    done;
    !result
  ;;

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

  let grow_to_include t idx ~default =
    (grow_to_include [@kind k]) t (to_int_exn idx) ~default
  ;;

  let grow_to' t ~len ~default =
    (grow_to' [@kind k]) t ~len ~default:(fun [@inline] idx -> default (of_int_exn idx))
  ;;

  let grow_to_include' t idx ~default =
    (grow_to_include' [@kind k]) t (to_int_exn idx) ~default:(fun [@inline] idx ->
      default (of_int_exn idx))
  ;;]

  module Inplace = struct
    include Inplace

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

    let sub t ~pos ~len = (sub [@kind k]) t ~pos:(to_int_exn pos) ~len

    let mapi t ~f =
      (mapi [@kind k]) t ~f:(fun [@inline] int x -> f (of_int_exn int) x) [@nontail]
    ;;]
  end

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

  let swap t index1 index2 = (swap [@kind k]) t (to_int_exn index1) (to_int_exn index2)
  let swap_to_last_and_pop t index = (swap_to_last_and_pop [@kind k]) t (to_int_exn index)]

  let swap_to_last_and_pop_imm t index = swap_to_last_and_pop_imm t (to_int_exn index)
end
[@@inline]
