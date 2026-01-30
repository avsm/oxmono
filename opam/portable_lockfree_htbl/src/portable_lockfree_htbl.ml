(*=The difficult part of implementing a lock-free hash table is resizing.

   To determine when to resize, this hash table keeps track of the length or number of
   bindings in a scalable non-linearizable counter.  A non-linearizable counter is fine as
   for the purpose of resizing it is not necessary to know the precise length.
   Furthermore, reading the length from a scalable counter is expensive enough that this
   implementation reads the length and considers resizing only occasionally -- randomly
   inversely proportionally to the number of buckets.

   Resizing itself is done mostly incrementally.

   1. Before the incremental phase, a new array of buckets is allocated.  The buckets in
   the new array are initialized to a physically unique [Resize _] value/block.  Then the
   root atomic of the hash table is updated to show that a resize is in progress.

   2. Resizing proceeds incrementally through the buckets.  Each source bucket is first
   marked as being [Resize _]d.  After being marked as [Resize _] the source bucket
   becomes frozen and will no longer be mutated.

   3. Then the bucket/buckets in the new bucket array are/is updated.  The uniqueness of
   the [Resize _] value in the target buckets is used to check, before update, whether or
   not the resize is still in progress.  Once all buckets have been processed the root
   atomic is updated with the new bucket array and the resize is finished.

   The numbers in the below bucket state diagram refer to the above phases:

                [1]              [create]
                 |                  |
                 v                  v
     (unique) Resize Nil --[3]--> (Nil <-> Cons _) --[2]--> Resize (Nil | Cons _)
                 ^                      ^                      ^
                 |                      |                      +-- Frozen for resize
                 |                      |
                 |                      +-- Bucket in use
                 |
                 +-- Resize target bucket

   The number of source and target buckets naturally depends on the operation:
   - Copy:  1 source, 1 target.
   - Split: 1 source, 2 target.
   - Merge: 2 source, 1 target.

   Reads from the hash table can safely ignore resizing and just read from a [Resize _]
   bucket.  This makes reads wait-free.  A bucket marked as [Resize _] is frozen and
   writes that would have been to the bucket linearize to a point after the resize.  The
   linearization point of a read operation on a [Resize _] bucket is at or before the
   point where it read the bucket, which is at a point before the resize was completed.

   Writes to the hash table ignore resizing unless they hit a bucket that has been marked
   as being [Resize _]d.  In that case they will help to finish the resize.  It makes
   sense performance wise to avoid participating in a resize unless necessary.

   As the number of buckets is always a power of two, any odd number is coprime with
   respect to the number of buckets.  Before going through the buckets, each thread
   helping with resizing picks a random odd number and uses it as the increment between
   buckets.  This allows higher parallelism during resizing than having each thread go
   through the buckets in the same order.

   Resizing is almost wait-free -- as soon as a thread encounters a [Resize _]d node it
   will help to finish the resize.  Theoretically, however, a thread that keeps on
   updating a bucket can prevent a resize from making progress.  That should be unlikely,
   however, as marking a bucket as being [Resize _]d is faster than a regular update of a
   bucket. *)

open Base
module Backoff = Stdlib.Backoff
open Portable_kernel
include Portable_lockfree_htbl_intf

type%fuelproof ('k, 'v, _) bucket : value mod contended portable =
  | Nil : ('k, 'v, [> `Nil ]) bucket
  | Cons :
      { key : 'k @@ contended portable
      ; data : 'v @@ contended portable
      ; rest : ('k, 'v, [ `Nil | `Cons ]) bucket
      }
      -> ('k, 'v, [> `Cons ]) bucket
  | Resize :
      { spine : ('k, 'v, [ `Nil | `Cons ]) bucket }
      -> ('k, 'v, [> `Resize ]) bucket
  (** During resizing and snapshotting target buckets will be initialized with a
      physically unique [Resize] value and the source buckets will then be gradually
      updated to [Resize] values and the target buckets updated with data from the source
      buckets. *)

type ('k, 'v) bucket_wrapper =
  | B : ('k, 'v, [< `Nil | `Cons | `Resize ]) bucket -> ('k, 'v) bucket_wrapper
[@@unboxed]

type ('k, 'v) state =
  { buckets : ('k, 'v) bucket_wrapper Atomic_array.t
  ; non_linearizable_length : Non_linearizable_counter.t
  ; pending_resize : ('k, 'v) state or_null
  }

type ('k, 'v) t_inner =
  { mutable state : ('k, 'v) state [@atomic]
  ; hash : 'k @ contended -> int @@ global portable
  ; equal : 'k @ contended -> 'k @ contended -> bool @@ global portable
  ; (* [min_buckets] and [max_buckets] could be [6 + 6] bits as they are powers of 2. *)
    min_buckets : int
  ; max_buckets : int
  ; shrinking_allowed : bool
  ; pad_to_8_words : int
  }

type ('k, 'v) t : value mod contended portable = ('k, 'v) t_inner contended

let lo_buckets = 1 lsl 3

and hi_buckets =
  (* This computes the maximum suitable number of buckets or array size for the hash
     table, which is [floor_pow2 Sys.max_array_length]. The implementation must not try to
     grow the table beyond that point. *)
  let mask = Bits.ceil_pow_2_minus_1 Sys.max_array_length in
  mask lxor (mask lsr 1)
;;

let min_buckets_default = 1 lsl 4
and max_buckets_default = Int.min hi_buckets (1 lsl 30 (* Limit of [hash] *))

let create
  (type k)
  ?min_buckets
  ?max_buckets
  ?(shrinking_allowed = true)
  ((module Hashable) : k hashable)
  =
  let min_buckets =
    match min_buckets with
    | None -> min_buckets_default
    | Some n ->
      let n = Int.max lo_buckets n |> Int.min hi_buckets in
      Bits.ceil_pow_2_minus_1 (n - 1) + 1
  in
  let max_buckets =
    match max_buckets with
    | None -> Int.max min_buckets max_buckets_default
    | Some n ->
      let n = Int.max min_buckets n |> Int.min hi_buckets in
      Bits.ceil_pow_2_minus_1 (n - 1) + 1
  in
  let buckets = Atomic_array.create ~len:min_buckets (B Nil) in
  let non_linearizable_length = Non_linearizable_counter.create 0 in
  let state = { buckets; non_linearizable_length; pending_resize = Null } in
  let equal, hash = Hashable.equal, Hashable.hash in
  { contended =
      { state
      ; hash
      ; equal
      ; min_buckets
      ; max_buckets
      ; shrinking_allowed
      ; pad_to_8_words = 0
      }
  }
;;

let hashable_of (type k) (t : (k, _) t) : k hashable =
  (module struct
    type t = k

    let hash = t.contended.hash
    and equal = t.contended.equal
  end)
;;

let min_buckets_of t = t.contended.min_buckets
let max_buckets_of t = t.contended.max_buckets

let estimate_current_num_buckets_of t =
  Atomic_array.length (Atomic.Loc.get [%atomic.loc t.contended.state]).buckets
;;

(** [take bs i] marks the bucket [bs.(i)] as being [Resize _]d, which means that it will
    not be mutated afterwards, and then returns the [Nil | Cons _] spine that was in the
    bucket. *)
let take bs i =
  let[@inline] rec loop = function
    | B ((Nil | Cons _) as spine) as before ->
      let current =
        (Atomic_array.unsafe_compare_exchange bs i)
          ~if_phys_equal_to:before
          ~replace_with:(B (Resize { spine }))
      in
      if phys_equal current before
      then spine
      else
        (* We do not bother to backoff, because after our update wins the bucket will be
           frozen and we want to be faster than regular updates to avoid starvation. *)
        loop current
    | B (Resize { spine }) -> spine
  in
  loop (Atomic_array.unsafe_get bs i)
;;

(** Goes through the source buckets (i.e. [r.bucket]), [take]s their spines, and copies
    them to the [target] buckets. This is used when taking a snapshot of a hash table
    without resizing the bucket table. *)
let rec copy_all r target i t step =
  let i = (i + step) land (Atomic_array.length target - 1) in
  let spine = take r.buckets i in
  let before = Atomic_array.unsafe_get target i in
  (* The [before] value is physically different for each resize and so checking that the
     resize has not finished is sufficient to ensure that the [compare_and_set] below does
     not disrupt the next resize. *)
  phys_equal (Atomic.Loc.get [%atomic.loc t.contended.state]) r
  &&
  ((match before with
    | B (Resize _) ->
      let _ : _ =
        (Atomic_array.unsafe_compare_and_set target i)
          ~if_phys_equal_to:before
          ~replace_with:(B spine)
      in
      ()
    | B (Nil | Cons _) -> ());
   i = 0 || copy_all r target i t step)
;;

(** Goes through the source buckets (i.e. [r.bucket]), [take]s their spines, and splits
    each spine into two [target] buckets. This is used when doubling the size of the
    bucket table. *)
let rec split_all r target i t step =
  let i = (i + step) land (Atomic_array.length r.buckets - 1) in
  let spine = take r.buckets i in
  let high = Atomic_array.length r.buckets in
  let[@inline] rec filter hash msk chk = function
    | Nil -> Nil
    | Cons r as reuse ->
      let rest = filter hash msk chk r.rest in
      if hash r.key land msk = chk
      then
        if phys_equal r.rest rest
        then
          (* Optimization: If we haven't changed anything, reuse the existing cons cell. *)
          reuse
        else Cons { r with rest }
      else rest
  in
  let before_lo = Atomic_array.unsafe_get target i in
  let before_hi = Atomic_array.unsafe_get target (i + high) in
  (* The [before_lo] and [before_hi] values are physically different for each resize and
     so checking that the resize has not finished is sufficient to ensure that the
     [compare_and_set] below does not disrupt the next resize. *)
  phys_equal (Atomic.Loc.get [%atomic.loc t.contended.state]) r
  &&
  ((match before_lo, before_hi with
    | B (Resize _), B (Resize _) ->
      (* Optimization: Avoid traversing bucket nodes and computing hashes twice. *)
      let #(after_lo, after_hi) =
        let[@inline] rec split hash msk = function
          | Nil -> #(Nil, Nil)
          | Cons r as reuse ->
            let #(lo, hi) = split hash msk r.rest in
            (* Optimization: If we haven't changed anything, reuse the existing cons cell. *)
            if hash r.key land msk = 0
            then (
              let lo =
                if phys_equal r.rest lo then reuse else Cons { r with rest = lo }
              in
              #(lo, hi))
            else (
              let hi =
                if phys_equal r.rest hi then reuse else Cons { r with rest = hi }
              in
              #(lo, hi))
        in
        split t.contended.hash high spine
      in
      let _ : _ =
        (Atomic_array.unsafe_compare_and_set target i)
          ~if_phys_equal_to:before_lo
          ~replace_with:(B after_lo)
      in
      let _ : _ =
        (Atomic_array.unsafe_compare_and_set target (i + high))
          ~if_phys_equal_to:before_hi
          ~replace_with:(B after_hi)
      in
      ()
    | B (Resize _), B (Nil | Cons _) ->
      let after_lo = filter t.contended.hash high 0 spine in
      let _ : _ =
        (Atomic_array.unsafe_compare_and_set target i)
          ~if_phys_equal_to:before_lo
          ~replace_with:(B after_lo)
      in
      ()
    | B (Nil | Cons _), B (Resize _) ->
      let after_hi = filter t.contended.hash high high spine in
      let _ : _ =
        (Atomic_array.unsafe_compare_and_set target (i + high))
          ~if_phys_equal_to:before_hi
          ~replace_with:(B after_hi)
      in
      ()
    | B (Nil | Cons _), B (Nil | Cons _) -> ());
   i = 0 || split_all r target i t step)
;;

(** Goes through pairs of source buckets (i.e. [r.bucket]), [take]s their spines, and
    merges each pair of spines into one [target] bucket. This is used when halving the
    size of the bucket table. *)
let rec merge_all r target i t step =
  let i = (i + step) land (Atomic_array.length target - 1) in
  let spine_lo = take r.buckets i in
  let spine_hi = take r.buckets (i + Atomic_array.length target) in
  let before = Atomic_array.unsafe_get target i in
  (* The [before] value is physically different for each resize and so checking that the
     resize has not finished is sufficient to ensure that the [compare_and_set] below does
     not disrupt the next resize. *)
  phys_equal (Atomic.Loc.get [%atomic.loc t.contended.state]) r
  &&
  ((match before with
    | B (Resize _) ->
      let rec merge_cons rest (Cons r : (_, _, [ `Cons ]) bucket) =
        let rest =
          match r.rest with
          | Nil -> rest
          | Cons _ as cons -> merge_cons rest cons
        in
        Cons { r with rest }
      in
      let after =
        match spine_lo, spine_hi with
        | Nil, spine -> spine
        | spine, Nil -> spine
        | rest, (Cons _ as cons) -> merge_cons rest cons
      in
      let _ : _ =
        (Atomic_array.unsafe_compare_and_set target i)
          ~if_phys_equal_to:before
          ~replace_with:(B after)
      in
      ()
    | B (Nil | Cons _) -> ());
   i = 0 || merge_all r target i t step)
;;

let[@inline] rec finish t r =
  match r.pending_resize with
  | Null -> r
  | This (_ as resize) -> finish_resize t r resize

and[@inline never] finish_resize t r ({ buckets; _ } as new_r) =
  let high_source = Atomic_array.length r.buckets in
  let high_target = Atomic_array.length buckets in
  (* We step by random amount to better allow cores to work in parallel. The number of
     buckets is always a power of two, so any odd number is relatively prime or coprime. *)
  let step = Int64.to_int_trunc (Random.bits64 ()) lor 1 in
  let all_done_and_unfinished =
    if high_source < high_target
    then (* We are growing the table. *)
      split_all r buckets 0 t step
    else if high_target < high_source
    then (* We are shrinking the table. *)
      merge_all r buckets 0 t step
    else (* We are snaphotting the table. *)
      copy_all r buckets 0 t step
  in
  if all_done_and_unfinished
  then (
    let cur_r =
      Atomic.Loc.compare_exchange
        [%atomic.loc t.contended.state]
        ~if_phys_equal_to:r
        ~replace_with:new_r
    in
    if phys_equal r cur_r then new_r else finish t cur_r)
  else finish t (Atomic.Loc.get [%atomic.loc t.contended.state])
;;

(** This must be called with [r.pending_resize = Null]. *)
let[@inline never] try_resize t r new_capacity ~clear =
  (* We must make sure that on every resize we use a physically different [Resize _] value
     to indicate unprocessed target buckets. *)
  let resize_avoid_aba =
    if clear
    then B Nil
    else (
      let spine = Sys.opaque_identity Nil in
      B (Resize { spine }))
  in
  let buckets = Atomic_array.create ~len:new_capacity resize_avoid_aba in
  let non_linearizable_length =
    if clear
    then Non_linearizable_counter.zero ~width_of:r.non_linearizable_length
    else r.non_linearizable_length
  in
  let new_r =
    { r with
      pending_resize = This { buckets; non_linearizable_length; pending_resize = Null }
    }
  in
  match
    Atomic.Loc.compare_and_set
      [%atomic.loc t.contended.state]
      ~if_phys_equal_to:r
      ~replace_with:new_r
  with
  | Set_here ->
    let _ : (_, _) state = finish t new_r in
    true
  | Compare_failed -> false
;;

let[@inline] mask buckets = Atomic_array.length buckets - 1

let[@inline never] attempt_resize t r =
  let estimated_size = Non_linearizable_counter.get r.non_linearizable_length in
  let capacity = Atomic_array.length r.buckets in
  if capacity < estimated_size && capacity < t.contended.max_buckets
  then (
    let _ : bool = try_resize t r (capacity + capacity) ~clear:false in
    ())
  else if t.contended.shrinking_allowed
          && t.contended.min_buckets < capacity
          && estimated_size + estimated_size + estimated_size < capacity
  then (
    let _ : bool = try_resize t r (capacity lsr 1) ~clear:false in
    ())
;;

(* [result] is passed to this function as an optimization. This function is always called
   from a tail position and is called from many places. Passing the return value and tail
   calling often generates less code than a non-tail call, which typically has to save the
   return value and restore it after the non-tail call returns. *)
let consider_resize t r result =
  (* Reading the size is potentially expensive, so we only check it occasionally. The
     bigger the table the less frequently we should need to resize. *)
  if Or_null.is_null r.pending_resize
     && Int64.to_int_trunc (Random.bits64 ()) land mask r.buckets = 0
     && phys_equal (Atomic.Loc.get [%atomic.loc t.contended.state]) r
  then attempt_resize t r;
  result
;;

let rec adjust_size t r delta result =
  match Non_linearizable_counter.add_or_resize r.non_linearizable_length delta with
  | Null -> consider_resize t r result
  | This new_nll ->
    (match r.pending_resize with
     | This (_ as resize) ->
       let r = finish_resize t r resize in
       adjust_size t r delta result
     | Null ->
       let new_r = { r with non_linearizable_length = new_nll } in
       let cur_r =
         Atomic.Loc.compare_exchange
           [%atomic.loc t.contended.state]
           ~if_phys_equal_to:r
           ~replace_with:new_r
       in
       let r = if phys_equal r cur_r then new_r else cur_r in
       adjust_size t r delta result)
;;

let rec find_in_cons ~equal key (Cons r : (_, _, [< `Cons ]) bucket) =
  if equal r.key key
  then This r.data
  else (
    match r.rest with
    | Nil -> Null
    | Cons _ as rest -> find_in_cons ~equal key rest)
;;

exception No_binding
exception Data_mismatch

let[@tail_mod_cons] rec remove_from_cons
  equal
  key
  expected_or_null
  (Cons r : (_, _, [< `Cons ]) bucket)
  =
  if equal r.key key
  then (
    match expected_or_null with
    | Null -> r.rest
    | This expected ->
      if phys_equal r.data expected then r.rest else Stdlib.raise_notrace Data_mismatch)
  else (
    match r.rest with
    | Nil -> Stdlib.raise_notrace No_binding
    | Cons _ as rest ->
      Cons { r with rest = remove_from_cons equal key expected_or_null rest })
;;

let find (t @ local) key =
  (* Reads can proceed in parallel with writes. *)
  let r = Atomic.Loc.get [%atomic.loc t.contended.state] in
  let h = t.contended.hash key in
  let i = h land mask r.buckets in
  match Atomic_array.unsafe_get r.buckets i with
  | B Nil -> Null
  | B (Cons cons_r) ->
    if t.contended.equal cons_r.key key
    then This cons_r.data
    else (
      match cons_r.rest with
      | Nil -> Null
      | Cons _ as rest ->
        (* A read might also end up performing a resize, to avoid the case where a hash
           table would otherwise remain at a suboptimal capacity.

           In cases where the number of bindings in the hash table does not change we only
           check for the need to resize on a slow path where the bucket had more than a
           single binding, which is an indication that growing the table might improve
           performance. *)
        consider_resize t r (find_in_cons ~equal:t.contended.equal key rest))
  | B (Resize resize_r) ->
    (* A resize is in progress. The spine of the resize still holds what was in the bucket
       before resize reached that bucket. *)
    (match resize_r.spine with
     | Nil -> Null
     | Cons _ as cons -> find_in_cons ~equal:t.contended.equal key cons)
;;

(* Either add a new binding of [key] to [data] or, when [~set:true], exchange an existing
   binding *)
let rec try_modify_binding t key data ~set backoff =
  let r = Atomic.Loc.get [%atomic.loc t.contended.state] in
  let h = t.contended.hash key in
  let i = h land mask r.buckets in
  match Atomic_array.unsafe_get r.buckets i with
  | B Nil as before ->
    let after = Cons { key; data; rest = Nil } in
    (match
       (Atomic_array.unsafe_compare_and_set r.buckets i)
         ~if_phys_equal_to:before
         ~replace_with:(B after)
     with
     | Set_here -> adjust_size t r 1 Null
     | Compare_failed -> try_modify_binding t key data ~set (Backoff.once backoff))
  | B (Cons cons_r as cons) as before ->
    if t.contended.equal cons_r.key key
    then
      if set
      then (
        let after = B (Cons { key; data; rest = cons_r.rest }) in
        match
          (Atomic_array.unsafe_compare_and_set r.buckets i)
            ~if_phys_equal_to:before
            ~replace_with:after
        with
        | Set_here -> consider_resize t r (This cons_r.data)
        | Compare_failed -> try_modify_binding t key data ~set (Backoff.once backoff))
      else This cons_r.data
    else (
      match cons_r.rest with
      | Nil ->
        (* We intentionally add new binding to front. *)
        let after = B (Cons { key; data; rest = cons }) in
        (match
           (Atomic_array.unsafe_compare_and_set r.buckets i)
             ~if_phys_equal_to:before
             ~replace_with:after
         with
         | Set_here -> adjust_size t r 1 Null
         | Compare_failed -> try_modify_binding t key data ~set (Backoff.once backoff))
      | Cons _ as rest_before ->
        (match find_in_cons ~equal:t.contended.equal key rest_before with
         | This _ as current ->
           if set
           then (
             let rest = remove_from_cons t.contended.equal key Null rest_before in
             (* We intentionally add new binding to front. *)
             let after = B (Cons { key; data; rest = Cons { cons_r with rest } }) in
             match
               (Atomic_array.unsafe_compare_and_set r.buckets i)
                 ~if_phys_equal_to:before
                 ~replace_with:after
             with
             | Set_here ->
               consider_resize t r (find_in_cons ~equal:t.contended.equal key rest_before)
             | Compare_failed -> try_modify_binding t key data ~set (Backoff.once backoff))
           else current
         | Null ->
           (* We intentionally add new binding to front. *)
           let after = B (Cons { key; data; rest = cons }) in
           (match
              (Atomic_array.unsafe_compare_and_set r.buckets i)
                ~if_phys_equal_to:before
                ~replace_with:after
            with
            | Set_here -> adjust_size t r 1 Null
            | Compare_failed -> try_modify_binding t key data ~set (Backoff.once backoff))))
  | B (Resize _) ->
    let _ : (_, _) state = finish t (Atomic.Loc.get [%atomic.loc t.contended.state]) in
    try_modify_binding t key data ~set Backoff.default
;;

let rec try_exchange_binding t key expected_or_null data backoff =
  let r = Atomic.Loc.get [%atomic.loc t.contended.state] in
  let h = t.contended.hash key in
  let i = h land mask r.buckets in
  match Atomic_array.unsafe_get r.buckets i with
  | B Nil -> Null
  | B (Cons cons_r) as before ->
    if t.contended.equal key cons_r.key
    then
      if match expected_or_null with
         | Null -> true
         | This present -> phys_equal cons_r.data present
      then (
        match
          (Atomic_array.unsafe_compare_and_set r.buckets i)
            ~if_phys_equal_to:before
            ~replace_with:(B (Cons { cons_r with data; rest = cons_r.rest }))
        with
        | Set_here -> This cons_r.data
        | Compare_failed ->
          try_exchange_binding t key expected_or_null data (Backoff.once backoff))
      else This cons_r.data
    else (
      match cons_r.rest with
      | Nil -> Null
      | Cons _ as rest_before ->
        (match remove_from_cons t.contended.equal key expected_or_null rest_before with
         | rest ->
           (* We intentionally move updated binding to front. *)
           let after = B (Cons { key; data; rest = Cons { cons_r with rest } }) in
           (match
              (Atomic_array.unsafe_compare_and_set r.buckets i)
                ~if_phys_equal_to:before
                ~replace_with:after
            with
            | Set_here ->
              let result =
                match expected_or_null with
                | This _ as current -> current
                | Null -> find_in_cons ~equal:t.contended.equal key rest_before
              in
              consider_resize t r result
            | Compare_failed ->
              try_exchange_binding t key expected_or_null data (Backoff.once backoff))
         | exception No_binding -> consider_resize t r Null
         | exception Data_mismatch ->
           consider_resize t r (find_in_cons ~equal:t.contended.equal key rest_before)))
  | B (Resize _) ->
    let _ : (_, _) state = finish t (Atomic.Loc.get [%atomic.loc t.contended.state]) in
    try_exchange_binding t key expected_or_null data Backoff.default
;;

let rec try_remove_binding t key expected_or_null backoff =
  let r = Atomic.Loc.get [%atomic.loc t.contended.state] in
  let h = t.contended.hash key in
  let i = h land mask r.buckets in
  match Atomic_array.unsafe_get r.buckets i with
  | B Nil -> Null
  | B (Cons cons_r) as before ->
    if t.contended.equal cons_r.key key
    then
      if match expected_or_null with
         | Null -> true
         | This present -> phys_equal cons_r.data present
      then (
        match
          (Atomic_array.unsafe_compare_and_set r.buckets i)
            ~if_phys_equal_to:before
            ~replace_with:(B cons_r.rest)
        with
        | Set_here -> adjust_size t r (-1) (This cons_r.data)
        | Compare_failed ->
          try_remove_binding t key expected_or_null (Backoff.once backoff))
      else This cons_r.data
    else (
      match cons_r.rest with
      | Nil -> Null
      | Cons _ as rest_before ->
        (match remove_from_cons t.contended.equal key expected_or_null rest_before with
         | rest ->
           (match
              (Atomic_array.unsafe_compare_and_set r.buckets i)
                ~if_phys_equal_to:before
                ~replace_with:(B (Cons { cons_r with rest }))
            with
            | Set_here ->
              let result =
                match expected_or_null with
                | This _ as current -> current
                | Null -> find_in_cons ~equal:t.contended.equal key rest_before
              in
              adjust_size t r (-1) result
            | Compare_failed ->
              try_remove_binding t key expected_or_null (Backoff.once backoff))
         | exception No_binding -> consider_resize t r Null
         | exception Data_mismatch ->
           consider_resize t r (find_in_cons ~equal:t.contended.equal key rest_before)))
  | B (Resize _) ->
    let _ : (_, _) state = finish t (Atomic.Loc.get [%atomic.loc t.contended.state]) in
    try_remove_binding t key expected_or_null Backoff.default
;;

(** [get] only returns with a state where [pending_resize = Null]. *)
let[@inline] get t =
  let r = Atomic.Loc.get [%atomic.loc t.contended.state] in
  if Or_null.is_null r.pending_resize then r else finish t r
;;

let[@inline never] rec clone (t @ local) ~clear backoff =
  let r = get t in
  if try_resize t r (Atomic_array.length r.buckets) ~clear
  then (
    (* We need to count a new length as the previous one may be used by the original. *)
    let length = ref 0 in
    (* We need to copy the array, because some threads might still be trying to resize. *)
    let buckets = r.buckets in
    let buckets =
      Atomic_array.init (Atomic_array.length buckets) ~f:(fun i ->
        match Atomic_array.unsafe_get buckets i with
        | B (Resize spine_r) ->
          let[@inline] rec count = function
            | Nil -> ()
            | Cons r ->
              Int.incr length;
              count r.rest
          in
          let spine = spine_r.spine in
          count spine;
          B spine
        | B (Nil | Cons _) -> assert false (* Only [Resize]s should be in buckets. *))
    in
    let non_linearizable_length = Non_linearizable_counter.create !length in
    { contended =
        { state = { r with non_linearizable_length; buckets }
        ; hash = t.contended.hash
        ; equal = t.contended.equal
        ; min_buckets = t.contended.min_buckets
        ; max_buckets = t.contended.max_buckets
        ; shrinking_allowed = t.contended.shrinking_allowed
        ; pad_to_8_words = 0
        }
    })
  else clone t ~clear (Backoff.once backoff)
;;

let[@inline never] rec snapshot t ~clear backoff =
  let r = get t in
  if try_resize t r (Atomic_array.length r.buckets) ~clear
  then (
    (* Resize has completed, new bucket array is used, and [r.buckets] is a snapshot of
       what was in the hash table. *)
    let snapshot = r.buckets in
    let[@tail_mod_cons] rec loop i kvs =
      match kvs with
      | Nil ->
        if i = 0
        then []
        else (
          let i = i - 1 in
          let spine =
            match Atomic_array.unsafe_get snapshot i with
            | B (Resize spine_r) -> spine_r.spine
            | B (Nil | Cons _) -> assert false (* Only [Resize]s should be in buckets. *)
          in
          loop i spine)
      | Cons r -> (r.key, r.data) :: loop i r.rest
    in
    loop (Atomic_array.length snapshot) Nil)
  else snapshot t ~clear (Backoff.once backoff)
;;

let random_key t =
  let try_find_random_non_empty_bucket t =
    let buckets = (Atomic.Loc.get [%atomic.loc t.contended.state]).buckets in
    let seed = Int64.to_int_trunc (Random.bits64 ()) in
    let rec try_find_random_non_empty_bucket buckets seed i =
      match Atomic_array.unsafe_get buckets i with
      | B Nil | B (Resize { spine = Nil }) ->
        let mask = mask buckets in
        let i = (i + 1) land mask in
        if i <> seed land mask
        then try_find_random_non_empty_bucket buckets seed i
        else Nil
      | B (Cons cons_r) | B (Resize { spine = Cons cons_r }) -> Cons cons_r
    in
    try_find_random_non_empty_bucket
      buckets
      seed
      (seed land (Atomic_array.length buckets - 1))
  in
  match try_find_random_non_empty_bucket t with
  | (Cons cons_r as spine : (_, _, [< `Nil | `Cons ]) bucket) ->
    (* We found a non-empty bucket - the fast way. *)
    if phys_equal cons_r.rest Nil
    then This cons_r.key
    else (
      let rec length spine n =
        match spine with
        | Nil -> n
        | Cons r -> length r.rest (n + 1)
      in
      let n = length cons_r.rest 1 in
      let rec nth spine i =
        match spine with
        | Nil -> failwith "impossible"
        | Cons r -> if i <= 0 then This r.key else nth r.rest (i - 1)
      in
      nth spine (Random.int n))
  | Nil ->
    (* We couldn't find a non-empty bucket - the slow way. *)
    let bindings = snapshot t ~clear:false Backoff.default in
    let n = List.length bindings in
    if n <> 0
    then (
      let[@inline] rec nth i = function
        | (key, _) :: kvs -> if 0 = i then This key else nth (i - 1) kvs
        | _ -> failwith "impossible"
      in
      nth (Random.int n) bindings)
    else Null
;;

let update t key ~pure_f =
  let[@inline] rec loop t key pure_f backoff =
    let expected = find t key in
    let desired = pure_f expected in
    if phys_equal expected desired
    then expected
    else (
      let current =
        match expected, desired with
        | Null, Null -> Null
        | Null, This data -> try_modify_binding t key data ~set:false Backoff.default
        | This expected, Null -> try_remove_binding t key (This expected) Backoff.default
        | This expected, This data ->
          try_exchange_binding t key (This expected) data Backoff.default
      in
      if phys_equal current expected
      then current
      else loop t key pure_f (Backoff.once backoff))
  in
  loop t key pure_f Backoff.default
;;

let[@inline] add t ~key ~data = try_modify_binding t key data ~set:false Backoff.default
let[@inline] set t ~key ~data = try_modify_binding t key data ~set:true Backoff.default
let[@inline] exchange t ~key ~data = try_exchange_binding t key Null data Backoff.default
let[@inline] remove t key = try_remove_binding t key Null Backoff.default

let[@inline] compare_exchange t key ~if_phys_equal_to:expected ~replace_with:data =
  try_exchange_binding t key (This expected) data Backoff.default
;;

let[@inline] compare_remove t key ~if_phys_equal_to:expected =
  try_remove_binding t key (This expected) Backoff.default
;;

let[@inline] copy (t @ local) = clone t ~clear:false Backoff.default [@nontail]
let[@inline] remove_all_as_htbl t = clone t ~clear:true Backoff.default
let[@inline] to_alist t = snapshot t ~clear:false Backoff.default
let[@inline] remove_all_as_alist t = snapshot t ~clear:true Backoff.default

let non_linearizable_length t =
  Non_linearizable_counter.get
    (Atomic.Loc.get [%atomic.loc t.contended.state]).non_linearizable_length
;;
