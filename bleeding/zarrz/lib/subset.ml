(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Ia = Stdlib_stable.Iarray

type t = { start : int iarray; shape : int iarray }

let rank t = Ia.length t.shape

let product a =
  let r = Ia.length a in
  let acc = ref 1 in
  for d = 0 to r - 1 do
    let dim = Ia.get a d in
    if dim < 0 then invalid_arg "Zarrz.Subset: negative dimension";
    if dim <> 0 && !acc > max_int / dim then
      invalid_arg "Zarrz.Subset: shape overflows an int";
    acc := !acc * dim
  done;
  !acc

let num_elements t = product t.shape

let validate ~outer t =
  let r = Ia.length outer in
  if Ia.length t.start <> r || Ia.length t.shape <> r then
    invalid_arg
      ("Zarrz.Subset: rank " ^ string_of_int (Ia.length t.shape)
     ^ " subset of a rank " ^ string_of_int r ^ " array");
  for d = 0 to r - 1 do
    let o = Ia.unsafe_get outer d in
    let s = Ia.unsafe_get t.start d in
    let h = Ia.unsafe_get t.shape d in
    if s < 0 || h < 0 || o < 0 || s > o - h then
      invalid_arg
        ("Zarrz.Subset: dimension " ^ string_of_int d ^ " range ["
       ^ string_of_int s ^ ", " ^ string_of_int (s + h)
       ^ ") is outside [0, " ^ string_of_int o ^ ")")
  done

let iter_runs ~outer t ~f =
  validate ~outer t;
  let r = Ia.length outer in
  if r = 0 then f ~src:0 ~dst:0 ~len:1
  else if product t.shape > 0 then begin
    (* Strides of the enclosing array, in elements. *)
    let ostride = Array.make r 1 in
    for d = r - 2 downto 0 do
      ostride.(d) <- ostride.(d + 1) * Ia.unsafe_get outer (d + 1)
    done;
    (* Dimensions [split + 1 .. r - 1] are spanned in full, so a run
       covers all of dimension [split] and everything inside it. *)
    let split = ref (r - 1) in
    let len = ref (Ia.unsafe_get t.shape (r - 1)) in
    while
      !split > 0 && Ia.unsafe_get t.shape !split = Ia.unsafe_get outer !split
    do
      decr split;
      len := !len * Ia.unsafe_get t.shape !split
    done;
    let split = !split and len = !len in
    (* The run start moves over dimensions [0 .. split - 1] only. *)
    let src = ref 0 in
    for d = 0 to split do
      src := !src + (Ia.unsafe_get t.start d * ostride.(d))
    done;
    let runs = ref 1 in
    for d = 0 to split - 1 do
      runs := !runs * Ia.unsafe_get t.shape d
    done;
    let idx = Array.make (max split 1) 0 in
    let dst = ref 0 in
    for _ = 1 to !runs do
      f ~src:!src ~dst:!dst ~len;
      dst := !dst + len;
      (* Odometer over the outer dimensions, carrying into [src]. *)
      let d = ref (split - 1) in
      let carry = ref true in
      while !carry && !d >= 0 do
        let i = !d in
        idx.(i) <- idx.(i) + 1;
        src := !src + ostride.(i);
        if idx.(i) = Ia.unsafe_get t.shape i then begin
          src := !src - (idx.(i) * ostride.(i));
          idx.(i) <- 0;
          decr d
        end
        else carry := false
      done
    done
  end

let check_lengths ~what ~elem_size ~outer ~sub ~outer_buf ~dense_buf =
  if elem_size < 0 then invalid_arg "Zarrz.Subset: negative element size";
  let outer_elems = product outer in
  let dense_elems = product sub.shape in
  if Base_bigstring.length outer_buf < outer_elems * elem_size then
    invalid_arg (what ^ ": array buffer is shorter than its shape");
  if Base_bigstring.length dense_buf < dense_elems * elem_size then
    invalid_arg (what ^ ": dense buffer is shorter than the subset")

let gather ~elem_size ~src ~outer t ~dst =
  validate ~outer t;
  check_lengths ~what:"Zarrz.Subset.gather" ~elem_size ~outer ~sub:t
    ~outer_buf:src ~dense_buf:dst;
  iter_runs ~outer t ~f:(fun ~src:s ~dst:d ~len ->
      Base_bigstring.blit ~src ~src_pos:(s * elem_size) ~dst
        ~dst_pos:(d * elem_size) ~len:(len * elem_size))

let scatter ~elem_size ~src ~dst ~outer t =
  validate ~outer t;
  check_lengths ~what:"Zarrz.Subset.scatter" ~elem_size ~outer ~sub:t
    ~outer_buf:dst ~dense_buf:src;
  iter_runs ~outer t ~f:(fun ~src:s ~dst:d ~len ->
      Base_bigstring.blit ~src ~src_pos:(d * elem_size) ~dst
        ~dst_pos:(s * elem_size) ~len:(len * elem_size))
