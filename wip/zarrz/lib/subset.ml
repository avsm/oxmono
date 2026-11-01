(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Ia = Stdlib_stable.Iarray

type t = { start : int iarray; shape : int iarray }

let rank t = Ia.length t.shape

let product a =
  let r = Ia.length a in
  let mutable acc = 1 in
  (* A zero dimension makes the full product zero even when an earlier
     prefix would overflow. The second pass still rejects every negative. *)
  for d = 0 to r - 1 do
    if Ia.get a d = 0 then acc <- 0
  done;
  for d = 0 to r - 1 do
    let dim = Ia.get a d in
    if dim < 0 then invalid_arg "Zarrz.Subset: negative dimension";
    if dim <> 0 && acc > max_int / dim then
      invalid_arg "Zarrz.Subset: shape overflows an int";
    acc <- acc * dim
  done;
  acc

let num_elements t = product t.shape
let zeros r = Ia.init r (fun _ -> 0)

let check_region ~outer ~start ~shape =
  let r = Ia.length outer in
  if Ia.length start <> r || Ia.length shape <> r then
    invalid_arg
      ("Zarrz.Subset: rank " ^ string_of_int (Ia.length shape)
     ^ " subset of a rank " ^ string_of_int r ^ " array");
  for d = 0 to r - 1 do
    let o = Ia.unsafe_get outer d in
    let s = Ia.unsafe_get start d in
    let h = Ia.unsafe_get shape d in
    if s < 0 || h < 0 || o < 0 || s > o - h then
      invalid_arg
        ("Zarrz.Subset: dimension " ^ string_of_int d ^ " range ["
       ^ string_of_int s ^ ", " ^ string_of_int (s + h)
       ^ ") is outside [0, " ^ string_of_int o ^ ")")
  done

let check_buf what buf outer elem_size =
  let n = product outer in
  if elem_size <> 0 && n > max_int / elem_size then
    invalid_arg "Zarrz.Subset: byte size overflows an int";
  if Base_bigstring.length buf < n * elem_size then
    invalid_arg
      ("Zarrz.Subset: the " ^ what ^ " buffer is shorter than its shape")

(* The one region walker. It calls [f] per maximal run with the linear
   element index of the run's first element in each of the two enclosing
   arrays and the run's length. A dimension folds into the run length
   only when [shape] spans it in full on both sides, which is what keeps
   a run contiguous in both arrays at once. Callers must have validated
   both regions: [src_start] and [dst_start] are read in every
   dimension, including the ones folded into the run, where a valid
   region has them at zero. *)
let walk ~shape ~src_outer ~src_start ~dst_outer ~dst_start ~f =
  let r = Ia.length shape in
  if r = 0 then f ~src:0 ~dst:0 ~len:1
  else if product shape > 0 then begin
    let local_ sstride = Base.Array.create_local ~len:r 1 in
    let local_ dstride = Base.Array.create_local ~len:r 1 in
    for d = r - 2 downto 0 do
      sstride.(d) <- sstride.(d + 1) * Ia.unsafe_get src_outer (d + 1);
      dstride.(d) <- dstride.(d + 1) * Ia.unsafe_get dst_outer (d + 1)
    done;
    let mutable split = r - 1 in
    let mutable len = Ia.unsafe_get shape (r - 1) in
    while
      split > 0
      && Ia.unsafe_get shape split = Ia.unsafe_get src_outer split
      && Ia.unsafe_get shape split = Ia.unsafe_get dst_outer split
    do
      split <- split - 1;
      len <- len * Ia.unsafe_get shape split
    done;
    let mutable soff = 0 in
    let mutable doff = 0 in
    for d = 0 to r - 1 do
      soff <- soff + (Ia.unsafe_get src_start d * sstride.(d));
      doff <- doff + (Ia.unsafe_get dst_start d * dstride.(d))
    done;
    let mutable runs = 1 in
    for d = 0 to split - 1 do
      runs <- runs * Ia.unsafe_get shape d
    done;
    let local_ idx = Base.Array.create_local ~len:(max split 1) 0 in
    for _ = 1 to runs do
      f ~src:soff ~dst:doff ~len;
      (* Odometer over the dimensions outside the run, carrying into
         both offsets. *)
      let mutable d = split - 1 in
      let mutable carry = true in
      while carry && d >= 0 do
        idx.(d) <- idx.(d) + 1;
        soff <- soff + sstride.(d);
        doff <- doff + dstride.(d);
        if idx.(d) = Ia.unsafe_get shape d then begin
          soff <- soff - (idx.(d) * sstride.(d));
          doff <- doff - (idx.(d) * dstride.(d));
          idx.(d) <- 0;
          d <- d - 1
        end
        else carry <- false
      done
    done
  end

let validate ~outer t = check_region ~outer ~start:t.start ~shape:t.shape

let iter_runs ~outer t ~f =
  validate ~outer t;
  ignore (product outer);
  walk ~shape:t.shape ~src_outer:outer ~src_start:t.start ~dst_outer:t.shape
    ~dst_start:(zeros (Ia.length t.shape)) ~f

let copy ~elem_size ~src ~src_outer ~src_start ~dst ~dst_outer ~dst_start
    ~shape =
  if elem_size < 0 then invalid_arg "Zarrz.Subset: negative element size";
  check_region ~outer:src_outer ~start:src_start ~shape;
  check_region ~outer:dst_outer ~start:dst_start ~shape;
  check_buf "source" src src_outer elem_size;
  check_buf "destination" dst dst_outer elem_size;
  walk ~shape ~src_outer ~src_start ~dst_outer ~dst_start
    ~f:(fun ~src:s ~dst:d ~len ->
      Base_bigstring.blit ~src ~src_pos:(s * elem_size) ~dst
        ~dst_pos:(d * elem_size) ~len:(len * elem_size))

let gather ~elem_size ~src ~outer t ~dst =
  copy ~elem_size ~src ~src_outer:outer ~src_start:t.start ~dst
    ~dst_outer:t.shape ~dst_start:(zeros (Ia.length t.shape)) ~shape:t.shape

let scatter ~elem_size ~src ~dst ~outer t =
  copy ~elem_size ~src ~src_outer:t.shape
    ~src_start:(zeros (Ia.length t.shape)) ~dst ~dst_outer:outer
    ~dst_start:t.start ~shape:t.shape
