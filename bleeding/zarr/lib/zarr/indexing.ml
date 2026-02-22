(** Slice and index operations for Zarr arrays *)

open Ztypes

(** Normalize a slice specification against a dimension size *)
let normalize_slice dim_size = function
  | Index i ->
    let i = if i < 0 then dim_size + i else i in
    if i < 0 || i >= dim_size then
      Error (`Invalid_slice (Printf.sprintf "index %d out of bounds for size %d" i dim_size))
    else
      Ok (i, i + 1, 1)  (* start, stop, step *)
  | Range (start, stop) ->
    let start = if start < 0 then dim_size + start else start in
    let stop = if stop < 0 then dim_size + stop else stop in
    let start = max 0 (min start dim_size) in
    let stop = max 0 (min stop dim_size) in
    if start >= stop then
      Error (`Invalid_slice (Printf.sprintf "invalid range [%d:%d)" start stop))
    else
      Ok (start, stop, 1)
  | RangeFrom start ->
    let start = if start < 0 then dim_size + start else start in
    let start = max 0 (min start dim_size) in
    Ok (start, dim_size, 1)
  | RangeTo stop ->
    let stop = if stop < 0 then dim_size + stop else stop in
    let stop = max 0 (min stop dim_size) in
    Ok (0, stop, 1)
  | All ->
    Ok (0, dim_size, 1)
  | Stepped (start, stop, step) ->
    if step = 0 then
      Error (`Invalid_slice "step cannot be zero")
    else
      let start = if start < 0 then dim_size + start else start in
      let stop = if stop < 0 then dim_size + stop else stop in
      let start = max 0 (min start dim_size) in
      let stop = max 0 (min stop dim_size) in
      Ok (start, stop, step)

(** Calculate output shape from slices *)
let output_shape shape slices =
  slices
  |> List.mapi (fun i slice ->
    match normalize_slice shape.(i) slice with
    | Ok (start, stop, step) ->
      let size = (stop - start + abs step - 1) / abs step in
      if size > 0 then Some size else None
    | Error _ -> None)
  |> List.filter_map Fun.id
  |> Array.of_list

(** Iterate over all indices in a slice specification *)
let iter_slices shape slices f =
  let ndim = Array.length shape in

  (* Normalize all slices *)
  let normalized = Array.mapi (fun i slice ->
    match normalize_slice shape.(i) slice with
    | Ok bounds -> bounds
    | Error _ -> (0, 0, 1)  (* Empty slice *)
  ) (Array.of_list slices) in

  (* Fill in missing dimensions with All *)
  let normalized =
    if Array.length normalized < ndim then
      Array.append normalized
        (Array.init (ndim - Array.length normalized) (fun i ->
          (0, shape.(Array.length normalized + i), 1)))
    else normalized
  in

  (* Generate all indices *)
  let current = Array.map (fun (start, _, _) -> start) normalized in
  let output_idx = Array.make ndim 0 in

  let rec iterate dim =
    if dim = ndim then
      f (Array.copy current) (Array.copy output_idx)
    else
      let (start, stop, step) = normalized.(dim) in
      let rec loop i out_i =
        if (step > 0 && i < stop) || (step < 0 && i > stop) then begin
          current.(dim) <- i;
          output_idx.(dim) <- out_i;
          iterate (dim + 1);
          loop (i + step) (out_i + 1)
        end
      in
      loop start 0
  in
  iterate 0

(** Calculate which chunks intersect with a given slice *)
let chunks_for_slice shape chunk_shape slices =
  let ndim = Array.length shape in
  let result = ref [] in

  (* Normalize slices *)
  let normalized = Array.mapi (fun i slice ->
    match normalize_slice shape.(i) slice with
    | Ok bounds -> bounds
    | Error _ -> (0, 0, 1)
  ) (Array.of_list slices) in

  (* Calculate chunk range for each dimension *)
  let chunk_ranges = Array.mapi (fun i (start, stop, _) ->
    let cs = chunk_shape.(i) in
    let first_chunk = start / cs in
    let last_chunk = (stop - 1) / cs in
    (first_chunk, last_chunk)
  ) normalized in

  (* Iterate over all chunk combinations *)
  let current = Array.make ndim 0 in
  let rec iterate dim =
    if dim = ndim then
      result := Array.copy current :: !result
    else begin
      let (first, last) = chunk_ranges.(dim) in
      for c = first to last do
        current.(dim) <- c;
        iterate (dim + 1)
      done
    end
  in
  iterate 0;
  List.rev !result

(** Calculate the intersection of a chunk with a slice *)
let chunk_slice_intersection shape chunk_shape chunk_coords slices =
  let ndim = Array.length shape in

  Array.init ndim (fun i ->
    let cs = chunk_shape.(i) in
    let cc = chunk_coords.(i) in
    let chunk_start = cc * cs in
    let chunk_end = min ((cc + 1) * cs) shape.(i) in

    let (slice_start, slice_stop, step) =
      match normalize_slice shape.(i) (List.nth slices i) with
      | Ok bounds -> bounds
      | Error _ -> (0, shape.(i), 1)
    in

    (* Intersection of chunk range and slice range *)
    let inter_start = max chunk_start slice_start in
    let inter_stop = min chunk_end slice_stop in

    if inter_start >= inter_stop then
      (0, 0, 0, 0)  (* No intersection *)
    else
      (* Offset within chunk, offset within output, length *)
      let chunk_offset = inter_start - chunk_start in
      let output_offset = (inter_start - slice_start) / step in
      let length = inter_stop - inter_start in
      (chunk_offset, output_offset, length, step)
  )

(** Convert slices to explicit start/stop/step arrays *)
let slices_to_ranges shape slices =
  let ndim = Array.length shape in
  let starts = Array.make ndim 0 in
  let stops = Array.make ndim 0 in
  let steps = Array.make ndim 1 in

  List.iteri (fun i slice ->
    if i < ndim then
      match normalize_slice shape.(i) slice with
      | Ok (start, stop, step) ->
        starts.(i) <- start;
        stops.(i) <- stop;
        steps.(i) <- step
      | Error _ -> ()
  ) slices;

  (* Fill remaining dimensions with full ranges *)
  for i = List.length slices to ndim - 1 do
    starts.(i) <- 0;
    stops.(i) <- shape.(i);
    steps.(i) <- 1
  done;

  (starts, stops, steps)
