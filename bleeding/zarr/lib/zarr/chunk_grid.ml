(** Chunk grid implementations for Zarr v3 *)

open Ztypes

(** Get the chunk shape from a chunk grid *)
let chunk_shape = function
  | Regular config -> config.chunk_shape

(** Calculate the number of chunks in each dimension *)
let num_chunks grid array_shape =
  let cs = chunk_shape grid in
  Array.mapi (fun i dim_size ->
    (dim_size + cs.(i) - 1) / cs.(i)
  ) array_shape

(** Get the coordinates of the chunk containing a given array index *)
let chunk_for_index grid idx =
  let cs = chunk_shape grid in
  Array.mapi (fun i v -> v / cs.(i)) idx

(** Get the bounds (start, end) of a chunk in array coordinates *)
let chunk_bounds grid array_shape chunk_coords =
  let cs = chunk_shape grid in
  Array.mapi (fun i cc ->
    let start = cc * cs.(i) in
    let stop = min ((cc + 1) * cs.(i)) array_shape.(i) in
    (start, stop)
  ) chunk_coords

(** Get the shape of a specific chunk (may be smaller at edges) *)
let chunk_size grid array_shape chunk_coords =
  let bounds = chunk_bounds grid array_shape chunk_coords in
  Array.map (fun (start, stop) -> stop - start) bounds

(** Check if chunk coordinates are valid *)
let is_valid_chunk grid array_shape chunk_coords =
  let num = num_chunks grid array_shape in
  let ndim = Array.length chunk_coords in
  if ndim <> Array.length num then false
  else
    Array.for_all2 (fun cc nc -> cc >= 0 && cc < nc) chunk_coords num

(** Iterate over all chunk coordinates *)
let iter_chunks grid array_shape f =
  let num = num_chunks grid array_shape in
  let ndim = Array.length num in
  let current = Array.make ndim 0 in

  let rec iterate dim =
    if dim = ndim then
      f (Array.copy current)
    else begin
      for i = 0 to num.(dim) - 1 do
        current.(dim) <- i;
        iterate (dim + 1)
      done
    end
  in
  iterate 0

(** Get total number of chunks *)
let total_chunks grid array_shape =
  let num = num_chunks grid array_shape in
  Array.fold_left ( * ) 1 num

(** Parse a chunk grid from JSON *)
let of_json json =
  let open Yojson.Safe.Util in
  let name = json |> member "name" |> to_string in
  match name with
  | "regular" ->
    let config = json |> member "configuration" in
    let chunk_shape = config |> member "chunk_shape" |> to_list |> List.map to_int |> Array.of_list in
    Ok (Regular { chunk_shape })
  | _ ->
    Error (`Invalid_metadata ("unsupported chunk grid: " ^ name))

(** Convert a chunk grid to JSON *)
let to_json = function
  | Regular config ->
    `Assoc [
      ("name", `String "regular");
      ("configuration", `Assoc [
        ("chunk_shape", `List (Array.to_list (Array.map (fun i -> `Int i) config.chunk_shape)))
      ])
    ]
