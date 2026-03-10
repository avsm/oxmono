(** {1 Chunk Grid}

    Regular chunk grid for Zarr v3 arrays.
    See Zarr v3.1 spec, "Regular Grid" section. *)

(** Chunk grid configuration. Only regular grids are supported. *)
type t = Regular of { chunk_shape : int array }

let chunk_shape (Regular c) = c.chunk_shape

let num_chunks grid array_shape =
  let cs = chunk_shape grid in
  Array.mapi (fun i d -> (d + cs.(i) - 1) / cs.(i)) array_shape

let chunk_for_index grid idx =
  let cs = chunk_shape grid in
  Array.mapi (fun i v -> v / cs.(i)) idx

let chunk_bounds grid array_shape chunk_coords =
  let cs = chunk_shape grid in
  Array.mapi (fun i cc ->
    let start = cc * cs.(i) in
    let stop = min ((cc + 1) * cs.(i)) array_shape.(i) in
    (start, stop)
  ) chunk_coords

let chunk_size grid array_shape chunk_coords =
  Array.map (fun (start, stop) -> stop - start)
    (chunk_bounds grid array_shape chunk_coords)

let is_valid_chunk grid array_shape chunk_coords =
  let num = num_chunks grid array_shape in
  Array.length chunk_coords = Array.length num &&
  Array.for_all2 (fun cc nc -> cc >= 0 && cc < nc) chunk_coords num

let iter_chunks grid array_shape f =
  let num = num_chunks grid array_shape in
  let ndim = Array.length num in
  let current = Array.make ndim 0 in
  let rec go dim =
    if dim = ndim then f (Array.copy current)
    else
      for i = 0 to num.(dim) - 1 do
        current.(dim) <- i;
        go (dim + 1)
      done
  in
  go 0

let total_chunks grid array_shape =
  Array.fold_left ( * ) 1 (num_chunks grid array_shape)

(** {2 JSON serialization} *)

let of_json (json : Jsont.json) =
  let name = Json_util.(member "name" json |> to_string_exn) in
  match name with
  | "regular" ->
    let config = Json_util.member "configuration" json in
    let cs = Json_util.(member "chunk_shape" config |> to_list_exn)
             |> List.map Json_util.to_int_exn |> Array.of_list in
    Regular { chunk_shape = cs }
  | _ -> failwith ("unsupported chunk grid: " ^ name)

let to_json (Regular c) =
  Json_util.(obj [mem "name" (str "regular");
       mem "configuration" (obj [
         mem "chunk_shape"
           (arr (Array.to_list (Array.map (fun i -> int i) c.chunk_shape)))])])
