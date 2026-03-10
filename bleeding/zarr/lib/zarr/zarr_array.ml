(** {1 Zarr v3 Array Operations}

    Provides chunked array read/write operations over an abstract store.
    See Zarr v3.1 spec, "Array" section. *)

type 'store t = {
  store : 'store;
  path : string;
  metadata : Zarr_metadata.array_metadata;
  codec_chain : Zarr_codec.codec_chain;
}

module type STORE_OPS = sig
  type t
  val get : t -> string -> bytes option
  val set : t -> string -> bytes -> unit
  val erase : t -> string -> unit
  val exists : t -> string -> bool
end

module Make (S : STORE_OPS) = struct
  type store = S.t
  type nonrec t = S.t t

  let create store ~path ~shape ~chunks ~dtype ?fill_value ?codecs () =
    let fill_value = match fill_value with
      | Some fv -> fv | None -> Zarr_fill.default dtype in
    let codecs = match codecs with
      | Some c -> c | None -> [Zarr_codec.Bytes { endian = Some Little }] in
    let metadata = Zarr_metadata.create_array_metadata
      ~shape ~chunks ~dtype ~fill_value ~codecs () in
    let codec_chain = Zarr_codec.build_chain codecs dtype chunks fill_value in
    let meta_path = Chunk_key.metadata_path path in
    S.set store meta_path (Bytes.of_string (Zarr_metadata.array_to_json metadata));
    { store; path; metadata; codec_chain }

  let open_ store ~path =
    let meta_path = Chunk_key.metadata_path path in
    match S.get store meta_path with
    | None -> failwith ("array not found: " ^ path)
    | Some meta_bytes ->
      let metadata = Zarr_metadata.array_of_json (Bytes.to_string meta_bytes) in
      let chunks = Chunk_grid.chunk_shape metadata.chunk_grid in
      let codec_chain = Zarr_codec.build_chain metadata.codecs metadata.data_type chunks metadata.fill_value in
      { store; path; metadata; codec_chain }

  let metadata arr = arr.metadata
  let shape arr = arr.metadata.shape
  let dtype arr = arr.metadata.data_type
  let chunks arr = Chunk_grid.chunk_shape arr.metadata.chunk_grid

  let get_chunk arr chunk_coords =
    let key = Chunk_key.full_path arr.path arr.metadata.chunk_key_encoding chunk_coords in
    let cs = Chunk_grid.chunk_size arr.metadata.chunk_grid arr.metadata.shape chunk_coords in
    match S.get arr.store key with
    | Some bytes ->
      Zarr_codec.decode arr.codec_chain cs arr.metadata.data_type bytes
    | None ->
      Chunk_data.create arr.metadata.data_type cs arr.metadata.fill_value

  let set_chunk arr chunk_coords data =
    let key = Chunk_key.full_path arr.path arr.metadata.chunk_key_encoding chunk_coords in
    S.set arr.store key (Zarr_codec.encode arr.codec_chain data)

  let get arr idx =
    let cc = Chunk_grid.chunk_for_index arr.metadata.chunk_grid idx in
    let chunk = get_chunk arr cc in
    let cs = Chunk_grid.chunk_shape arr.metadata.chunk_grid in
    let local_idx = Array.mapi (fun i v -> v mod cs.(i)) idx in
    Chunk_data.get chunk local_idx

  let set arr idx value =
    let cc = Chunk_grid.chunk_for_index arr.metadata.chunk_grid idx in
    let chunk = get_chunk arr cc in
    let cs = Chunk_grid.chunk_shape arr.metadata.chunk_grid in
    let local_idx = Array.mapi (fun i v -> v mod cs.(i)) idx in
    Chunk_data.set chunk local_idx value;
    set_chunk arr cc chunk

  (** [all_unit_step intersection] is [true] when every dimension of
      the intersection has step 1, meaning the region is contiguous
      and eligible for bulk [Chunk_data.blit]. *)
  let all_unit_step intersection =
    Array.for_all (fun (_, _, _, step) -> step = 1) intersection

  let get_slice arr slices =
    let shape = arr.metadata.shape in
    let cs = Chunk_grid.chunk_shape arr.metadata.chunk_grid in
    let output_shape = Slice.output_shape shape slices in
    let result = Chunk_data.create arr.metadata.data_type output_shape arr.metadata.fill_value in
    let chunk_list = Slice.chunks_for_slice shape cs slices in
    List.iter (fun chunk_coords ->
      let chunk = get_chunk arr chunk_coords in
      let intersection = Slice.chunk_intersection shape cs chunk_coords slices in
      let ndim = Array.length shape in
      if all_unit_step intersection then begin
        (* Fast path: contiguous region, use Bytes.blit via Chunk_data.blit *)
        let src_off = Array.map (fun (co, _, _, _) -> co) intersection in
        let dst_off = Array.map (fun (_, oo, _, _) -> oo) intersection in
        let region  = Array.map (fun (_, _, len, _) -> len) intersection in
        if Array.for_all (fun l -> l > 0) region then
          Chunk_data.blit ~src:chunk ~src_off ~dst:result ~dst_off ~shape:region
      end else begin
        (* Slow path: stepped slice, element-by-element *)
        let chunk_idx  = Array.make ndim 0 in
        let result_idx = Array.make ndim 0 in
        let rec copy dim =
          if dim = ndim then
            Chunk_data.set result result_idx (Chunk_data.get chunk chunk_idx)
          else begin
            let (co, oo, length, _step) = intersection.(dim) in
            for i = 0 to length - 1 do
              chunk_idx.(dim)  <- co + i;
              result_idx.(dim) <- oo + i;
              copy (dim + 1)
            done
          end
        in
        copy 0
      end
    ) chunk_list;
    result

  let set_slice arr slices data =
    let shape = arr.metadata.shape in
    let cs = Chunk_grid.chunk_shape arr.metadata.chunk_grid in
    let chunk_list = Slice.chunks_for_slice shape cs slices in
    List.iter (fun chunk_coords ->
      let chunk = get_chunk arr chunk_coords in
      let intersection = Slice.chunk_intersection shape cs chunk_coords slices in
      let ndim = Array.length shape in
      if all_unit_step intersection then begin
        let dst_off = Array.map (fun (co, _, _, _) -> co) intersection in
        let src_off = Array.map (fun (_, oo, _, _) -> oo) intersection in
        let region  = Array.map (fun (_, _, len, _) -> len) intersection in
        if Array.for_all (fun l -> l > 0) region then
          Chunk_data.blit ~src:data ~src_off ~dst:chunk ~dst_off ~shape:region
      end else begin
        let chunk_idx = Array.make ndim 0 in
        let data_idx  = Array.make ndim 0 in
        let rec copy dim =
          if dim = ndim then
            Chunk_data.set chunk chunk_idx (Chunk_data.get data data_idx)
          else begin
            let (co, oo, length, _step) = intersection.(dim) in
            for i = 0 to length - 1 do
              chunk_idx.(dim) <- co + i;
              data_idx.(dim)  <- oo + i;
              copy (dim + 1)
            done
          end
        in
        copy 0
      end;
      set_chunk arr chunk_coords chunk
    ) chunk_list

  let attrs arr = Option.value ~default:Json_util.null arr.metadata.attributes

  let set_attrs arr new_attrs =
    let metadata = { arr.metadata with attributes = Some new_attrs } in
    let meta_path = Chunk_key.metadata_path arr.path in
    S.set arr.store meta_path (Bytes.of_string (Zarr_metadata.array_to_json metadata))

  let delete arr =
    S.erase arr.store (Chunk_key.metadata_path arr.path)
end
