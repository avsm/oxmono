(** Sharding codec - stores multiple inner chunks in a single shard *)

module D = Ztypes.Dtype
module E = Ztypes.Endianness
module IL = Ztypes.Index_location

(** Marker for empty chunks: 2^64 - 1 = -1 in signed representation *)
let empty_marker = Int64.minus_one

(** Shard index entry *)
type index_entry = {
  offset : int64;
  nbytes : int64;
}

(** Check if an index entry represents an empty chunk *)
let is_empty_entry entry =
  Int64.equal entry.offset empty_marker && Int64.equal entry.nbytes empty_marker

(** Calculate the number of inner chunks per shard dimension *)
let inner_chunks_per_shard outer_shape inner_shape =
  Array.mapi (fun i outer_dim ->
    (outer_dim + inner_shape.(i) - 1) / inner_shape.(i)
  ) outer_shape

(** Calculate total number of inner chunks in a shard *)
let total_inner_chunks outer_shape inner_shape =
  let per_dim = inner_chunks_per_shard outer_shape inner_shape in
  Array.fold_left ( * ) 1 per_dim

(** Convert inner chunk coordinates to linear index (C-order) *)
let inner_coords_to_index chunks_per_dim coords =
  Ndarray.index_to_offset chunks_per_dim coords

(** Convert linear index to inner chunk coordinates (C-order) *)
let index_to_inner_coords chunks_per_dim idx =
  Ndarray.offset_to_index chunks_per_dim idx

(** Encode bytes through bytes-to-bytes codecs in a chain *)
let encode_bytes (chain : Codec_intf.codec_chain) (buf : bytes) : bytes =
  List.fold_left (fun b (codec : Codec_intf.bytes_to_bytes) ->
    codec.encode b
  ) buf chain.bytes_to_bytes

(** Decode bytes through bytes-to-bytes codecs in a chain (reverse order) *)
let decode_bytes (chain : Codec_intf.codec_chain) (buf : bytes) : bytes =
  List.fold_right (fun (codec : Codec_intf.bytes_to_bytes) b ->
    match codec.decode b with
    | Ok decoded -> decoded
    | Error (`Codec_error msg) -> failwith ("sharding bytes-to-bytes decode error: " ^ msg)
    | Error `Checksum_mismatch -> failwith "sharding checksum mismatch"
    | Error _ -> failwith "sharding bytes-to-bytes decode error"
  ) chain.bytes_to_bytes buf

(** Encode an ndarray through a codec chain *)
let encode_chain (chain : Codec_intf.codec_chain) (arr : Ndarray.t) : bytes =
  (* Apply array-to-array codecs *)
  let arr = List.fold_left (fun a (codec : Codec_intf.array_to_array) ->
    codec.encode a
  ) arr chain.array_to_array in

  (* Apply array-to-bytes codec *)
  let bytes = chain.array_to_bytes.encode arr in

  (* Apply bytes-to-bytes codecs *)
  List.fold_left (fun b (codec : Codec_intf.bytes_to_bytes) ->
    codec.encode b
  ) bytes chain.bytes_to_bytes

(** Decode bytes through a codec chain to an ndarray *)
let decode_chain (chain : Codec_intf.codec_chain) (shape : int array) (dtype : D.t) (bytes : bytes) : Ndarray.t =
  (* Apply bytes-to-bytes codecs in reverse *)
  let bytes = List.fold_right (fun (codec : Codec_intf.bytes_to_bytes) b ->
    match codec.decode b with
    | Ok decoded -> decoded
    | Error (`Codec_error msg) -> failwith ("sharding inner decode error: " ^ msg)
    | Error `Checksum_mismatch -> failwith "sharding inner checksum mismatch"
    | Error _ -> failwith "sharding inner decode error"
  ) chain.bytes_to_bytes bytes in

  (* Calculate intermediate shape after a2a codecs *)
  let intermediate_shape = List.fold_left (fun s (codec : Codec_intf.array_to_array) ->
    codec.compute_output_shape s
  ) shape chain.array_to_array in

  (* Apply array-to-bytes codec *)
  let arr = chain.array_to_bytes.decode intermediate_shape dtype bytes in

  (* Apply array-to-array codecs in reverse *)
  List.fold_right (fun (codec : Codec_intf.array_to_array) a ->
    codec.decode a
  ) chain.array_to_array arr

(** Encode an index to bytes *)
let encode_index index_entries index_chain =
  let n = Array.length index_entries in
  let buf = Bytes.create (n * 16) in  (* 8 bytes offset + 8 bytes nbytes *)
  Array.iteri (fun i entry ->
    Bytes.set_int64_le buf (i * 16) entry.offset;
    Bytes.set_int64_le buf (i * 16 + 8) entry.nbytes
  ) index_entries;
  encode_bytes index_chain buf

(** Decode an index from bytes *)
let decode_index bytes index_chain num_inner_chunks =
  let decoded_bytes = decode_bytes index_chain bytes in
  let n = min (Bytes.length decoded_bytes / 16) num_inner_chunks in
  Array.init n (fun i ->
    {
      offset = Bytes.get_int64_le decoded_bytes (i * 16);
      nbytes = Bytes.get_int64_le decoded_bytes (i * 16 + 8);
    }
  )

(** Create a sharding codec with pre-built codec chains *)
let create_with_chains
    ~outer_chunk_shape
    ~inner_chunk_shape
    ~inner_chain
    ~index_chain
    ~index_location
    ~dtype =
  let chunks_per_dim = inner_chunks_per_shard outer_chunk_shape inner_chunk_shape in
  let num_inner_chunks = total_inner_chunks outer_chunk_shape inner_chunk_shape in

  let encode arr =
    let ndim = Array.length outer_chunk_shape in

    (* Encode each inner chunk *)
    let encoded_chunks = Array.make num_inner_chunks Bytes.empty in
    let index = Array.make num_inner_chunks { offset = empty_marker; nbytes = empty_marker } in

    let current_offset = ref 0L in

    (* Iterate over all inner chunks *)
    for i = 0 to num_inner_chunks - 1 do
      let inner_coords = index_to_inner_coords chunks_per_dim i in

      (* Calculate bounds of this inner chunk within the shard *)
      let inner_start = Array.mapi (fun d c -> c * inner_chunk_shape.(d)) inner_coords in
      let inner_end = Array.mapi (fun d c ->
        min ((c + 1) * inner_chunk_shape.(d)) outer_chunk_shape.(d)
      ) inner_coords in
      let actual_shape = Array.mapi (fun d _ -> inner_end.(d) - inner_start.(d)) inner_start in

      (* Check if chunk is entirely within bounds *)
      if Array.for_all (fun s -> s > 0) actual_shape then begin
        (* Extract inner chunk data *)
        let chunk_arr = Ndarray.create dtype actual_shape in

        (* Copy data from shard array to chunk array *)
        let rec copy idx dim =
          if dim = ndim then begin
            let src_idx = Array.mapi (fun d j -> inner_start.(d) + j) idx in
            let value = Ndarray.get arr src_idx in
            Ndarray.set chunk_arr idx value
          end else begin
            for j = 0 to actual_shape.(dim) - 1 do
              idx.(dim) <- j;
              copy idx (dim + 1)
            done
          end
        in
        copy (Array.make ndim 0) 0;

        (* Encode the chunk *)
        let encoded = encode_chain inner_chain chunk_arr in
        let nbytes = Int64.of_int (Bytes.length encoded) in

        encoded_chunks.(i) <- encoded;
        index.(i) <- { offset = !current_offset; nbytes };
        current_offset := Int64.add !current_offset nbytes
      end
    done;

    (* Encode the index *)
    let encoded_index = encode_index index index_chain in
    let index_size = Bytes.length encoded_index in

    (* Assemble the shard *)
    let total_data_size = Int64.to_int !current_offset in
    let shard_size = match index_location with
      | IL.Start -> index_size + total_data_size
      | IL.End -> total_data_size + index_size
    in
    let shard = Bytes.create shard_size in

    let data_start = match index_location with
      | IL.Start -> index_size
      | IL.End -> 0
    in

    (* Copy encoded chunks *)
    let pos = ref data_start in
    for i = 0 to num_inner_chunks - 1 do
      if not (is_empty_entry index.(i)) then begin
        Bytes.blit encoded_chunks.(i) 0 shard !pos (Bytes.length encoded_chunks.(i));
        pos := !pos + Bytes.length encoded_chunks.(i)
      end
    done;

    (* Copy index *)
    let index_start = match index_location with
      | IL.Start -> 0
      | IL.End -> total_data_size
    in
    Bytes.blit encoded_index 0 shard index_start index_size;

    (* If index is at start, adjust offsets *)
    if index_location = IL.Start then begin
      let adjusted_index = Array.map (fun entry ->
        if is_empty_entry entry then entry
        else { entry with offset = Int64.add entry.offset (Int64.of_int index_size) }
      ) index in
      let adjusted_encoded = encode_index adjusted_index index_chain in
      Bytes.blit adjusted_encoded 0 shard 0 index_size
    end;

    shard
  in

  let decode shape dtype bytes =
    let shard_size = Bytes.length bytes in
    let ndim = Array.length shape in

    (* Calculate actual encoded index size by encoding an empty index *)
    let empty_index = Array.make num_inner_chunks { offset = empty_marker; nbytes = empty_marker } in
    let encoded_empty = encode_index empty_index index_chain in
    let index_size = Bytes.length encoded_empty in

    (* Determine index location and decode index *)
    let index, _data_start = match index_location with
      | IL.Start ->
        let index_bytes = Bytes.sub bytes 0 (min index_size shard_size) in
        (decode_index index_bytes index_chain num_inner_chunks, index_size)
      | IL.End ->
        let index_start = max 0 (shard_size - index_size) in
        let index_bytes = Bytes.sub bytes index_start (shard_size - index_start) in
        (decode_index index_bytes index_chain num_inner_chunks, 0)
    in

    (* Create output array filled with fill value *)
    let result = Ndarray.create dtype shape in

    (* Decode each inner chunk *)
    for i = 0 to num_inner_chunks - 1 do
      let entry = if i < Array.length index then index.(i) else { offset = empty_marker; nbytes = empty_marker } in
      if not (is_empty_entry entry) then begin
        let inner_coords = index_to_inner_coords chunks_per_dim i in

        (* Calculate bounds of this inner chunk *)
        let inner_start = Array.mapi (fun d c -> c * inner_chunk_shape.(d)) inner_coords in
        let inner_end = Array.mapi (fun d c ->
          min ((c + 1) * inner_chunk_shape.(d)) shape.(d)
        ) inner_coords in
        let actual_shape = Array.mapi (fun d _ -> inner_end.(d) - inner_start.(d)) inner_start in

        if Array.for_all (fun s -> s > 0) actual_shape then begin
          (* Extract and decode chunk data *)
          let offset = Int64.to_int entry.offset in
          let nbytes = Int64.to_int entry.nbytes in
          if offset >= 0 && offset + nbytes <= shard_size then begin
            let chunk_bytes = Bytes.sub bytes offset nbytes in
            let chunk_arr = decode_chain inner_chain actual_shape dtype chunk_bytes in

            (* Copy decoded data to result *)
            let rec copy idx dim =
              if dim = ndim then begin
                let dst_idx = Array.mapi (fun d j -> inner_start.(d) + j) idx in
                let value = Ndarray.get chunk_arr idx in
                Ndarray.set result dst_idx value
              end else begin
                for j = 0 to actual_shape.(dim) - 1 do
                  idx.(dim) <- j;
                  copy idx (dim + 1)
                done
              end
            in
            copy (Array.make ndim 0) 0
          end
        end
      end
    done;

    result
  in

  ({ encode; decode } : Codec_intf.array_to_bytes)
