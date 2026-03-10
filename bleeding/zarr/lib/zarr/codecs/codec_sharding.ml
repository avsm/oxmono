(** Sharding indexed codec -- bundles multiple inner chunks in a single shard.

    See Zarr v3.1 spec, "sharding_indexed" codec. This is an array-to-bytes
    codec that splits an outer chunk into a grid of inner chunks, encodes
    each independently, and stores them with a binary index of
    (offset, nbytes) pairs as uint64 little-endian values.

    {b Optimisations:}
    - Index entries use unboxed [int64#] fields (mixed blocks) to eliminate
      per-entry boxing of the offset and nbytes values.
    - Coordinate, start-offset, and shape arrays are pre-allocated once
      outside the encode/decode loops and reused for each inner chunk,
      avoiding 3×N array allocations per shard.
    - [offset_to_index] is inlined to write directly into the pre-allocated
      coordinate array. *)

open Stdlib_upstream_compatible

(** Sentinel marking an empty (uninitialised) inner chunk. *)
let empty_marker = Int64.minus_one

(** Shard index entry with unboxed [int64#] fields.

    Each entry is a mixed block: the record header is followed by the two
    64-bit values stored flat, with no separate boxed [int64] allocation.
    For a shard with [N] inner chunks, this saves [2×N] heap allocations
    compared to the boxed representation. *)
type index_entry = { offset : int64#; nbytes : int64# }

let empty_entry =
  { offset = Int64_u.of_int64 empty_marker;
    nbytes = Int64_u.of_int64 empty_marker }

let[@inline] is_empty e =
  Int64_u.equal e.offset empty_entry.offset
  && Int64_u.equal e.nbytes empty_entry.nbytes

(** [inner_chunks_per_shard outer inner] returns chunks per dimension. *)
let inner_chunks_per_shard outer inner =
  Array.mapi (fun i o -> (o + inner.(i) - 1) / inner.(i)) outer

(** [total_inner_chunks outer inner] returns the total inner chunk count. *)
let total_inner_chunks outer inner =
  Array.fold_left ( * ) 1 (inner_chunks_per_shard outer inner)

(** Encode a chunk through a full codec chain. *)
let encode_full (chain : Zarr_codec.codec_chain) arr =
  let arr = List.fold_left (fun a (c : Zarr_codec.array_to_array) -> c.encode a)
    arr chain.array_to_array in
  let bytes = chain.array_to_bytes.encode arr in
  List.fold_left (fun b (c : Zarr_codec.bytes_to_bytes) -> c.encode b)
    bytes chain.bytes_to_bytes

(** Decode bytes through a full codec chain. *)
let decode_full (chain : Zarr_codec.codec_chain) shape dtype bytes =
  let bytes = List.fold_right (fun (c : Zarr_codec.bytes_to_bytes) b ->
    c.decode b) chain.bytes_to_bytes bytes in
  let intermediate = List.fold_left (fun s (c : Zarr_codec.array_to_array) ->
    c.compute_output_shape s) shape chain.array_to_array in
  let arr = chain.array_to_bytes.decode intermediate dtype bytes in
  List.fold_right (fun (c : Zarr_codec.array_to_array) a -> c.decode a)
    chain.array_to_array arr

(** Encode index entries to bytes via the full index codec chain.

    The Zarr v3.1 spec says the index is conceptually an array of uint64
    values with shape [\[num_inner_chunks; 2\]], encoded through the
    index codec chain (which must contain an array-to-bytes codec).  We
    build a {!Chunk_data.t} and pass it through the full chain. *)
let encode_index entries index_chain =
  let n = Array.length entries in
  let shape = [| n * 2 |] in
  let chunk = Chunk_data.create_zero Zarr_dtype.Uint64 shape in
  Array.iteri (fun i e ->
    Chunk_data.set_int64 chunk [| i * 2 |] (Int64_u.to_int64 e.offset);
    Chunk_data.set_int64 chunk [| i * 2 + 1 |] (Int64_u.to_int64 e.nbytes)
  ) entries;
  encode_full index_chain chunk

(** Decode index from bytes via the full index codec chain. *)
let decode_index bytes index_chain num =
  let shape = [| num * 2 |] in
  let chunk = decode_full index_chain shape Zarr_dtype.Uint64 bytes in
  Array.init num (fun i ->
    { offset = Int64_u.of_int64 (Chunk_data.get_int64 chunk [| i * 2 |]);
      nbytes = Int64_u.of_int64 (Chunk_data.get_int64 chunk [| i * 2 + 1 |]) })

(** [inline_offset_to_index chunks_per_dim linear dst] writes the
    multi-dimensional index for [linear] into the pre-allocated [dst]
    array without allocating. *)
let[@inline] inline_offset_to_index chunks_per_dim linear dst =
  let ndim = Array.length chunks_per_dim in
  let mutable remaining = linear in
  for d = ndim - 1 downto 0 do
    dst.(d) <- remaining mod chunks_per_dim.(d);
    remaining <- remaining / chunks_per_dim.(d)
  done

(** [create ...] builds a sharding codec with pre-built codec chains.

    @param fill_value Fill value for uninitialised inner chunks during
    decode.  The Zarr v3.1 spec requires that empty inner chunks (marked
    with the 2{^64}-1 sentinel) are filled with the array's fill value,
    not necessarily zero. *)
let create ~outer_chunk_shape ~inner_chunk_shape
    ~inner_chain ~index_chain ~index_location ~dtype ~fill_value =
  let chunks_per_dim = inner_chunks_per_shard outer_chunk_shape inner_chunk_shape in
  let num_inner = total_inner_chunks outer_chunk_shape inner_chunk_shape in
  let ndim = Array.length outer_chunk_shape in
  (* Precompute index size: num_inner * 16 bytes + codec overhead *)
  let index_size =
    let empty = Array.make num_inner empty_entry in
    Bytes.length (encode_index empty index_chain)
  in

  let encode arr =
    let encoded_chunks = Array.make num_inner Bytes.empty in
    let index = Array.make num_inner empty_entry in
    let mutable current_offset = Int64_u.of_int 0 in

    (* Pre-allocate scratch arrays -- reused for every inner chunk *)
    let coords = Array.make ndim 0 in
    let inner_start = Array.make ndim 0 in
    let actual_shape = Array.make ndim 0 in
    let zero_off = Array.make ndim 0 in

    for i = 0 to num_inner - 1 do
      (* Inline offset_to_index to avoid per-chunk array allocation *)
      inline_offset_to_index chunks_per_dim i coords;
      let mutable all_positive = true in
      for d = 0 to ndim - 1 do
        let c = coords.(d) in
        inner_start.(d) <- c * inner_chunk_shape.(d);
        let s = min inner_chunk_shape.(d)
                    (outer_chunk_shape.(d) - c * inner_chunk_shape.(d)) in
        actual_shape.(d) <- s;
        if s <= 0 then all_positive <- false
      done;

      if all_positive then begin
        let chunk = Chunk_data.create_zero dtype actual_shape in
        Chunk_data.blit ~src:arr ~src_off:inner_start ~dst:chunk
          ~dst_off:zero_off ~shape:actual_shape;
        let encoded = encode_full inner_chain chunk in
        let nb = Int64_u.of_int (Bytes.length encoded) in
        encoded_chunks.(i) <- encoded;
        index.(i) <- { offset = current_offset; nbytes = nb };
        current_offset <- Int64_u.add current_offset nb;
        (* Reset zero_off in case blit modified it (it shouldn't, but
           be defensive) *)
        for d = 0 to ndim - 1 do zero_off.(d) <- 0 done
      end
    done;

    let encoded_index = encode_index index index_chain in
    let total_data = Int64_u.to_int current_offset in
    let shard_size = match index_location with
      | Zarr_codec.Start -> index_size + total_data
      | End -> total_data + index_size
    in
    let shard = Bytes.create shard_size in
    let data_start = match index_location with Start -> index_size | End -> 0 in

    (* Copy encoded chunks *)
    let mutable pos = data_start in
    for i = 0 to num_inner - 1 do
      if not (is_empty index.(i)) then begin
        let len = Bytes.length encoded_chunks.(i) in
        Bytes.blit encoded_chunks.(i) 0 shard pos len;
        pos <- pos + len
      end
    done;

    (* Copy index *)
    let idx_start = match index_location with Start -> 0 | End -> total_data in
    Bytes.blit encoded_index 0 shard idx_start index_size;

    (* Adjust offsets if index is at start *)
    if index_location = Start then begin
      let index_size_u = Int64_u.of_int index_size in
      let adjusted = Array.map (fun e ->
        if is_empty e then e
        else { e with offset = Int64_u.add e.offset index_size_u }
      ) index in
      let adj_encoded = encode_index adjusted index_chain in
      Bytes.blit adj_encoded 0 shard 0 index_size
    end;
    shard
  in

  let decode shape dtype bytes =
    let shard_size = Bytes.length bytes in
    let index, _data_start = match index_location with
      | Start ->
        let ib = Bytes.sub bytes 0 (min index_size shard_size) in
        (decode_index ib index_chain num_inner, index_size)
      | End ->
        let start = max 0 (shard_size - index_size) in
        let ib = Bytes.sub bytes start (shard_size - start) in
        (decode_index ib index_chain num_inner, 0)
    in
    let result = Chunk_data.create dtype shape fill_value in

    (* Pre-allocate scratch arrays -- reused for every inner chunk *)
    let coords = Array.make ndim 0 in
    let inner_start = Array.make ndim 0 in
    let actual_shape = Array.make ndim 0 in
    let zero_off = Array.make ndim 0 in

    for i = 0 to num_inner - 1 do
      let entry = if i < Array.length index then index.(i) else empty_entry in
      if not (is_empty entry) then begin
        inline_offset_to_index chunks_per_dim i coords;
        let mutable all_positive = true in
        for d = 0 to ndim - 1 do
          let c = coords.(d) in
          inner_start.(d) <- c * inner_chunk_shape.(d);
          let s = min inner_chunk_shape.(d)
                      (shape.(d) - c * inner_chunk_shape.(d)) in
          actual_shape.(d) <- s;
          if s <= 0 then all_positive <- false
        done;

        if all_positive then begin
          let off = Int64_u.to_int entry.offset in
          let nb = Int64_u.to_int entry.nbytes in
          if off >= 0 && off + nb <= shard_size then begin
            let chunk_bytes = Bytes.sub bytes off nb in
            let chunk = decode_full inner_chain actual_shape dtype chunk_bytes in
            Chunk_data.blit ~src:chunk ~src_off:zero_off ~dst:result
              ~dst_off:inner_start ~shape:actual_shape;
            for d = 0 to ndim - 1 do zero_off.(d) <- 0 done
          end
        end
      end
    done;
    result
  in
  ({ encode; decode } : Zarr_codec.array_to_bytes)
