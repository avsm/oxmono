(** Array representation and operations for Zarr v3 *)

(* Create alias for fill_value module before opening Ztypes *)
module FV_mod = Fill_value

open Ztypes

module E = Ztypes.Endianness

(** Array handle with store and metadata *)
type 'store t = {
  store : 'store;
  path : string;
  metadata : array_metadata;
  codec_chain : Codec.codec_chain;
}

(** Module type for store operations needed by array *)
module type STORE_OPS = sig
  type t
  val get : t -> string -> bytes option
  val set : t -> string -> bytes -> unit
  val erase : t -> string -> unit
  val exists : t -> string -> bool
end

(** Functor to create array operations for a given store type *)
module Make (S : STORE_OPS) = struct
  type store = S.t
  type nonrec t = S.t t

  (** Create a new array *)
  let create store ~path ~shape ~chunks ~dtype ?fill_value ?codecs () =
    let fill_value = match fill_value with
      | Some fv -> fv
      | None -> FV_mod.default dtype
    in
    let codecs = match codecs with
      | Some c -> c
      | None -> [Bytes { endian = Some E.Little }]
    in
    let metadata = Metadata.create_array_metadata
      ~shape ~chunks ~dtype ~fill_value ~codecs () in

    (* Build codec chain *)
    match Codec.build_chain codecs dtype chunks with
    | Error e -> Error e
    | Ok codec_chain ->

    (* Write metadata *)
    let meta_path = Chunk_key.metadata_path path in
    let meta_json = Metadata.array_to_json metadata in
    S.set store meta_path (Bytes.of_string meta_json);

    Ok { store; path; metadata; codec_chain }

  (** Open an existing array *)
  let open_ store ~path =
    let meta_path = Chunk_key.metadata_path path in
    match S.get store meta_path with
    | None -> Error (`Not_found ("array not found: " ^ path))
    | Some meta_bytes ->
      match Metadata.array_of_json (Bytes.to_string meta_bytes) with
      | Error e -> Error e
      | Ok metadata ->
        let chunks = Chunk_grid.chunk_shape metadata.chunk_grid in
        match Codec.build_chain metadata.codecs metadata.data_type chunks with
        | Error e -> Error e
        | Ok codec_chain ->
          Ok { store; path; metadata; codec_chain }

  (** Get array metadata *)
  let metadata arr = arr.metadata

  (** Get array shape *)
  let shape arr = arr.metadata.shape

  (** Get array data type *)
  let dtype arr = arr.metadata.data_type

  (** Get chunk shape *)
  let chunks arr = Chunk_grid.chunk_shape arr.metadata.chunk_grid

  (** Get or create a chunk filled with fill value *)
  let get_chunk arr chunk_coords =
    let chunk_key = Chunk_key.full_path arr.path arr.metadata.chunk_key_encoding chunk_coords in
    let chunk_shape = Chunk_grid.chunk_size arr.metadata.chunk_grid arr.metadata.shape chunk_coords in

    match S.get arr.store chunk_key with
    | Some bytes ->
      (match Codec.decode arr.codec_chain chunk_shape arr.metadata.data_type bytes with
       | Ok arr -> arr
       | Error (`Codec_error msg) -> failwith ("chunk decode error: " ^ msg)
       | Error _ -> failwith "chunk decode error")
    | None ->
      (* Return chunk filled with fill value *)
      let chunk = Ndarray.create arr.metadata.data_type chunk_shape in
      Ndarray.fill chunk arr.metadata.fill_value;
      chunk

  (** Set a chunk *)
  let set_chunk arr chunk_coords data =
    let chunk_key = Chunk_key.full_path arr.path arr.metadata.chunk_key_encoding chunk_coords in
    let encoded = Codec.encode arr.codec_chain data in
    S.set arr.store chunk_key encoded

  (** Get a scalar value at given indices *)
  let get arr idx =
    let chunk_coords = Chunk_grid.chunk_for_index arr.metadata.chunk_grid idx in
    let chunk = get_chunk arr chunk_coords in
    let chunks = Chunk_grid.chunk_shape arr.metadata.chunk_grid in
    let local_idx = Array.mapi (fun i v -> v mod chunks.(i)) idx in
    Ndarray.get chunk local_idx

  (** Set a scalar value at given indices *)
  let set arr idx value =
    let chunk_coords = Chunk_grid.chunk_for_index arr.metadata.chunk_grid idx in
    let chunk = get_chunk arr chunk_coords in
    let chunks = Chunk_grid.chunk_shape arr.metadata.chunk_grid in
    let local_idx = Array.mapi (fun i v -> v mod chunks.(i)) idx in
    Ndarray.set chunk local_idx value;
    set_chunk arr chunk_coords chunk

  (** Get a slice of the array *)
  let get_slice arr slices =
    let shape = arr.metadata.shape in
    let chunks = Chunk_grid.chunk_shape arr.metadata.chunk_grid in
    let output_shape = Indexing.output_shape shape slices in
    let result = Ndarray.create arr.metadata.data_type output_shape in
    Ndarray.fill result arr.metadata.fill_value;

    (* Find all chunks that intersect with the slice *)
    let chunk_list = Indexing.chunks_for_slice shape chunks slices in

    (* Process each chunk *)
    List.iter (fun chunk_coords ->
      let chunk = get_chunk arr chunk_coords in
      let intersection = Indexing.chunk_slice_intersection shape chunks chunk_coords slices in

      (* Copy data from chunk to result *)
      let ndim = Array.length shape in
      let rec copy idx dim =
        if dim = ndim then begin
          let chunk_idx = Array.mapi (fun d _ ->
            let (chunk_offset, _, _, _) = intersection.(d) in
            chunk_offset + idx.(d)
          ) idx in
          let result_idx = Array.mapi (fun d _ ->
            let (_, output_offset, _, _) = intersection.(d) in
            output_offset + idx.(d)
          ) idx in
          let value = Ndarray.get chunk chunk_idx in
          Ndarray.set result result_idx value
        end else begin
          let (_, _, length, _) = intersection.(dim) in
          for i = 0 to length - 1 do
            idx.(dim) <- i;
            copy idx (dim + 1)
          done
        end
      in
      copy (Array.make ndim 0) 0
    ) chunk_list;

    result

  (** Set a slice of the array *)
  let set_slice arr slices data =
    let shape = arr.metadata.shape in
    let chunks = Chunk_grid.chunk_shape arr.metadata.chunk_grid in

    (* Find all chunks that intersect with the slice *)
    let chunk_list = Indexing.chunks_for_slice shape chunks slices in

    (* Process each chunk *)
    List.iter (fun chunk_coords ->
      let chunk = get_chunk arr chunk_coords in
      let intersection = Indexing.chunk_slice_intersection shape chunks chunk_coords slices in

      (* Copy data from input to chunk *)
      let ndim = Array.length shape in
      let rec copy idx dim =
        if dim = ndim then begin
          let chunk_idx = Array.mapi (fun d _ ->
            let (chunk_offset, _, _, _) = intersection.(d) in
            chunk_offset + idx.(d)
          ) idx in
          let data_idx = Array.mapi (fun d _ ->
            let (_, output_offset, _, _) = intersection.(d) in
            output_offset + idx.(d)
          ) idx in
          let value = Ndarray.get data data_idx in
          Ndarray.set chunk chunk_idx value
        end else begin
          let (_, _, length, _) = intersection.(dim) in
          for i = 0 to length - 1 do
            idx.(dim) <- i;
            copy idx (dim + 1)
          done
        end
      in
      copy (Array.make ndim 0) 0;

      (* Write chunk back *)
      set_chunk arr chunk_coords chunk
    ) chunk_list

  (** Get array attributes *)
  let attrs arr = Option.value ~default:`Null arr.metadata.attributes

  (** Set array attributes *)
  let set_attrs arr new_attrs =
    let metadata = { arr.metadata with attributes = Some new_attrs } in
    let meta_path = Chunk_key.metadata_path arr.path in
    let meta_json = Metadata.array_to_json metadata in
    S.set arr.store meta_path (Bytes.of_string meta_json)

  (** Delete the array *)
  let delete arr =
    let meta_path = Chunk_key.metadata_path arr.path in
    S.erase arr.store meta_path;
    (* Also delete all chunks - requires LISTABLE store *)
    ()
end
