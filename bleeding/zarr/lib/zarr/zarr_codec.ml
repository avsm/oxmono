(** {1 Zarr v3 Codec Interface and Pipeline}

    Codecs transform chunk data through a chain of array-to-array,
    exactly one array-to-bytes, and bytes-to-bytes codecs.
    See Zarr v3.1 spec, "Codecs" section. *)

(** {2 Exceptions} *)

exception Codec_error of string
exception Checksum_mismatch

(** {2 Codec types} *)

(** Array-to-array codec: transforms chunk shape/layout without changing
    the representation domain (e.g. transpose). *)
type array_to_array = {
  encode : Chunk_data.t -> Chunk_data.t;
  decode : Chunk_data.t -> Chunk_data.t;
  compute_output_shape : int array -> int array;
}

(** Array-to-bytes codec: serialises a typed chunk to raw bytes
    (e.g. bytes codec, sharding). *)
type array_to_bytes = {
  encode : Chunk_data.t -> bytes;
  decode : int array -> Zarr_dtype.t -> bytes -> Chunk_data.t;
}

(** Bytes-to-bytes codec: transforms raw bytes (e.g. compression, checksum).
    [decode] raises {!Codec_error} or {!Checksum_mismatch} on failure. *)
type bytes_to_bytes = {
  encode : bytes -> bytes;
  decode : bytes -> bytes;
  compute_encoded_size : int -> int option;
}

(** A validated codec chain: 0+ array-to-array, 1 array-to-bytes,
    0+ bytes-to-bytes. *)
type codec_chain = {
  array_to_array : array_to_array list;
  array_to_bytes : array_to_bytes;
  bytes_to_bytes : bytes_to_bytes list;
}

(** {2 Codec classification} *)

type codec_class =
  | ArrayToArray of array_to_array
  | ArrayToBytes of array_to_bytes
  | BytesToBytes of bytes_to_bytes

(** {2 Codec registry}

    Allows external packages (e.g. zarr-blosc) to register codec builders
    so the core codec chain can construct them from metadata JSON. *)

type codec_builder =
  Jsont.json -> Zarr_dtype.t -> int array -> codec_class

let registry : (string, codec_builder) Hashtbl.t = Hashtbl.create 16

let register_codec name builder =
  Hashtbl.replace registry name builder

let find_codec name =
  Hashtbl.find_opt registry name

let is_registered name =
  Hashtbl.mem registry name

(** {2 Codec specifications}

    Parsed from / serialised to JSON metadata. *)

(** Separator character for chunk key encoding. *)
type separator = Slash | Dot

(** Index location for sharding codec. *)
type index_location = Start | End

(** Codec specification -- the JSON-level description before building. *)
type codec_spec =
  | Bytes of { endian : Zarr_dtype.endian option }
  | Transpose of { order : int array }
  | Gzip of { level : int }
  | Zstd of { level : int; checksum : bool }
  | Crc32c
  | Sharding of {
      chunk_shape : int array;
      codecs : codec_spec list;
      index_codecs : codec_spec list;
      index_location : index_location;
    }
  | Extension of { name : string; config : Jsont.json }

(** {2 Chain building} *)

(** Forward reference for building a single codec from a spec.
    Set by {!Codec_chain_builder.init}. Lazily initialised: first
    call triggers the init function.

    The [fill_value] parameter is threaded through so that the sharding
    codec can correctly fill uninitialised inner chunks on decode (Zarr
    v3.1 spec requirement). *)
let build_codec_ref :
  (codec_spec -> Zarr_dtype.t -> int array -> Zarr_fill.t ->
   (codec_spec list -> Zarr_dtype.t -> int array -> Zarr_fill.t -> codec_chain) ->
   codec_class) ref =
  ref (fun _ _ _ _ _ -> failwith "codec builder not initialized")

(** Build a validated codec chain from a list of specifications.

    Enforces the ordering constraint: 0+ array-to-array, then exactly 1
    array-to-bytes, then 0+ bytes-to-bytes.

    @param fill_value The array fill value, needed by the sharding codec
    to correctly fill uninitialised inner chunks during decode.
    @raise Codec_error if ordering is invalid or a codec fails to build. *)
let rec build_chain specs dtype chunk_shape fill_value =
  let step (a2a, a2b, b2b, cur_shape) spec =
    let cls = !build_codec_ref spec dtype cur_shape fill_value build_chain in
    match cls with
    | ArrayToArray codec ->
      if Option.is_some a2b then
        raise (Codec_error "array-to-array codec after array-to-bytes");
      (codec :: a2a, a2b, b2b, codec.compute_output_shape cur_shape)
    | ArrayToBytes codec ->
      if Option.is_some a2b then
        raise (Codec_error "multiple array-to-bytes codecs");
      (a2a, Some codec, b2b, cur_shape)
    | BytesToBytes codec ->
      if Option.is_none a2b then
        raise (Codec_error "bytes-to-bytes before array-to-bytes");
      (a2a, a2b, codec :: b2b, cur_shape)
  in
  let (a2a, a2b, b2b, _) =
    List.fold_left step ([], None, [], chunk_shape) specs
  in
  match a2b with
  | None -> raise (Codec_error "chain must contain exactly one array->bytes codec")
  | Some a2b ->
    { array_to_array = List.rev a2a;
      array_to_bytes = a2b;
      bytes_to_bytes = List.rev b2b }

(** [build_chain_default specs dtype chunk_shape] is
    [build_chain specs dtype chunk_shape (Zarr_fill.default dtype)].
    Convenience wrapper for callers that use the default fill value. *)
let build_chain_default specs dtype chunk_shape =
  build_chain specs dtype chunk_shape (Zarr_fill.default dtype)

(** {2 Encode / Decode} *)

(** [encode chain chunk] encodes chunk data through the full codec chain. *)
let encode (chain : codec_chain) arr =
  let arr = List.fold_left (fun a (c : array_to_array) -> c.encode a)
    arr chain.array_to_array in
  let bytes = chain.array_to_bytes.encode arr in
  List.fold_left (fun b (c : bytes_to_bytes) -> c.encode b)
    bytes chain.bytes_to_bytes

(** [decode chain shape dtype bytes] decodes bytes through the codec chain.
    @raise Codec_error if any codec in the chain fails.
    @raise Checksum_mismatch if a checksum codec detects corruption. *)
let decode (chain : codec_chain) shape dtype bytes =
  let bytes = List.fold_right (fun (c : bytes_to_bytes) b -> c.decode b)
    chain.bytes_to_bytes bytes in
  let intermediate_shape = List.fold_left (fun s (c : array_to_array) ->
    c.compute_output_shape s
  ) shape chain.array_to_array in
  let arr = chain.array_to_bytes.decode intermediate_shape dtype bytes in
  List.fold_right (fun (c : array_to_array) a -> c.decode a)
    chain.array_to_array arr

(** {2 JSON serialization} *)

let rec specs_of_json json_list =
  let parse_one json =
    let name = Json_util.(member "name" json |> to_string_exn) in
    let config = Json_util.member "configuration" json in
    match name with
    | "bytes" ->
      let endian = match Json_util.(member "endian" config |> to_string_opt) with
        | Some "little" -> Some Zarr_dtype.Little
        | Some "big" -> Some Big
        | _ -> None
      in
      Bytes { endian }
    | "transpose" ->
      let order = Json_util.(member "order" config |> to_list_exn)
                  |> List.map Json_util.to_int_exn |> Array.of_list in
      Transpose { order }
    | "gzip" ->
      let level = Json_util.(member "level" config |> to_int_opt)
                  |> Option.value ~default:5 in
      Gzip { level }
    | "zstd" ->
      let level = Json_util.(member "level" config |> to_int_opt)
                  |> Option.value ~default:3 in
      let checksum = Json_util.(member "checksum" config |> to_bool_opt)
                     |> Option.value ~default:false in
      Zstd { level; checksum }
    | "crc32c" -> Crc32c
    | "sharding_indexed" ->
      let chunk_shape = Json_util.(member "chunk_shape" config |> to_list_exn)
                        |> List.map Json_util.to_int_exn |> Array.of_list in
      let codecs_json = Json_util.(member "codecs" config |> to_list_exn) in
      let index_json = Json_util.(member "index_codecs" config |> to_list_exn) in
      let index_location = match Json_util.(member "index_location" config |> to_string_opt) with
        | Some "start" -> Start
        | _ -> End
      in
      let codecs = specs_of_json codecs_json in
      let index_codecs = specs_of_json index_json in
      Sharding { chunk_shape; codecs; index_codecs; index_location }
    | name ->
      if is_registered name then
        let config = if Json_util.is_null config then Json_util.(obj []) else config in
        Extension { name; config }
      else
        raise (Codec_error ("unsupported codec: " ^ name))
  in
  List.map parse_one json_list

let rec specs_to_json specs = List.map spec_to_json specs

and spec_to_json = function
  | Bytes { endian } ->
    let e = match endian with
      | Some Big -> "big" | _ -> "little" in
    Json_util.(obj [mem "name" (str "bytes");
            mem "configuration" (obj [mem "endian" (str e)])])
  | Transpose { order } ->
    Json_util.(obj [mem "name" (str "transpose");
            mem "configuration" (obj [
               mem "order" (arr (Array.to_list (Array.map (fun i -> int i) order)))])])
  | Gzip { level } ->
    Json_util.(obj [mem "name" (str "gzip");
            mem "configuration" (obj [mem "level" (int level)])])
  | Zstd { level; checksum } ->
    Json_util.(obj [mem "name" (str "zstd");
            mem "configuration" (obj [mem "level" (int level);
                                      mem "checksum" (bool checksum)])])
  | Crc32c ->
    Json_util.(obj [mem "name" (str "crc32c"); mem "configuration" (obj [])])
  | Sharding { chunk_shape; codecs; index_codecs; index_location } ->
    Json_util.(obj [mem "name" (str "sharding_indexed");
            mem "configuration" (obj [
               mem "chunk_shape" (arr (Array.to_list (Array.map (fun i -> int i) chunk_shape)));
               mem "codecs" (arr (specs_to_json codecs));
               mem "index_codecs" (arr (specs_to_json index_codecs));
               mem "index_location" (str (match index_location with Start -> "start" | End -> "end"))])])
  | Extension { name; config } ->
    Json_util.(obj [mem "name" (str name); mem "configuration" config])
