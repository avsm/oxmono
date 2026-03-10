(** Blosc codec - meta-compressor with shuffling *)

(** Shuffle modes *)
type shuffle = NoShuffle | Shuffle | BitShuffle

let shuffle_to_int = function
  | NoShuffle -> 0
  | Shuffle -> 1
  | BitShuffle -> 2

let shuffle_of_string = function
  | "noshuffle" -> NoShuffle
  | "shuffle" -> Shuffle
  | "bitshuffle" -> BitShuffle
  | s -> raise (Zarr.Codec.Codec_error ("unknown shuffle mode: " ^ s))

let shuffle_to_string = function
  | NoShuffle -> "noshuffle"
  | Shuffle -> "shuffle"
  | BitShuffle -> "bitshuffle"

(** Compressor names *)
type compressor = LZ4 | LZ4HC | BloscLZ | Zstd | Snappy | Zlib

let compressor_to_string = function
  | LZ4 -> "lz4"
  | LZ4HC -> "lz4hc"
  | BloscLZ -> "blosclz"
  | Zstd -> "zstd"
  | Snappy -> "snappy"
  | Zlib -> "zlib"

let compressor_of_string = function
  | "lz4" -> LZ4
  | "lz4hc" -> LZ4HC
  | "blosclz" -> BloscLZ
  | "zstd" -> Zstd
  | "snappy" -> Snappy
  | "zlib" -> Zlib
  | s -> raise (Zarr.Codec.Codec_error ("unknown blosc compressor: " ^ s))

(** C stub bindings *)
external blosc_compress_raw :
  int -> int -> int -> bytes -> string -> int -> bytes
  = "blosc_compress_stub_bytecode" "blosc_compress_stub"

external blosc_decompress_raw : bytes -> bytes
  = "blosc_decompress_stub"

(** Compress bytes using blosc *)
let compress ~cname ~clevel ~shuffle ~typesize ~blocksize input =
  if Bytes.length input = 0 then Bytes.empty
  else
    blosc_compress_raw clevel (shuffle_to_int shuffle) typesize
      input (compressor_to_string cname) blocksize

(** Decompress bytes using blosc *)
let decompress input =
  if Bytes.length input = 0 then Bytes.empty
  else begin
    try blosc_decompress_raw input
    with
    | Failure msg -> raise (Zarr.Codec.Codec_error msg)
    | exn -> raise (Zarr.Codec.Codec_error ("blosc decompress error: " ^ Printexc.to_string exn))
  end

(** Create a blosc codec with specified parameters *)
let create ~cname ~clevel ~shuffle ~typesize ~blocksize : Zarr.Codec.bytes_to_bytes = {
  encode = (fun bytes -> compress ~cname ~clevel ~shuffle ~typesize ~blocksize bytes);
  decode = (fun bytes -> decompress bytes);
  compute_encoded_size = (fun _ -> None);
}

(** Create a codec_spec for blosc with the given parameters *)
let codec_spec ~cname ~clevel ~shuffle ~typesize ~blocksize =
  let none = Jsont.Meta.none in
  let str s = Jsont.String (s, none) in
  let int i = Jsont.Number (float_of_int i, none) in
  let mem n v : Jsont.mem = ((n, none), v) in
  Zarr.Codec.Extension { name = "blosc";
    config = Jsont.Object ([
      mem "cname" (str (compressor_to_string cname));
      mem "clevel" (int clevel);
      mem "shuffle" (str (shuffle_to_string shuffle));
      mem "typesize" (int typesize);
      mem "blocksize" (int blocksize);
    ], none)
  }

(** Find a member value in a jsont object *)
let find_member name = function
  | Jsont.Object (mems, _) ->
    (match List.find_opt (fun ((n, _), _) -> n = name) mems with
     | Some (_, v) -> Some v
     | None -> None)
  | _ -> None

let to_string_opt = function
  | Jsont.String (s, _) -> Some s
  | _ -> None

let to_int_opt = function
  | Jsont.Number (f, _) -> Some (int_of_float f)
  | _ -> None

(** Build a blosc codec from JSON configuration and dtype *)
let build_from_json config _dtype _chunk_shape =
  let cname_str = Option.bind (find_member "cname" config) to_string_opt
                  |> Option.value ~default:"lz4" in
  let clevel = Option.bind (find_member "clevel" config) to_int_opt
               |> Option.value ~default:5 in
  let shuffle_str = Option.bind (find_member "shuffle" config) to_string_opt
                    |> Option.value ~default:"noshuffle" in
  let typesize = Option.bind (find_member "typesize" config) to_int_opt
                 |> Option.value ~default:1 in
  let blocksize = Option.bind (find_member "blocksize" config) to_int_opt
                  |> Option.value ~default:0 in
  let cname = compressor_of_string cname_str in
  let shuffle = shuffle_of_string shuffle_str in
  let codec = create ~cname ~clevel ~shuffle ~typesize ~blocksize in
  Zarr.Codec.BytesToBytes codec

(** Register the blosc codec with the codec registry *)
let register () =
  Zarr.Codec.register_codec "blosc" build_from_json

(** Auto-register at link time *)
let () = register ()
