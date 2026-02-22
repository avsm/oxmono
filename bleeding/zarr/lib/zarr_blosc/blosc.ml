(** Blosc codec - meta-compressor with shuffling *)

(** Shuffle modes *)
type shuffle = NoShuffle | Shuffle | BitShuffle

let shuffle_to_int = function
  | NoShuffle -> 0
  | Shuffle -> 1
  | BitShuffle -> 2

let shuffle_of_string = function
  | "noshuffle" -> Ok NoShuffle
  | "shuffle" -> Ok Shuffle
  | "bitshuffle" -> Ok BitShuffle
  | s -> Error (`Codec_error ("unknown shuffle mode: " ^ s))

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
  | "lz4" -> Ok LZ4
  | "lz4hc" -> Ok LZ4HC
  | "blosclz" -> Ok BloscLZ
  | "zstd" -> Ok Zstd
  | "snappy" -> Ok Snappy
  | "zlib" -> Ok Zlib
  | s -> Error (`Codec_error ("unknown blosc compressor: " ^ s))

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
  if Bytes.length input = 0 then Ok Bytes.empty
  else begin
    try Ok (blosc_decompress_raw input)
    with
    | Failure msg -> Error (`Codec_error msg)
    | exn -> Error (`Codec_error ("blosc decompress error: " ^ Printexc.to_string exn))
  end

(** Create a blosc codec with specified parameters *)
let create ~cname ~clevel ~shuffle ~typesize ~blocksize : Zarr.Codec_intf.bytes_to_bytes = {
  encode = (fun bytes -> compress ~cname ~clevel ~shuffle ~typesize ~blocksize bytes);
  decode = (fun bytes -> decompress bytes);
  compute_encoded_size = (fun _ -> None);
}

(** Create a codec_spec for blosc with the given parameters *)
let codec_spec ~cname ~clevel ~shuffle ~typesize ~blocksize =
  let open Zarr.Ztypes in
  Extension { name = "blosc";
    config = `Assoc [
      ("cname", `String (compressor_to_string cname));
      ("clevel", `Int clevel);
      ("shuffle", `String (shuffle_to_string shuffle));
      ("typesize", `Int typesize);
      ("blocksize", `Int blocksize);
    ]
  }

(** Build a blosc codec from JSON configuration and dtype *)
let build_from_json config _dtype _chunk_shape =
  let open Yojson.Safe.Util in
  try
    let cname_str = config |> member "cname" |> to_string_option |> Option.value ~default:"lz4" in
    let clevel = config |> member "clevel" |> to_int_option |> Option.value ~default:5 in
    let shuffle_str = config |> member "shuffle" |> to_string_option |> Option.value ~default:"noshuffle" in
    let typesize = config |> member "typesize" |> to_int_option |> Option.value ~default:1 in
    let blocksize = config |> member "blocksize" |> to_int_option |> Option.value ~default:0 in
    match compressor_of_string cname_str, shuffle_of_string shuffle_str with
    | Ok cname, Ok shuffle ->
      let codec = create ~cname ~clevel ~shuffle ~typesize ~blocksize in
      Ok (Zarr.Codec_registry.BytesToBytes codec)
    | Error e, _ -> Error e
    | _, Error e -> Error e
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (`Codec_error ("blosc config error: " ^ msg))

(** Register the blosc codec with the codec registry *)
let register () =
  Zarr.Codec_registry.register "blosc" build_from_json

(** Auto-register at link time *)
let () = register ()
