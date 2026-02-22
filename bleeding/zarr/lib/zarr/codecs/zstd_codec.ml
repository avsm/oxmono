(** Zstd codec - Zstandard compression *)

(** Compress bytes using zstd *)
let compress ~level input =
  if Bytes.length input = 0 then Bytes.empty
  else
    Bytes.of_string (Zstd.compress ~level (Bytes.to_string input))

(** Decompress bytes using zstd *)
let decompress input =
  let input_len = Bytes.length input in
  if input_len = 0 then Ok Bytes.empty
  else begin
    try
      let input_str = Bytes.to_string input in
      let orig_size = Zstd.get_decompressed_size input_str in
      if orig_size = 0 then
        (* Fallback: try with a large buffer *)
        Ok (Bytes.of_string (Zstd.decompress (input_len * 10) input_str))
      else
        Ok (Bytes.of_string (Zstd.decompress orig_size input_str))
    with
    | Zstd.Error msg -> Error (`Codec_error ("zstd decompress failed: " ^ msg))
    | exn -> Error (`Codec_error ("zstd decompress error: " ^ Printexc.to_string exn))
  end

(** Create a zstd codec with specified compression level *)
let create level : Codec_intf.bytes_to_bytes = {
  encode = (fun bytes -> compress ~level bytes);
  decode = (fun bytes -> decompress bytes);
  compute_encoded_size = (fun _ -> None);
}
