(** Zstd codec -- Zstandard compression using the [zstd] package.

    See Zarr v3.1 spec (extension). This is a bytes-to-bytes codec.
    Uses the buffer-passing API from the OxCaml-optimised zstd bindings
    to avoid intermediate string allocations. *)

let compress ~level input =
  let len = Bytes.length input in
  if len = 0 then Bytes.empty
  else begin
    let src = Bytes.unsafe_to_string input in
    let bound = Zstd.compress_bound len in
    let dst = Bytes.create bound in
    let n = Zstd.compress_to ~level src dst in
    Bytes.sub dst 0 n
  end

let decompress input =
  let len = Bytes.length input in
  if len = 0 then Ok Bytes.empty
  else begin
    try
      let src = Bytes.unsafe_to_string input in
      let orig =
        try Zstd.get_decompressed_size src
        with Zstd.Error _ -> len * 10
      in
      let dst = Bytes.create orig in
      let n = Zstd.decompress_to orig src dst in
      if n = orig then Ok dst
      else Ok (Bytes.sub dst 0 n)
    with
    | Zstd.Error msg -> Error (`Codec_error ("zstd: " ^ msg))
    | exn -> Error (`Codec_error ("zstd: " ^ Printexc.to_string exn))
  end

(** [create level] builds a zstd codec at the given compression level (1-22). *)
let create level : Zarr_codec.bytes_to_bytes = {
  encode = (fun bytes -> compress ~level bytes);
  decode = (fun bytes -> decompress bytes);
  compute_encoded_size = (fun _ -> None);
}
