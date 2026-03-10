(** Zstandard - fast lossless compression algorithm

    Direct C bindings with OxCaml optimizations for unboxed/untagged values. *)

exception Error of string

let version () =
  let n = Zstd_stubs.version_number () in
  (n / 10_000, (n / 100) mod 100, n mod 100)

(* Check for zstd errors and raise *)
let[@inline] check r =
  if Zstd_stubs.is_error r then
    raise (Error (Zstd_stubs.get_error_name r))

(* Bracket pattern for safe resource cleanup *)
let bracket res destroy k =
  match k res with
  | r ->
    destroy res;
    r
  | exception exn ->
    destroy res;
    raise exn

let compress ~level ?dict s =
  let src_len = String.length s in
  let dst_size = Zstd_stubs.compress_bound src_len in
  let dst = Bytes.create dst_size in
  let r =
    match dict with
    | None ->
      Zstd_stubs.compress dst dst_size s src_len level
    | Some dict ->
      let dict_len = String.length dict in
      bracket (Zstd_stubs.create_cctx ()) Zstd_stubs.free_cctx (fun cctx ->
        Zstd_stubs.compress_using_dict cctx dst dst_size s src_len dict dict_len level)
  in
  check r;
  Bytes.sub_string dst 0 r

let decompress orig ?dict s =
  let src_len = String.length s in
  let dst = Bytes.create orig in
  let r =
    match dict with
    | None ->
      Zstd_stubs.decompress dst orig s src_len
    | Some dict ->
      let dict_len = String.length dict in
      bracket (Zstd_stubs.create_dctx ()) Zstd_stubs.free_dctx (fun dctx ->
        Zstd_stubs.decompress_using_dict dctx dst orig s src_len dict dict_len)
  in
  check r;
  Bytes.sub_string dst 0 r

let get_decompressed_size s =
  let r = Zstd_stubs.get_frame_content_size s in
  if r = Zstd_stubs.content_size_error () then
    raise (Error "content size error")
  else if r = Zstd_stubs.content_size_unknown () then
    raise (Error "content size unknown")
  else
    Int64.to_int r

(* Buffer-passing API for avoiding allocation of result strings *)

let compress_to ~level ?dict s (dst : bytes) : int =
  let src_len = String.length s in
  let dst_size = Bytes.length dst in
  let r =
    match dict with
    | None ->
      Zstd_stubs.compress dst dst_size s src_len level
    | Some dict ->
      let dict_len = String.length dict in
      bracket (Zstd_stubs.create_cctx ()) Zstd_stubs.free_cctx (fun cctx ->
        Zstd_stubs.compress_using_dict cctx dst dst_size s src_len dict dict_len level)
  in
  check r;
  r

let decompress_to orig ?dict s (dst : bytes) : int =
  let src_len = String.length s in
  let r =
    match dict with
    | None ->
      Zstd_stubs.decompress dst orig s src_len
    | Some dict ->
      let dict_len = String.length dict in
      bracket (Zstd_stubs.create_dctx ()) Zstd_stubs.free_dctx (fun dctx ->
        Zstd_stubs.decompress_using_dict dctx dst orig s src_len dict dict_len)
  in
  check r;
  r

(* Compression bound - useful for pre-allocating buffers *)
let compress_bound = Zstd_stubs.compress_bound
