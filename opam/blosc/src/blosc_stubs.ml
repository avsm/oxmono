(** Direct C bindings to libblosc with OxCaml optimizations *)

(* Max overhead constant - noalloc *)
external max_overhead : unit -> (int[@untagged])
  = "ocaml_blosc_max_overhead" "ocaml_blosc_max_overhead"
  [@@noalloc]

(* Compress using blosc_compress_ctx - noalloc
   9 args total: dst (bytes), dst_size, src (string), src_size,
   clevel, doshuffle, typesize, compressor (string), blocksize *)
external compress
  : (bytes[@local_opt]) -> (int[@untagged])
  -> string -> (int[@untagged])
  -> (int[@untagged]) -> (int[@untagged])
  -> (int[@untagged]) -> string
  -> (int[@untagged])
  -> (int[@untagged])
  = "ocaml_blosc_compress_bytecode" "ocaml_blosc_compress"
  [@@noalloc]

(* Decompress using blosc_decompress_ctx - noalloc *)
external decompress
  : (bytes[@local_opt]) -> (int[@untagged]) -> string -> (int[@untagged])
  -> (int[@untagged])
  = "ocaml_blosc_decompress" "ocaml_blosc_decompress"
  [@@noalloc]

(* Get original (uncompressed) size from compressed buffer header - noalloc *)
external cbuffer_sizes : string -> (int[@untagged])
  = "ocaml_blosc_cbuffer_sizes" "ocaml_blosc_cbuffer_sizes"
  [@@noalloc]

(* Get compressor library name from compressed buffer - allocates string *)
external cbuffer_complib : string -> string
  = "ocaml_blosc_cbuffer_complib"

(* OxCaml: allocate bytes locally on stack - avoids GC for temporary buffers *)
external alloc_local_bytes : (int[@untagged]) -> (bytes[@local])
  = "ocaml_blosc_alloc_local_bytes" "ocaml_blosc_alloc_local_bytes"

(* Error checking helpers *)
let[@inline] is_error code = code < 0
