(** Direct C bindings to libzstd with OxCaml optimizations *)

(* Opaque context types *)
type cctx
type dctx

(* Version - noalloc *)
external version_number : unit -> (int[@untagged])
  = "ocaml_zstd_version_number" "ocaml_zstd_version_number"
  [@@noalloc]

(* Error checking - noalloc, returns int (0 = no error, non-zero = error) *)
external is_error_int : (int[@untagged]) -> (int[@untagged])
  = "ocaml_zstd_is_error" "ocaml_zstd_is_error"
  [@@noalloc]

let[@inline] is_error code = is_error_int code <> 0

(* Get error name - allocates string *)
external get_error_name : (int[@untagged]) -> string
  = "ocaml_zstd_get_error_name" "ocaml_zstd_get_error_name"

(* Compress bound - noalloc *)
external compress_bound : (int[@untagged]) -> (int[@untagged])
  = "ocaml_zstd_compress_bound" "ocaml_zstd_compress_bound"
  [@@noalloc]

(* Frame content size - noalloc, returns int64 for large sizes *)
external get_frame_content_size : string -> (int64[@unboxed])
  = "ocaml_zstd_get_frame_content_size" "ocaml_zstd_get_frame_content_size"
  [@@noalloc]

(* Content size constants *)
external content_size_unknown : unit -> (int64[@unboxed])
  = "ocaml_zstd_content_size_unknown" "ocaml_zstd_content_size_unknown"
  [@@noalloc]

external content_size_error : unit -> (int64[@unboxed])
  = "ocaml_zstd_content_size_error" "ocaml_zstd_content_size_error"
  [@@noalloc]

(* Simple compression - dst must be large enough (use compress_bound) *)
(* Uses [@local_opt] to accept both local and global bytes *)
external compress
  : (bytes[@local_opt]) -> (int[@untagged]) -> string -> (int[@untagged]) -> (int[@untagged])
  -> (int[@untagged])
  = "ocaml_zstd_compress" "ocaml_zstd_compress"
  [@@noalloc]

(* Simple decompression *)
external decompress
  : (bytes[@local_opt]) -> (int[@untagged]) -> string -> (int[@untagged])
  -> (int[@untagged])
  = "ocaml_zstd_decompress" "ocaml_zstd_decompress"
  [@@noalloc]

(* Compression context *)
external create_cctx : unit -> cctx = "ocaml_zstd_create_cctx"
external free_cctx : cctx -> unit = "ocaml_zstd_free_cctx" [@@noalloc]

(* Compression with dictionary *)
external compress_using_dict
  : cctx -> (bytes[@local_opt]) -> (int[@untagged])
  -> string -> (int[@untagged])
  -> string -> (int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged])
  = "ocaml_zstd_compress_using_dict_bytecode" "ocaml_zstd_compress_using_dict"
  [@@noalloc]

(* Decompression context *)
external create_dctx : unit -> dctx = "ocaml_zstd_create_dctx"
external free_dctx : dctx -> unit = "ocaml_zstd_free_dctx" [@@noalloc]

(* Decompression with dictionary *)
external decompress_using_dict
  : dctx -> (bytes[@local_opt]) -> (int[@untagged])
  -> string -> (int[@untagged])
  -> string -> (int[@untagged])
  -> (int[@untagged])
  = "ocaml_zstd_decompress_using_dict_bytecode" "ocaml_zstd_decompress_using_dict"
  [@@noalloc]

(* OxCaml: allocate bytes locally on stack - avoids GC for temporary buffers *)
external alloc_local_bytes : (int[@untagged]) -> (bytes[@local])
  = "ocaml_zstd_alloc_local_bytes" "ocaml_zstd_alloc_local_bytes"
