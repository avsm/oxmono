/*---------------------------------------------------------------------------
   Copyright (c) 2024 The zstd-ocaml programmers. All rights reserved.
   SPDX-License-Identifier: BSD-3-Clause
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>

#include <zstd.h>
#include <string.h>

#define ZSTD_CCtx_val(v) (*((ZSTD_CCtx **) Data_custom_val(v)))
#define ZSTD_DCtx_val(v) (*((ZSTD_DCtx **) Data_custom_val(v)))

/* Version */

CAMLprim intnat ocaml_zstd_version_number(value unit)
{
  (void)unit;
  return ZSTD_versionNumber();
}

/* Error checking - noalloc, returns int (0 = no error, non-zero = error) */

CAMLprim intnat ocaml_zstd_is_error(intnat code)
{
  return ZSTD_isError((size_t)code) ? 1 : 0;
}

/* Get error name - allocates string */

CAMLprim value ocaml_zstd_get_error_name(intnat code)
{
  return caml_copy_string(ZSTD_getErrorName((size_t)code));
}

/* Compress bound - noalloc */

CAMLprim intnat ocaml_zstd_compress_bound(intnat src_size)
{
  return (intnat)ZSTD_compressBound((size_t)src_size);
}

/* Get frame content size - noalloc, returns int64 */

CAMLprim int64_t ocaml_zstd_get_frame_content_size(value src)
{
  return (int64_t)ZSTD_getFrameContentSize(String_val(src), caml_string_length(src));
}

/* Content size constants */

CAMLprim int64_t ocaml_zstd_content_size_unknown(value unit)
{
  (void)unit;
  return (int64_t)ZSTD_CONTENTSIZE_UNKNOWN;
}

CAMLprim int64_t ocaml_zstd_content_size_error(value unit)
{
  (void)unit;
  return (int64_t)ZSTD_CONTENTSIZE_ERROR;
}

/* Simple compression - dst is a bytes buffer */

CAMLprim intnat ocaml_zstd_compress(value dst, intnat dst_size,
                                     value src, intnat src_size,
                                     intnat level)
{
  return (intnat)ZSTD_compress(Bytes_val(dst), (size_t)dst_size,
                               String_val(src), (size_t)src_size,
                               (int)level);
}

/* Simple decompression */

CAMLprim intnat ocaml_zstd_decompress(value dst, intnat dst_size,
                                       value src, intnat src_size)
{
  return (intnat)ZSTD_decompress(Bytes_val(dst), (size_t)dst_size,
                                 String_val(src), (size_t)src_size);
}

/* Compression context management */

static void finalize_cctx(value v)
{
  ZSTD_CCtx *ctx = ZSTD_CCtx_val(v);
  if (ctx != NULL) {
    ZSTD_freeCCtx(ctx);
  }
}

static struct custom_operations cctx_ops = {
  "ocaml.zstd.cctx",
  finalize_cctx,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value ocaml_zstd_create_cctx(value unit)
{
  (void)unit;
  ZSTD_CCtx *ctx = ZSTD_createCCtx();
  if (ctx == NULL) {
    caml_failwith("ZSTD_createCCtx: allocation failed");
  }
  value v = caml_alloc_custom(&cctx_ops, sizeof(ZSTD_CCtx *), 0, 1);
  ZSTD_CCtx_val(v) = ctx;
  return v;
}

CAMLprim value ocaml_zstd_free_cctx(value v)
{
  ZSTD_CCtx *ctx = ZSTD_CCtx_val(v);
  if (ctx != NULL) {
    ZSTD_freeCCtx(ctx);
    ZSTD_CCtx_val(v) = NULL;
  }
  return Val_unit;
}

/* Compression with dictionary */

CAMLprim intnat ocaml_zstd_compress_using_dict(value cctx,
                                                value dst, intnat dst_size,
                                                value src, intnat src_size,
                                                value dict, intnat dict_size,
                                                intnat level)
{
  return (intnat)ZSTD_compress_usingDict(ZSTD_CCtx_val(cctx),
                                         Bytes_val(dst), (size_t)dst_size,
                                         String_val(src), (size_t)src_size,
                                         String_val(dict), (size_t)dict_size,
                                         (int)level);
}

/* Bytecode wrapper for compress_using_dict (8 args) */
CAMLprim value ocaml_zstd_compress_using_dict_bytecode(value *argv, int argn)
{
  (void)argn;
  return Val_long(ocaml_zstd_compress_using_dict(argv[0], argv[1], Long_val(argv[2]),
                                                  argv[3], Long_val(argv[4]),
                                                  argv[5], Long_val(argv[6]),
                                                  Long_val(argv[7])));
}

/* Decompression context management */

static void finalize_dctx(value v)
{
  ZSTD_DCtx *ctx = ZSTD_DCtx_val(v);
  if (ctx != NULL) {
    ZSTD_freeDCtx(ctx);
  }
}

static struct custom_operations dctx_ops = {
  "ocaml.zstd.dctx",
  finalize_dctx,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value ocaml_zstd_create_dctx(value unit)
{
  (void)unit;
  ZSTD_DCtx *ctx = ZSTD_createDCtx();
  if (ctx == NULL) {
    caml_failwith("ZSTD_createDCtx: allocation failed");
  }
  value v = caml_alloc_custom(&dctx_ops, sizeof(ZSTD_DCtx *), 0, 1);
  ZSTD_DCtx_val(v) = ctx;
  return v;
}

CAMLprim value ocaml_zstd_free_dctx(value v)
{
  ZSTD_DCtx *ctx = ZSTD_DCtx_val(v);
  if (ctx != NULL) {
    ZSTD_freeDCtx(ctx);
    ZSTD_DCtx_val(v) = NULL;
  }
  return Val_unit;
}

/* Decompression with dictionary */

CAMLprim intnat ocaml_zstd_decompress_using_dict(value dctx,
                                                  value dst, intnat dst_size,
                                                  value src, intnat src_size,
                                                  value dict, intnat dict_size)
{
  return (intnat)ZSTD_decompress_usingDict(ZSTD_DCtx_val(dctx),
                                           Bytes_val(dst), (size_t)dst_size,
                                           String_val(src), (size_t)src_size,
                                           String_val(dict), (size_t)dict_size);
}

/* Bytecode wrapper for decompress_using_dict (7 args) */
CAMLprim value ocaml_zstd_decompress_using_dict_bytecode(value *argv, int argn)
{
  (void)argn;
  return Val_long(ocaml_zstd_decompress_using_dict(argv[0], argv[1], Long_val(argv[2]),
                                                    argv[3], Long_val(argv[4]),
                                                    argv[5], Long_val(argv[6])));
}

/* OxCaml-optimized: allocate local bytes buffer for compression/decompression
   This avoids heap allocation for temporary buffers */

CAMLprim value ocaml_zstd_alloc_local_bytes(intnat len)
{
  return caml_alloc_local_string((mlsize_t)len);
}
