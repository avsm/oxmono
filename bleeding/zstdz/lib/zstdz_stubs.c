/*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* C stubs for zstdz.

   Bounds are checked by the OCaml wrappers in zstdz.ml. Every stub here
   trusts its offsets and lengths.

   Source and destination are bigarray data pointers. That memory is
   outside the OCaml heap, so it neither moves under the collector nor
   needs to be copied, and the pointers stay valid while the runtime lock
   is released.

   Locally allocated results
   -------------------------
   zstdz_frame_info and zstdz_error_name return blocks allocated with
   caml_alloc_local and caml_alloc_local_string. These live in the
   caller's local stack region, not the minor heap, and the OCaml side
   receives them at mode local so they cannot escape.

   The contract, which is the reason this comment exists:

   - caml_alloc_local returns an uninitialised block. Its words hold
     whatever the local region last held, so a collector that scanned
     them would follow garbage. Nothing between the allocation and the
     last field store may allocate on the OCaml heap, call back into
     OCaml, or release the runtime lock. Both stubs below compute every
     field into C locals first and store them in one uninterrupted run.

   - A local block is not in the minor heap, so the write barrier must
     not be used on it. Fields are assigned directly with Field(v, i) =
     ..., never with caml_modify or Store_field. Only immediates are
     stored here, which is safe under either rule, but the direct form is
     the one that stays correct if a pointer field is ever added.

   - Local allocation itself never triggers a collection and never moves
     an existing block, so nesting local allocations is safe.

   ZSTD_getFrameHeader is declared under ZSTD_STATIC_LINKING_ONLY. It is
   exported from the shared library and has been stable since 1.4, but it
   is the one interface here outside libzstd's stability guarantee. The
   window size and the checksum flag are not reachable any other way. */

#define ZSTD_STATIC_LINKING_ONLY

#include <stdint.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <zstd.h>

/* Releasing and reacquiring the runtime lock costs more than compressing
   a small buffer, so only the larger calls pay for it. */
#define ZSTDZ_UNLOCK_THRESHOLD 65536

/* Compression contexts. */

#define Cctx_val(v) (*((ZSTD_CCtx **)Data_custom_val(v)))
#define Dctx_val(v) (*((ZSTD_DCtx **)Data_custom_val(v)))

static void zstdz_cctx_finalize(value v)
{
  ZSTD_CCtx *ctx = Cctx_val(v);
  if (ctx != NULL) {
    Cctx_val(v) = NULL;
    ZSTD_freeCCtx(ctx);
  }
}

static void zstdz_dctx_finalize(value v)
{
  ZSTD_DCtx *ctx = Dctx_val(v);
  if (ctx != NULL) {
    Dctx_val(v) = NULL;
    ZSTD_freeDCtx(ctx);
  }
}

static struct custom_operations zstdz_cctx_ops = {
  "org.recoil.zstdz.cctx",
  zstdz_cctx_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations zstdz_dctx_ops = {
  "org.recoil.zstdz.dctx",
  zstdz_dctx_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* The context workspace is malloced by libzstd and is far larger than
   the custom block, so it is declared as dependent memory to keep the
   collector's pressure estimate honest. The figure is a lower bound at
   the default level rather than a measurement. */
#define ZSTDZ_CTX_MEM (256 * 1024)

CAMLprim value zstdz_create_cctx(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(v);
  ZSTD_CCtx *ctx;

  v = caml_alloc_custom_mem(&zstdz_cctx_ops, sizeof(ZSTD_CCtx *),
                            ZSTDZ_CTX_MEM);
  /* Set before the create call so that a finalizer running on the path
     out of caml_raise_out_of_memory sees a null pointer, not garbage. */
  Cctx_val(v) = NULL;
  ctx = ZSTD_createCCtx();
  if (ctx == NULL) caml_raise_out_of_memory();
  Cctx_val(v) = ctx;
  CAMLreturn(v);
}

CAMLprim value zstdz_create_dctx(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(v);
  ZSTD_DCtx *ctx;

  v = caml_alloc_custom_mem(&zstdz_dctx_ops, sizeof(ZSTD_DCtx *),
                            ZSTDZ_CTX_MEM);
  Dctx_val(v) = NULL;
  ctx = ZSTD_createDCtx();
  if (ctx == NULL) caml_raise_out_of_memory();
  Dctx_val(v) = ctx;
  CAMLreturn(v);
}

/* An error result from libzstd is the size_t (0 - code). Both directions
   of that mapping are needed: here to hand the code to OCaml, and in
   zstdz_error_name to hand it back for naming. */
static intnat zstdz_result(size_t r)
{
  if (ZSTD_isError(r)) return -(intnat)ZSTD_getErrorCode(r);
  return (intnat)r;
}

CAMLprim value zstdz_compress_bound(value vn)
{
  return Val_long((intnat)ZSTD_compressBound((size_t)Long_val(vn)));
}

CAMLprim value zstdz_compress_native(value vctx, value vsrc, value vsrc_off,
                                     value vsrc_len, value vdst,
                                     value vdst_off, value vdst_len,
                                     value vlevel, value vchecksum)
{
  /* Every parameter is read into a C local here, before the runtime lock
     is released: no value may be touched while another domain may be
     collecting. */
  ZSTD_CCtx *ctx = Cctx_val(vctx);
  const char *src = (const char *)Caml_ba_data_val(vsrc) + Long_val(vsrc_off);
  char *dst = (char *)Caml_ba_data_val(vdst) + Long_val(vdst_off);
  size_t src_len = (size_t)Long_val(vsrc_len);
  size_t dst_len = (size_t)Long_val(vdst_len);
  int level = (int)Long_val(vlevel);
  int checksum = Bool_val(vchecksum);
  size_t r;

  r = ZSTD_CCtx_reset(ctx, ZSTD_reset_session_and_parameters);
  if (!ZSTD_isError(r))
    r = ZSTD_CCtx_setParameter(ctx, ZSTD_c_compressionLevel, level);
  if (!ZSTD_isError(r))
    r = ZSTD_CCtx_setParameter(ctx, ZSTD_c_checksumFlag, checksum);
  if (ZSTD_isError(r)) return Val_long(zstdz_result(r));

  if (src_len + dst_len > ZSTDZ_UNLOCK_THRESHOLD) {
    caml_release_runtime_system();
    r = ZSTD_compress2(ctx, dst, dst_len, src, src_len);
    caml_acquire_runtime_system();
  } else {
    r = ZSTD_compress2(ctx, dst, dst_len, src, src_len);
  }
  return Val_long(zstdz_result(r));
}

CAMLprim value zstdz_compress_bytecode(value *argv, int argn)
{
  (void)argn;
  return zstdz_compress_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                               argv[5], argv[6], argv[7], argv[8]);
}

CAMLprim value zstdz_decompress_native(value vctx, value vsrc, value vsrc_off,
                                       value vsrc_len, value vdst,
                                       value vdst_off, value vdst_len)
{
  ZSTD_DCtx *ctx = Dctx_val(vctx);
  const char *src = (const char *)Caml_ba_data_val(vsrc) + Long_val(vsrc_off);
  char *dst = (char *)Caml_ba_data_val(vdst) + Long_val(vdst_off);
  size_t src_len = (size_t)Long_val(vsrc_len);
  size_t dst_len = (size_t)Long_val(vdst_len);
  size_t r;

  if (src_len + dst_len > ZSTDZ_UNLOCK_THRESHOLD) {
    caml_release_runtime_system();
    r = ZSTD_decompressDCtx(ctx, dst, dst_len, src, src_len);
    caml_acquire_runtime_system();
  } else {
    r = ZSTD_decompressDCtx(ctx, dst, dst_len, src, src_len);
  }
  return Val_long(zstdz_result(r));
}

CAMLprim value zstdz_decompress_bytecode(value *argv, int argn)
{
  (void)argn;
  return zstdz_decompress_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                                 argv[5], argv[6]);
}

/* The native half of an external whose result type is int64# returns the
   raw int64_t. The bytecode half boxes it. */
int64_t zstdz_content_size_native(value vbuf, value voff, value vlen)
{
  const char *p = (const char *)Caml_ba_data_val(vbuf) + Long_val(voff);
  unsigned long long n = ZSTD_getFrameContentSize(p, (size_t)Long_val(vlen));

  if (n == ZSTD_CONTENTSIZE_UNKNOWN) return -1;
  if (n == ZSTD_CONTENTSIZE_ERROR) return -2;
  return (int64_t)n;
}

CAMLprim value zstdz_content_size_bytecode(value vbuf, value voff, value vlen)
{
  return caml_copy_int64(zstdz_content_size_native(vbuf, voff, vlen));
}

/* Fields, in the order of the frame_info record in zstdz.mli:
   content_size, window_size, dict_id, has_checksum. All four are
   immediates, so the block is filled without a write barrier. See the
   local allocation contract at the top of this file. */
CAMLprim value zstdz_frame_info(value vbuf, value voff, value vlen)
{
  const char *p = (const char *)Caml_ba_data_val(vbuf) + Long_val(voff);
  ZSTD_frameHeader h;
  intnat content, window, dict, checksum;
  size_t r;
  value v;

  r = ZSTD_getFrameHeader(&h, p, (size_t)Long_val(vlen));
  if (r != 0) {
    /* A non-zero result is either an error code or the number of bytes
       the header still needs. Neither leaves h filled. */
    content = -2;
    window = 0;
    dict = 0;
    checksum = 0;
  } else {
    content = (h.frameContentSize == ZSTD_CONTENTSIZE_UNKNOWN)
                ? -1
                : (intnat)h.frameContentSize;
    window = (intnat)h.windowSize;
    dict = (intnat)h.dictID;
    checksum = (h.checksumFlag != 0);
  }

  v = caml_alloc_local(4, 0);
  Field(v, 0) = Val_long(content);
  Field(v, 1) = Val_long(window);
  Field(v, 2) = Val_long(dict);
  Field(v, 3) = Val_bool(checksum);
  return v;
}

/* ZSTD_getErrorName takes a function result, not an error code, so the
   code is mapped back to the (0 - code) form libzstd uses. A code that
   names no error yields "No error detected", and one out of range yields
   "Unspecified error code". */
CAMLprim value zstdz_error_name(value vcode)
{
  const char *s = ZSTD_getErrorName((size_t)0 - (size_t)Long_val(vcode));
  size_t n = strlen(s);
  value v;

  v = caml_alloc_local_string(n);
  memcpy((char *)String_val(v), s, n);
  return v;
}
