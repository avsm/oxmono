/*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* C stubs for bloscz.

   Bounds, the compressor name length, the compression level and the
   type size are checked by the OCaml wrappers in bloscz.ml. Every stub
   here trusts them. The type size in particular must not be zero:
   blosc divides by it and raises SIGFPE.

   Source and destination are bigarray data pointers. That memory is
   outside the OCaml heap, so it neither moves under the collector nor
   needs to be copied, and the pointers stay valid while the runtime
   lock is released.

   The compressor name is the one argument that lives in the OCaml heap.
   A moving collector may relocate it the moment the lock is released,
   so bloscz_compress_native copies it into a stack buffer first, along
   with every other parameter, and hands blosc the copy.

   Only the context interface is bound. blosc_compress_ctx and
   blosc_decompress_ctx take no global lock, need no blosc_init and keep
   no state between calls, so nothing here has to be serialised against
   anything else. numinternalthreads is always 1: blosc would otherwise
   spawn a pool of its own behind the runtime's back, and a caller that
   wants parallelism has domains. */

#include <stdint.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <blosc.h>

#if BLOSC_VERSION_MAJOR < 1 || \
    (BLOSC_VERSION_MAJOR == 1 && BLOSC_VERSION_MINOR < 21)
#error "bloscz needs C-Blosc 1.21 or later for blosc_cbuffer_validate"
#endif

/* Releasing and reacquiring the runtime lock costs more than
   compressing a small buffer, so only the larger calls pay for it. */
#define BLOSCZ_UNLOCK_THRESHOLD 65536

/* One more than the longest name blosc knows, which is "blosclz". The
   OCaml side refuses anything that would not fit. */
#define BLOSCZ_CNAME_MAX 32

CAMLprim value bloscz_max_overhead(value unit)
{
  (void)unit;
  return Val_long(BLOSC_MAX_OVERHEAD);
}

/* The returned pointer is into static storage owned by blosc, so it is
   copied and never freed. */
CAMLprim value bloscz_list_compressors(value unit)
{
  (void)unit;
  return caml_copy_string(blosc_list_compressors());
}

CAMLprim value bloscz_compress_native(value vcname, value vsrc,
                                      value vsrc_off, value vsrc_len,
                                      value vdst, value vdst_off,
                                      value vdst_len, value vlevel,
                                      value vshuffle, value vtypesize,
                                      value vblocksize)
{
  /* Every parameter is read into a C local here, before the runtime
     lock is released: no value may be touched while another domain may
     be collecting. The name is copied rather than pointed at for the
     same reason. */
  char cname[BLOSCZ_CNAME_MAX];
  const char *src = (const char *)Caml_ba_data_val(vsrc) + Long_val(vsrc_off);
  char *dst = (char *)Caml_ba_data_val(vdst) + Long_val(vdst_off);
  size_t src_len = (size_t)Long_val(vsrc_len);
  size_t dst_len = (size_t)Long_val(vdst_len);
  int level = (int)Long_val(vlevel);
  int shuffle = (int)Long_val(vshuffle);
  size_t typesize = (size_t)Long_val(vtypesize);
  size_t blocksize = (size_t)Long_val(vblocksize);
  size_t cname_len = caml_string_length(vcname);
  int r;

  memcpy(cname, String_val(vcname), cname_len);
  cname[cname_len] = '\0';

  if (src_len + dst_len > BLOSCZ_UNLOCK_THRESHOLD) {
    caml_release_runtime_system();
    r = blosc_compress_ctx(level, shuffle, typesize, src_len, src, dst,
                           dst_len, cname, blocksize, 1);
    caml_acquire_runtime_system();
  } else {
    r = blosc_compress_ctx(level, shuffle, typesize, src_len, src, dst,
                           dst_len, cname, blocksize, 1);
  }
  return Val_long((intnat)r);
}

CAMLprim value bloscz_compress_bytecode(value *argv, int argn)
{
  (void)argn;
  return bloscz_compress_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5], argv[6], argv[7], argv[8], argv[9],
                                argv[10]);
}

/* src_len is not passed to blosc: blosc_decompress_ctx takes the frame
   at src and reads its own header for the length. The OCaml side has
   already checked that those bytes are inside the bigstring, and a
   frame that lies about its length is caught by the block offset checks
   blosc does on the way in. */
CAMLprim value bloscz_decompress_native(value vsrc, value vsrc_off,
                                        value vsrc_len, value vdst,
                                        value vdst_off, value vdst_len)
{
  const char *src = (const char *)Caml_ba_data_val(vsrc) + Long_val(vsrc_off);
  char *dst = (char *)Caml_ba_data_val(vdst) + Long_val(vdst_off);
  size_t src_len = (size_t)Long_val(vsrc_len);
  size_t dst_len = (size_t)Long_val(vdst_len);
  int r;

  if (src_len + dst_len > BLOSCZ_UNLOCK_THRESHOLD) {
    caml_release_runtime_system();
    r = blosc_decompress_ctx(src, dst, dst_len, 1);
    caml_acquire_runtime_system();
  } else {
    r = blosc_decompress_ctx(src, dst, dst_len, 1);
  }
  return Val_long((intnat)r);
}

CAMLprim value bloscz_decompress_bytecode(value *argv, int argn)
{
  (void)argn;
  return bloscz_decompress_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                                  argv[5]);
}

/* Fields, in the order of the labelled tuple in bloscz.mli: nbytes,
   cbytes, blocksize. A labelled tuple has the representation of the
   plain tuple of its components, so this is an ordinary three word
   block of immediates. blosc_cbuffer_sizes fills all three with zero
   when it does not recognise the header. */
CAMLprim value bloscz_buffer_sizes(value vbuf, value voff)
{
  CAMLparam2(vbuf, voff);
  CAMLlocal1(v);
  const char *p = (const char *)Caml_ba_data_val(vbuf) + Long_val(voff);
  size_t nbytes = 0, cbytes = 0, blocksize = 0;

  blosc_cbuffer_sizes(p, &nbytes, &cbytes, &blocksize);
  v = caml_alloc_tuple(3);
  Field(v, 0) = Val_long((intnat)nbytes);
  Field(v, 1) = Val_long((intnat)cbytes);
  Field(v, 2) = Val_long((intnat)blocksize);
  CAMLreturn(v);
}

/* -1 for a range that is not exactly one safe frame, otherwise the
   decompressed size. blosc_cbuffer_validate checks the length itself,
   so a range shorter than a header is rejected rather than read. */
CAMLprim value bloscz_validate(value vbuf, value voff, value vlen)
{
  const char *p = (const char *)Caml_ba_data_val(vbuf) + Long_val(voff);
  size_t nbytes = 0;

  if (blosc_cbuffer_validate(p, (size_t)Long_val(vlen), &nbytes) != 0)
    return Val_long(-1);
  return Val_long((intnat)nbytes);
}
