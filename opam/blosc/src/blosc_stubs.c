/*---------------------------------------------------------------------------
   Copyright (c) 2024 The blosc-ocaml programmers. All rights reserved.
   SPDX-License-Identifier: BSD-3-Clause
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <blosc.h>
#include <string.h>

/* Max overhead constant - noalloc */

CAMLprim intnat ocaml_blosc_max_overhead(value unit)
{
  (void)unit;
  return (intnat)BLOSC_MAX_OVERHEAD;
}

/* Compress - noalloc
   dst is a bytes buffer, src is a string, compressor is a string.
   Uses blosc_compress_ctx with numinternalthreads=1.
   Returns compressed size (negative on error). */

CAMLprim intnat ocaml_blosc_compress(value dst, intnat dst_size,
                                      value src, intnat src_size,
                                      intnat clevel, intnat doshuffle,
                                      intnat typesize, value compressor,
                                      intnat blocksize)
{
  return (intnat)blosc_compress_ctx((int)clevel, (int)doshuffle,
                                    (size_t)typesize,
                                    (size_t)src_size,
                                    String_val(src),
                                    Bytes_val(dst),
                                    (size_t)dst_size,
                                    String_val(compressor),
                                    (size_t)blocksize, 1);
}

/* Bytecode wrapper for compress (9 args: 3 value + 6 intnat) */
CAMLprim value ocaml_blosc_compress_bytecode(value *argv, int argn)
{
  (void)argn;
  return Val_long(ocaml_blosc_compress(argv[0], Long_val(argv[1]),
                                        argv[2], Long_val(argv[3]),
                                        Long_val(argv[4]), Long_val(argv[5]),
                                        Long_val(argv[6]), argv[7],
                                        Long_val(argv[8])));
}

/* Decompress - noalloc
   dst is a bytes buffer, src is a string.
   Uses blosc_decompress_ctx with numinternalthreads=1.
   Returns decompressed size (negative on error). */

CAMLprim intnat ocaml_blosc_decompress(value dst, intnat dst_size,
                                        value src, intnat src_size)
{
  (void)src_size;
  return (intnat)blosc_decompress_ctx(String_val(src),
                                      Bytes_val(dst),
                                      (size_t)dst_size, 1);
}

/* Get cbuffer sizes - noalloc, returns nbytes (original uncompressed size) */

CAMLprim intnat ocaml_blosc_cbuffer_sizes(value src)
{
  size_t nbytes, cbytes, blocksize;
  blosc_cbuffer_sizes(String_val(src), &nbytes, &cbytes, &blocksize);
  return (intnat)nbytes;
}

/* Get cbuffer complib - allocates a string */

CAMLprim value ocaml_blosc_cbuffer_complib(value src)
{
  const char *complib = blosc_cbuffer_complib(String_val(src));
  return caml_copy_string(complib);
}

/* OxCaml-optimized: allocate local bytes buffer
   This avoids heap allocation for temporary buffers */

CAMLprim value ocaml_blosc_alloc_local_bytes(intnat len)
{
  return caml_alloc_local_string((mlsize_t)len);
}
