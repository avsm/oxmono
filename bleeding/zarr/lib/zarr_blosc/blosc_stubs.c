#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <string.h>
#include <blosc.h>

/* blosc_compress_ctx(clevel, doshuffle, typesize, nbytes, src, dest, destsize, compressor, blocksize, numinternalthreads) */
CAMLprim value blosc_compress_stub(value v_clevel, value v_doshuffle, value v_typesize,
                                    value v_src, value v_compressor, value v_blocksize)
{
  CAMLparam5(v_clevel, v_doshuffle, v_typesize, v_src, v_compressor);
  CAMLxparam1(v_blocksize);

  int clevel = Int_val(v_clevel);
  int doshuffle = Int_val(v_doshuffle);
  size_t typesize = Int_val(v_typesize);
  size_t nbytes = caml_string_length(v_src);
  const char *src = (const char *)Bytes_val(v_src);
  const char *compressor = String_val(v_compressor);
  size_t blocksize = Int_val(v_blocksize);

  /* Allocate destination buffer */
  size_t destsize = nbytes + BLOSC_MAX_OVERHEAD;
  char *dest = caml_stat_alloc(destsize);

  int compressed_size = blosc_compress_ctx(clevel, doshuffle, typesize,
                                            nbytes, src, dest, destsize,
                                            compressor, blocksize, 1);

  if (compressed_size < 0) {
    caml_stat_free(dest);
    caml_failwith("blosc_compress_ctx failed");
  }

  value result = caml_alloc_string(compressed_size);
  memcpy(Bytes_val(result), dest, compressed_size);
  caml_stat_free(dest);

  CAMLreturn(result);
}

CAMLprim value blosc_compress_stub_bytecode(value *argv, int argn)
{
  (void)argn;
  return blosc_compress_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value blosc_decompress_stub(value v_src)
{
  CAMLparam1(v_src);

  size_t nbytes, cbytes, blocksize;
  const char *src = (const char *)Bytes_val(v_src);

  blosc_cbuffer_sizes(src, &nbytes, &cbytes, &blocksize);

  if (nbytes == 0) {
    caml_failwith("blosc: invalid compressed data (nbytes=0)");
  }

  char *dest = caml_stat_alloc(nbytes);

  int decompressed_size = blosc_decompress_ctx(src, dest, nbytes, 1);

  if (decompressed_size < 0) {
    caml_stat_free(dest);
    caml_failwith("blosc: decompression failed");
  }

  value result = caml_alloc_string(decompressed_size);
  memcpy(Bytes_val(result), dest, decompressed_size);
  caml_stat_free(dest);

  CAMLreturn(result);
}
