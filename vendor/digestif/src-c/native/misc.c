#include "digestif.h"

#define u_long_s sizeof(unsigned long)

static inline void xor_into(uint8_t *src, uint8_t *dst, size_t n) {
#if defined(__digestif_SSE2__)
  while (n >= 16) {
    _mm_storeu_si128((__m128i *)dst,
                     _mm_xor_si128(_mm_loadu_si128((__m128i *)src),
                                   _mm_loadu_si128((__m128i *)dst)));
    src += 16;
    dst += 16;
    n -= 16;
  }
#endif
#ifdef ARCH_SIXTYFOUR
  uint64_t s;
  for (; n >= 8; n -= 8, src += 8, dst += 8)
    *(uint64_t *)dst ^= *(uint64_t *)memcpy(&s, src, 8);
#endif

  uint32_t t;
  for (; n >= 4; n -= 4, src += 4, dst += 4)
    *(uint32_t *)dst ^= *(uint32_t *)memcpy(&t, src, 4);

  for (; n--; ++src, ++dst)
    *dst = *src ^ *dst;
}

CAMLprim value caml_digestif_ba_xor_into(value b1, value off1, value b2,
                                         value off2, value n) {
  xor_into(_ba_uint8_off(b1, off1), _ba_uint8_off(b2, off2), Int_val(n));
  return Val_unit;
}

CAMLprim value caml_digestif_st_xor_into(value b1, value off1, value b2,
                                         value off2, value n) {
  xor_into(_st_uint8_off(b1, off1), _st_uint8_off(b2, off2), Int_val(n));
  return Val_unit;
}
