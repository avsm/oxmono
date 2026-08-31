/*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* C stubs for zarrz. The bigarray reinterpret view lives here. */

#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

/* Bytes per element of each bigarray kind, indexed by enum caml_ba_kind.
   The runtime's own caml_ba_element_size is only declared under
   CAML_INTERNALS, so keep a copy here. Re-vendoring check: the order
   must stay in step with enum caml_ba_kind in caml/bigarray.h. */
static const int zarrz_ba_element_size[] = {
    4,               /* CAML_BA_FLOAT32 */
    8,               /* CAML_BA_FLOAT64 */
    1,               /* CAML_BA_SINT8 */
    1,               /* CAML_BA_UINT8 */
    2,               /* CAML_BA_SINT16 */
    2,               /* CAML_BA_UINT16 */
    4,               /* CAML_BA_INT32 */
    8,               /* CAML_BA_INT64 */
    sizeof(value),   /* CAML_BA_CAML_INT */
    sizeof(intnat),  /* CAML_BA_NATIVE_INT */
    8,               /* CAML_BA_COMPLEX32 */
    16,              /* CAML_BA_COMPLEX64 */
    1,               /* CAML_BA_CHAR */
    2                /* CAML_BA_FLOAT16 */
};

/* Make [dst] share the storage of [src] and keep it alive.

   caml_ba_finalize frees a managed array's data only when its proxy is
   NULL, and otherwise drops a reference and frees the data when the
   count reaches zero. So attaching one proxy to both arrays is what
   stops the buffer being freed while either view is reachable. This
   mirrors the runtime's own caml_ba_update_proxy, which caml_ba_sub
   uses and which the runtime does not export. The proxy is malloc'd
   because caml_ba_finalize frees it with free. */
static void zarrz_ba_share(struct caml_ba_array *src,
                           struct caml_ba_array *dst)
{
  struct caml_ba_proxy *proxy;

  if ((src->flags & CAML_BA_MANAGED_MASK) == CAML_BA_EXTERNAL) return;
  if (src->proxy != NULL) {
    dst->proxy = src->proxy;
    atomic_fetch_add(&src->proxy->refcount, 1);
    return;
  }
  proxy = malloc(sizeof(struct caml_ba_proxy));
  if (proxy == NULL) caml_raise_out_of_memory();
  atomic_store(&proxy->refcount, 2); /* the source view and the new one */
  proxy->data = src->data;
  proxy->size =
      (src->flags & CAML_BA_MAPPED_FILE) ? caml_ba_byte_size(src) : 0;
  src->proxy = proxy;
  dst->proxy = proxy;
}

/* zarrz_ba_reinterpret buf kind dims

   A new bigarray header of element kind [kind] and dimensions [dims]
   over the data of the one dimensional char bigarray [buf], with no
   copy. [dims] is an OCaml int array. The element count times the
   element size of [kind] must be exactly the length of [buf]. */
CAMLprim value zarrz_ba_reinterpret(value vbuf, value vkind, value vdims)
{
  CAMLparam3(vbuf, vkind, vdims);
  CAMLlocal1(res);
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  struct caml_ba_array *src;
  void *data;
  intnat num_elts, avail;
  int num_dims, kind, i, flags;

  num_dims = (int)Wosize_val(vdims);
  if (num_dims > CAML_BA_MAX_NUM_DIMS)
    caml_invalid_argument("Zarrz.Slab.to_genarray: rank exceeds 16");

  kind = Caml_ba_kind_val(vkind);
  if (kind < 0 || kind >= CAML_BA_FIRST_UNIMPLEMENTED_KIND)
    caml_invalid_argument("Zarrz.Slab.to_genarray: unsupported bigarray kind");

  num_elts = 1;
  for (i = 0; i < num_dims; i++) {
    intnat d = Long_val(Field(vdims, i));
    if (d < 0)
      caml_invalid_argument("Zarrz.Slab.to_genarray: negative dimension");
    if (d != 0 && num_elts > Max_long / d)
      caml_invalid_argument("Zarrz.Slab.to_genarray: shape overflow");
    num_elts *= d;
    dim[i] = d;
  }
  if (num_elts > Max_long / zarrz_ba_element_size[kind])
    caml_invalid_argument("Zarrz.Slab.to_genarray: shape overflow");

  src = Caml_ba_array_val(vbuf);
  if (src->num_dims != 1)
    caml_invalid_argument("Zarrz.Slab.to_genarray: source is not linear");
  avail = src->dim[0];
  if (num_elts * zarrz_ba_element_size[kind] != avail)
    caml_invalid_argument("Zarrz.Slab.to_genarray: shape does not fill buffer");

  /* Bigarray data is off heap, so this pointer survives the allocation
     below even though the header block may move. */
  data = src->data;
  flags = (int)(src->flags & CAML_BA_MANAGED_MASK) | kind | CAML_BA_C_LAYOUT |
          CAML_BA_SUBARRAY;

  res = caml_ba_alloc(flags, num_dims, data, dim);
  src = Caml_ba_array_val(vbuf); /* re-read: the block may have moved */
  zarrz_ba_share(src, Caml_ba_array_val(res));
  CAMLreturn(res);
}
