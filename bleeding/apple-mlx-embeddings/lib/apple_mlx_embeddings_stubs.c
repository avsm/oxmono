#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern int32_t ame_available(void);
extern int32_t ame_memory_stats(int64_t *, int64_t *, int64_t *, int64_t *);
extern int32_t ame_load(const char *, int32_t, void **, char **);
extern int32_t ame_embed(void *, const char *const *, int32_t, int32_t,
                         float *, char **);
extern void ame_release(void *);

#define Handle(v) (*(void **)Data_custom_val(v))

CAMLprim value caml_ame_memory_stats(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  int64_t active, cache, peak, limit;
  caml_enter_blocking_section();
  int32_t code = ame_memory_stats(&active, &cache, &peak, &limit);
  caml_leave_blocking_section();
  if (code != 0) caml_failwith("MLX memory statistics are unavailable");
  result = caml_alloc_small(4, 0);
  Field(result, 0) = Val_long(active);
  Field(result, 1) = Val_long(cache);
  Field(result, 2) = Val_long(peak);
  Field(result, 3) = Val_long(limit);
  CAMLreturn(result);
}

static void finalize(value handle) {
  void *pointer = Handle(handle);
  Handle(handle) = NULL;
  if (pointer != NULL) ame_release(pointer);
}

static struct custom_operations operations = {
  .identifier = "apple-mlx-embeddings.model.v1",
  .finalize = finalize,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default,
  .compare_ext = custom_compare_ext_default,
  .fixed_length = custom_fixed_length_default
};

static void fail(char *error) {
  CAMLparam0();
  CAMLlocal1(message);
  message = caml_copy_string(error != NULL ? error : "MLX embedding failed");
  free(error);
  caml_failwith_value(message);
  CAMLreturn0;
}

CAMLprim value caml_ame_available(value unit) {
  (void)unit;
  return Val_bool(ame_available() != 0);
}

CAMLprim value caml_ame_load(value path, value max_tokens) {
  CAMLparam2(path, max_tokens);
  CAMLlocal1(handle);
  /* Allocate the finalizable owner before creating a native model. */
  handle = caml_alloc_custom(&operations, sizeof(void *), 0, 1);
  Handle(handle) = NULL;
  char *copy = strdup(String_val(path));
  if (copy == NULL) caml_raise_out_of_memory();
  int32_t limit = Int_val(max_tokens);
  void *pointer = NULL;
  char *error = NULL;
  caml_enter_blocking_section();
  int32_t code = ame_load(copy, limit, &pointer, &error);
  free(copy);
  caml_leave_blocking_section();
  Handle(handle) = pointer;
  if (code != 0) fail(error);
  free(error);
  if (pointer == NULL) caml_failwith("MLX returned an empty model handle");
  CAMLreturn(handle);
}

CAMLprim value caml_ame_embed(value handle, value inputs, value output) {
  CAMLparam3(handle, inputs, output);
  void *pointer = Handle(handle);
  if (pointer == NULL) caml_invalid_argument("MLX model is closed");
  mlsize_t count = Wosize_val(inputs);
  struct caml_ba_array *array = Caml_ba_array_val(output);
  if (count < 1 || count > 16 || array->num_dims != 2 ||
      array->dim[0] != (intnat)count || array->dim[1] < 32 ||
      array->dim[1] > 4096 ||
      (array->flags & CAML_BA_KIND_MASK) != CAML_BA_FLOAT32 ||
      (array->flags & CAML_BA_LAYOUT_MASK) != CAML_BA_C_LAYOUT)
    caml_invalid_argument("Invalid MLX output buffer");
  int32_t dimensions = array->dim[1];
  float *data = array->data;
  char *copies[16] = {NULL};
  for (mlsize_t i = 0; i < count; ++i) {
    copies[i] = strdup(String_val(Field(inputs, i)));
    if (copies[i] == NULL) {
      for (mlsize_t j = 0; j < i; ++j) free(copies[j]);
      caml_raise_out_of_memory();
    }
  }
  char *error = NULL;
  caml_enter_blocking_section();
  int32_t code = ame_embed(pointer, (const char *const *)copies,
                           (int32_t)count, dimensions, data, &error);
  for (mlsize_t i = 0; i < count; ++i) free(copies[i]);
  caml_leave_blocking_section();
  if (code != 0) fail(error);
  free(error);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ame_close(value handle) {
  CAMLparam1(handle);
  void *pointer = Handle(handle);
  Handle(handle) = NULL;
  if (pointer != NULL) {
    caml_enter_blocking_section();
    ame_release(pointer);
    caml_leave_blocking_section();
  }
  CAMLreturn(Val_unit);
}
