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

extern int32_t ame_vision_load(const char *, int32_t, void **, char **);
extern int32_t ame_vision_embed(void *, int32_t, const char *,
                                const char *const *, int32_t, int32_t,
                                float *, char **);
extern void ame_vision_release(void *);

#define Handle(v) (*(void **)Data_custom_val(v))

static void finalize(value handle) {
  void *pointer = Handle(handle);
  Handle(handle) = NULL;
  if (pointer != NULL) ame_vision_release(pointer);
}

static struct custom_operations operations = {
  .identifier = "apple-mlx-embeddings.vision.v1",
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

CAMLprim value caml_ame_vision_load(value path, value max_tokens) {
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
  int32_t code = ame_vision_load(copy, limit, &pointer, &error);
  free(copy);
  caml_leave_blocking_section();
  Handle(handle) = pointer;
  if (code != 0) fail(error);
  free(error);
  if (pointer == NULL) caml_failwith("MLX returned an empty model handle");
  CAMLreturn(handle);
}

CAMLprim value caml_ame_vision_embed(value handle, value images, value instruction,
                                    value inputs, value output) {
  CAMLparam5(handle, images, instruction, inputs, output);
  void *pointer = Handle(handle);
  if (pointer == NULL) caml_invalid_argument("MLX model is closed");
  mlsize_t count = Wosize_val(inputs);
  struct caml_ba_array *array = Caml_ba_array_val(output);
  if (count < 1 || count > 2 || array->num_dims != 2 ||
      array->dim[0] != (intnat)count || array->dim[1] < 64 ||
      array->dim[1] > 4096 ||
      (array->flags & CAML_BA_KIND_MASK) != CAML_BA_FLOAT32 ||
      (array->flags & CAML_BA_LAYOUT_MASK) != CAML_BA_C_LAYOUT)
    caml_invalid_argument("Invalid MLX output buffer");
  int32_t dimensions = array->dim[1];
  float *data = array->data;
  int32_t image_mode = Bool_val(images);
  char *instruction_copy = strdup(String_val(instruction));
  if (instruction_copy == NULL) caml_raise_out_of_memory();
  char *copies[2] = {NULL};
  for (mlsize_t i = 0; i < count; ++i) {
    copies[i] = strdup(String_val(Field(inputs, i)));
    if (copies[i] == NULL) {
      for (mlsize_t j = 0; j < i; ++j) free(copies[j]);
      free(instruction_copy);
      caml_raise_out_of_memory();
    }
  }
  char *error = NULL;
  caml_enter_blocking_section();
  int32_t code = ame_vision_embed(pointer, image_mode, instruction_copy,
                                  (const char *const *)copies,
                                  (int32_t)count, dimensions, data, &error);
  free(instruction_copy);
  for (mlsize_t i = 0; i < count; ++i) free(copies[i]);
  caml_leave_blocking_section();
  if (code != 0) fail(error);
  free(error);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ame_vision_close(value handle) {
  CAMLparam1(handle);
  void *pointer = Handle(handle);
  Handle(handle) = NULL;
  if (pointer != NULL) {
    caml_enter_blocking_section();
    ame_vision_release(pointer);
    caml_leave_blocking_section();
  }
  CAMLreturn(Val_unit);
}
