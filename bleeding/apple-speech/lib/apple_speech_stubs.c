#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern int32_t asp_available(void);
extern int32_t asp_locales(int32_t, char **);
extern int32_t asp_status(const char *, char **);
extern int32_t asp_install(const char *, char **);
extern int32_t asp_transcribe(const char *, const char *, int32_t, char **);
extern int32_t asp_duration(const char *, char **);

/* Every bridge call returns a code and one string: the result when the code
   is 0, otherwise the error message. */
static value pair(int32_t code, char *text) {
  CAMLparam0();
  CAMLlocal2(result, copy);
  copy = caml_copy_string(text != NULL ? text : "");
  free(text);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(code));
  Store_field(result, 1, copy);
  CAMLreturn(result);
}

/* OCaml strings may move once the runtime lock is released. */
static char *copy_option(value option) {
  if (Is_long(option)) return NULL;
  char *copy = strdup(String_val(Field(option, 0)));
  if (copy == NULL) caml_raise_out_of_memory();
  return copy;
}

CAMLprim value caml_apple_speech_available(value unit) {
  (void)unit;
  return Val_bool(asp_available() != 0);
}

CAMLprim value caml_apple_speech_locales(value installed) {
  CAMLparam1(installed);
  char *output = NULL;
  int32_t flag = Bool_val(installed);
  caml_enter_blocking_section();
  int32_t code = asp_locales(flag, &output);
  caml_leave_blocking_section();
  CAMLreturn(pair(code, output));
}

CAMLprim value caml_apple_speech_status(value locale) {
  CAMLparam1(locale);
  char *output = NULL;
  char *copy = copy_option(locale);
  caml_enter_blocking_section();
  int32_t code = asp_status(copy, &output);
  caml_leave_blocking_section();
  free(copy);
  CAMLreturn(pair(code, output));
}

CAMLprim value caml_apple_speech_install(value locale) {
  CAMLparam1(locale);
  char *output = NULL;
  char *copy = copy_option(locale);
  caml_enter_blocking_section();
  int32_t code = asp_install(copy, &output);
  caml_leave_blocking_section();
  free(copy);
  CAMLreturn(pair(code, output));
}

CAMLprim value caml_apple_speech_transcribe(value path, value locale,
                                            value install) {
  CAMLparam3(path, locale, install);
  char *output = NULL;
  char *path_copy = strdup(String_val(path));
  if (path_copy == NULL) caml_raise_out_of_memory();
  char *locale_copy = copy_option(locale);
  int32_t flag = Bool_val(install);
  caml_enter_blocking_section();
  int32_t code = asp_transcribe(path_copy, locale_copy, flag, &output);
  caml_leave_blocking_section();
  free(path_copy);
  free(locale_copy);
  CAMLreturn(pair(code, output));
}

CAMLprim value caml_apple_speech_duration(value path) {
  CAMLparam1(path);
  char *output = NULL;
  char *copy = strdup(String_val(path));
  if (copy == NULL) caml_raise_out_of_memory();
  caml_enter_blocking_section();
  int32_t code = asp_duration(copy, &output);
  caml_leave_blocking_section();
  free(copy);
  CAMLreturn(pair(code, output));
}
