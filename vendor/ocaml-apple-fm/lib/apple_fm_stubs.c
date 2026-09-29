#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern int32_t afm_availability(char **);
extern int32_t afm_model_info(const char *, const char *, char **, char **);
extern int32_t afm_model_token_count(const char *, int32_t, const char *,
                                     int64_t *, char **);
extern int32_t afm_transcript_compact(const char *, const char *, int32_t,
                                      char **, char **);
extern void *afm_session_create(const char *, const char *, const char *,
                                const char *, char **);
extern void afm_session_destroy(void *);
extern int32_t afm_session_prewarm(void *, const char *, char **);
extern int32_t afm_session_start(void *, const char *, const char *, int32_t,
                                 const char *, int32_t, double, int32_t, uint64_t,
                                 int32_t, double, int32_t, int32_t, char **);
extern int32_t afm_session_next_event(void *, int32_t, int32_t *, int64_t *,
                                      char **, char **);
extern int32_t afm_session_resolve_tool(void *, int64_t, const char *, char **);
extern int32_t afm_session_transcript(void *, char **, char **);
extern int32_t afm_session_replace_transcript(void *, const char *, char **);
extern int32_t afm_session_usage(void *, char **, char **);
extern void afm_session_cancel(void *);

static value copy_pair(int code, const char *message) {
  CAMLparam0();
  CAMLlocal2(pair, text);
  text = caml_copy_string(message != NULL ? message : "");
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, Val_int(code));
  Store_field(pair, 1, text);
  CAMLreturn(pair);
}

static const char *option_string(value option) {
  return Is_long(option) ? NULL : String_val(Field(option, 0));
}

static value call_string_result(int32_t code, char *output, char *error) {
  value result = copy_pair(code, code == 0 ? output : error);
  free(output);
  free(error);
  return result;
}

CAMLprim value caml_apple_fm_availability(value unit) {
  CAMLparam1(unit);
  (void)unit;
  char *detail = NULL;
  int32_t code = afm_availability(&detail);
  value result = copy_pair(code, detail);
  free(detail);
  CAMLreturn(result);
}

CAMLprim value caml_apple_fm_model_info(value config, value locale) {
  CAMLparam2(config, locale);
  char *output = NULL;
  char *error = NULL;
  int32_t code = afm_model_info(String_val(config), option_string(locale),
                                &output, &error);
  CAMLreturn(call_string_result(code, output, error));
}

CAMLprim value caml_apple_fm_model_token_count(value config, value kind,
                                                value payload) {
  CAMLparam3(config, kind, payload);
  CAMLlocal3(pair, count_value, error_value);
  char *error = NULL;
  int64_t count = -1;
  int32_t input_kind = Int_val(kind);
  char *config_copy = strdup(String_val(config));
  char *payload_copy = strdup(String_val(payload));
  if (config_copy == NULL || payload_copy == NULL) {
    free(config_copy);
    free(payload_copy);
    caml_raise_out_of_memory();
  }
  caml_enter_blocking_section();
  int32_t code = afm_model_token_count(config_copy, input_kind, payload_copy,
                                       &count, &error);
  caml_leave_blocking_section();
  free(config_copy);
  free(payload_copy);
  count_value = caml_copy_int64(count);
  error_value = caml_copy_string(code == 0 ? "" :
                                 (error != NULL ? error : ""));
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, count_value);
  Store_field(pair, 1, error_value);
  free(error);
  CAMLreturn(pair);
}

CAMLprim value caml_apple_fm_transcript_compact(value transcript, value summary,
                                                 value keep_last_turns) {
  CAMLparam3(transcript, summary, keep_last_turns);
  char *output = NULL;
  char *error = NULL;
  int32_t code = afm_transcript_compact(
      String_val(transcript), String_val(summary), Int_val(keep_last_turns),
      &output, &error);
  CAMLreturn(call_string_result(code, output, error));
}

CAMLprim value caml_apple_fm_session_create(value instructions, value tools,
                                             value config, value transcript) {
  CAMLparam4(instructions, tools, config, transcript);
  CAMLlocal3(pair, pointer, error_value);
  char *error = NULL;
  void *handle = afm_session_create(option_string(instructions), String_val(tools),
                                    String_val(config), option_string(transcript),
                                    &error);
  pointer = caml_copy_nativeint((intnat)handle);
  error_value = caml_copy_string(error != NULL ? error : "");
  free(error);
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, pointer);
  Store_field(pair, 1, error_value);
  CAMLreturn(pair);
}

CAMLprim value caml_apple_fm_session_destroy(value pointer) {
  CAMLparam1(pointer);
  if (Nativeint_val(pointer) != 0)
    afm_session_destroy((void *)Nativeint_val(pointer));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_apple_fm_session_prewarm(value pointer, value prefix) {
  CAMLparam2(pointer, prefix);
  char *error = NULL;
  int32_t code = afm_session_prewarm((void *)Nativeint_val(pointer),
                                     option_string(prefix), &error);
  value result = copy_pair(code, error);
  free(error);
  CAMLreturn(result);
}

CAMLprim value caml_apple_fm_session_start(value pointer, value request) {
  CAMLparam2(pointer, request);
  value prompt = Field(request, 0);
  value schema = Field(request, 1);
  value include_schema = Field(request, 2);
  value reasoning = Field(request, 3);
  value options = Field(request, 4);
  char *error = NULL;
  int32_t result = afm_session_start(
      (void *)Nativeint_val(pointer), String_val(prompt), option_string(schema),
      Int_val(include_schema), option_string(reasoning),
      Int_val(Field(options, 0)), Double_val(Field(options, 1)),
      Bool_val(Field(options, 2)), Int64_val(Field(options, 3)),
      Bool_val(Field(options, 4)), Double_val(Field(options, 5)),
      Int_val(Field(options, 6)), Int_val(Field(options, 7)), &error);
  value output = copy_pair(result, error);
  free(error);
  CAMLreturn(output);
}

static value session_string_result(value pointer,
                                   int32_t (*operation)(void *, char **, char **)) {
  char *output = NULL;
  char *error = NULL;
  int32_t code = operation((void *)Nativeint_val(pointer), &output, &error);
  return call_string_result(code, output, error);
}

CAMLprim value caml_apple_fm_session_transcript(value pointer) {
  CAMLparam1(pointer);
  CAMLreturn(session_string_result(pointer, afm_session_transcript));
}

CAMLprim value caml_apple_fm_session_usage(value pointer) {
  CAMLparam1(pointer);
  CAMLreturn(session_string_result(pointer, afm_session_usage));
}

CAMLprim value caml_apple_fm_session_replace_transcript(value pointer,
                                                         value transcript) {
  CAMLparam2(pointer, transcript);
  char *error = NULL;
  int32_t code = afm_session_replace_transcript(
      (void *)Nativeint_val(pointer), String_val(transcript), &error);
  value result = copy_pair(code, error);
  free(error);
  CAMLreturn(result);
}

CAMLprim value caml_apple_fm_session_next_event(value pointer, value timeout) {
  CAMLparam2(pointer, timeout);
  CAMLlocal4(tuple, id_value, first_value, second_value);
  int32_t kind = 0;
  int64_t id = 0;
  char *first = NULL;
  char *second = NULL;
  int32_t result;
  void *handle = (void *)Nativeint_val(pointer);
  int32_t timeout_ms = Int_val(timeout);
  caml_enter_blocking_section();
  result = afm_session_next_event(handle, timeout_ms, &kind, &id, &first,
                                  &second);
  caml_leave_blocking_section();
  if (result != 0) {
    free(first);
    free(second);
    caml_failwith("Foundation Models event queue failed");
  }
  id_value = caml_copy_int64(id);
  first_value = caml_copy_string(first != NULL ? first : "");
  second_value = caml_copy_string(second != NULL ? second : "");
  free(first);
  free(second);
  tuple = caml_alloc_tuple(4);
  Store_field(tuple, 0, Val_int(kind));
  Store_field(tuple, 1, id_value);
  Store_field(tuple, 2, first_value);
  Store_field(tuple, 3, second_value);
  CAMLreturn(tuple);
}

CAMLprim value caml_apple_fm_session_resolve_tool(value pointer, value id,
                                                   value result) {
  CAMLparam3(pointer, id, result);
  char *error = NULL;
  int32_t code = afm_session_resolve_tool(
      (void *)Nativeint_val(pointer), Int64_val(id), String_val(result), &error);
  value output = copy_pair(code, error);
  free(error);
  CAMLreturn(output);
}

CAMLprim value caml_apple_fm_session_cancel(value pointer) {
  CAMLparam1(pointer);
  if (Nativeint_val(pointer) != 0)
    afm_session_cancel((void *)Nativeint_val(pointer));
  CAMLreturn(Val_unit);
}
