#include <caml/fail.h>
#include <caml/mlvalues.h>

/* Keep the primitive signatures in sync with csrc/ds4_stubs.c. This backend
 * links without Metal or the engine and fails before opening any model. */
CAMLprim value caml_ds4_set_log_handler(value v_closure) {
  (void)v_closure;
  return Val_unit;
}

CAMLprim value caml_ds4_clear_log_handler(value v_unit) {
  (void)v_unit;
  return Val_unit;
}

CAMLprim value caml_ds4_set_log_min_level(value v_level) {
  (void)v_level;
  return Val_unit;
}

CAMLprim value caml_ds4_vision_token_start(value v_span) {
  (void)v_span;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_vision_token_count(value v_span) {
  (void)v_span;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_engine_open(
    value v_backend,
    value v_model,
    value v_vision,
    value v_mtp) {
  (void)v_backend;
  (void)v_model;
  (void)v_vision;
  (void)v_mtp;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_engine_has_vision(value v_engine) {
  (void)v_engine;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_engine_mtp_draft_tokens(value v_engine) {
  (void)v_engine;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_engine_model_name(value v_engine) {
  (void)v_engine;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_engine_vocab_size(value v_engine) {
  (void)v_engine;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_token_eos(value v_engine) {
  (void)v_engine;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_token_is_stop(value v_engine, value v_token) {
  (void)v_engine;
  (void)v_token;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_engine_family(value v_engine) {
  (void)v_engine;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_think_mode_for_context(value v_mode, value v_ctx) {
  (void)v_mode;
  (void)v_ctx;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_encode_chat_prompt(
    value v_engine,
    value v_system,
    value v_prompt,
    value v_think) {
  (void)v_engine;
  (void)v_system;
  (void)v_prompt;
  (void)v_think;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_tokenize_rendered_chat(value v_engine, value v_text) {
  (void)v_engine;
  (void)v_text;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_tokens_begin(value v_engine) {
  (void)v_engine;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_tokens_append_rendered(
    value v_engine,
    value v_tokens,
    value v_text) {
  (void)v_engine;
  (void)v_tokens;
  (void)v_text;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_tokens_append_message(
    value v_engine,
    value v_tokens,
    value v_role,
    value v_content) {
  (void)v_engine;
  (void)v_tokens;
  (void)v_role;
  (void)v_content;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_tokens_append_multimodal(
    value v_engine,
    value v_tokens,
    value v_role,
    value v_parts,
    value v_images) {
  (void)v_engine;
  (void)v_tokens;
  (void)v_role;
  (void)v_parts;
  (void)v_images;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_tokens_append_think_prefix(
    value v_engine,
    value v_tokens,
    value v_think) {
  (void)v_engine;
  (void)v_tokens;
  (void)v_think;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_tokens_append_assistant_prefix(
    value v_engine,
    value v_tokens,
    value v_think) {
  (void)v_engine;
  (void)v_tokens;
  (void)v_think;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_create(
    value v_engine,
    value v_ctx,
    value v_seed) {
  (void)v_engine;
  (void)v_ctx;
  (void)v_seed;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_close(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_sync(value v_session, value v_tokens) {
  (void)v_session;
  (void)v_tokens;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_sync_multimodal(
    value v_session,
    value v_tokens,
    value v_images) {
  (void)v_session;
  (void)v_tokens;
  (void)v_images;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_cancel(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_clear_cancel(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_is_cancelled(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_common_prefix(value v_session, value v_tokens) {
  (void)v_session;
  (void)v_tokens;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_checkpoint_valid(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_directional_steering_ffn(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_set_directional_steering_ffn(
    value v_session,
    value v_scale) {
  (void)v_session;
  (void)v_scale;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_prefill_progress(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_ctx(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_pos(value v_session) {
  (void)v_session;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_sample(
    value v_session,
    value v_temp,
    value v_top_p,
    value v_min_p) {
  (void)v_session;
  (void)v_temp;
  (void)v_top_p;
  (void)v_min_p;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_eval(value v_session, value v_token) {
  (void)v_session;
  (void)v_token;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_eval_speculative(
    value v_session,
    value v_token,
    value v_max,
    value v_sampling) {
  (void)v_session;
  (void)v_token;
  (void)v_max;
  (void)v_sampling;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_session_rewind(value v_session, value v_pos) {
  (void)v_session;
  (void)v_pos;
  caml_failwith("DS4 Metal backend requires macOS");
}

CAMLprim value caml_ds4_token_text(value v_engine, value v_token) {
  (void)v_engine;
  (void)v_token;
  caml_failwith("DS4 Metal backend requires macOS");
}
