/* =========================================================================
 * OCaml 5 FFI to the DS4 inference engine.
 *
 * Backend-agnostic: the backend (Metal/CUDA/CPU) is chosen at link time by
 * which engine archive this stub is bundled with, and passed in at open time as
 * a ds4_backend value (see caml_ds4_engine_open). One copy of this file is
 * compiled into each dune implementation library (lib_metal/, lib_cpu/).
 * ========================================================================= */

#include <pthread.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include "ds4.h"

/* =========================================================================
 * Diagnostic log forwarding.
 *
 * The C engine emits diagnostics through ds4_set_log_callback (csrc/ds4.h).  We
 * register [ds4_log_trampoline], which forwards each message to an OCaml
 * closure ([V4.forward_logs] installs one that reports into the Logs library).
 *
 * Two hazards shape this:
 *
 *  - Runtime lock.  The heavy engine calls below run inside
 *    caml_release_runtime_system(); a diagnostic fired there must re-acquire the
 *    runtime lock before touching any OCaml value.  [t_runtime_released] tracks
 *    whether this thread is in such a section, and DS4_ENTER/LEAVE_BLOCKING keep
 *    it in sync.  The few calls that stay under the lock (a field read, a token
 *    lookup) leave it 0 and the trampoline calls straight through.
 *
 *  - Foreign threads.  A few engine diagnostics (the Metal streaming-expert
 *    pread workers) run on raw pthreads the OCaml runtime does not know about;
 *    calling into OCaml from there is undefined.  [t_ocaml_thread] is set by
 *    DS4_MARK_OCAML_THREAD at the top of every stub, so it is 1 exactly on
 *    threads that have entered this file from OCaml and 0 on engine-internal
 *    pthreads, whose messages go to stderr instead.
 *
 * Both flags are thread-local.  They describe the calling thread, not the
 * process: [V4.create ?domain_mgr] gives each engine its own worker domain, so
 * process-wide flags would let one engine's state describe another's thread —
 * and a stale "lock already held" reading would call into OCaml without the
 * runtime lock, which corrupts the heap.
 * ========================================================================= */

static value g_log_closure = Val_unit;
static int g_log_handler_installed = 0;
static _Atomic int g_log_min = 3; /* 0=Error 1=Warning 2=Info 3=Debug */
static _Thread_local int t_runtime_released = 0;
static _Thread_local int t_ocaml_thread = 0;

/* Every CAMLprim below is, by construction, entered from OCaml; recording that
 * is what lets the trampoline tell an OCaml thread from an engine pthread. */
#define DS4_MARK_OCAML_THREAD() (t_ocaml_thread = 1)

/* Mark the start/end of a runtime-lock-released engine call. */
#define DS4_ENTER_BLOCKING()              \
    do {                                  \
        caml_release_runtime_system();    \
        t_runtime_released = 1;           \
    } while (0)
#define DS4_LEAVE_BLOCKING()              \
    do {                                  \
        t_runtime_released = 0;           \
        caml_acquire_runtime_system();    \
    } while (0)

/* Whether an untyped diagnostic reports that something went wrong.  The vendor
 * patch turns every upstream fprintf(stderr, ...) into an untyped diagnostic,
 * so the reason a model failed to open arrives with the same type as the name
 * of the GPU.  The caller only sees "cannot open model", so a message that
 * explains a failure must stay visible at the default verbosity, and the rest
 * must not.  Only the start of the message is read, which is where upstream
 * says what happened. */
static int ds4_log_reports_failure(const char *msg) {
    static const char *const words[] = {
        "fail",       "error",       "cannot",       "can't",
        "unable",     "invalid",     "unsupported",  "not supported",
        "compatible", "requires",    "needs",        "must ",
        "missing",    "not found",   "out of memory", "too large",
        "exceed",     "mismatch",    "outside",      "refus",
    };
    char lower[256];
    size_t n = 0;
    for (; msg[n] != '\0' && n < sizeof(lower) - 1; n++) {
        char c = msg[n];
        lower[n] = (c >= 'A' && c <= 'Z') ? (char)(c - 'A' + 'a') : c;
    }
    lower[n] = '\0';
    for (size_t i = 0; i < sizeof(words) / sizeof(words[0]); i++)
        if (strstr(lower, words[i]) != NULL) return 1;
    return 0;
}

/* Map a diagnostic to the integer level the OCaml side expects. An untyped one
 * is a warning when it reports a failure, a debug message when it ends in a
 * carriage return, which is how upstream redraws a progress line in place, and
 * an informational one otherwise. */
static int ds4_log_level_of(ds4_log_type type, const char *msg) {
    switch (type) {
    case DS4_LOG_ERROR:
        return 0;
    case DS4_LOG_WARNING:
        return 1;
    case DS4_LOG_DEFAULT: {
        size_t len = strlen(msg);
        if (len > 0 && msg[len - 1] == '\r') return 3;
        return ds4_log_reports_failure(msg) ? 1 : 2;
    }
    case DS4_LOG_OK:
    case DS4_LOG_TIMING:
        return 2;
    default:
        return 3; /* PREFILL, GENERATION, KVCACHE, TOOL */
    }
}

static void ds4_log_trampoline(void *ud, ds4_log_type type, const char *msg) {
    (void)ud;
    const int level = ds4_log_level_of(type, msg);
    if (level > g_log_min) {
        /* Everything below the requested verbosity is dropped before crossing
         * into OCaml, except an error.  The engine calls exit() straight after
         * reporting one, so silencing it would leave the process dying without
         * saying why, and no verbosity setting should be able to ask for
         * that. */
        if (type == DS4_LOG_ERROR) {
            fputs(msg, stderr);
            fflush(stderr);
        }
        return;
    }

    if (!t_ocaml_thread) {
        /* An engine-internal pthread (e.g. a Metal streaming-expert pread
         * worker): the OCaml runtime does not know it, so calling in would be
         * undefined.  Fall back to stderr. */
        fputs(msg, stderr);
        return;
    }

    const int reacquire = t_runtime_released;
    if (reacquire)
        caml_acquire_runtime_system();
    value v_msg = caml_copy_string(msg);
    /* The closure runs a Logs reporter — arbitrary user code.  Let it raise and
     * the exception would unwind through the engine's C frames, skipping the
     * release below and leaving the engine's own state half-updated, so swallow
     * it here: a failing log sink must not take the inference down with it. */
    value res = caml_callback2_exn(g_log_closure, Val_int(level), v_msg);
    if (Is_exception_result(res))
        fputs("ds4: log handler raised; diagnostic dropped\n", stderr);
    if (reacquire)
        caml_release_runtime_system();
}

/* Install [v_closure] as the diagnostic handler.  Called with an OCaml closure
 * of type [int -> string -> unit] (level, message). */
CAMLprim value caml_ds4_set_log_handler(value v_closure) {
    CAMLparam1(v_closure);
    DS4_MARK_OCAML_THREAD();
    if (!g_log_handler_installed) {
        caml_register_generational_global_root(&g_log_closure);
        g_log_handler_installed = 1;
    }
    caml_modify_generational_global_root(&g_log_closure, v_closure);
    ds4_set_log_callback(ds4_log_trampoline, NULL);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_ds4_clear_log_handler(value v_unit) {
    CAMLparam1(v_unit);
    DS4_MARK_OCAML_THREAD();
    ds4_set_log_callback(NULL, NULL);
    if (g_log_handler_installed)
        caml_modify_generational_global_root(&g_log_closure, Val_unit);
    CAMLreturn(Val_unit);
}

/* Set the minimum severity that crosses into OCaml (0=Error .. 3=Debug, or -1
 * to drop everything).  Lets the firehose of debug diagnostics be discarded in
 * C when the Logs level is below debug. */
CAMLprim value caml_ds4_set_log_min_level(value v_level) {
    CAMLparam1(v_level);
    DS4_MARK_OCAML_THREAD();
    g_log_min = Int_val(v_level);
    CAMLreturn(Val_unit);
}

/* The backend is chosen at link time, so every engine in this process shares
 * it.  Recorded at open so that a session can price its KV cache. */
static ds4_backend g_backend = DS4_BACKEND_METAL;

#define Engine_val(v) (*((ds4_engine **)Data_custom_val(v)))
#define Vision_val(v) ((ds4_vision_span *)Data_custom_val(v))

static void finalize_engine(value v) {
    ds4_engine *e = Engine_val(v);
    if (e) {
        Engine_val(v) = NULL;
        ds4_engine_close(e);
    }
}

static struct custom_operations ds4_engine_ops = {
    "ds4.engine",
    finalize_engine,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static void finalize_vision(value v) {
    ds4_vision_embedding_free(&Vision_val(v)->embedding);
}

static struct custom_operations ds4_vision_ops = {
    "ds4.vision_span",
    finalize_vision,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

CAMLprim value caml_ds4_vision_token_start(value v_span) {
    CAMLparam1(v_span);
    CAMLreturn(Val_int(Vision_val(v_span)->token_start));
}

CAMLprim value caml_ds4_vision_token_count(value v_span) {
    CAMLparam1(v_span);
    CAMLreturn(Val_int(Vision_val(v_span)->embedding.token_count));
}

/* [engine] is a generational global root holding the engine custom block this
 * session belongs to.  ds4_session_free() dereferences s->engine (the tensor-
 * parallel teardown path reads s->engine->tp.ctx), but the GC gives no ordering
 * between the two finalizers — so without this root a collection that drops
 * both at once could close the engine first and free the session through
 * dangling memory.  Rooting the engine here keeps it alive for as long as the
 * session box is, which makes finalize_engine strictly later. */
typedef struct {
    ds4_session *s;
    uint64_t rng;
    value engine;
    /* Tokens prefilled by the sync now running, and tokens it expects.  Written
     * by ds4_prefill_progress below and read by caml_ds4_session_prefill_
     * progress.  See the comment on the trampoline for why they are atomics. */
    _Atomic uint32_t prefill_done;
    _Atomic uint32_t prefill_total;
    _Atomic bool cancel;
} ds4_session_box;

#define Session_box(v) (*((ds4_session_box **)Data_custom_val(v)))

/* =========================================================================
 * Prefill progress.
 *
 * The engine reports "prefill_chunk" as it works through a prompt, which is
 * the only sign a caller gets that a sync of tens of thousands of tokens is
 * moving rather than wedged.  This trampoline is the session's progress hook.
 *
 * It touches no OCaml value, and that is the whole design.  It fires on the
 * engine's worker domain from inside a caml_release_runtime_system() section,
 * where entering OCaml would need the runtime lock reacquired mid-prefill, and
 * the value it produced would then have to cross a domain to reach the fiber
 * waiting to render it.  Two relaxed atomic stores need neither: they are safe
 * from any thread, inside a released section or out of it, and the reader picks
 * them up with caml_ds4_session_prefill_progress without entering the engine at
 * all.  A reader that had to enter the engine would block behind the very sync
 * it wanted to observe.
 *
 * Relaxed ordering is enough because the two counters are read for display and
 * nothing is published through them.
 * ========================================================================= */
static void ds4_prefill_progress(void *ud, const char *event, int current,
                                 int total) {
    ds4_session_box *box = (ds4_session_box *)ud;
    if (!box || !event || strcmp(event, "prefill_chunk") != 0)
        return;
    atomic_store_explicit(&box->prefill_done,
                          current > 0 ? (uint32_t)current : 0u,
                          memory_order_relaxed);
    atomic_store_explicit(&box->prefill_total,
                          total > 0 ? (uint32_t)total : 0u,
                          memory_order_relaxed);
}

static bool ds4_session_cancelled(void *ud) {
    ds4_session_box *box = (ds4_session_box *)ud;
    return box && atomic_load_explicit(&box->cancel, memory_order_relaxed);
}

/* Release the engine-side session, leaving the box for the finalizer.  Called
 * both by finalize_session and by an explicit close. */
static void release_session(ds4_session_box *box) {
    if (box->s) {
        ds4_session_free(box->s);
        box->s = NULL;
    }
}

static void finalize_session(value v) {
    ds4_session_box *box = Session_box(v);
    if (box) {
        Session_box(v) = NULL;
        release_session(box);
        caml_remove_generational_global_root(&box->engine);
        free(box);
    }
}

static struct custom_operations ds4_session_ops = {
    "ds4.session",
    finalize_session,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

CAMLprim value caml_ds4_engine_open(value v_backend, value v_model,
                                    value v_vision, value v_mtp) {
    CAMLparam4(v_backend, v_model, v_vision, v_mtp);
    DS4_MARK_OCAML_THREAD();
    CAMLlocal1(v_engine);

    /* Copy the path out before releasing the runtime lock; the GC may move the
     * OCaml string while we are blocked loading the model. */
    char *model_path = caml_stat_strdup(String_val(v_model));
    char *vision_path = caml_stat_strdup(String_val(v_vision));

    ds4_engine_options opt;
    memset(&opt, 0, sizeof(opt));
    opt.model_path = model_path;
    opt.vision_path = vision_path[0] ? vision_path : NULL;
    /* The id comes from the linked implementation's Backend.id and must match
     * the backend object compiled into this library's archive. */
    opt.backend = (ds4_backend)Int_val(v_backend);
    g_backend = opt.backend;
    opt.mtp_draft_tokens = 1;
    opt.mtp_margin = 3.0f;
    /* Upstream's --mtp: arms the draft head a GLM or Qwen GGUF carries. */
    opt.glm_mtp = Bool_val(v_mtp);

    ds4_engine *engine = NULL;
    DS4_ENTER_BLOCKING();
    int rc = ds4_engine_open(&engine, &opt);
    DS4_LEAVE_BLOCKING();

    if (rc != 0 || engine == NULL) {
        /* Say which model failed.  The engine reports the reason itself and
         * usually exits before returning, so reaching here means it declined
         * without a diagnosis of its own. */
        char err[512];
        snprintf(err, sizeof(err), "cannot open model %s", model_path);
        caml_stat_free(model_path);
        caml_stat_free(vision_path);
        caml_failwith(err);
    }
    caml_stat_free(model_path);
    caml_stat_free(vision_path);

    /* Tell the GC what this handle really costs.  The weights are the bulk of
     * it and live outside the OCaml heap, so with the usual (0, 1) ratio the
     * collector sees a one-word block and has no reason to ever finalize it —
     * leaving a model that can exceed 150 GB resident indefinitely. */
    uint64_t bytes = ds4_engine_model_bytes(engine);
    v_engine = caml_alloc_custom_mem(&ds4_engine_ops, sizeof(ds4_engine *),
                                     bytes ? (size_t)bytes : sizeof(ds4_engine *));
    Engine_val(v_engine) = engine;
    CAMLreturn(v_engine);
}

CAMLprim value caml_ds4_engine_has_vision(value v_engine) {
    CAMLparam1(v_engine);
    DS4_MARK_OCAML_THREAD();
    CAMLreturn(Val_bool(ds4_engine_has_vision(Engine_val(v_engine))));
}

CAMLprim value caml_ds4_engine_mtp_draft_tokens(value v_engine) {
    CAMLparam1(v_engine);
    DS4_MARK_OCAML_THREAD();
    CAMLreturn(Val_int(ds4_engine_mtp_draft_tokens(Engine_val(v_engine))));
}

CAMLprim value caml_ds4_engine_model_name(value v_engine) {
    CAMLparam1(v_engine);
    DS4_MARK_OCAML_THREAD();
    const char *name = ds4_engine_model_name(Engine_val(v_engine));
    CAMLreturn(caml_copy_string(name ? name : ""));
}

CAMLprim value caml_ds4_engine_vocab_size(value v_engine) {
    CAMLparam1(v_engine);
    DS4_MARK_OCAML_THREAD();
    CAMLreturn(Val_int(ds4_engine_vocab_size(Engine_val(v_engine))));
}

CAMLprim value caml_ds4_token_eos(value v_engine) {
    CAMLparam1(v_engine);
    DS4_MARK_OCAML_THREAD();
    CAMLreturn(Val_int(ds4_token_eos(Engine_val(v_engine))));
}

/* Whether a sampled token ends the turn. For a GLM model that is any role
 * marker as well as EOS, so a loop that compares against ds4_token_eos alone
 * runs on into the next simulated turn. */
CAMLprim value caml_ds4_token_is_stop(value v_engine, value v_token) {
    CAMLparam2(v_engine, v_token);
    DS4_MARK_OCAML_THREAD();
    CAMLreturn(
        Val_bool(ds4_token_is_stop(Engine_val(v_engine), Int_val(v_token))));
}

/* The loaded model's family, which decides the prompt markup and the reply
 * grammar an agent must speak to it: 0 DeepSeek V4, 1 GLM, 2 DeepSeek V4.1,
 * 3 Qwen3.8.  V4.family decodes the same numbering. */
CAMLprim value caml_ds4_engine_family(value v_engine) {
    CAMLparam1(v_engine);
    DS4_MARK_OCAML_THREAD();
    ds4_engine *e = Engine_val(v_engine);
    int family = ds4_engine_is_qwen4(e)        ? 3
                 : ds4_engine_is_deepseek41(e) ? 2
                 : ds4_engine_is_glm_dsa(e)    ? 1
                                               : 0;
    CAMLreturn(Val_int(family));
}

/* Mirrors the CLI: MAX effort is downgraded to HIGH below the min context. */
CAMLprim value caml_ds4_think_mode_for_context(value v_mode, value v_ctx) {
    CAMLparam2(v_mode, v_ctx);
    DS4_MARK_OCAML_THREAD();
    ds4_think_mode m =
        ds4_think_mode_for_context((ds4_think_mode)Int_val(v_mode), Int_val(v_ctx));
    CAMLreturn(Val_int((int)m));
}

CAMLprim value caml_ds4_encode_chat_prompt(value v_engine, value v_system,
                                           value v_prompt, value v_think) {
    CAMLparam4(v_engine, v_system, v_prompt, v_think);
    DS4_MARK_OCAML_THREAD();
    CAMLlocal1(v_arr);

    /* Copy both strings out of the OCaml heap before releasing the lock: the
     * GC may move or collect them while we are blocked. Everything else the
     * call needs is either a C pointer or an immediate, read here for the same
     * reason — the custom block holding the engine pointer can itself move,
     * even though the ds4_engine it names cannot. */
    ds4_engine *e = Engine_val(v_engine);
    ds4_think_mode think = (ds4_think_mode)Int_val(v_think);
    char *system = caml_stat_strdup(String_val(v_system));
    char *prompt = caml_stat_strdup(String_val(v_prompt));

    ds4_tokens out;
    memset(&out, 0, sizeof(out));
    /* Tokenising a whole rendered conversation is not free, and an OCaml 5
     * domain sitting in C without releasing the lock stalls stop-the-world
     * collection for every other domain. */
    DS4_ENTER_BLOCKING();
    ds4_encode_chat_prompt(e, system, prompt, think, &out);
    DS4_LEAVE_BLOCKING();

    caml_stat_free(system);
    caml_stat_free(prompt);

    v_arr = caml_alloc(out.len, 0);
    for (int i = 0; i < out.len; i++)
        Field(v_arr, i) = Val_int(out.v[i]);
    ds4_tokens_free(&out);
    CAMLreturn(v_arr);
}

/* Tokenise an already-rendered prompt verbatim, WITHOUT re-applying the chat
 * template (unlike caml_ds4_encode_chat_prompt). The agent renders the whole
 * conversation with the Dsml library, so the template must not be applied a
 * second time here. */
CAMLprim value caml_ds4_tokenize_rendered_chat(value v_engine, value v_text) {
    CAMLparam2(v_engine, v_text);
    DS4_MARK_OCAML_THREAD();
    CAMLlocal1(v_arr);

    /* As in caml_ds4_encode_chat_prompt: the text has to leave the OCaml heap
     * before the lock does. Agents use this for their initial rendered prompt;
     * later turns extend its exact token transcript. */
    ds4_engine *e = Engine_val(v_engine);
    char *text = caml_stat_strdup(String_val(v_text));

    ds4_tokens out;
    memset(&out, 0, sizeof(out));
    DS4_ENTER_BLOCKING();
    ds4_tokenize_rendered_chat(e, text, &out);
    DS4_LEAVE_BLOCKING();

    caml_stat_free(text);

    v_arr = caml_alloc(out.len, 0);
    for (int i = 0; i < out.len; i++)
        Field(v_arr, i) = Val_int(out.v[i]);
    ds4_tokens_free(&out);
    CAMLreturn(v_arr);
}

static void tokens_from_ocaml(value v_tokens, ds4_tokens *out) {
    int n = Wosize_val(v_tokens);
    memset(out, 0, sizeof(*out));
    for (int i = 0; i < n; i++)
        ds4_tokens_push(out, Int_val(Field(v_tokens, i)));
}

static value tokens_to_ocaml(const ds4_tokens *tokens) {
    CAMLparam0();
    CAMLlocal1(v_out);
    v_out = caml_alloc(tokens->len, 0);
    for (int i = 0; i < tokens->len; i++)
        Store_field(v_out, i, Val_int(tokens->v[i]));
    CAMLreturn(v_out);
}

CAMLprim value caml_ds4_tokens_begin(value v_engine) {
    CAMLparam1(v_engine);
    CAMLlocal1(v_out);
    DS4_MARK_OCAML_THREAD();
    ds4_engine *engine = Engine_val(v_engine);
    ds4_tokens tokens = {0};
    ds4_chat_begin(engine, &tokens);
    v_out = tokens_to_ocaml(&tokens);
    ds4_tokens_free(&tokens);
    CAMLreturn(v_out);
}

CAMLprim value caml_ds4_tokens_append_rendered(value v_engine, value v_tokens,
                                               value v_text) {
    CAMLparam3(v_engine, v_tokens, v_text);
    CAMLlocal1(v_out);
    DS4_MARK_OCAML_THREAD();
    ds4_tokens tokens;
    tokens_from_ocaml(v_tokens, &tokens);
    ds4_engine *engine = Engine_val(v_engine);
    char *text = caml_stat_strdup(String_val(v_text));
    DS4_ENTER_BLOCKING();
    ds4_tokenize_rendered_chat(engine, text, &tokens);
    DS4_LEAVE_BLOCKING();
    caml_stat_free(text);
    v_out = tokens_to_ocaml(&tokens);
    ds4_tokens_free(&tokens);
    CAMLreturn(v_out);
}

CAMLprim value caml_ds4_tokens_append_message(value v_engine, value v_tokens,
                                               value v_role, value v_content) {
    CAMLparam4(v_engine, v_tokens, v_role, v_content);
    CAMLlocal1(v_out);
    DS4_MARK_OCAML_THREAD();
    ds4_tokens tokens;
    tokens_from_ocaml(v_tokens, &tokens);
    ds4_engine *engine = Engine_val(v_engine);
    char *role = caml_stat_strdup(String_val(v_role));
    char *content = caml_stat_strdup(String_val(v_content));
    DS4_ENTER_BLOCKING();
    ds4_chat_append_message(engine, &tokens, role, content);
    DS4_LEAVE_BLOCKING();
    caml_stat_free(role);
    caml_stat_free(content);
    v_out = tokens_to_ocaml(&tokens);
    ds4_tokens_free(&tokens);
    CAMLreturn(v_out);
}

CAMLprim value caml_ds4_tokens_append_multimodal(
        value v_engine, value v_tokens, value v_role, value v_parts,
        value v_images) {
    CAMLparam5(v_engine, v_tokens, v_role, v_parts, v_images);
    CAMLlocal4(v_out, v_spans, v_span, v_pair);
    DS4_MARK_OCAML_THREAD();
    const size_t image_count = Wosize_val(v_images);
    if (Wosize_val(v_parts) != image_count + 1)
        caml_invalid_argument(
            "V4.Transcript.append_multimodal_message: text_parts must contain "
            "one more item than images");

    ds4_tokens tokens;
    tokens_from_ocaml(v_tokens, &tokens);
    char *role = caml_stat_strdup(String_val(v_role));
    char **parts = calloc(image_count + 1, sizeof(*parts));
    ds4_vision_embedding *embeddings =
        calloc(image_count, sizeof(*embeddings));
    ds4_vision_span *spans = calloc(image_count, sizeof(*spans));
    uint8_t **encoded = calloc(image_count, sizeof(*encoded));
    size_t *encoded_len = calloc(image_count, sizeof(*encoded_len));
    if (!parts || (image_count && (!embeddings || !spans || !encoded ||
                                   !encoded_len))) {
        ds4_tokens_free(&tokens);
        caml_stat_free(role);
        free(parts); free(embeddings); free(spans); free(encoded);
        free(encoded_len);
        caml_raise_out_of_memory();
    }
    for (size_t i = 0; i <= image_count; i++)
        parts[i] = caml_stat_strdup(String_val(Field(v_parts, i)));
    int copy_ok = 1;
    for (size_t i = 0; i < image_count; i++) {
        value image = Field(v_images, i);
        encoded_len[i] = caml_string_length(image);
        encoded[i] = malloc(encoded_len[i]);
        if (!encoded[i] && encoded_len[i]) {
            copy_ok = 0;
            break;
        }
        if (encoded_len[i])
            memcpy(encoded[i], String_val(image), encoded_len[i]);
    }
    if (!copy_ok) {
        for (size_t i = 0; i <= image_count; i++) caml_stat_free(parts[i]);
        for (size_t i = 0; i < image_count; i++) free(encoded[i]);
        ds4_tokens_free(&tokens);
        caml_stat_free(role);
        free(parts); free(embeddings); free(spans); free(encoded);
        free(encoded_len);
        caml_raise_out_of_memory();
    }

    char err[512] = {0};
    int ok = 1;
    ds4_engine *engine = Engine_val(v_engine);
    DS4_ENTER_BLOCKING();
    for (size_t i = 0; ok && i < image_count; i++)
        ok = ds4_engine_vision_encode_memory(engine, encoded[i], encoded_len[i],
                                             &embeddings[i], err, sizeof(err));
    if (ok)
        ok = ds4_chat_append_multimodal_message(
            engine, &tokens, role, (const char *const *)parts, embeddings,
            image_count, spans, err, sizeof(err));
    DS4_LEAVE_BLOCKING();

    for (size_t i = 0; i <= image_count; i++) caml_stat_free(parts[i]);
    for (size_t i = 0; i < image_count; i++) free(encoded[i]);
    free(parts); free(encoded); free(encoded_len);
    caml_stat_free(role);
    for (size_t i = 0; i < image_count; i++)
        ds4_vision_embedding_free(&embeddings[i]);
    free(embeddings);
    if (!ok) {
        for (size_t i = 0; i < image_count; i++)
            ds4_vision_embedding_free(&spans[i].embedding);
        free(spans);
        ds4_tokens_free(&tokens);
        caml_failwith(err[0] ? err : "multimodal message failed");
    }

    v_out = tokens_to_ocaml(&tokens);
    ds4_tokens_free(&tokens);
    v_spans = caml_alloc(image_count, 0);
    for (size_t i = 0; i < image_count; i++) {
        v_span = caml_alloc_custom_mem(&ds4_vision_ops,
                                       sizeof(ds4_vision_span),
                                       spans[i].embedding.token_count *
                                           4096u * sizeof(float));
        *Vision_val(v_span) = spans[i];
        memset(&spans[i], 0, sizeof(spans[i]));
        Store_field(v_spans, i, v_span);
    }
    free(spans);
    v_pair = caml_alloc_tuple(2);
    Store_field(v_pair, 0, v_out);
    Store_field(v_pair, 1, v_spans);
    CAMLreturn(v_pair);
}

/* The reasoning-effort instruction that opens a conversation, placed as
 * upstream's agent places it.  Qwen takes it as a system turn of its own, and
 * every other family through the engine's think prefix, which writes nothing
 * for a mode that has no instruction. */
CAMLprim value caml_ds4_tokens_append_think_prefix(value v_engine,
                                                   value v_tokens,
                                                   value v_think) {
    CAMLparam3(v_engine, v_tokens, v_think);
    CAMLlocal1(v_out);
    DS4_MARK_OCAML_THREAD();
    ds4_tokens tokens;
    tokens_from_ocaml(v_tokens, &tokens);
    ds4_engine *engine = Engine_val(v_engine);
    ds4_think_mode think = (ds4_think_mode)Int_val(v_think);
    DS4_ENTER_BLOCKING();
    if (ds4_engine_is_qwen4(engine)) {
        const char *effort = ds4_qwen4_reasoning_effort_text(think);
        if (effort) ds4_chat_append_message(engine, &tokens, "system", effort);
    } else {
        ds4_chat_append_think_prefix(engine, &tokens, think);
    }
    DS4_LEAVE_BLOCKING();
    v_out = tokens_to_ocaml(&tokens);
    ds4_tokens_free(&tokens);
    CAMLreturn(v_out);
}

CAMLprim value caml_ds4_tokens_append_assistant_prefix(value v_engine,
                                                        value v_tokens,
                                                        value v_think) {
    CAMLparam3(v_engine, v_tokens, v_think);
    CAMLlocal1(v_out);
    DS4_MARK_OCAML_THREAD();
    ds4_tokens tokens;
    tokens_from_ocaml(v_tokens, &tokens);
    ds4_engine *engine = Engine_val(v_engine);
    ds4_think_mode think = (ds4_think_mode)Int_val(v_think);
    DS4_ENTER_BLOCKING();
    ds4_chat_append_assistant_prefix(engine, &tokens, think);
    DS4_LEAVE_BLOCKING();
    v_out = tokens_to_ocaml(&tokens);
    ds4_tokens_free(&tokens);
    CAMLreturn(v_out);
}

CAMLprim value caml_ds4_session_create(value v_engine, value v_ctx,
                                       value v_seed) {
    CAMLparam3(v_engine, v_ctx, v_seed);
    DS4_MARK_OCAML_THREAD();
    CAMLlocal1(v_session);

    ds4_session *s = NULL;
    ds4_engine *e = Engine_val(v_engine);
    int ctx = Int_val(v_ctx);

    DS4_ENTER_BLOCKING();
    int rc = ds4_session_create(&s, e, ctx);
    DS4_LEAVE_BLOCKING();

    if (rc != 0 || s == NULL)
        caml_failwith("ds4_session_create failed");

    ds4_session_box *box = malloc(sizeof(*box));
    if (!box) {
        ds4_session_free(s);
        caml_failwith("ds4_session_create: out of memory");
    }
    box->s = s;
    box->rng = (uint64_t)Int64_val(v_seed);
    atomic_store_explicit(&box->prefill_done, 0u, memory_order_relaxed);
    atomic_store_explicit(&box->prefill_total, 0u, memory_order_relaxed);
    atomic_store_explicit(&box->cancel, false, memory_order_relaxed);
    /* Register before storing the engine: the root must always name a valid
     * value, and Val_unit is one. */
    box->engine = Val_unit;
    caml_register_generational_global_root(&box->engine);
    caml_modify_generational_global_root(&box->engine, v_engine);

    /* The hook is the box, not the OCaml handle, so it stays valid however the
     * collector moves the custom block naming it. */
    ds4_session_set_progress(s, ds4_prefill_progress, box);
    ds4_session_set_display_progress(s, ds4_prefill_progress, box);
    ds4_session_set_cancel(s, ds4_session_cancelled, box);

    /* A session's cost is its KV cache, which is outside the OCaml heap and
     * grows with the context.  Reporting it is what lets a collection be
     * triggered by dropping one, which matters when a session is replaced by a
     * larger one and both are briefly live. */
    ds4_context_memory mem = ds4_context_memory_estimate(g_backend, ctx);
    v_session = caml_alloc_custom_mem(&ds4_session_ops,
                                      sizeof(ds4_session_box *),
                                      mem.total_bytes ? (size_t)mem.total_bytes
                                                      : sizeof(ds4_session_box));
    Session_box(v_session) = box;
    CAMLreturn(v_session);
}

/* A session whose cache has been released, or one that never had one, must not
 * reach the engine: it dereferences the pointer without checking, so a closed
 * session would be a crash rather than an error. */
static ds4_session_box *live_box(value v_session) {
    ds4_session_box *box = Session_box(v_session);
    if (box == NULL || box->s == NULL)
        caml_failwith("ds4: session is closed");
    return box;
}

/* Release a session's KV cache now rather than at the next collection.  Safe
 * to call twice, and the handle stays valid but unusable afterwards. */
CAMLprim value caml_ds4_session_close(value v_session) {
    CAMLparam1(v_session);
    DS4_MARK_OCAML_THREAD();
    ds4_session_box *box = Session_box(v_session);
    if (box && box->s) {
        ds4_session *s = box->s;
        box->s = NULL;
        DS4_ENTER_BLOCKING();
        ds4_session_free(s);
        DS4_LEAVE_BLOCKING();
    }
    CAMLreturn(Val_unit);
}

CAMLprim value caml_ds4_session_sync(value v_session, value v_tokens) {
    CAMLparam2(v_session, v_tokens);
    DS4_MARK_OCAML_THREAD();

    ds4_session_box *box = live_box(v_session);
    int n = Wosize_val(v_tokens);

    /* Copy tokens into a C buffer before releasing the lock. */
    ds4_tokens prompt;
    memset(&prompt, 0, sizeof(prompt));
    for (int i = 0; i < n; i++)
        ds4_tokens_push(&prompt, Int_val(Field(v_tokens, i)));

    /* Clear before the engine can report anything, so a reader never sees the
     * previous sync's tail as this one's progress. */
    atomic_store_explicit(&box->prefill_done, 0u, memory_order_relaxed);
    atomic_store_explicit(&box->prefill_total, 0u, memory_order_relaxed);

    char err[256] = {0};
    DS4_ENTER_BLOCKING();
    int rc = ds4_session_sync(box->s, &prompt, err, sizeof(err));
    DS4_LEAVE_BLOCKING();

    ds4_tokens_free(&prompt);
    if (rc == DS4_SESSION_SYNC_INTERRUPTED)
        caml_raise_constant(*caml_named_value("ds4.session_interrupted"));
    if (rc != 0)
        caml_failwith(err[0] ? err : "ds4_session_sync failed");
    /* The last chunk the engine reports is not always the last it prefills, and
     * a sync that has returned is done whatever it reported.  Without this a
     * finished prefill would read as stalled just short of its total. */
    atomic_store_explicit(
        &box->prefill_done,
        atomic_load_explicit(&box->prefill_total, memory_order_relaxed),
        memory_order_relaxed);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_ds4_session_sync_multimodal(value v_session,
                                                 value v_tokens,
                                                 value v_images) {
    CAMLparam3(v_session, v_tokens, v_images);
    DS4_MARK_OCAML_THREAD();
    ds4_session_box *box = live_box(v_session);
    ds4_tokens prompt;
    tokens_from_ocaml(v_tokens, &prompt);
    const size_t image_count = Wosize_val(v_images);
    ds4_vision_span *spans = calloc(image_count, sizeof(*spans));
    if (!spans && image_count) {
        ds4_tokens_free(&prompt);
        caml_raise_out_of_memory();
    }
    for (size_t i = 0; i < image_count; i++)
        spans[i] = *Vision_val(Field(v_images, i));
    atomic_store_explicit(&box->prefill_done, 0u, memory_order_relaxed);
    atomic_store_explicit(&box->prefill_total, 0u, memory_order_relaxed);
    char err[256] = {0};
    DS4_ENTER_BLOCKING();
    int rc = ds4_session_sync_multimodal(box->s, &prompt, spans, image_count,
                                         err, sizeof(err));
    DS4_LEAVE_BLOCKING();
    free(spans);
    ds4_tokens_free(&prompt);
    if (rc == DS4_SESSION_SYNC_INTERRUPTED)
        caml_raise_constant(*caml_named_value("ds4.session_interrupted"));
    if (rc != 0)
        caml_failwith(err[0] ? err : "ds4_session_sync_multimodal failed");
    atomic_store_explicit(
        &box->prefill_done,
        atomic_load_explicit(&box->prefill_total, memory_order_relaxed),
        memory_order_relaxed);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_ds4_session_cancel(value v_session) {
    CAMLparam1(v_session);
    ds4_session_box *box = Session_box(v_session);
    if (box)
        atomic_store_explicit(&box->cancel, true, memory_order_relaxed);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_ds4_session_clear_cancel(value v_session) {
    CAMLparam1(v_session);
    ds4_session_box *box = Session_box(v_session);
    if (box)
        atomic_store_explicit(&box->cancel, false, memory_order_relaxed);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_ds4_session_is_cancelled(value v_session) {
    CAMLparam1(v_session);
    ds4_session_box *box = Session_box(v_session);
    bool cancelled = box &&
        atomic_load_explicit(&box->cancel, memory_order_relaxed);
    CAMLreturn(Val_bool(cancelled));
}

CAMLprim value caml_ds4_session_common_prefix(value v_session, value v_tokens) {
    CAMLparam2(v_session, v_tokens);
    DS4_MARK_OCAML_THREAD();
    ds4_session_box *box = live_box(v_session);
    int n = Wosize_val(v_tokens);
    ds4_tokens prompt = {0};
    for (int i = 0; i < n; i++)
        ds4_tokens_push(&prompt, Int_val(Field(v_tokens, i)));
    int common = ds4_session_common_prefix(box->s, &prompt);
    ds4_tokens_free(&prompt);
    CAMLreturn(Val_int(common));
}

CAMLprim value caml_ds4_session_checkpoint_valid(value v_session) {
    CAMLparam1(v_session);
    DS4_MARK_OCAML_THREAD();
    CAMLreturn(Val_bool(ds4_session_checkpoint_valid(live_box(v_session)->s)));
}

CAMLprim value caml_ds4_session_directional_steering_ffn(value v_session) {
    CAMLparam1(v_session);
    ds4_session_box *box = live_box(v_session);
    CAMLreturn(caml_copy_double(ds4_session_directional_steering_ffn(box->s)));
}

CAMLprim value caml_ds4_session_set_directional_steering_ffn(value v_session,
                                                              value v_scale) {
    CAMLparam2(v_session, v_scale);
    ds4_session_box *box = live_box(v_session);
    if (!ds4_session_set_directional_steering_ffn(box->s, Double_val(v_scale)))
        caml_failwith("directional steering is unavailable for this session");
    CAMLreturn(Val_unit);
}

/* Read the counters the progress hook keeps.  This is deliberately not routed
 * through the engine's worker domain: it reads two atomics and touches no
 * engine state, and a caller that had to take the worker would be waiting for
 * the sync it is trying to report on.  A closed session answers (0, 0) rather
 * than raising, since a reader polling alongside another fiber has no say in
 * when the session it is watching is closed. */
CAMLprim value caml_ds4_session_prefill_progress(value v_session) {
    CAMLparam1(v_session);
    CAMLlocal1(v_pair);
    DS4_MARK_OCAML_THREAD();
    uint32_t done = 0, total = 0;
    ds4_session_box *box = Session_box(v_session);
    if (box) {
        done = atomic_load_explicit(&box->prefill_done, memory_order_relaxed);
        total = atomic_load_explicit(&box->prefill_total, memory_order_relaxed);
    }
    v_pair = caml_alloc_tuple(2);
    Field(v_pair, 0) = Val_int((int)done);
    Field(v_pair, 1) = Val_int((int)total);
    CAMLreturn(v_pair);
}

CAMLprim value caml_ds4_session_ctx(value v_session) {
    CAMLparam1(v_session);
    DS4_MARK_OCAML_THREAD();
    CAMLreturn(Val_int(ds4_session_ctx(live_box(v_session)->s)));
}

CAMLprim value caml_ds4_session_pos(value v_session) {
    CAMLparam1(v_session);
    DS4_MARK_OCAML_THREAD();
    CAMLreturn(Val_int(ds4_session_pos(live_box(v_session)->s)));
}

CAMLprim value caml_ds4_session_sample(value v_session, value v_temp,
                                       value v_top_p, value v_min_p) {
    CAMLparam4(v_session, v_temp, v_top_p, v_min_p);
    DS4_MARK_OCAML_THREAD();
    /* Unbox the parameters first — they live in the OCaml heap. The box itself
     * is C memory reached through the custom block, so box->rng stays valid
     * across the release. */
    ds4_session_box *box = live_box(v_session);
    const float temp = (float)Double_val(v_temp);
    const float top_p = (float)Double_val(v_top_p);
    const float min_p = (float)Double_val(v_min_p);

    /* Sampling sorts a ~129k-entry vocabulary; long enough to be worth not
     * stalling every other domain's collection for. */
    int tok;
    DS4_ENTER_BLOCKING();
    tok = ds4_session_sample(box->s, temp, 0 /* top_k */, top_p, min_p,
                             &box->rng);
    DS4_LEAVE_BLOCKING();

    CAMLreturn(Val_int(tok));
}

CAMLprim value caml_ds4_session_eval(value v_session, value v_token) {
    CAMLparam2(v_session, v_token);
    DS4_MARK_OCAML_THREAD();
    ds4_session_box *box = live_box(v_session);
    int token = Int_val(v_token);

    char err[256] = {0};
    DS4_ENTER_BLOCKING();
    int rc = ds4_session_eval(box->s, token, err, sizeof(err));
    DS4_LEAVE_BLOCKING();

    if (rc != 0)
        caml_failwith(err[0] ? err : "ds4_session_eval failed");
    CAMLreturn(Val_unit);
}

/* The most tokens one speculative step commits.  The engine drafts at most two
 * for GLM and Qwen and a DSpark block's size for DeepSeek, so this is slack. */
#define DS4_SPEC_ACCEPTED_CAP 32

/* Commit [v_token], which the caller sampled, and whatever drafted tokens the
 * engine verifies after it, up to [v_max] in all.  [v_sampling] is the
 * temperature, top-p and min-p as a float array, which keeps this stub within
 * the arity a native-only external may take.  Returns the committed tokens,
 * [v_token] first. */
CAMLprim value caml_ds4_session_eval_speculative(value v_session, value v_token,
                                                 value v_max, value v_sampling) {
    CAMLparam4(v_session, v_token, v_max, v_sampling);
    CAMLlocal1(v_accepted);
    DS4_MARK_OCAML_THREAD();
    ds4_session_box *box = live_box(v_session);
    ds4_engine *e = Engine_val(box->engine);
    const int token = Int_val(v_token);
    int max_tokens = Int_val(v_max);
    if (max_tokens < 1) max_tokens = 1;
    const float temp = (float)Double_flat_field(v_sampling, 0);
    const float top_p = (float)Double_flat_field(v_sampling, 1);
    const float min_p = (float)Double_flat_field(v_sampling, 2);

    int accepted[DS4_SPEC_ACCEPTED_CAP];
    char err[256] = {0};
    DS4_ENTER_BLOCKING();
    const int eos = ds4_token_eos(e);
    int n = ds4_session_eval_speculative(box->s, token, max_tokens, eos, temp,
                                         0 /* top_k */, top_p, min_p,
                                         &box->rng, accepted,
                                         DS4_SPEC_ACCEPTED_CAP, err,
                                         sizeof(err));
    DS4_LEAVE_BLOCKING();

    if (n <= 0)
        caml_failwith(err[0] ? err : "ds4_session_eval_speculative failed");
    v_accepted = caml_alloc_tuple((mlsize_t)n);
    for (int i = 0; i < n; i++)
        Store_field(v_accepted, i, Val_int(accepted[i]));
    CAMLreturn(v_accepted);
}

/* Drop the tokens after [v_pos] from the session's cache, so that a following
 * sync of a prompt that shares only that prefix is not a full refill. */
CAMLprim value caml_ds4_session_rewind(value v_session, value v_pos) {
    CAMLparam2(v_session, v_pos);
    DS4_MARK_OCAML_THREAD();
    ds4_session_box *box = live_box(v_session);
    const int pos = Int_val(v_pos);
    DS4_ENTER_BLOCKING();
    ds4_session_rewind(box->s, pos);
    DS4_LEAVE_BLOCKING();
    CAMLreturn(Val_unit);
}

CAMLprim value caml_ds4_token_text(value v_engine, value v_token) {
    CAMLparam2(v_engine, v_token);
    DS4_MARK_OCAML_THREAD();
    CAMLlocal1(v_str);
    size_t len = 0;
    char *piece = ds4_token_text(Engine_val(v_engine), Int_val(v_token), &len);
    v_str = caml_alloc_initialized_string(len, piece);
    free(piece); /* ds4_token_text returns malloc'd memory the caller owns */
    CAMLreturn(v_str);
}
