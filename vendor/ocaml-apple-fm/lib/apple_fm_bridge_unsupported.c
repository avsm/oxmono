#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Match the Swift bridge's error protocol, including ownership of strings. */
static int32_t unsupported(char **error) {
  *error = strdup("{\"kind\":\"unsupported_version\","
                  "\"message\":\"Apple Foundation Models requires macOS\"}");
  return 1;
}

int32_t afm_availability(char **detail) {
  *detail = strdup("Apple Foundation Models requires macOS");
  return 4;
}

int32_t afm_model_info(const char *config, const char *locale, char **output,
                       char **error) {
  (void)config;
  (void)locale;
  *output = NULL;
  return unsupported(error);
}

int32_t afm_model_token_count(const char *config, int32_t kind,
                              const char *payload, int64_t *count,
                              char **error) {
  (void)config;
  (void)kind;
  (void)payload;
  *count = -1;
  return unsupported(error);
}

int32_t afm_transcript_compact(const char *transcript, const char *summary,
                               int32_t keep_last_turns, char **output,
                               char **error) {
  (void)keep_last_turns;
  return afm_model_info(transcript, summary, output, error);
}

void *afm_session_create(const char *instructions, const char *tools,
                         const char *config, const char *transcript,
                         char **error) {
  (void)instructions;
  (void)tools;
  (void)config;
  (void)transcript;
  unsupported(error);
  return NULL;
}

/* No session can be created on this platform. Cleanup remains harmless. */
void afm_session_destroy(void *session) { (void)session; }
void afm_session_cancel(void *session) { (void)session; }

int32_t afm_session_prewarm(void *session, const char *prefix, char **error) {
  (void)session;
  (void)prefix;
  return unsupported(error);
}

int32_t afm_session_start(void *session, const char *prompt, const char *schema,
                          int32_t include_schema, const char *reasoning,
                          int32_t sampling, double temperature,
                          int32_t has_seed, uint64_t seed,
                          int32_t has_probability, double probability,
                          int32_t top_k, int32_t max_tokens, char **error) {
  (void)session;
  (void)prompt;
  (void)schema;
  (void)include_schema;
  (void)reasoning;
  (void)sampling;
  (void)temperature;
  (void)has_seed;
  (void)seed;
  (void)has_probability;
  (void)probability;
  (void)top_k;
  (void)max_tokens;
  return unsupported(error);
}

int32_t afm_session_next_event(void *session, int32_t timeout, int32_t *kind,
                               int64_t *id, char **first, char **second) {
  (void)session;
  (void)timeout;
  *kind = 0;
  *id = 0;
  *first = NULL;
  *second = NULL;
  return 1;
}

int32_t afm_session_resolve_tool(void *session, int64_t id, const char *result,
                                 char **error) {
  (void)id;
  return afm_session_prewarm(session, result, error);
}

int32_t afm_session_transcript(void *session, char **output, char **error) {
  (void)session;
  *output = NULL;
  return unsupported(error);
}

int32_t afm_session_replace_transcript(void *session, const char *transcript,
                                       char **error) {
  return afm_session_prewarm(session, transcript, error);
}

int32_t afm_session_usage(void *session, char **output, char **error) {
  return afm_session_transcript(session, output, error);
}
