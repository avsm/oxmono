#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int32_t ame_available(void) { return 0; }

int32_t ame_memory_stats(int64_t *active, int64_t *cache, int64_t *peak,
                         int64_t *limit) {
  (void)active;
  (void)cache;
  (void)peak;
  (void)limit;
  return 1;
}

static int32_t unavailable(char **error) {
  *error = strdup("MLX embeddings require an Apple silicon macOS build");
  return 1;
}

int32_t ame_load(const char *path, int32_t max_tokens,
                 void **output, char **error) {
  (void)path;
  (void)max_tokens;
  *output = NULL;
  return unavailable(error);
}

int32_t ame_embed(void *handle, const char *const *inputs, int32_t count,
                  int32_t dimensions, float *output, char **error) {
  (void)handle;
  (void)inputs;
  (void)count;
  (void)dimensions;
  (void)output;
  return unavailable(error);
}

void ame_release(void *handle) { (void)handle; }

int32_t ame_vision_load(const char *path, int32_t max_tokens,
                        void **output, char **error) {
  return ame_load(path, max_tokens, output, error);
}

int32_t ame_vision_embed(void *handle, int32_t images, const char *instruction,
                         const char *const *inputs, int32_t count,
                         int32_t dimensions, float *output, char **error) {
  (void)images;
  (void)instruction;
  return ame_embed(handle, inputs, count, dimensions, output, error);
}

void ame_vision_release(void *handle) { (void)handle; }
