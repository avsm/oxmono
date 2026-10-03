#include <stdint.h>

/* Code 1 maps to Apple_speech.Error Unavailable. No Swift runtime is needed. */
int32_t asp_available(void) { return 0; }

int32_t asp_locales(int32_t installed, char **output) {
  (void)installed;
  *output = 0;
  return 1;
}

int32_t asp_status(const char *locale, char **output) {
  (void)locale;
  *output = 0;
  return 1;
}

int32_t asp_install(const char *locale, char **output) {
  return asp_status(locale, output);
}

int32_t asp_transcribe(const char *path, const char *locale, int32_t install,
                       char **output) {
  (void)path;
  (void)install;
  return asp_status(locale, output);
}

int32_t asp_duration(const char *path, char **output) {
  return asp_status(path, output);
}
