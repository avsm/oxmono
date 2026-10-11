#include <caml/mlvalues.h>
#include <caml/fail.h>
#include "sqlite-vec.h"

CAMLprim value caml_sqlite_vec_register(value unit) {
  int rc = sqlite3_auto_extension((void (*)(void))sqlite3_vec_init);
  if (rc != SQLITE_OK) caml_failwith("sqlite-vec registration failed");
  return Val_unit;
}
