#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/fail.h>
CAMLprim value ox_k7_signal_to_int(value signal) {
 int n = caml_convert_signal_number(Int_val(signal));
 if (n < 0) caml_invalid_argument("Sys.signal_to_int");
 return Val_int(n);
}
