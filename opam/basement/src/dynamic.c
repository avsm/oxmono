#include <caml/config.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

// Define bindings using "basement_*" to avoid clashing with defs in other libraries.

#ifdef CAML_RUNTIME_5

value basement_dynamic_supported(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_true);
}


extern value caml_alloc_stack_bind(value, value, value, value, value);
extern value caml_dynamic_make(value);
extern value caml_dynamic_get(value);

value basement_alloc_stack_bind(value hval, value hexn, value heff, value dyn,
                                value val) {
  return caml_alloc_stack_bind(hval, hexn, heff, dyn, val);
}

value basement_dynamic_make(value init) { return caml_dynamic_make(init); }

value basement_dynamic_get(value init) { return caml_dynamic_get(init); }

#else

value basement_dynamic_supported(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_false);
}

/*
 * Stub versions of functions for runtime dynamics, only defined on runtime4 to avoid link
 * errors. These functions are never actually called if runtime5() returns false.
 */

#define unsupported() caml_failwith("Fiber-based dynamics are not supported on runtime 4")

value basement_alloc_stack_bind(value hval, value hexn, value heff, value dyn,
                                value val) {
  CAMLparam5(hval, hexn, heff, dyn, val);
  unsupported();
  CAMLreturn(Val_unit);
}

value basement_dynamic_make(value init) {
  CAMLparam1(init);
  unsupported();
  CAMLreturn(Val_unit);
}

value basement_dynamic_get(value init) {
  CAMLparam1(init);
  unsupported();
  CAMLreturn(Val_unit);
}

#endif
