
#define CAML_INTERNALS

#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/version.h"
#include "caml/sys.h"
#include "caml/signals.h"

#include "blocking_mutex.h"
#include "blocking_condition.h"

// In native code, this weak symbol will be overridden when linked with threads.
// In bytecode, it will do nothing. We only use bytecode for jsoo internally, but this
// is a problem for upstream bytecode.
CAMLweakdef CAMLprim value caml_thread_yield(__attribute__((unused)) value unit) {
  return Val_unit;
}

// Copy-pasted from [runtime/sync_posix.h] which is not included in the distribution.
Caml_inline void capsule_check_error(int rc, char *msg) {
  char *err;
  char buf[1024];
  int errlen, msglen;
  value str;

  if (rc == 0)
    return;
  if (rc == ENOMEM)
    caml_raise_out_of_memory();
  err = caml_strerror(rc, buf, sizeof(buf));
  msglen = strlen(msg);
  errlen = strlen(err);
  str = caml_alloc_string(msglen + 2 + errlen);
  memcpy(&Byte(str, 0), msg, msglen);
  memcpy(&Byte(str, msglen), ": ", 2);
  memcpy(&Byte(str, msglen + 2), err, errlen);
  caml_raise_sys_error(str);
}

/* Mutex ops, ported from the runtime implementations */
static void caml_blocking_mutex_finalize(value wrapper) {
  blocking_mutex_destroy(Mutex_val(wrapper));
}

static int caml_blocking_mutex_compare(value wrapper1, value wrapper2) {
  blocking_mutex rwl1 = Mutex_val(wrapper1);
  blocking_mutex rwl2 = Mutex_val(wrapper2);
  return rwl1 == rwl2 ? 0 : rwl1 < rwl2 ? -1 : 1;
}

static intnat caml_blocking_mutex_hash(value wrapper) {
  return (intnat)Mutex_val(wrapper);
}

static struct custom_operations caml_blocking_mutex_ops = {
    "_blocking_mutex",          caml_blocking_mutex_finalize, caml_blocking_mutex_compare,
    caml_blocking_mutex_hash,   custom_serialize_default,     custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value caml_blocking_mutex_new(__attribute__((unused)) value unit) {
  blocking_mutex mut = NULL;
  capsule_check_error(blocking_mutex_create(&mut), "Mutex.create");
  value wrapper =
      caml_alloc_custom(&caml_blocking_mutex_ops, sizeof(blocking_mutex *), 0, 1);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

CAMLprim value caml_blocking_mutex_lock(value wrapper) {
  CAMLparam1(wrapper);
  blocking_mutex mut = Mutex_val(wrapper);
  capsule_check_error(blocking_mutex_lock(mut), "Mutex.lock");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_blocking_mutex_unlock(value wrapper) {
  CAMLparam1(wrapper);
  blocking_mutex mut = Mutex_val(wrapper);
  capsule_check_error(blocking_mutex_unlock(mut), "Mutex.unlock");
  CAMLreturn(Val_unit);
}

/* Condition ops, ported from the runtime implementations */
static void caml_blocking_condition_finalize(value wrapper) {
  blocking_condition_destroy(Condition_val(wrapper));
}

static int caml_blocking_condition_compare(value wrapper1, value wrapper2) {
  blocking_condition rwl1 = Condition_val(wrapper1);
  blocking_condition rwl2 = Condition_val(wrapper2);
  return rwl1 == rwl2 ? 0 : rwl1 < rwl2 ? -1 : 1;
}

static intnat caml_blocking_condition_hash(value wrapper) {
  return (intnat)(Condition_val(wrapper));
}

static struct custom_operations caml_blocking_condition_ops = {
    "_blocking_condition",           caml_blocking_condition_finalize,
    caml_blocking_condition_compare, caml_blocking_condition_hash,
    custom_serialize_default,        custom_deserialize_default,
    custom_compare_ext_default,      custom_fixed_length_default};

CAMLprim value caml_blocking_condition_new(__attribute__((unused)) value unit) {
  blocking_condition cond = NULL;
  capsule_check_error(blocking_condition_create(&cond), "Condition.create");
  value wrapper =
      caml_alloc_custom(&caml_blocking_condition_ops, sizeof(blocking_condition *), 0, 1);
  Condition_val(wrapper) = cond;
  return wrapper;
}

CAMLprim value caml_blocking_condition_signal(value wrapper) {
  CAMLparam1(wrapper);
  blocking_condition cond = Condition_val(wrapper);
  capsule_check_error(blocking_condition_signal(cond), "Condition.signal");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_blocking_condition_broadcast(value wrapper) {
  CAMLparam1(wrapper);
  blocking_condition mut = Condition_val(wrapper);
  capsule_check_error(blocking_condition_broadcast(mut), "Condition.broadcast");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_blocking_condition_wait(value condition_wrapper,
                                            value mutex_wrapper) {
  CAMLparam2(condition_wrapper, mutex_wrapper);
  blocking_condition cond = Condition_val(condition_wrapper);
  blocking_mutex mut = Mutex_val(mutex_wrapper);
  capsule_check_error(blocking_condition_wait(cond, mut), "Condition.wait");
  CAMLreturn(Val_unit);
}
