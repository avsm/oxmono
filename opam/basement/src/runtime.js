
//Provides: caml_is_runtime5_stub
function caml_is_runtime5_stub(unit) {
  return 0;
}

//Provides: caml_atomic_compare_exchange_stub
function caml_atomic_compare_exchange_stub(ref, o, n) {
  var old = ref[1];
  if (old === o) {
    ref[1] = n;
  }
  return old;
}

//Provides: caml_atomic_set_stub
function caml_atomic_set_stub(ref, i) {
  ref[1] = i;
  return 0;
}

//Provides: caml_atomic_add_stub
function caml_atomic_add_stub(ref, i) {
  ref[1] += i;
  return 0;
}

//Provides: caml_atomic_sub_stub
function caml_atomic_sub_stub(ref, i) {
  ref[1] -= i;
  return 0;
}

//Provides: caml_atomic_land_stub
function caml_atomic_land_stub(ref, i) {
  ref[1] &= i;
  return 0;
}

//Provides: caml_atomic_lor_stub
function caml_atomic_lor_stub(ref, i) {
  ref[1] |= i;
  return 0;
}

//Provides: caml_atomic_lxor_stub
function caml_atomic_lxor_stub(ref, i) {
  ref[1] ^= i;
  return 0;
}

//Provides: BlockingMutex
class BlockingMutex {
  constructor() {
    this.locked = false;
  }
}

//Provides: caml_blocking_mutex_new
//Requires: BlockingMutex
function caml_blocking_mutex_new(unit) {
  return new BlockingMutex();
}

//Provides: caml_blocking_mutex_lock
//Requires: caml_raise_sys_error
function caml_blocking_mutex_lock(t) {
  if (t.locked) caml_raise_sys_error("Mutex.lock: mutex already locked.");
  else t.locked = true;
  return 0;
}

//Provides: caml_blocking_mutex_unlock
function caml_blocking_mutex_unlock(t) {
  t.locked = false;
  return 0;
}

//Provides: caml_blocking_condition_new
function caml_blocking_condition_new(unit) {
  return { condition: 1 };
}

//Provides: caml_blocking_condition_wait
//Requires: caml_raise_sys_error
function caml_blocking_condition_wait(t, mutext) {
  caml_raise_sys_error("Condition.wait: cannot wait.");
  return 0;
}

//Provides: caml_blocking_condition_broadcast
function caml_blocking_condition_broadcast(t) {
  return 0;
}

//Provides: caml_blocking_condition_signal
function caml_blocking_condition_signal(t) {
  return 0;
}

//Provides: caml_thread_yield
function caml_thread_yield(unit) {
  return 0;
}

//Provides: basement_dynamic_supported
function basement_dynamic_supported(unit) {
  return 0;
}

//Provides: unsupported_dynamics
//Requires: caml_raise_sys_error
function unsupported_dynamics() {
  caml_raise_sys_error("Fiber-based dynamics are not supported in javascript");
}

//Provides: basement_alloc_stack_bind
//Requires: unsupported_dynamics
function basement_alloc_stack_bind(t1, t2, t3, t4, t5) {
  unsupported_dynamics();
}

//Provides: basement_dynamic_make
//Requires: unsupported_dynamics
function basement_dynamic_make(t) {
  unsupported_dynamics();
}

//Provides: basement_dynamic_get
//Requires: unsupported_dynamics
function basement_dynamic_get(t) {
  unsupported_dynamics();
}
