
#define CAML_INTERNALS

#include <caml/version.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

// We can't use [#ifdef CAML_RUNTIME_5] here, because upstream doesn't define it. This
// seems to be the only way to accept js and upstream runtime4 and not js or upstream
// runtime5
#if (OCAML_VERSION_MAJOR <= 4) || (defined JANE_STREET_HAS_NO_DOMAINS)

CAMLprim value caml_is_runtime5_stub(value unit) {
  (void)unit;
  return Val_false;
}

// Copied from the flambda-backend 4 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_compare_exchange_stub(value ref, value oldv, value newv) {
  value *p = Op_val(ref);
  if (*p == oldv) {
    caml_modify(p, newv);
    return oldv;
  } else {
    return *p;
  }
}

// Implemented manually, fallback implementation for shims.
CAMLprim value caml_atomic_set_stub(value ref, value newv) {
  value *p = Op_val(ref);
  caml_modify(p, newv);
  return Val_unit;
}

// Copied from the flambda-backend 4 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_add_stub(value ref, value incr) {
  value *p = Op_val(ref);
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) + Long_val(incr));
  return Val_unit;
}

// Copied from the flambda-backend 4 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_sub_stub(value ref, value incr) {
  value *p = Op_val(ref);
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) - Long_val(incr));
  return Val_unit;
}

// Copied from the flambda-backend 4 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_land_stub(value ref, value incr) {
  value *p = Op_val(ref);
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) & Long_val(incr));
  return Val_unit;
}

// Copied from the flambda-backend 4 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_lor_stub(value ref, value incr) {
  value *p = Op_val(ref);
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) | Long_val(incr));
  return Val_unit;
}

// Copied from the flambda-backend 4 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_lxor_stub(value ref, value incr) {
  value *p = Op_val(ref);
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) ^ Long_val(incr));
  return Val_unit;
}

#else

#include <caml/camlatomic.h>
#include <caml/domain.h>

CAMLprim value caml_is_runtime5_stub(value unit) {
  (void)unit;
  return Val_true;
}

// Copied (with verbatim comments) from the upstream OCaml 5 runtime, which does not
// export [write_barrier]. Its logic differs from the flambda-backend runtime, which is
// okay because the existing implementation of [caml_atomic_compare_exchange] in
// flambda-backend will be used whenever it is available.
Caml_inline void write_barrier(value obj, intnat field, value old_val, value new_val) {
  /* HACK: can't assert when get old C-api style pointers
    CAMLassert (Is_block(obj)); */

  if (!Is_young(obj)) {

    if (Is_block(old_val)) {
      /* if old is in the minor heap,
         then this is in a remembered set already */
      if (Is_young(old_val))
        return;
      /* old is a block and in the major heap */
      caml_darken(Caml_state, old_val, 0);
    }
    /* this update is creating a new link from major to minor, remember it */
    if (Is_block_and_young(new_val)) {
      Ref_table_add(&Caml_state->minor_tables->major_ref, Op_val(obj) + field);
    }
  }
}

// Copied from the flambda-backend 5 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_exchange_stub(value ref, value v) {
  value ret;
  if (caml_domain_alone()) {
    ret = Field(ref, 0);
    Field(ref, 0) = v;
  } else {
    atomic_thread_fence(memory_order_acquire);
    ret = atomic_exchange(Op_atomic_val(ref), v);
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  write_barrier(ref, 0, ret, v);
  return ret;
}

// Copied from the flambda-backend 5 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_compare_exchange_stub(value ref, value oldv, value newv) {
  if (caml_domain_alone()) {
    value *p = Op_val(ref);
    if (*p == oldv) {
      *p = newv;
      write_barrier(ref, 0, oldv, newv);
      return oldv;
    } else {
      return *p;
    }
  } else {
    atomic_value *p = &Op_atomic_val(ref)[0];
    int cas_ret = atomic_compare_exchange_strong(p, &oldv, newv);
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
    if (cas_ret) {
      write_barrier(ref, 0, oldv, newv);
    }
    return oldv;
  }
}

// Implemented manually, used in public release.
CAMLprim value caml_atomic_set_stub(value ref, value newv) {
  (void)caml_atomic_exchange_stub(ref, newv);
  return Val_unit;
}

// Copied from the flambda-backend 5 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_add_stub(value ref, value incr) {
  if (caml_domain_alone()) {
    value *p = Op_val(ref);
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) + Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[0];
    atomic_fetch_add(p, 2 * Long_val(incr));   /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

// Copied from the flambda-backend 5 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_sub_stub(value ref, value incr) {
  if (caml_domain_alone()) {
    value *p = Op_val(ref);
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) - Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[0];
    atomic_fetch_sub(p, 2 * Long_val(incr));   /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

// Copied from the flambda-backend 5 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_land_stub(value ref, value incr) {
  if (caml_domain_alone()) {
    value *p = Op_val(ref);
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) & Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[0];
    atomic_fetch_and(p, incr);                 /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

// Copied from the flambda-backend 5 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_lor_stub(value ref, value incr) {
  if (caml_domain_alone()) {
    value *p = Op_val(ref);
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) | Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[0];
    atomic_fetch_or(p, incr);                  /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

// Copied from the flambda-backend 5 runtime. Weak symbol so that it will be overridden
// when linking against the flambda-backend runtime.
CAMLweakdef value caml_atomic_lxor_stub(value ref, value incr) {
  if (caml_domain_alone()) {
    value *p = Op_val(ref);
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) ^ Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[0];
    atomic_fetch_xor(p, 2 * Long_val(incr));   /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

#endif
