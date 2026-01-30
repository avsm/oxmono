(module
   (import "env" "caml_raise_sys_error" (func $caml_raise_sys_error (param (ref eq))))
   (import "env" "custom_next_id" (func $custom_next_id (result i64)))
   (import "env" "custom_compare_id" (func $custom_compare_id (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (import "env" "custom_hash_id" (func $custom_hash_id (param (ref eq)) (result i32)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $bytes (array (mut i8)))
   (type $compare
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $hash
      (func (param (ref eq)) (result i32)))
   (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
   (type $serialize
      (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
   (type $deserialize (func (param (ref eq)) (result (ref eq)) (result i32)))
   (type $dup (func (param (ref eq)) (result (ref eq))))
   (type $custom_operations
      (struct
         (field $id (ref $bytes))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         (field $deserialize (ref null $deserialize))
         (field $dup (ref null $dup))))
   (type $custom (sub (struct (field (ref $custom_operations)))))
   (type $custom_with_id
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field $id i64))))

   (global $mutex_ops (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_capsule_mutex")
         (ref.func $custom_compare_id)
         (ref.null $compare)
         (ref.func $custom_hash_id)
         (ref.null $fixed_length)
         (ref.null $serialize)
         (ref.null $deserialize)
         (ref.null $dup)))

   (type $mutex
      (sub final $custom_with_id
         (struct
            (field (ref $custom_operations))
            (field i64)
            (field $state (mut i32)))))

   (func (export "caml_blocking_mutex_new") (param (ref eq)) (result (ref eq))
      (struct.new $mutex
         (global.get $mutex_ops) (call $custom_next_id) (i32.const 0)))

   (@string $lock_failure "Mutex.lock: mutex already locked.")

   (func (export "caml_blocking_mutex_lock") (param (ref eq)) (result (ref eq))
      (local $t (ref $mutex))
      (local.set $t (ref.cast (ref $mutex) (local.get 0)))
      (if (struct.get $mutex $state (local.get $t))
         (then (call $caml_raise_sys_error (global.get $lock_failure))))
      (struct.set $mutex $state (local.get $t) (i32.const 1))
      (ref.i31 (i32.const 0)))

   (func (export "caml_blocking_mutex_unlock") (param (ref eq)) (result (ref eq))
      (struct.set $mutex $state
         (ref.cast (ref $mutex) (local.get 0)) (i32.const 0))
      (ref.i31 (i32.const 0)))

   (func (export "caml_blocking_condition_new") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (@string $condition_failure "Condition.wait: cannot wait.")

   (func (export "caml_blocking_condition_wait")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_raise_sys_error (global.get $condition_failure))
      (ref.i31 (i32.const 0)))

   (func (export "caml_blocking_condition_signal") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_thread_yield") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_blocking_condition_broadcast")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_is_runtime5_stub") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_compare_exchange_stub")
      (param $ref (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (if (result (ref eq))
         (ref.eq (local.get $old) (local.get $o))
         (then
            (array.set $block (local.get $b) (i32.const 1) (local.get $n))
            (local.get $old))
         (else
            (local.get $old))))

   (func (export "caml_atomic_set_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (array.set $block (ref.cast (ref $block) (local.get $ref)) (i32.const 1)
         (local.get $i))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_add_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.add (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_sub_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.sub (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_land_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.and (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_lor_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.or (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_lxor_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.xor (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (@string $dynamics_failure "Fiber-based dynamics are not supported in wasm")

   (func (export "basement_dynamic_supported") (param (ref eq)) (result (ref eq))
     (ref.i31 (i32.const 0)))

   (func $unsupported_dynamics
      (call $caml_raise_sys_error (global.get $dynamics_failure)))

   (func (export "basement_alloc_stack_bind") (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $unsupported_dynamics)
      (ref.i31 (i32.const 0)))

   (func (export "basement_dynamic_make") (param (ref eq)) (result (ref eq))
      (call $unsupported_dynamics)
      (ref.i31 (i32.const 0)))

   (func (export "basement_dynamic_get") (param (ref eq)) (result (ref eq))
      (call $unsupported_dynamics)
      (ref.i31 (i32.const 0)))
)
