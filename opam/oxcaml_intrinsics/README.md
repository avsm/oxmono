# oxcaml_intrinsics

This library re-exports all modules from `ocaml_intrinsics`, along with OxCaml-only functions for operating on unboxed values.

## Native_pointer

The `Native_pointer` module provides OxCaml-specific functionality for loading unboxed values from native pointers.

## Re-exported modules

All other modules are re-exported from `ocaml_intrinsics`:
- Atomic
- Atomic_expert
- Bigstring_intf
- Conditional
- Crc
- Ext_pointer
- Fences
- Float
- Int
- Int32
- Int64
- Nativeint
- Perfmon
- Prefetch
- Prefetch_expert
