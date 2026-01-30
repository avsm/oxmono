include Ocaml_intrinsics.Native_pointer

(** Intrinsics for unboxed types. *)
module Unboxed = struct
  external load_unboxed_nativeint
    :  t
    -> nativeint#
    = "caml_native_pointer_load_unboxed_nativeint_bytecode"
      "caml_native_pointer_load_unboxed_nativeint"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

  external store_unboxed_nativeint
    : ('a : word).
    (t[@unboxed]) -> local_ ('a[@unboxed]) -> unit
    = "caml_native_pointer_store_unboxed_nativeint_bytecode"
      "caml_native_pointer_store_unboxed_nativeint"
  [@@noalloc] [@@builtin] [@@no_coeffects]

  external load_unboxed_int64
    :  t
    -> int64#
    = "caml_native_pointer_load_unboxed_int64_bytecode"
      "caml_native_pointer_load_unboxed_int64"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

  external store_unboxed_int64
    : ('a : bits64).
    (t[@unboxed]) -> local_ ('a[@unboxed]) -> unit
    = "caml_native_pointer_store_unboxed_int64_bytecode"
      "caml_native_pointer_store_unboxed_int64"
  [@@noalloc] [@@builtin] [@@no_coeffects]

  external load_unboxed_int32
    :  t
    -> int32#
    = "caml_native_pointer_load_unboxed_int32_bytecode"
      "caml_native_pointer_load_unboxed_int32"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

  external store_unboxed_int32
    : ('a : bits32).
    (t[@unboxed]) -> local_ ('a[@unboxed]) -> unit
    = "caml_native_pointer_store_unboxed_int32_bytecode"
      "caml_native_pointer_store_unboxed_int32"
  [@@noalloc] [@@builtin] [@@no_coeffects]

  external load_unboxed_float
    :  t
    -> float#
    = "caml_native_pointer_load_unboxed_float_bytecode"
      "caml_native_pointer_load_unboxed_float"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

  external store_unboxed_float
    : ('a : float64).
    (t[@unboxed]) -> local_ ('a[@unboxed]) -> unit
    = "caml_native_pointer_store_unboxed_float_bytecode"
      "caml_native_pointer_store_unboxed_float"
  [@@noalloc] [@@builtin] [@@no_coeffects]

  module Unchecked = struct
    external load_unboxed_nativeint
      : ('a : word).
      t -> 'a
      = "caml_native_pointer_load_unboxed_nativeint_bytecode"
        "caml_native_pointer_load_unboxed_nativeint"
    [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

    external load_unboxed_int64
      : ('a : bits64).
      t -> 'a
      = "caml_native_pointer_load_unboxed_int64_bytecode"
        "caml_native_pointer_load_unboxed_int64"
    [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

    external load_unboxed_int32
      : ('a : bits32).
      t -> 'a
      = "caml_native_pointer_load_unboxed_int32_bytecode"
        "caml_native_pointer_load_unboxed_int32"
    [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

    external load_unboxed_float
      : ('a : float64).
      t -> 'a
      = "caml_native_pointer_load_unboxed_float_bytecode"
        "caml_native_pointer_load_unboxed_float"
    [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]
  end
end

module Unchecked = struct
  external load_immediate
    : ('a : immediate64).
    (t[@unboxed]) -> 'a
    = "caml_native_pointer_load_immediate_bytecode" "caml_native_pointer_load_immediate"
  [@@noalloc] [@@builtin] [@@no_effects]
end

external store_immediate
  : ('a : immediate64).
  (t[@unboxed]) -> 'a -> unit
  = "caml_native_pointer_store_immediate_bytecode" "caml_native_pointer_store_immediate"
[@@noalloc] [@@builtin] [@@no_coeffects]
