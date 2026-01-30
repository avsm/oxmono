(** Test that the [[%template]]-less monomorphization works. *)

[@@@disable_unused_warnings]

[%%template
[@@@kind.default k = (value, any)]
[@@@mode.default m = (global, local)]
[@@@modality.default n = (nonportable, portable)]

let func x = x [@@alloc a @ m = (heap_global, stack_local)]

module Module = struct end

type ty = unit

module type module_type]

[@@@expand_inline
  let _a = (func [@mode local] [@kind any] [@modality portable] [@alloc stack])]

let _a = func__any__local__portable__stack

[@@@end]
[@@@expand_inline let _b = (func [@mode local]) [%template func [@mode local]]]

let _b = func__local func__local

[@@@end]
[@@@expand_inline module Foo = Module [@modality portable]]

module Foo = Module__portable

[@@@end]
[@@@expand_inline type _c = (ty[@kind any])]

type _c = ty__any

[@@@end]
[@@@expand_inline module type _d = module_type [@modality portable]]

module type _d = module_type__portable

[@@@end]
[@@@expand_inline let _e : (ty[@kind any]) = ()]

let _e : ty__any = ()

[@@@end]
